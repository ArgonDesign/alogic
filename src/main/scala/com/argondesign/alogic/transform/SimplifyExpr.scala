////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Constant expression folding
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.transform

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees.Expr.Integral
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Math.clog2
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.BigIntOps._
import com.argondesign.alogic.util.PartialMatch
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.language.implicitConversions

final class SimplifyExpr(implicit cc: CompilerContext)
    extends StatelessTreeTransformer
    with PartialMatch {

  implicit private def boolean2BigInt(bool: Boolean): BigInt = if (bool) BigInt(1) else BigInt(0)

  private val shiftOps = Set("<<", ">>", "<<<", ">>>")

  private def foldShiftUnsized(expr: ExprBinary): Expr = expr match {
    case ExprBinary(ExprNum(ls, lv), op, rhs) =>
      val rv = rhs.value.get
      val negl = lv < 0
      val negr = rv < 0
      val num = (ls, op) match {
        case _ if negr =>
          cc.error(expr, "Negative shift amount")
          ExprError()
        case (true, ">>") if negl =>
          cc.error(expr, "'>>' is not well defined for negative unsized values")
          ExprError()
        case (signed, "<<")  => ExprNum(signed, lv << rv.toInt)
        case (signed, ">>")  => ExprNum(signed, lv >> rv.toInt)
        case (signed, "<<<") => ExprNum(signed, lv << rv.toInt)
        case (signed, ">>>") => ExprNum(signed, lv >> rv.toInt)
        case _               => unreachable
      }
      num withLoc expr.loc
    case _ => unreachable
  }

  private def foldBinOpOneKnown(
      kSigned: Boolean,
      kWidthOpt: Option[Int],
      kValue: BigInt,
      op: String,
      unk: Expr,
      lKnown: Boolean
    ): Option[Expr] = {
    val tpe = unk.tpe
    lazy val width = tpe.width.toInt
    lazy val allOnes = kValue.extract(0, width) == BigInt.mask(width)
    val bPack = kWidthOpt.isDefined && tpe.isPacked
    op partialMatch {
      // Arithmetic
      case "*" if bPack && kValue == 0           => ExprInt(kSigned && tpe.isSigned, width, 0)
      case "*" if bPack && kValue == 1           => if (kSigned || !tpe.isSigned) unk else unk.castUnsigned
      case "/" if bPack && kValue == 0 && lKnown => ExprInt(kSigned && tpe.isSigned, width, 0)
      case "/" if bPack && kValue == 1 && !lKnown =>
        if (kSigned || !tpe.isSigned) unk else unk.castUnsigned
      case "%" if bPack && kValue == 0 && lKnown  => ExprInt(kSigned && tpe.isSigned, width, 0)
      case "%" if bPack && kValue == 1 && !lKnown => ExprInt(kSigned && tpe.isSigned, width, 0)
      case "+" if bPack && kValue == 0            => if (kSigned || !tpe.isSigned) unk else unk.castUnsigned
      case "-" if bPack && kValue == 0 && lKnown =>
        if (kSigned || !tpe.isSigned) -unk else (-unk).castUnsigned
      case "-" if bPack && kValue == 0 => if (kSigned || !tpe.isSigned) unk else unk.castUnsigned
      // Shift
      case ">>" | ">>>" | "<<" | "<<<" if kValue == 0 =>
        if (lKnown) ExprInt(kSigned, kWidthOpt.get, 0) else unk
      // Logical
      case "&&" if kValue == 0                => ExprInt(false, 1, 0)
      case "&&" if tpe.isPacked && width == 1 => if (tpe.isSigned) unk.castUnsigned else unk
      case "||" if kValue != 0                => ExprInt(false, 1, 1)
      case "||" if tpe.isPacked && width == 1 => if (tpe.isSigned) unk.castUnsigned else unk
      // Bitwise
      case "&" if bPack && kValue == 0 => ExprInt(kSigned && tpe.isSigned, width, 0)
      case "&" if bPack && allOnes     => if (kSigned || !tpe.isSigned) unk else unk.castUnsigned
      case "|" if bPack && kValue == 0 => if (kSigned || !tpe.isSigned) unk else unk.castUnsigned
      case "|" if bPack && allOnes     => ExprInt(kSigned && tpe.isSigned, width, 0)
      case "^" if bPack && kValue == 0 => if (kSigned || !tpe.isSigned) unk else unk.castUnsigned
      case "^" if bPack && allOnes     => if (kSigned || !tpe.isSigned) ~unk else (~unk).castUnsigned
    }
  }

  override def start(tree: Tree): Unit = tree match {
    case _: Expr =>
    case _       => cc.ice(tree, "Cannot invoke SimplifyExpr on non Expr tree.")
  }

  override def skip(tree: Tree): Boolean = tree match {
    case _: ExprInt => true
    case _: ExprNum => true
    case _          => false
  }

  override def enter(tree: Tree): Option[Tree] = tree pipe {
    //////////////////////////////////////////////////////////////////////////
    // Only substitute in index/slice target if the indices are known
    // constants, in which case fold them as well. This is to avoid creating
    // non-constant indices into non-symbols. In addition, we need to fold
    // index/slice over constants in pre-order as the target type might be
    // a vector
    //////////////////////////////////////////////////////////////////////////

    case expr @ ExprIndex(tgt, idx) =>
      Some {
        (walk(tgt), walk(idx)) match {
          case (Integral(_, _, tgtValue), Integral(_, _, idxValue)) =>
            // Fold known index into known value TODO: error on out of range
            TypeAssigner {
              tgt.tpe match {
                case TypeVector(eKind, _) =>
                  val eSigned = eKind.isSigned
                  val eWidth = eKind.width.toInt
                  val result = tgtValue.extract((eWidth * idxValue).toInt, eWidth, eSigned)
                  ExprInt(eSigned, eWidth, result) withLoc tree.loc
                case _ =>
                  ExprInt(false, 1, (tgtValue >> idxValue.toInt) & 1) withLoc tree.loc
              }
            }
          case (newTgt, newIdx) if (newTgt eq tgt) && (newIdx eq idx) =>
            // If all children are final, simply apply the transform to the input
            transform(expr)
          case (_: ExprInt | _: ExprNum, newIdx: Expr) =>
            // Do not fold unknown index into known value, but transform the
            // simplified expression
            transform {
              TypeAssigner {
                expr.copy(index = newIdx) withLoc tree.loc
              }
            }
          case (newTgt: Expr, newIdx: Expr) =>
            // Transform the simplified expression
            transform {
              TypeAssigner {
                expr.copy(expr = newTgt, index = newIdx) withLoc tree.loc
              }
            }
          case _ => unreachable
        }
      }

    case expr @ ExprSlice(tgt, lIdx, op, rIdx) =>
      Some {
        (walk(tgt), walk(lIdx), walk(rIdx)) match {
          case (Integral(_, _, tgtValue), Integral(_, _, lIdxValue), Integral(_, _, rIdxValue)) =>
            // Fold known slice into known value TODO: error on out of range
            val lsb = op match {
              case ":"  => rIdxValue.toInt
              case "+:" => lIdxValue.toInt
              case "-:" => (lIdxValue - rIdxValue + 1).toInt
              case _    => unreachable
            }
            val width = op match {
              case ":" => (lIdxValue - rIdxValue + 1).toInt
              case _   => rIdxValue.toInt
            }
            TypeAssigner {
              tgt.tpe match {
                case TypeVector(eKind, _) =>
                  val rWidth = (eKind.width * width).toInt
                  val result = tgtValue.extract((eKind.width * lsb).toInt, rWidth)
                  ExprInt(false, rWidth, result) withLoc tree.loc
                case _ =>
                  ExprInt(false, width, tgtValue.extract(lsb, width)) withLoc tree.loc
              }
            }
          case (newTgt, newLIdx, newRIdx)
              if (newTgt eq tgt) && (newLIdx eq lIdx) && (newRIdx eq rIdx) =>
            // If all children are final, simply apply the transform to the input
            transform(expr)
          case (_: ExprInt | _: ExprNum, newLIdx: Expr, newRIdx: Expr) =>
            // Do not fold unknown slice into known value, but transform the
            // simplified expression
            transform {
              TypeAssigner {
                expr.copy(lIdx = newLIdx, rIdx = newRIdx) withLoc tree.loc
              }
            }
          case (newTgt: Expr, newLIdx: Expr, newRIdx: Expr) =>
            // Transform the simplified expression
            transform {
              TypeAssigner {
                expr.copy(expr = newTgt, lIdx = newLIdx, rIdx = newRIdx) withLoc tree.loc
              }
            }
          case _ => unreachable
        }
      }

    //////////////////////////////////////////////////////////////////////////
    // Fold selects
    //////////////////////////////////////////////////////////////////////////

    case expr @ ExprSelect(tgt, sel, idxs) =>
      assert(idxs.isEmpty)
      Some {
        walk(tgt) match {
          case Integral(_, _, value) =>
            // Fold select into known value
            val (fieldTpe, lessSigFieldTpes) =
              tgt.tpe.asRecord.publicSymbols dropWhile { _.name != sel } map { _.kind } match {
                case head :: tail => (head, tail)
                case Nil          => unreachable
              }
            val lsb = lessSigFieldTpes.map(_.width).sum.toInt
            val result = value.extract(lsb, fieldTpe.width.toInt, fieldTpe.isSigned)
            TypeAssigner {
              ExprInt(fieldTpe.isSigned, fieldTpe.width.toInt, result) withLoc tree.loc
            }
          case newTgt if newTgt eq tgt =>
            // If all children are final, simply apply the transform to the input
            transform(expr)
          case newTgt: Expr =>
            // Transform the simplified expression
            transform {
              TypeAssigner {
                expr.copy(expr = newTgt) withLoc tree.loc
              }
            }
          case _ => unreachable
        }
      }

    //////////////////////////////////////////////////////////////////////////
    // Fold ternary up front and only evaluate the take branch. Sometimes it's
    // easy to have 'gen' create "1 ? 0 : 0-1", where the untaken expression is
    // invalid (in this case its a negative value with an unsigned type). It's
    // arguable whether this should be legal or not, it is however type sound
    // so we will support it.
    //////////////////////////////////////////////////////////////////////////

    case expr @ ExprTernary(cond, thenExpr, elseExpr) =>
      Some {
        walk(cond) match {
          case Integral(_, _, value) =>
            walk {
              val taken = if (value != 0) thenExpr else elseExpr
              if (!expr.tpe.isSigned && taken.tpe.isSigned) taken.castUnsigned else taken
            }
          case newC: Expr =>
            val newTE = walk(thenExpr).asInstanceOf[Expr]
            val newEE = walk(elseExpr).asInstanceOf[Expr]
            transform(TypeAssigner(ExprTernary(newC, newTE, newEE) withLoc tree.loc))
          case _ => unreachable
        }
      }

    case _ => None
  } tap {
    case Some(result) => checkResult(tree, result)
    case None         =>
  }

  override def transform(tree: Tree): Tree = tree pipe {
    //////////////////////////////////////////////////////////////////////////
    // Don't fold anything with errors
    //////////////////////////////////////////////////////////////////////////

    case tree if tree.children collect { case t: Tree => t } exists { _.tpe.isError } => tree

    //////////////////////////////////////////////////////////////////////////
    // Everything else, dispatch based on the root node to speed things up
    //////////////////////////////////////////////////////////////////////////

    case expr: ExprSym     => transformSym(expr)
    case expr: ExprUnary   => transformUnary(expr)
    case expr: ExprBinary  => transformBinary(expr)
    case expr: ExprTernary => transformTernary(expr)
    case expr: ExprIndex   => transformIndex(expr)
    case expr: ExprSlice   => transformSlice(expr)
    case expr: ExprCat     => transformCat(expr)
    case expr: ExprRep     => transformRep(expr)
    case expr: ExprCast    => transformCast(expr)
    case expr: ExprCall    => transformCall(expr)

    //////////////////////////////////////////////////////////////////////////
    // Leave rest alone
    //////////////////////////////////////////////////////////////////////////

    case _ => tree
  } tap { result =>
    if (!result.hasTpe) {
      TypeAssigner(result)
    }
    checkResult(tree, result)
  } pipe {
    case result: ExprInt          => result
    case result: ExprNum          => result
    case result: ExprType         => result
    case result: ExprStr          => result
    case result: ExprError        => result
    case result: ExprSym          => result
    case result if result ne tree => walk(result) // Recursively fold the resulting expression
    case result                   => result
  }

  private def checkResult(tree: Tree, result: Tree) = {
    if (!result.tpe.isError) {
      lazy val hints: List[String] = List(
        "Old tree:",
        tree.toString,
        tree.toSource,
        "Old type:",
        tree.tpe.underlying.toString,
        "New tree:",
        result.toString,
        result.toSource,
        "New type:",
        result.tpe.underlying.toString
      )
      if (result.tpe.isPacked != tree.tpe.isPacked) {
        cc.ice(tree, s"SimplifyExpr changed packedness." :: hints: _*)
      }
      if (result.tpe.isSigned != tree.tpe.isSigned) {
        cc.ice(tree, s"SimplifyExpr changed signedness." :: hints: _*)
      }
      if (result.tpe.isPacked && result.tpe.width != tree.tpe.width) {
        cc.ice(tree, s"SimplifyExpr changed width." :: hints: _*)
      }
      if (result.tpe.isNum != tree.tpe.isNum) {
        cc.ice(tree, s"SimplifyExpr changed Num'ness." :: hints: _*)
      }
      if (result.tpe.isType != tree.tpe.isType) {
        cc.ice(tree, s"SimplifyExpr changed Type'ness." :: hints: _*)
      }
    }
  }

  private def transformSym(tree: ExprSym): Expr = {
    val symbol = tree.symbol
    symbol.kind match {
      //////////////////////////////////////////////////////////////////////////
      // Fold refs to type symbols (unless it's directly to the named type)
      //////////////////////////////////////////////////////////////////////////

      case TypeType(kind) =>
        kind match {
          case kind: TypeInt           => ExprType(kind) withLoc tree.loc
          case kind: TypeNum           => ExprType(kind) withLoc tree.loc
          case kind: TypeVector        => ExprType(kind) withLoc tree.loc
          case TypeVoid                => ExprType(TypeVoid) withLoc tree.loc
          case TypeStr                 => ExprType(TypeStr) withLoc tree.loc
          case TypeEntity(`symbol`, _) => tree
          case TypeRecord(`symbol`, _) => tree
          case TypeEntity(s, _)        => ExprSym(s) withLoc tree.loc
          case TypeRecord(s, _)        => ExprSym(s) withLoc tree.loc
        }

      //////////////////////////////////////////////////////////////////////////
      // Fold references to const symbols
      //////////////////////////////////////////////////////////////////////////

      case _: TypeConst => symbol.init getOrElse unreachable

      //////////////////////////////////////////////////////////////////////////
      // Fold references to val symbols, iff they have a constant initializer
      //////////////////////////////////////////////////////////////////////////

      case _: TypeFund =>
        symbol.defn match {
          case _: DefnVal =>
            symbol.init.get.simplify match {
              case value: ExprNum => value
              case value: ExprInt => value
              case _              => tree
            }
          case _ => tree
        }

      //////////////////////////////////////////////////////////////////////////
      // Otherwise don't fold
      //////////////////////////////////////////////////////////////////////////

      case _ => tree
    }
  }

  private def transformUnary(tree: ExprUnary): Expr = tree match {

    ////////////////////////////////////////////////////////////////////////////
    // Fold unary expressions with an unsized operand
    ////////////////////////////////////////////////////////////////////////////

    case ExprUnary(op, expr @ ExprNum(signed, value)) =>
      val num = op match {
        // Invalid cases
        case "-" if !signed && value > 0 =>
          cc.error(tree, "Unary '-' is not well defined for unsigned values")
          ExprError()
        case "~" if !signed =>
          cc.error(tree, "Unary '~' is not well defined for unsized unsigned values")
          ExprError()
        // Valid cases
        case "+" => expr
        case "-" => ExprNum(signed, -value)
        case "~" => ExprNum(true, -value - 1)
        case "!" => ExprInt(false, 1, value == 0)
      }
      if (num.hasLoc) num else num withLoc tree.loc

    ////////////////////////////////////////////////////////////////////////////
    // Fold some special unary over unary combinations
    ////////////////////////////////////////////////////////////////////////////

    case ExprUnary(aOp, ExprUnary(bOp, expr)) =>
      val isBoolType = expr.tpe.isPacked && !expr.tpe.isSigned && expr.tpe.width == 1
      val res = (aOp, bOp) match {
        case ("~", "~")               => expr
        case ("~", "!") if isBoolType => expr
        case ("!", "~") if isBoolType => expr
        case ("!", "!") if isBoolType => expr
        case _                        => tree
      }
      if (res.hasLoc) res else res withLoc tree.loc

    ////////////////////////////////////////////////////////////////////////////
    // Fold unary expressions with a sized operand
    ////////////////////////////////////////////////////////////////////////////

    case ExprUnary(op, expr @ ExprInt(signed, width, value)) =>
      lazy val mask = (BigInt(1) << width) - 1
      val num = op match {
        // Invalid cases
        case "-" if !signed && value > 0 =>
          cc.error(tree, "Unary '-' is not well defined for unsigned values")
          ExprError()
        // Valid cases
        case "+"           => expr
        case "-"           => ExprInt(signed, width, -value)
        case "~" if signed => ExprInt(true, width, ~value)
        case "~"           => ExprInt(false, width, ~value & mask)
        case "!"           => ExprInt(false, 1, value == 0)
        case "&"           => ExprInt(false, 1, (value & mask) == mask)
        case "|"           => ExprInt(false, 1, value != 0)
        case "^"           => ExprInt(false, 1, (value & mask).bitCount & 1)
      }
      if (num.hasLoc) num else num withLoc tree.loc

    case _ => tree
  }

  private def transformBinary(tree: ExprBinary): Expr = tree match {

    ////////////////////////////////////////////////////////////////////////////
    // Fold binary '
    ////////////////////////////////////////////////////////////////////////////

    case ExprBinary(lhs, "'", rhs) =>
      val width = lhs.value match {
        case Some(v) if v > 0 => v.toInt
        case Some(_)          => cc.ice(tree, "LHS of binary ' is non positive")
        case None             => cc.ice(tree, "Cannot compute value of LHS of binary '")
      }
      val rhsWidth = rhs.tpe.width.toInt // Must be packed so must know width
      require(width >= rhsWidth)
      rhs match {
        case expr: ExprInt          => expr.copy(width = width) withLoc tree.loc
        case _ if width == rhsWidth => rhs
        case _ if rhs.tpe.isSigned  => rhs sx width
        case _                      => rhs zx width
      }

    ////////////////////////////////////////////////////////////////////////////
    // Fold shifts with an unsized left hand side
    ////////////////////////////////////////////////////////////////////////////

    case expr @ ExprBinary(_: ExprNum, op, _: ExprNum | _: ExprInt) if shiftOps contains op =>
      foldShiftUnsized(expr)

    ////////////////////////////////////////////////////////////////////////////
    // Fold other binary expressions with 2 unsized operands
    ////////////////////////////////////////////////////////////////////////////

    case ExprBinary(ExprNum(ls, lv), op, ExprNum(rs, rv)) =>
      def makeUnsignedExprNum(value: BigInt) = {
        if (value >= 0) {
          ExprNum(false, value)
        } else {
          cc.error(tree, s"Result of operator '$op' is unsigned, but value is negative: $value")
          ExprError()
        }
      }

      val negl = lv < 0
      val negr = rv < 0
      val nege = negl || negr
      val num = (ls, op, rs) match {
        // Always valid
        case (_, ">", _)  => ExprInt(false, 1, lv > rv)
        case (_, "<", _)  => ExprInt(false, 1, lv < rv)
        case (_, ">=", _) => ExprInt(false, 1, lv >= rv)
        case (_, "<=", _) => ExprInt(false, 1, lv <= rv)
        case (_, "==", _) => ExprInt(false, 1, lv == rv)
        case (_, "!=", _) => ExprInt(false, 1, lv != rv)
        case (_, "&&", _) => ExprInt(false, 1, (lv != 0) && (rv != 0))
        case (_, "||", _) => ExprInt(false, 1, (lv != 0) || (rv != 0))

        // Arith
        case (true, "*", true) => ExprNum(true, lv * rv)
        case (true, "/", true) => ExprNum(true, lv / rv)
        case (true, "%", true) => ExprNum(true, lv % rv)
        case (true, "+", true) => ExprNum(true, lv + rv)
        case (true, "-", true) => ExprNum(true, lv - rv)
        case (_, "*", _)       => makeUnsignedExprNum(lv * rv)
        case (_, "/", _)       => makeUnsignedExprNum(lv / rv)
        case (_, "%", _)       => makeUnsignedExprNum(lv % rv)
        case (_, "+", _)       => makeUnsignedExprNum(lv + rv)
        case (_, "-", _)       => makeUnsignedExprNum(lv - rv)

        // Bitwise
        case (_, op @ ("&" | "^" | "|"), _) if nege =>
          cc.error(tree, s"Bitwise '$op' operator is not well defined for negative unsized values")
          ExprError()
        case (_, "~^", _) =>
          cc.error(tree, "Bitwise '~^' operator is not well defined for unsized values")
          ExprError()
        case (true, "&", true) => ExprNum(true, lv & rv)
        case (true, "^", true) => ExprNum(true, lv ^ rv)
        case (true, "|", true) => ExprNum(true, lv | rv)
        case (_, "&", _)       => ExprNum(false, lv & rv)
        case (_, "^", _)       => ExprNum(false, lv ^ rv)
        case (_, "|", _)       => ExprNum(false, lv | rv)

        case _ => unreachable
      }
      num withLoc tree.loc

    ////////////////////////////////////////////////////////////////////////////
    // Fold binary expressions with equally sized operands
    ////////////////////////////////////////////////////////////////////////////

    case ExprBinary(ExprInt(ls, lw, lv), op, ExprInt(rs, rw, rv)) if lw == rw =>
      val w = lw
      val s = ls && rs
      val sm = ls == rs
      val num = op match {
        // Always valid
        case ">"  => ExprInt(false, 1, lv > rv)
        case "<"  => ExprInt(false, 1, lv < rv)
        case ">=" => ExprInt(false, 1, lv >= rv)
        case "<=" => ExprInt(false, 1, lv <= rv)
        case "==" => ExprInt(false, 1, lv == rv)
        case "!=" => ExprInt(false, 1, lv != rv)
        case "&&" => ExprInt(false, 1, (lv != 0) && (rv != 0))
        case "||" => ExprInt(false, 1, (lv != 0) || (rv != 0))

        // Arith with matching sign
        case "+" if sm => ExprInt(s, w, (lv + rv).extract(0, w, s))
        case "-" if sm => ExprInt(s, w, (lv - rv).extract(0, w, s))
        case "*" if sm => ExprInt(s, w, (lv * rv).extract(0, w, s))
        case "/" if sm => ExprInt(s, w, (lv / rv).extract(0, w, s))
        case "%" if sm => ExprInt(s, w, (lv % rv).extract(0, w, s))

        // Bitwise
        case "&" => ExprInt(s, w, (lv & rv).extract(0, w, s))
        case "^" => ExprInt(s, w, (lv ^ rv).extract(0, w, s))
        case "|" => ExprInt(s, w, (lv | rv).extract(0, w, s))

        // TODO: handle
        case _ => tree
      }
      if (num.hasLoc) num else num withLoc tree.loc

    ////////////////////////////////////////////////////////////////////////////
    // Fold binary operators with one known operand if possible
    ////////////////////////////////////////////////////////////////////////////

    case ExprBinary(Integral(ks, kw, kv), op, unk) =>
      foldBinOpOneKnown(ks, kw, kv, op, unk, lKnown = true) getOrElse {
        tree
      } tap {
        case result if result.hasLoc => result
        case result                  => result withLoc tree.loc
      }

    case ExprBinary(unk, op, Integral(ks, kw, kv)) =>
      foldBinOpOneKnown(ks, kw, kv, op, unk, lKnown = false) getOrElse {
        tree
      } tap {
        case result if result.hasLoc => result
        case result                  => result withLoc tree.loc
      }

    case _ => tree
  }

  private def transformTernary(tree: ExprTernary): Expr = tree match {
    case ExprTernary(_, thenExpr, elseExpr) =>
      // Condition folded in enter
      if (thenExpr == elseExpr && thenExpr.tpe == elseExpr.tpe) {
        thenExpr
      } else {
        tree
      }
  }

  private def transformIndex(tree: ExprIndex): Expr = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Fold index over a slice
    ////////////////////////////////////////////////////////////////////////////

    case ExprIndex(ExprSlice(expr, lIdx, op, rIdx), idx) =>
      // TODO: error on out of range
      val lsb = op match {
        case ":"  => rIdx
        case "+:" => lIdx
        case "-:" => lIdx - rIdx + 1 // FIXME: needs typing
        case _    => unreachable
      }
      ExprIndex(expr, lsb + (idx zx lsb.tpe.width.toInt)) withLoc tree.loc

    ////////////////////////////////////////////////////////////////////////////
    // Fold index over $signed/$unsigned
    ////////////////////////////////////////////////////////////////////////////

    case ExprIndex(ExprCall(ExprSym(Symbol("$signed" | "$unsigned")), args), idx) =>
      ExprIndex(args.head.expr, idx) withLoc tree.loc

    ////////////////////////////////////////////////////////////////////////////
    // Fold index with known value
    ////////////////////////////////////////////////////////////////////////////

    case ExprIndex(expr, ExprInt(_, _, index)) =>
      expr match {
        ////////////////////////////////////////////////////////////////////////
        // Fold constant index of concatenations
        ////////////////////////////////////////////////////////////////////////

        case ExprCat(parts) =>
          @tailrec
          def loop(remaining_parts: List[Expr], remaining_index: BigInt): ExprIndex = {
            val head_width = remaining_parts.head.tpe.width;
            if (remaining_index >= head_width) {
              loop(remaining_parts.tail, remaining_index - head_width)
            } else {
              val part = remaining_parts.head
              ExprIndex(part, ExprInt(false, clog2(head_width) max 1, remaining_index))
            }
          }
          loop(parts.reverse, index) regularize tree.loc

        ////////////////////////////////////////////////////////////////////////
        // Fold constant index of repetitions
        ////////////////////////////////////////////////////////////////////////

        case ExprRep(_, expr) =>
          val expr_width = expr.tpe.width
          ExprIndex(
            expr,
            ExprInt(false, clog2(expr_width) max 1, index % expr_width)
          ) regularize tree.loc

        ////////////////////////////////////////////////////////////////////////////
        // Fold index zero of width one
        ////////////////////////////////////////////////////////////////////////////

        case _ => if (index == 0 && expr.tpe.isPacked && expr.tpe.width == 1) expr else tree
      }

    case _ => tree
  }

  private def transformSlice(tree: ExprSlice): Expr = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Fold width 1 slices
    ////////////////////////////////////////////////////////////////////////////

    case ExprSlice(expr, lIdx, ":", rIdx) if lIdx == rIdx =>
      // TODO: strictly, lidx/ridx could be stuff like @randbit, or other non-pure function
      // Vector slices always yield a vector, so leave them
      if (expr.tpe.underlying.isVector) tree else ExprIndex(expr, lIdx) withLoc tree.loc

    case ExprSlice(expr, lIdx, ("-:" | "+:"), ExprInt(_, _, rIdx)) if rIdx == 1 =>
      // Vector slices always yield a vector, so leave them
      if (expr.tpe.underlying.isVector) tree else ExprIndex(expr, lIdx) withLoc tree.loc

    ////////////////////////////////////////////////////////////////////////////
    // Fold Slice over a slice
    ////////////////////////////////////////////////////////////////////////////

    case ExprSlice(ExprSlice(expr, aLIdx, aOp, aRIdx), bLIdx, bOp, bRIdx) =>
      val idxWidth = aLIdx.tpe.width.toInt

      val bLidx = bLIdx zx idxWidth
      val bRidx = bRIdx zx idxWidth

      // TODO: error on out of range
      val aLsb = aOp match {
        case ":"  => aRIdx
        case "+:" => aLIdx
        case "-:" => aLIdx - (aRIdx.value.get.toInt - 1)
        case _    => unreachable
      }
      val bLsb = bOp match {
        case ":"  => bRidx
        case "+:" => bLidx
        case "-:" => bLidx - (bRidx.value.get.toInt - 1)
        case _    => unreachable
      }
      val bMsb = bOp match {
        case ":"  => bLidx
        case "+:" => bLidx + (bRidx.value.get.toInt - 1)
        case "-:" => bLidx
        case _    => unreachable
      }
      lazy val bWidth = bOp match {
        case ":" => bLidx - bRidx + 1
        case _   => bRidx
      }

      val (nLidx, nOp, nRidx) = (aOp, bOp) match {
        case (_, "+:") => (aLsb + bLsb, "+:", bWidth)
        case (_, "-:") => (aLsb + bMsb, "-:", bWidth)
        case ("+:", _) => (aLsb + bLsb, "+:", bWidth)
        case ("-:", _) => (aLsb + bMsb, "-:", bWidth)
        case _         => (aLsb + bMsb, ":", aLsb + bLsb)
      }

      ExprSlice(expr, nLidx, nOp, nRidx) withLoc tree.loc

    ////////////////////////////////////////////////////////////////////////////
    // Fold slice over $signed/$unsigned
    ////////////////////////////////////////////////////////////////////////////

    case ExprSlice(ExprCall(ExprSym(Symbol("$signed" | "$unsigned")), args), _, _, _) =>
      tree.copy(expr = args.head.expr) withLoc tree.loc

    ////////////////////////////////////////////////////////////////////////////
    // Fold slices with known indices
    ////////////////////////////////////////////////////////////////////////////

    case ExprSlice(expr, ExprInt(_, _, lIdx), op, ExprInt(_, _, rIdx)) =>
      expr match {
        ////////////////////////////////////////////////////////////////////////
        // Fold known slice of concatenations
        ////////////////////////////////////////////////////////////////////////

        case ExprCat(parts) =>
          val (msb, lsb) = op match {
            case ":"  => (lIdx, rIdx)
            case "+:" => (lIdx + rIdx - 1, lIdx)
            case "-:" => (lIdx, lIdx - rIdx + 1)
            case _    => unreachable
          }

          def slice_part(part: Expr, m: BigInt, l: BigInt): ExprSlice = {
            val width_bits = clog2(part.tpe.width) max 1
            ExprSlice(part, ExprInt(false, width_bits, m), ":", ExprInt(false, width_bits, l))
          }

          @tailrec def loop(
              concat_list: List[Expr],
              rem_parts: List[Expr],
              msb_rem: BigInt,
              lsb_rem: BigInt
            ): List[Expr] = {
            val next_part_width = rem_parts.head.tpe.width
            if (lsb_rem >= next_part_width) {
              // None of this part included in slice
              loop(
                concat_list,
                rem_parts.tail,
                msb_rem - next_part_width,
                lsb_rem - next_part_width
              )
            } else if (msb_rem < next_part_width) {
              // This is the final part included in slice
              slice_part(rem_parts.head, msb_rem, lsb_rem) :: concat_list
            } else {
              // Slice this part and continue
              val new_concat_list =
                slice_part(rem_parts.head, next_part_width - 1, lsb_rem) :: concat_list
              loop(new_concat_list, rem_parts.tail, msb_rem - next_part_width, 0)
            }
          }

          ExprCat(loop(List(), parts.reverse, msb, lsb)) regularize tree.loc

        ////////////////////////////////////////////////////////////////////////
        // Fold known slice of repetitions
        ////////////////////////////////////////////////////////////////////////

        case ExprRep(_, expr) =>
          val (msb, lsb) = op match {
            case ":"  => (lIdx, rIdx)
            case "+:" => (lIdx + rIdx - 1, lIdx)
            case "-:" => (lIdx, lIdx - rIdx + 1)
            case _    => unreachable
          }
          val expr_width = expr.tpe.width
          val expr_bits = clog2(expr_width) max 1

          def slice_expr(m: BigInt, l: BigInt): ExprSlice = {
            ExprSlice(expr, ExprInt(false, expr_bits, m), ":", ExprInt(false, expr_bits, l))
          }

          if (msb / expr_width == lsb / expr_width) {
            // Just a single slice
            slice_expr(msb % expr_width, lsb % expr_width) regularize tree.loc
          } else {
            val ms_slice_width = (msb + 1) % expr_width
            val ls_slice_width = (((-lsb) % expr_width) + expr_width) % expr_width
            val num_intermediate_reps =
              ((msb - lsb + 1) - ms_slice_width - ls_slice_width) / expr_width
            val ms_slice = if (ms_slice_width > 0) Some(slice_expr(ms_slice_width - 1, 0)) else None
            val ls_slice =
              if (ls_slice_width > 0) Some(slice_expr(expr_width - 1, expr_width - ls_slice_width))
              else None
            val intermediate_reps =
              if (num_intermediate_reps > 0)
                Some(ExprRep(ExprNum(false, num_intermediate_reps), expr))
              else
                None
            ExprCat(List(ms_slice, intermediate_reps, ls_slice).flatten) regularize tree.loc
          }

        ////////////////////////////////////////////////////////////////////////
        // Fold full width slices
        ////////////////////////////////////////////////////////////////////////

        case _ =>
          op match {
            case ":" =>
              if (rIdx == 0 && lIdx == expr.tpe.width - 1) {
                if (expr.tpe.isSigned) expr.castUnsigned else expr
              } else {
                tree
              }
            case "+:" | "-:" =>
              // Note: This assumes the slice is well formed, i.e.:
              // lIdx is 0 if "+:" or rIdx is size - 1 if "-:"
              if (rIdx == expr.tpe.width) {
                if (expr.tpe.isSigned) expr.castUnsigned else expr
              } else {
                tree
              }
            case _ => unreachable
          }
      }

    case _ => tree
  }

  private def transformCat(tree: ExprCat): Expr = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Remove pointless $signed/$unsigned from Cat arguments
    ////////////////////////////////////////////////////////////////////////////

    case ExprCat(args) if args exists {
          case ExprCall(ExprSym(Symbol("$signed" | "$unsigned")), _) => true
          case _                                                     => false
        } =>
      ExprCat {
        args map {
          case ExprCall(ExprSym(Symbol("$signed" | "$unsigned")), args) => args.head.expr
          case arg                                                      => arg
        }
      } withLoc tree.loc

    ////////////////////////////////////////////////////////////////////////////
    // Fold concatenations of sized integers
    ////////////////////////////////////////////////////////////////////////////

    case ExprCat(parts) if parts forall { _.isInstanceOf[ExprInt] } =>
      val start = (0, BigInt(0))
      val (width, value) = parts.foldRight(start) {
        case (ExprInt(_, w, v), (aw, av)) => (w.toInt + aw, (v.extract(0, w.toInt) << aw) | av)
        case _                            => unreachable
      }
      ExprInt(signed = false, width, value) withLoc tree.loc

    ////////////////////////////////////////////////////////////////////////////
    // Flatten concatenations
    ////////////////////////////////////////////////////////////////////////////

    case ExprCat(List(expr: Expr)) => if (expr.tpe.isSigned) expr.castUnsigned else expr

    case ExprCat(parts) if parts exists { _.isInstanceOf[ExprCat] } =>
      ExprCat {
        parts flatMap {
          case ExprCat(nested) => nested
          case expr            => Iterator.single(expr)
        }
      } withLoc tree.loc

    case _ => tree
  }

  private def transformRep(tree: ExprRep): Expr = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Remove pointless $signed/$unsigned Rep arguments
    ////////////////////////////////////////////////////////////////////////////

    case ExprRep(count, ExprCall(ExprSym(Symbol("$signed" | "$unsigned")), args)) =>
      ExprRep(count, args.head.expr) withLoc tree.loc

    ////////////////////////////////////////////////////////////////////////////
    // Fold repetitions of count 1
    ////////////////////////////////////////////////////////////////////////////

    case ExprRep(Integral(_, _, rep), expr) if rep == 1 =>
      if (expr.tpe.isSigned) expr.castUnsigned else expr

    ////////////////////////////////////////////////////////////////////////////
    // Fold repetitions of sized integers
    ////////////////////////////////////////////////////////////////////////////

    case ExprRep(count, ExprInt(_, width, value)) =>
      count.value map { cnt =>
        val c = cnt.toInt
        val w = width.toInt
        val b = value.extract(0, w)
        val v = (0 until c).foldLeft(BigInt(0)) { case (a, _) => (a << w) | b }
        ExprInt(signed = false, c * w, v) withLoc tree.loc
      } getOrElse tree

    case _ => tree
  }

  private def transformCast(tree: ExprCast): Expr = {
    val ExprCast(kind, expr) = tree
    kind match {
      case TypeNum(signed) =>
        expr match {
          case ExprNum(_, v)    => ExprNum(signed, v) withLoc tree.loc
          case ExprInt(_, _, v) => ExprNum(signed, v) withLoc tree.loc
          case _                => unreachable
        }

      case TypeInt(signed, width) =>
        def mkValue(value: BigInt): Expr = {
          // TODO: should check out of range width inference in the typer
          val lo = if (signed) -(BigInt(1) << (width.toInt - 1)) else BigInt(0)
          val hi = if (signed) BigInt.mask(width.toInt - 1) else BigInt.mask(width)
          if (value > hi || value < lo) {
            val signedness = if (signed) "signed" else "unsigned"
            cc.error(tree, s"Value $value cannot be represented with $width $signedness bits")
            ExprError() withLoc tree.loc
          } else {
            ExprInt(signed, width.toInt, value) withLoc tree.loc
          }
        }

        expr match {
          case ExprNum(_, v) =>
            mkValue(v)
          case ExprInt(_, eWidth, v) =>
            require(width.toInt >= eWidth)
            mkValue(v)
          case _ =>
            val kWidth = width.toInt
            val eWidth = expr.tpe.width.toInt
            require(kWidth >= eWidth)
            if (kWidth == eWidth) {
              expr
            } else if (expr.tpe.isSigned) {
              expr sx kWidth
            } else {
              expr zx kWidth
            }
        }

      case _ => unreachable
    }
  }

  private def transformCall(tree: ExprCall): Expr = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Fold built-in functions
    ////////////////////////////////////////////////////////////////////////////

    case call @ ExprCall(ExprSym(symbol), _) if symbol.isBuiltin =>
      val result = cc.foldBuiltinCall(call)
      if (result eq call) call else result regularize tree.loc

    case _ => tree
  }

}
