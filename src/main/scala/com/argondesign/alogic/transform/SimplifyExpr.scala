////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Transform that simplifies expressions, including constant folding, but also
// more.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.transform

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.Expr.Integral
import com.argondesign.alogic.builtins.DollarSigned
import com.argondesign.alogic.builtins.DollarUnsigned
import com.argondesign.alogic.core.Messages.Fatal
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Math.clog2
import com.argondesign.alogic.util.BigIntOps._
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.language.implicitConversions

object SimplifyExpr extends StatelessTreeTransformer {

  implicit private def boolean2BigInt(bool: Boolean): BigInt = if (bool) BigInt(1) else BigInt(0)

  // simplify commutative and comparison operator with one known operand
  private def simplifyComBinary(unknown: Expr, op: String, known: ExprInt): Option[Expr] = {
    val ExprInt(ks, w, v) = known
    require(unknown.tpe.width == w)
    val s = unknown.tpe.isSigned && ks
    op match {
      // Comparisons
      case "==" | "!=" => None
      case "<" =>
        if (s) {
          Option.when(v == BigInt.iMin(w))(ExprInt(false, 1, 0))
        } else {
          Option.when(v.asU(w) == 0)(ExprInt(false, 1, 0))
        }
      case ">" =>
        if (s) {
          Option.when(v == BigInt.iMax(w))(ExprInt(false, 1, 0))
        } else {
          Option.when(v.asU(w) == BigInt.uMax(w))(ExprInt(false, 1, 0))
        }
      case "<=" =>
        if (s) {
          Option.when(v == BigInt.iMax(w))(ExprInt(false, 1, 1))
        } else {
          Option.when(v.asU(w) == BigInt.uMax(w))(ExprInt(false, 1, 1))
        }
      case ">=" =>
        if (s) {
          Option.when(v == BigInt.iMin(w))(ExprInt(false, 1, 1))
        } else {
          Option.when(v.asU(w) == 0)(ExprInt(false, 1, 1))
        }

      // Arithmetic
      case "+" =>
        if (v == 0) {
          Some(unknown.withSignedness(s))
        } else {
          None
        }
      case "*" =>
        if (v == 0) {
          Some(ExprInt(s, w, 0))
        } else if (v == 1) {
          Some(unknown.withSignedness(s))
        } else {
          None
        }

      // Bitwise
      case "&" =>
        if (v == 0) {
          Some(ExprInt(s, w, 0))
        } else if (v.asI(w) == -1) {
          Some(unknown.withSignedness(s))
        } else {
          None
        }
      case "|" =>
        if (v == 0) {
          Some(unknown.withSignedness(s))
        } else if (v.asI(w) == -1) {
          Some(ExprInt(s, w, if (s) -1 else BigInt.mask(w)))
        } else {
          None
        }
      case "^" =>
        if (v == 0) {
          Some(unknown.withSignedness(s))
        } else if (v.asI(w) == -1) {
          Some((~unknown).withSignedness(s))
        } else {
          None
        }

      // Boolean
      case "&&" =>
        Some(if (v == 0) ExprInt(false, 1, 0) else unknown.asUnsigned)
      case "||" =>
        Some(if (v != 0) ExprInt(false, 1, 1) else unknown.asUnsigned)

      //
      case _ => unreachable
    }
  }

  override def start(tree: Tree): Unit = tree match {
    case _: Expr =>
    case _       => throw Ice(tree, "Cannot invoke SimplifyExpr on non Expr tree.")
  }

  override def enter(tree: Tree): Option[Tree] = tree pipe {
    ////////////////////////////////////////////////////////////////////////////
    // Skip terms already constant
    ////////////////////////////////////////////////////////////////////////////
    case _: ExprInt | _: ExprNum => Some(tree)

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
          case (_: ExprInt | _: ExprNum | ExprCast(_, _: ExprInt | _: ExprNum), newIdx: Expr) =>
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
          case (
                _: ExprInt | _: ExprNum | ExprCast(_, _: ExprInt | _: ExprNum),
                newLIdx: Expr,
                newRIdx: Expr
              ) =>
            // Do not fold unknown slice into known symbol value, but transform
            // the simplified expression
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

    case expr @ (_: ExprSel | _: ExprSymSel) =>
      Some {
        // Note: the  type ascription on the scrutinee is needed to squelch a
        // warning about exhaustvivity which differs in Scala 2.13 and 3
        val (tgt, predicate, copy) = (expr: Tree) match {
          case e: ExprSel =>
            (
              e.expr,
              { (symbol: Symbol) => symbol.name != e.selector },
              { (newTgt: Expr) => e.copy(expr = newTgt) }
            )
          case e: ExprSymSel =>
            (
              e.expr,
              { (symbol: Symbol) => symbol != e.symbol },
              { (newTgt: Expr) => e.copy(expr = newTgt) }
            )
          case _ => unreachable
        }

        walk(tgt) match {
          case Integral(_, _, value) =>
            // Fold select into known value
            val (fieldTpe, lessSigFieldTpes) =
              tgt.tpe.asRecord.publicSymbols dropWhile predicate map { _.kind } match {
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
                copy(newTgt) withLoc tree.loc
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

    case expr @ ExprCond(cond, thenExpr, elseExpr) =>
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
            transform(TypeAssigner(ExprCond(newC, newTE, newEE) withLoc tree.loc))
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
    // Everything else, dispatch based on the root node to speed things up
    //////////////////////////////////////////////////////////////////////////

    case expr: ExprSym     => transformSym(expr)
    case expr: ExprUnary   => transformUnary(expr)
    case expr: ExprBinary  => transformBinary(expr)
    case expr: ExprCond    => transformTernary(expr)
    case expr: ExprIndex   => transformIndex(expr)
    case expr: ExprSlice   => transformSlice(expr)
    case expr: ExprCat     => transformCat(expr)
    case expr: ExprRep     => transformRep(expr)
    case expr: ExprCast    => transformCast(expr)
    case expr: ExprBuiltin => expr.builtin.simplify(expr)

    //////////////////////////////////////////////////////////////////////////
    // Leave rest alone
    //////////////////////////////////////////////////////////////////////////

    case _ => tree
  } tap { result =>
    if (!result.hasLoc) {
      result withLoc tree.loc
    }
    if (!result.hasTpe) {
      TypeAssigner(result)
    }
    checkResult(tree, result)
  } pipe {
    case result: ExprInt          => result
    case result: ExprNum          => result
    case result: ExprType         => result
    case result: ExprStr          => result
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
        tree.tpe.toString,
        "New tree:",
        result.toString,
        result.toSource,
        "New type:",
        result.tpe.toString
      )
      if (result.tpe.isPacked != tree.tpe.isPacked) {
        throw Ice(tree, s"SimplifyExpr changed packedness." :: hints: _*)
      }
      if (result.tpe.isSigned != tree.tpe.isSigned) {
        throw Ice(tree, s"SimplifyExpr changed signedness." :: hints: _*)
      }
      if (result.tpe.isPacked && result.tpe.width != tree.tpe.width) {
        throw Ice(tree, s"SimplifyExpr changed width." :: hints: _*)
      }
      if (result.tpe.underlying.isNum != tree.tpe.underlying.isNum) {
        throw Ice(tree, s"SimplifyExpr changed Num'ness." :: hints: _*)
      }
      if (result.tpe.isType != tree.tpe.isType) {
        throw Ice(tree, s"SimplifyExpr changed Type'ness." :: hints: _*)
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
          case TypeStr                 => unreachable // Cannot write in source
          case TypeEntity(`symbol`, _) => tree
          case TypeRecord(`symbol`, _) => tree
          case TypeEntity(s, _)        => ExprSym(s) withLoc tree.loc
          case TypeRecord(s, _)        => ExprSym(s) withLoc tree.loc
        }

      //////////////////////////////////////////////////////////////////////////
      // Fold references to const symbols if possible
      //////////////////////////////////////////////////////////////////////////

      case _: TypeConst =>
        symbol.defnOption match {
          case Some(defn) => defn.asInstanceOf[DefnConst].init
          case None       => tree
        }

      //////////////////////////////////////////////////////////////////////////
      // Fold references to val symbols, iff they have a constant initializer
      //////////////////////////////////////////////////////////////////////////

      case _: TypeFund =>
        symbol.defnOption match {
          case Some(DefnVal(_, init)) =>
            init.simplify match {
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
    // Unary + over anything is identity
    ////////////////////////////////////////////////////////////////////////////

    case ExprUnary("+", expr) => expr

    ////////////////////////////////////////////////////////////////////////////
    // Unary expressions with an unsized operand
    ////////////////////////////////////////////////////////////////////////////

    case ExprUnary(op, ExprNum(s, v)) =>
      op match {
        case "-" => assert(s || v == 0); ExprNum(s, -v)
        case "~" => assert(s); ExprNum(true, ~v)
        case "&" => ExprInt(false, 1, v == -1)
        case "|" => ExprInt(false, 1, v != 0)
        case "^" => assert(v >= 0); ExprInt(false, 1, v.bitCount & 1)
        case _   => unreachable // Includes "!" which must have a 1 bit operand
      }

    ////////////////////////////////////////////////////////////////////////////
    // Unary expressions with a known sized operand
    ////////////////////////////////////////////////////////////////////////////

    case ExprUnary(op, ExprInt(s, w, v)) =>
      op match {
        case "-" => ExprInt(s, w, (-v).extract(0, w, s))
        case "~" => ExprInt(s, w, (~v).extract(0, w, s))
        case "&" => ExprInt(false, 1, v.asU(w) == BigInt.mask(w))
        case "|" => ExprInt(false, 1, v != 0)
        case "^" => ExprInt(false, 1, v.asU(w).bitCount & 1)
        case "!" => ExprInt(false, 1, v == 0)
        case _   => unreachable
      }

    ////////////////////////////////////////////////////////////////////////////
    // Unary over unary combinations
    ////////////////////////////////////////////////////////////////////////////

    case ExprUnary(aOp, ExprUnary(bOp, expr)) =>
      (aOp, bOp) match {
        case ("~", "~") => expr
        case ("~", "!") => expr.asUnsigned
        case ("!", "~") => expr.asUnsigned
        case ("!", "!") => expr.asUnsigned
        case ("-", "-") => expr
        case _          => tree
        // TODO: Is bubble pushing (De Morgan) any useful? &~ => ~| and |~ => ~&
      }

    ////////////////////////////////////////////////////////////////////////////
    // Fold unary expressions over an unknown one bit value
    ////////////////////////////////////////////////////////////////////////////

    case ExprUnary(op, expr) if expr.tpe.width == 1 =>
      op match {
        case "-" => expr
        case "&" => expr.asUnsigned
        case "|" => expr.asUnsigned
        case "^" => expr.asUnsigned
        case _   => tree
      }

    //
    case _ => tree
  }

  private def transformBinary(tree: ExprBinary): Expr = tree match {

    ////////////////////////////////////////////////////////////////////////////
    // Binary '
    ////////////////////////////////////////////////////////////////////////////

    case ExprBinary(lhs, "'", rhs) =>
      val width = lhs.valueOption match {
        case Some(v) if v > 0 => v.toInt
        case Some(_)          => throw Ice(tree, "LHS of binary ' is non positive")
        case None             => throw Ice(tree, "Cannot compute value of LHS of binary '")
      }
      val rhsWidth = rhs.tpe.width.toInt // Must be packed so must know width
      require(width >= rhsWidth)
      rhs match {
        case expr: ExprInt          => expr.copy(width = width)
        case _ if width == rhsWidth => rhs
        case _ if rhs.tpe.isSigned  => rhs sx width
        case _                      => rhs zx width
      }

    ////////////////////////////////////////////////////////////////////////////
    // Shifts with an known right hand side
    ////////////////////////////////////////////////////////////////////////////

    case expr @ ExprBinary(lhs, op @ ("<<" | ">>" | "<<<" | ">>>"), Integral(_, _, rv)) =>
      assert(rv >= 0, "Negative shift amount")
      if (rv == 0) {
        lhs
      } else {
        val shift = rv.toInt
        lhs match {
          case ExprNum(s, v) =>
            assert(v >= 0 || op != ">>", "'>>' is not well defined for negative unsized values")
            op match {
              case "<<" | "<<<" => ExprNum(s, v << shift)
              case ">>" | ">>>" => ExprNum(s, v >> shift)
              case _            => unreachable
            }
          case ExprInt(false, w, v) =>
            op match {
              case "<<" | "<<<" => ExprInt(false, w, (v << shift).asU(w))
              case ">>" | ">>>" => ExprInt(false, w, (v >> shift).asU(w))
              case _            => unreachable
            }
          case ExprInt(true, w, v) =>
            op match {
              case "<<" | "<<<" => ExprInt(true, w, (v << shift).asI(w))
              case ">>>"        => ExprInt(true, w, (v >> shift).asI(w))
              case ">>"         => ExprInt(true, w, (v >> shift).asU((w - shift) max 0))
              case _            => unreachable
            }
          case _ =>
            assert(lhs.tpe.isPacked)
            val w = lhs.tpe.width.toInt
            if (rv >= w) {
              // Shift by width or more
              val s = lhs.tpe.isSigned
              if (s && op == ">>>") {
                ExprInt(true, w, -1)
              } else {
                ExprInt(s, w, 0)
              }
            } else {
              expr
            }
        }
      }

    ////////////////////////////////////////////////////////////////////////////
    // Shifts with an unknown right hand side
    ////////////////////////////////////////////////////////////////////////////

    case ExprBinary(lhs, "<<" | ">>" | "<<<" | ">>>", _) =>
      lhs match {
        case Integral(_, _, lv) if lv == 0 => lhs
        case _                             => tree
      }

    ////////////////////////////////////////////////////////////////////////////
    // Other binary operators with known unsized operands
    ////////////////////////////////////////////////////////////////////////////

    case ExprBinary(ExprNum(ls, lv), op, ExprNum(rs, rv)) =>
      def makeNum(signed: Boolean, value: BigInt): Expr = if (signed || value >= 0) {
        ExprNum(signed, value)
      } else {
        throw Fatal(tree, s"Result of operator '$op' is unsigned, but value is negative: $value")
      }

      val s = ls && rs
      op match {
        // Comparison
        case ">"  => ExprInt(false, 1, lv > rv)
        case "<"  => ExprInt(false, 1, lv < rv)
        case ">=" => ExprInt(false, 1, lv >= rv)
        case "<=" => ExprInt(false, 1, lv <= rv)
        case "==" => ExprInt(false, 1, lv == rv)
        case "!=" => ExprInt(false, 1, lv != rv)

        // Arithmetic
        case "*" => makeNum(s, lv * rv)
        case "/" => makeNum(s, lv / rv)
        case "%" => makeNum(s, lv % rv)
        case "+" => makeNum(s, lv + rv)
        case "-" => makeNum(s, lv - rv)

        // Bitwise
        case "&" => ExprNum(s, lv & rv)
        case "|" => ExprNum(s, lv | rv)
        case "^" => ExprNum(s, lv ^ rv)

        //
        case _ => unreachable // Covers '&&' and '||' which must have 1 bit operands
      }

    ////////////////////////////////////////////////////////////////////////////
    // Other binary operators with known sized operands
    ////////////////////////////////////////////////////////////////////////////

    case ExprBinary(ExprInt(ls, lw, lv), op, ExprInt(rs, rw, rv)) =>
      assert(lw == rw)
      val w = lw
      val s = ls && rs
      op match {
        // Comparison
        case ">"  => ExprInt(false, 1, if (ls == rs) lv > rv else lv.asU(w) > rv.asU(w))
        case "<"  => ExprInt(false, 1, if (ls == rs) lv < rv else lv.asU(w) < rv.asU(w))
        case ">=" => ExprInt(false, 1, if (ls == rs) lv >= rv else lv.asU(w) >= rv.asU(w))
        case "<=" => ExprInt(false, 1, if (ls == rs) lv <= rv else lv.asU(w) <= rv.asU(w))
        case "==" => ExprInt(false, 1, if (ls == rs) lv == rv else lv.asU(w) == rv.asU(w))
        case "!=" => ExprInt(false, 1, if (ls == rs) lv != rv else lv.asU(w) != rv.asU(w))

        // Arithmetic
        case "+" => ExprInt(s, w, (lv + rv).extract(0, w, s))
        case "-" => ExprInt(s, w, (lv - rv).extract(0, w, s))
        case "*" => ExprInt(s, w, (lv * rv).extract(0, w, s))
        case "/" => ExprInt(s, w, lv.extract(0, w, s) / rv.extract(0, w, s))
        case "%" => ExprInt(s, w, lv.extract(0, w, s) % rv.extract(0, w, s))

        // Bitwise
        case "&" => ExprInt(s, w, (lv & rv).extract(0, w, s))
        case "^" => ExprInt(s, w, (lv ^ rv).extract(0, w, s))
        case "|" => ExprInt(s, w, (lv | rv).extract(0, w, s))

        // Boolean
        case "&&" => ExprInt(false, 1, if (lv != 0 && rv != 0) 1 else 0)
        case "||" => ExprInt(false, 1, if (lv != 0 || rv != 0) 1 else 0)

        //
        case _ => unreachable
      }

    ////////////////////////////////////////////////////////////////////////////
    // Commutative and comparison operators with one known operand
    ////////////////////////////////////////////////////////////////////////////

    case ExprBinary(
          lhs,
          op @ ("<" | ">" | "<=" | ">=" | "==" | "!=" | "+" | "*" | "&" | "|" | "^" | "&&" | "||"),
          rhs: ExprInt
        ) =>
      simplifyComBinary(lhs, op, rhs) getOrElse tree

    case ExprBinary(
          lhs: ExprInt,
          op @ ("<" | ">" | "<=" | ">=" | "==" | "!=" | "+" | "*" | "&" | "|" | "^" | "&&" | "||"),
          rhs
        ) =>
      def swap(s: String): String = s map {
        case '<'   => '>'
        case '>'   => '<'
        case other => other
      }
      simplifyComBinary(rhs, swap(op), lhs) getOrElse tree

    ////////////////////////////////////////////////////////////////////////////
    // Non commutative operators with a known right hand side
    ////////////////////////////////////////////////////////////////////////////

    case ExprBinary(lhs, op @ ("/" | "%" | "-"), ExprInt(rs, w, v)) =>
      assert(lhs.tpe.width == w)
      val s = lhs.tpe.isSigned && rs
      op match {
        case "/" => if (v == 1) lhs.withSignedness(s) else tree
        case "%" => if (v == 1) ExprInt(s, w, 0) else tree
        case "-" => if (v == 0) lhs.withSignedness(s) else tree
      }

    ////////////////////////////////////////////////////////////////////////////
    // Non commutative operators with a known left hand side
    ////////////////////////////////////////////////////////////////////////////

    case ExprBinary(ExprInt(ls, w, v), op @ ("/" | "%" | "-"), rhs) =>
      assert(w == rhs.tpe.width)
      val s = ls && rhs.tpe.isSigned
      op match {
        case "/" => if (v == 0) ExprInt(s, w, 0) else tree
        case "%" => if (v == 0) ExprInt(s, w, 0) else tree
        case "-" => if (v == 0) (-rhs).withSignedness(s) else tree
      }

    //
    case _ => tree
  }

  private def transformTernary(tree: ExprCond): Expr = tree match {
    case ExprCond(_, thenExpr, elseExpr) =>
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

    case ExprIndex(ExprBuiltin(DollarSigned | DollarUnsigned, args), idx) =>
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

        case _ =>
          if (index == 0 && expr.tpe.isPacked && expr.tpe.width == 1) expr.asUnsigned else tree
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
        case "-:" => aLIdx - (aRIdx.value.toInt - 1)
        case _    => unreachable
      }
      val bLsb = bOp match {
        case ":"  => bRidx
        case "+:" => bLidx
        case "-:" => bLidx - (bRidx.value.toInt - 1)
        case _    => unreachable
      }
      val bMsb = bOp match {
        case ":"  => bLidx
        case "+:" => bLidx + (bRidx.value.toInt - 1)
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

    case ExprSlice(ExprBuiltin(DollarSigned | DollarUnsigned, args), _, _, _) =>
      tree.copy(expr = args.head.expr) withLoc tree.loc

    ////////////////////////////////////////////////////////////////////////////
    // Fold slices with known indices
    ////////////////////////////////////////////////////////////////////////////

    case ExprSlice(expr, lExpr @ Integral(_, lWOpt, lIdx), op, rExpr @ Integral(_, _, rIdx)) =>
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
        // Special cases
        ////////////////////////////////////////////////////////////////////////

        case _ =>
          op match {
            case ":" =>
              if (rIdx == 0 && lIdx == expr.tpe.width - 1) {
                // Full width slice
                if (expr.tpe.isSigned) expr.castUnsigned else expr
              } else {
                tree
              }
            case "+:" | "-:" =>
              if (rIdx == expr.tpe.width) {
                // Full width slice. Note: This assumes the slice is well
                // formed, i.e.: lIdx is 0 for '+:' or width - 1 for '-:'
                if (expr.tpe.isSigned) expr.castUnsigned else expr
              } else {
                // Convert into : slice
                val addend = TypeAssigner(
                  lWOpt.fold[Expr](Expr(rIdx - 1))(ExprInt(false, _, rIdx - 1)) withLocOf rExpr
                )
                if (op.head == '+') {
                  ExprSlice(expr, lExpr + addend, ":", lExpr) withLocOf tree
                } else {
                  ExprSlice(expr, lExpr, ":", lExpr - addend) withLocOf tree
                }
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
          case ExprBuiltin(DollarSigned | DollarUnsigned, _) => true
          case _                                             => false
        } =>
      ExprCat {
        args map {
          case ExprBuiltin(DollarSigned | DollarUnsigned, args) => args.head.expr
          case arg                                              => arg
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

    case ExprRep(count, ExprBuiltin(DollarSigned | DollarUnsigned, args)) =>
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
      count.valueOption map { cnt =>
        val c = cnt.toInt
        val w = width.toInt
        val b = value.asU(w)
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
          case Integral(_, _, v) => ExprNum(signed, v) withLoc tree.loc
          case _                 => unreachable
        }

      case TypeInt(signed, width) =>
        def mkValue(value: BigInt): Expr = {
          // TODO: should check out of range width inference in the typer
          val lo = if (signed) -(BigInt(1) << (width.toInt - 1)) else BigInt(0)
          val hi = if (signed) BigInt.mask(width.toInt - 1) else BigInt.mask(width)
          if (value > hi || value < lo) {
            val signedness = if (signed) "signed" else "unsigned"
            throw Fatal(tree, s"Value $value cannot be represented with $width $signedness bits")
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
          case ExprCast(_, ExprNum(_, v)) =>
            mkValue(v)
          case ExprCast(_, ExprInt(_, eWidth, v)) =>
            require(width.toInt >= eWidth)
            mkValue(v)
          case _ =>
            val kWidth = width.toInt
            val eWidth = expr.tpe.width.toInt
            require(kWidth >= eWidth)
            if (kWidth == eWidth) {
              if (!signed && expr.tpe.isSigned) {
                expr.castUnsigned
              } else if (signed && !expr.tpe.isSigned) {
                expr.castSigned
              } else {
                expr
              }
            } else {
              if (expr.tpe.isSigned) {
                expr sx kWidth
              } else {
                expr zx kWidth
              }
            }
        }

      case _ => tree
    }
  }

}
