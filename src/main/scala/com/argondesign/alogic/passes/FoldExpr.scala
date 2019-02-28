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

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees.Expr.Integral
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.TreeInTypeTransformer
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.BigIntOps._
import com.argondesign.alogic.util.unreachable
import com.argondesign.alogic.core.Symbols.Symbol

import scala.language.implicitConversions

final class FoldExpr(
    assignTypes: Boolean, // Ensure results have type assigned
    foldRefs: Boolean // Replace references to known constant symbols with their value
)(
    implicit cc: CompilerContext
) extends TreeTransformer {

  override val typed: Boolean = assignTypes

  private implicit def boolean2BigInt(bool: Boolean) = if (bool) BigInt(1) else BigInt(0)

  private val shiftOps = Set("<<", ">>", "<<<", ">>>")

  private def foldShiftUnsized(expr: ExprBinary): Expr = {
    val ExprBinary(ExprNum(ls, lv), op, rhs) = expr
    val rv = rhs.value.get
    val negl = lv < 0
    val negr = rv < 0
    val num = (ls, op) match {
      case _ if negr => {
        cc.error(expr, "Negative shift amount")
        ExprError()
      }
      case (true, ">>") if negl => {
        cc.error(expr, "'>>' is not well defined for negative unsized values")
        ExprError()
      }
      case (signed, "<<")  => ExprNum(signed, lv << rv.toInt)
      case (signed, ">>")  => ExprNum(signed, lv >> rv.toInt)
      case (signed, "<<<") => ExprNum(signed, lv << rv.toInt)
      case (signed, ">>>") => ExprNum(signed, lv >> rv.toInt)
      case _               => unreachable
    }
    num withLoc expr.loc
  }

  private def isBuiltinSU(symbol: Symbol) = {
    symbol.name == "$signed" || symbol.name == "$unsigned"
  }

  // In types we always fold refs so that widths are always computable
  private val typeFoldExpr = if (foldRefs) this else new FoldExpr(assignTypes, foldRefs = true)
  private object TypeFoldExpr extends TreeInTypeTransformer(typeFoldExpr)

  override def transform(tree: Tree): Tree = {
    val result = tree match {

      ////////////////////////////////////////////////////////////////////////////
      // Propagate error expressions
      ////////////////////////////////////////////////////////////////////////////

      case ExprUnary(_, expr: ExprError)      => expr
      case ExprBinary(expr: ExprError, _, _)  => expr
      case ExprBinary(_, _, expr: ExprError)  => expr
      case ExprTernary(expr: ExprError, _, _) => expr
      case ExprTernary(_, expr: ExprError, _) => expr
      case ExprTernary(_, _, expr: ExprError) => expr

      ////////////////////////////////////////////////////////////////////////////
      // Fold refs
      ////////////////////////////////////////////////////////////////////////////

      case ExprRef(symbol) if foldRefs && symbol.kind.isConst => {
        symbol.attr.init getOrElse tree
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold shifts with an unsized left hand side
      ////////////////////////////////////////////////////////////////////////////

      case expr @ ExprBinary(_: ExprNum, op, _: ExprNum) if shiftOps contains op => {
        foldShiftUnsized(expr)
      }

      case expr @ ExprBinary(_: ExprNum, op, _: ExprInt) if shiftOps contains op => {
        foldShiftUnsized(expr)
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold unary expressions with an unsized operand
      ////////////////////////////////////////////////////////////////////////////

      case ExprUnary(op, expr @ ExprNum(signed, value)) => {
        val num = op match {
          // Invalid cases
          case "-" if !signed && value > 0 => {
            cc.error(tree, "Unary '-' is not well defined for unsigned values")
            ExprError()
          }
          case "^" if value < 0 => {
            cc.error(tree, "Unary '^' is not well defined for unsized negative values")
            ExprError()
          }
          case "~" if !signed => {
            cc.error(tree, "Unary '~' is not well defined for unsized unsigned values")
            ExprError()
          }
          // Valid cases
          case "+" => expr
          case "-" => ExprNum(signed, -value)
          case "~" => ExprNum(true, ~value)
          case "!" => ExprInt(false, 1, value == 0)
          case "&" => ExprInt(false, 1, value == -1)
          case "|" => ExprInt(false, 1, value != 0)
          case "^" => ExprInt(false, 1, value.bitCount & 1)
        }
        if (num.hasLoc) num else num withLoc tree.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold some special unary over unary combinations
      ////////////////////////////////////////////////////////////////////////////

      case ExprUnary(aOp, ExprUnary(bOp, expr)) => {
        val isBoolType = expr.isPacked && !expr.isSigned && expr.width == 1
        val res = (aOp, bOp) match {
          case ("~", "~")               => expr
          case ("~", "!") if isBoolType => expr
          case ("!", "~") if isBoolType => expr
          case ("!", "!") if isBoolType => expr
          case _                        => tree
        }
        if (res.hasLoc) res else res withLoc tree.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold other binary expressions with 2 unsized operands
      ////////////////////////////////////////////////////////////////////////////

      case ExprBinary(ExprNum(ls, lv), op, ExprNum(rs, rv)) => {
        def makeUnsignedExprNum(value: BigInt) = {
          if (value >= 0) {
            ExprNum(false, value)
          } else {
            cc.error(tree, s"Result of operator '${op}' is unsigned, but value is negative")
            ExprError()
          }
        }

        val negl = lv < 0
        val negr = rv < 0
        val nege = negl || negr
        val num = (ls, op, rs) match {
          // Always valid
          case (_, ">", _)  => ExprNum(false, lv > rv)
          case (_, "<", _)  => ExprNum(false, lv < rv)
          case (_, ">=", _) => ExprNum(false, lv >= rv)
          case (_, "<=", _) => ExprNum(false, lv <= rv)
          case (_, "==", _) => ExprNum(false, lv == rv)
          case (_, "!=", _) => ExprNum(false, lv != rv)
          case (_, "&&", _) => ExprNum(false, (lv != 0) && (rv != 0))
          case (_, "||", _) => ExprNum(false, (lv != 0) || (rv != 0))

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
          case (_, op @ ("&" | "^" | "|"), _) if nege => {
            cc.error(tree,
                     s"Bitwise '${op}' operator is not well defined for negative unsized values")
            ExprError()
          }
          case (_, "~^", _) => {
            cc.error(tree, "Bitwise '~^' operator is not well defined for unsized values")
            ExprError()
          }
          case (true, "&", true) => ExprNum(true, lv & rv)
          case (true, "^", true) => ExprNum(true, lv ^ rv)
          case (true, "|", true) => ExprNum(true, lv | rv)
          case (_, "&", _)       => ExprNum(false, lv & rv)
          case (_, "^", _)       => ExprNum(false, lv ^ rv)
          case (_, "|", _)       => ExprNum(false, lv | rv)

          case _ => unreachable
        }
        num withLoc tree.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold unary expressions with a sized operand
      ////////////////////////////////////////////////////////////////////////////

      case ExprUnary(op, expr @ ExprInt(signed, width, value)) => {
        lazy val mask = (BigInt(1) << width) - 1
        val num = op match {
          // Invalid cases
          case "-" if !signed && value > 0 => {
            cc.error(tree, "Unary '-' is not well defined for unsigned values")
            ExprError()
          }
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
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold binary expressions with equally sized operands
      ////////////////////////////////////////////////////////////////////////////

      case ExprBinary(ExprInt(ls, lw, lv), op, ExprInt(rs, rw, rv)) if lw == rw => {
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
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold binary operators with one known operand if possible
      ////////////////////////////////////////////////////////////////////////////

      case ExprBinary(Integral(_, _, lv), op, rhs) => {
        val res = op match {
          case "&&" if lv == 0                                         => ExprInt(false, 1, 0)
          case "&&" if rhs.isPacked && !rhs.isSigned && rhs.width == 1 => rhs
          case "||" if lv != 0                                         => ExprInt(false, 1, 1)
          case "||" if rhs.isPacked && !rhs.isSigned && rhs.width == 1 => rhs
          case _                                                       => tree
        }
        if (res.hasLoc) res else res withLoc tree.loc
      }

      case ExprBinary(lhs, op, Integral(_, _, rv)) => {
        val res = op match {
          case "&&" if rv == 0                                         => ExprInt(false, 1, 0)
          case "&&" if lhs.isPacked && !lhs.isSigned && lhs.width == 1 => lhs
          case "||" if rv != 0                                         => ExprInt(false, 1, 1)
          case "||" if lhs.isPacked && !lhs.isSigned && lhs.width == 1 => lhs
          case _                                                       => tree
        }
        if (res.hasLoc) res else res withLoc tree.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold binary expressions with a mixed operand
      ////////////////////////////////////////////////////////////////////////////

      // TODO: get rid of these once unsized integers are better checked
      case ExprBinary(ExprInt(false, 32, lv), op, ExprNum(false, rv)) => {
        op match {
          case "+" => ExprInt(false, 32, lv + rv) withLoc tree.loc
          case "-" => ExprInt(false, 32, lv - rv) withLoc tree.loc
          case "*" => ExprInt(false, 32, lv * rv) withLoc tree.loc
          case _   => tree
        }
      }

      case ExprBinary(ExprNum(false, lv), op, ExprInt(false, 32, rv)) => {
        op match {
          case "+" => ExprInt(false, 32, lv + rv) withLoc tree.loc
          case "-" => ExprInt(false, 32, lv - rv) withLoc tree.loc
          case "*" => ExprInt(false, 32, lv * rv) withLoc tree.loc
          case _   => tree
        }
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold ternary expressions
      ////////////////////////////////////////////////////////////////////////////

      case ExprTernary(cond, thenExpr, elseExpr) => {
        cond.value map { value =>
          if (value != 0) thenExpr else elseExpr
        } getOrElse {
          if (!thenExpr.hasTpe || !elseExpr.hasTpe) {
            tree
          } else if (thenExpr == elseExpr && thenExpr.tpe == elseExpr.tpe) {
            thenExpr
          } else {
            tree
          }
        }
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold index into sized/unsized integers
      ////////////////////////////////////////////////////////////////////////////

      case ExprIndex(Integral(_, _, value), Integral(_, _, idx)) => {
        // TODO: error on out of range
        ExprInt(false, 1, (value >> idx.toInt) & 1) withLoc tree.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold slice into sized/unsized integers
      ////////////////////////////////////////////////////////////////////////////

      case ExprSlice(Integral(_, _, value), Integral(_, _, blidx), op, Integral(_, _, bridx)) => {
        // TODO: error on out of range
        val lidx = blidx.toInt
        val ridx = bridx.toInt
        val lsb = op match {
          case ":"  => ridx
          case "+:" => lidx
          case "-:" => lidx - ridx + 1
          case _    => unreachable
        }
        val width = op match {
          case ":" => lidx - ridx + 1
          case _   => ridx
        }
        ExprInt(false, width, value.extract(lsb, width)) withLoc tree.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold index over a slice
      ////////////////////////////////////////////////////////////////////////////

      case ExprIndex(ExprSlice(expr, lidx, op, ridx), idx) => {
        // TODO: error on out of range
        val lsb = op match {
          case ":"  => ridx
          case "+:" => lidx
          case "-:" => lidx - ridx + 1
          case _    => unreachable
        }
        ExprIndex(expr, lsb + idx) withLoc tree.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold Slice over a slice
      ////////////////////////////////////////////////////////////////////////////

      case ExprSlice(ExprSlice(expr, aLidx, aOp, aRidx), bLidx, bOp, bRidx) => {
        // TODO: error on out of range
        val aLsb = aOp match {
          case ":"  => aRidx
          case "+:" => aLidx
          case "-:" => aLidx - aRidx + 1
          case _    => unreachable
        }
        val bLsb = bOp match {
          case ":"  => bRidx
          case "+:" => bLidx
          case "-:" => bLidx - bRidx + 1
          case _    => unreachable
        }
        val bMsb = bOp match {
          case ":"  => bLidx
          case "+:" => bLidx + bRidx - 1
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
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold index over $signed/$unsigned
      ////////////////////////////////////////////////////////////////////////////

      case ExprIndex(ExprCall(ExprRef(symbol), List(arg)), idx) if isBuiltinSU(symbol) => {
        ExprIndex(arg, idx) withLoc tree.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold slice over $signed/$unsigned
      ////////////////////////////////////////////////////////////////////////////

      case ExprSlice(ExprCall(ExprRef(symbol), List(arg)), lidx, op, ridx)
          if isBuiltinSU(symbol) => {
        ExprSlice(arg, lidx, op, ridx) withLoc tree.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Remove pointless $signed/$unsigned from Cat/Rep arguments
      ////////////////////////////////////////////////////////////////////////////

      case ExprCat(args) if args exists {
            case ExprCall(ExprRef(symbol), _) => isBuiltinSU(symbol)
            case _                            => false
          } => {
        ExprCat {
          args map {
            case ExprCall(ExprRef(symbol), List(arg)) if isBuiltinSU(symbol) => arg
            case arg                                                         => arg
          }
        } withLoc tree.loc
      }

      case ExprRep(count, ExprCall(ExprRef(symbol), List(arg))) if isBuiltinSU(symbol) => {
        ExprRep(count, arg) withLoc tree.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold width 1 slices
      ////////////////////////////////////////////////////////////////////////////

      case ExprSlice(expr, lidx, ":", ridx) if lidx == ridx => {
        // TODO: strictly, lidx/ridx could be stuff like @randbit, or other non-pure function
        ExprIndex(expr, lidx) withLoc tree.loc
      }

      case ExprSlice(expr, lidx, ("-:" | "+:"), ridx) if ridx.value contains BigInt(1) => {
        ExprIndex(expr, lidx) withLoc tree.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold concatenations of sized integers
      ////////////////////////////////////////////////////////////////////////////

      case ExprCat(parts) if parts forall { _.isInstanceOf[ExprInt] } => {
        val start = (0, BigInt(0))
        val (width, value) = (parts :\ start) {
          case (ExprInt(_, w, v), (aw, av)) => (w.toInt + aw, (v.extract(0, w.toInt) << aw) | av)
          case _                            => unreachable
        }
        ExprInt(signed = false, width, value) withLoc tree.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold repetitions of sized integers
      ////////////////////////////////////////////////////////////////////////////

      case ExprRep(count, ExprInt(_, width, value)) => {
        count.value map { cnt =>
          val c = cnt.toInt
          val w = width.toInt
          val b = value.extract(0, w)
          val v = (BigInt(0) /: (0 until c)) { case (a, _) => (a << w) | b }
          ExprInt(signed = false, c * w, v) withLoc tree.loc
        } getOrElse tree
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold built-in functions
      ////////////////////////////////////////////////////////////////////////////

      case call @ ExprCall(ExprRef(symbol), _) if symbol.isBuiltin => {
        val result = cc.foldBuiltinCall(call)
        if (result eq call) {
          call
        } else if (assignTypes) {
          result regularize tree.loc
        } else {
          result
        }
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold expressions inside Type instances
      ////////////////////////////////////////////////////////////////////////////

      case decl @ DeclIdent(_, kind, _) => {
        val newKind = kind rewrite TypeFoldExpr
        if (kind eq newKind) decl else decl.copy(kind = newKind) withLoc tree.loc
      }

      case expr @ ExprType(kind) => {
        val newKind = kind rewrite TypeFoldExpr
        if (kind eq newKind) expr else ExprType(newKind) withLoc tree.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Leave rest alone
      ////////////////////////////////////////////////////////////////////////////

      case _ => tree
    }

    // Recursively fold the resulting expression
    val result2 = if (result ne tree) { walk(result) } else { result }

    if (assignTypes && !result2.hasTpe) {
      TypeAssigner(result2)
    } else {
      result2
    }
  }

}

object FoldExpr {

  class FoldExprPass(assignTypes: Boolean, foldRefs: Boolean) extends TreeTransformerPass {
    val name = "fold-expr"
    def create(implicit cc: CompilerContext) = new FoldExpr(assignTypes, foldRefs)(cc)
  }

  def apply(assignTypes: Boolean, foldRefs: Boolean): Pass = {
    new FoldExprPass(assignTypes, foldRefs)
  }
}
