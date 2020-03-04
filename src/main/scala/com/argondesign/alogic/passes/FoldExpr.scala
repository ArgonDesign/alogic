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
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Math.clog2
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.BigIntOps._
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.implicitConversions

final class FoldExpr(
    foldRefs: Boolean // Replace references to known constant symbols with their value
)(
    implicit cc: CompilerContext
) extends TreeTransformer {
  private implicit def boolean2BigInt(bool: Boolean): BigInt = if (bool) BigInt(1) else BigInt(0)

  private val shiftOps = Set("<<", ">>", "<<<", ">>>")

  private var dontFoldNextSym = false

  private val tgtTpe = mutable.Stack[Type]()

  private def foldShiftUnsized(expr: ExprBinary): Expr = expr match {
    case ExprBinary(ExprNum(ls, lv), op, rhs) =>
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
    case _ => unreachable
  }

  private def isBuiltinSU(symbol: Symbol) = {
    symbol.name == "$signed" || symbol.name == "$unsigned"
  }

  override def enter(tree: Tree): Option[Tree] = {
    if (foldRefs) {
      tree match {
        case ExprIndex(_, idx) =>
          val idxValOpt = idx.value
          // If idx is not a constant, don't fold symbol at start of target
          val idxNonConst = idxValOpt.isEmpty
          dontFoldNextSym ||= idxNonConst
        case ExprSlice(_, lidx, _, _) =>
          val lidxValOpt = lidx.value
          // If lidx is not a constant, don't fold symbol at start of target
          val lidxNonConst = lidxValOpt.isEmpty
          dontFoldNextSym ||= lidxNonConst
        case _ => ()
      }

      // Next keep track of tgt type so that when transforming Indices or Slices,
      // if we see ExprInt in the place of the target, we know how to index/slice from it.
      tree match {
        case ExprIndex(tgt, _)       => tgtTpe push tgt.tpe.underlying
        case ExprSlice(tgt, _, _, _) => tgtTpe push tgt.tpe.underlying
        case _                       => tgtTpe push TypeError
      }
    }

    // Pre-order folds to avoid creating invalid intermediate nodes
    tree pipe {
      ////////////////////////////////////////////////////////////////////////////
      // Fold selects
      ////////////////////////////////////////////////////////////////////////////

      case ExprSelect(tgt, sel, _) if tgt.tpe.isRecord =>
        walk(tgt) match {
          case Integral(_, _, value) =>
            val (fieldTpe, lessSigFieldTpes) =
              tgt.tpe.asRecord.publicSymbols dropWhile { _.name != sel } map { _.kind } match {
                case head :: tail => (head, tail)
                case Nil          => unreachable
              }
            val lsb = lessSigFieldTpes.map(_.width).sum.toInt
            val result = value.extract(lsb, fieldTpe.width.toInt, fieldTpe.isSigned)
            Some {
              TypeAssigner {
                ExprInt(fieldTpe.isSigned, fieldTpe.width.toInt, result) withLoc tree.loc
              }
            }
          case _ => None
        }

      //
      case _ => None
    } tap {
      case Some(_) if foldRefs => tgtTpe.pop()
      case _                   =>
    }
  }

  override def transform(tree: Tree): Tree = {
    val result = tree match {

      ////////////////////////////////////////////////////////////////////////////
      // Don't fold anything with errors
      ////////////////////////////////////////////////////////////////////////////

      case tree if tree.children collect { case t: Tree => t } exists { _.tpe.isError } => tree

      ////////////////////////////////////////////////////////////////////////////
      // Fold refs
      ////////////////////////////////////////////////////////////////////////////

      case ExprSym(symbol) if foldRefs =>
        if (dontFoldNextSym) {
          dontFoldNextSym = false
          tree
        } else if (symbol.kind.isConst) {
          symbol.init getOrElse tree
        } else {
          tree
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
          case "~" if !signed => {
            cc.error(tree, "Unary '~' is not well defined for unsized unsigned values")
            ExprError()
          }
          // Valid cases
          case "+" => expr
          case "-" => ExprNum(signed, -value)
          case "~" => ExprNum(true, -value - 1)
          case "!" => ExprInt(false, 1, value == 0)
        }
        if (num.hasLoc) num else num withLoc tree.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold some special unary over unary combinations
      ////////////////////////////////////////////////////////////////////////////

      case ExprUnary(aOp, ExprUnary(bOp, expr)) => {
        val isBoolType = expr.tpe.isPacked && !expr.tpe.isSigned && expr.tpe.width == 1
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
          case "&&" if lv == 0                                                     => ExprInt(false, 1, 0)
          case "&&" if rhs.tpe.isPacked && !rhs.tpe.isSigned && rhs.tpe.width == 1 => rhs
          case "||" if lv != 0                                                     => ExprInt(false, 1, 1)
          case "||" if rhs.tpe.isPacked && !rhs.tpe.isSigned && rhs.tpe.width == 1 => rhs
          case _                                                                   => tree
        }
        if (res.hasLoc) res else res withLoc tree.loc
      }

      case ExprBinary(lhs, op, Integral(_, _, rv)) => {
        val res = op match {
          case "&&" if rv == 0                                                     => ExprInt(false, 1, 0)
          case "&&" if lhs.tpe.isPacked && !lhs.tpe.isSigned && lhs.tpe.width == 1 => lhs
          case "||" if rv != 0                                                     => ExprInt(false, 1, 1)
          case "||" if lhs.tpe.isPacked && !lhs.tpe.isSigned && lhs.tpe.width == 1 => lhs
          case _                                                                   => tree
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
        tgtTpe.headOption match {
          case Some(TypeVector(eKind, _)) =>
            val result = value.extract((eKind.width * idx).toInt, eKind.width.toInt, eKind.isSigned)
            ExprInt(eKind.isSigned, eKind.width.toInt, result) withLoc tree.loc
          case _ =>
            ExprInt(false, 1, (value >> idx.toInt) & 1) withLoc tree.loc
        }
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
        tgtTpe.headOption match {
          case Some(TypeVector(eKind, _)) =>
            val result =
              value.extract((eKind.width * lsb).toInt, (eKind.width * width).toInt)
            ExprInt(false, (eKind.width * width).toInt, result) withLoc tree.loc
          case _ =>
            ExprInt(false, width, value.extract(lsb, width)) withLoc tree.loc
        }
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold index over a slice
      ////////////////////////////////////////////////////////////////////////////

      case ExprIndex(ExprSlice(expr, lidx, op, ridx), idx) => {
        // TODO: error on out of range
        val lsb = op match {
          case ":"  => ridx
          case "+:" => lidx
          case "-:" => lidx - ridx + 1 // FIXME: needs typing
          case _    => unreachable
        }
        ExprIndex(expr, lsb + (idx zx lsb.tpe.width.toInt)) withLoc tree.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold Slice over a slice
      ////////////////////////////////////////////////////////////////////////////

      case ExprSlice(ExprSlice(expr, aLidx, aOp, aRidx), bL, bOp, bR) => {
        val idxWidth = aLidx.tpe.width.toInt

        val bLidx = bL zx idxWidth
        val bRidx = bR zx idxWidth

        // TODO: error on out of range
        val aLsb = aOp match {
          case ":"  => aRidx
          case "+:" => aLidx
          case "-:" => aLidx - (aRidx.value.get.toInt - 1)
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
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold index over $signed/$unsigned
      ////////////////////////////////////////////////////////////////////////////

      case ExprIndex(ExprCall(ExprSym(symbol), List(ArgP(arg))), idx) if isBuiltinSU(symbol) => {
        ExprIndex(arg, idx) withLoc tree.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold slice over $signed/$unsigned
      ////////////////////////////////////////////////////////////////////////////

      case ExprSlice(ExprCall(ExprSym(symbol), List(ArgP(arg))), lidx, op, ridx)
          if isBuiltinSU(symbol) => {
        ExprSlice(arg, lidx, op, ridx) withLoc tree.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Remove pointless $signed/$unsigned from Cat/Rep arguments
      ////////////////////////////////////////////////////////////////////////////

      case ExprCat(args) if args exists {
            case ExprCall(ExprSym(symbol), _) => isBuiltinSU(symbol)
            case _                            => false
          } => {
        ExprCat {
          args map {
            case ExprCall(ExprSym(symbol), List(ArgP(arg))) if isBuiltinSU(symbol) => arg
            case arg                                                               => arg
          }
        } withLoc tree.loc
      }

      case ExprRep(count, ExprCall(ExprSym(symbol), List(ArgP(arg)))) if isBuiltinSU(symbol) => {
        ExprRep(count, arg) withLoc tree.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold width 1 slices
      ////////////////////////////////////////////////////////////////////////////

      case ExprSlice(expr, lidx, ":", ridx) if !expr.tpe.underlying.isVector && lidx == ridx => {
        // TODO: strictly, lidx/ridx could be stuff like @randbit, or other non-pure function
        ExprIndex(expr, lidx) withLoc tree.loc
      }

      case ExprSlice(expr, lidx, ("-:" | "+:"), ridx)
          if !expr.tpe.underlying.isVector && (ridx.value contains BigInt(1)) => {
        ExprIndex(expr, lidx) withLoc tree.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold index zero of width one and full width slices
      ////////////////////////////////////////////////////////////////////////////

      case ExprIndex(expr, ExprInt(_, _, i))
          if i == 0 && expr.tpe.isPacked && expr.tpe.width == 1 => {
        expr
      }

      case ExprSlice(expr, ExprInt(_, _, m), ":", ExprInt(_, _, l))
          if m == expr.tpe.width - 1 && l == 0 => {
        expr
      }

      case ExprSlice(expr, ExprInt(_, _, l), "+:", ExprInt(_, _, w))
          if w == expr.tpe.width && l == 0 => {
        expr
      }

      case ExprSlice(expr, ExprInt(_, _, m), "-:", ExprInt(_, _, w))
          if m == expr.tpe.width - 1 && w == expr.tpe.width => {
        expr
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold repetitions of count 1
      ////////////////////////////////////////////////////////////////////////////

      case ExprRep(Integral(_, _, rep), expr) if rep == 1 => {
        expr
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold concatenations of sized integers
      ////////////////////////////////////////////////////////////////////////////

      case ExprCat(parts) if parts forall { _.isInstanceOf[ExprInt] } => {
        val start = (0, BigInt(0))
        val (width, value) = parts.foldRight(start) {
          case (ExprInt(_, w, v), (aw, av)) => (w.toInt + aw, (v.extract(0, w.toInt) << aw) | av)
          case _                            => unreachable
        }
        ExprInt(signed = false, width, value) withLoc tree.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Flatten concatenations
      ////////////////////////////////////////////////////////////////////////////

      case ExprCat(List(part: Expr)) => part

      case ExprCat(parts) if parts exists { _.isInstanceOf[ExprCat] } => {
        ExprCat {
          parts flatMap {
            case ExprCat(nested) => nested
            case expr            => Iterator.single(expr)
          }
        } withLoc tree.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold repetitions of sized integers
      ////////////////////////////////////////////////////////////////////////////

      case ExprRep(count, ExprInt(_, width, value)) => {
        count.value map { cnt =>
          val c = cnt.toInt
          val w = width.toInt
          val b = value.extract(0, w)
          val v = (0 until c).foldLeft(BigInt(0)) { case (a, _) => (a << w) | b }
          ExprInt(signed = false, c * w, v) withLoc tree.loc
        } getOrElse tree
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold constant index of concatenations
      ////////////////////////////////////////////////////////////////////////////

      case ExprIndex(ExprCat(parts), ExprInt(_, _, index)) => {
        @tailrec def loop(remaining_parts: List[Expr], remaining_index: BigInt): ExprIndex = {
          val head_width = remaining_parts.head.tpe.width;
          if (remaining_index >= head_width) {
            loop(remaining_parts.tail, remaining_index - head_width)
          } else {
            val part = remaining_parts.head
            ExprIndex(part, ExprInt(false, clog2(head_width) max 1, remaining_index))
          }
        }

        loop(parts.reverse, index) regularize tree.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold constant slice of concatenations
      ////////////////////////////////////////////////////////////////////////////

      case ExprSlice(ExprCat(parts), Integral(_, _, lidx), op, Integral(_, _, ridx)) => {
        val (msb, lsb) = op match {
          case ":"  => (lidx, ridx)
          case "+:" => (lidx + ridx - 1, lidx)
          case "-:" => (lidx, lidx - ridx + 1)
          case _    => unreachable
        }

        def slice_part(part: Expr, m: BigInt, l: BigInt): ExprSlice = {
          val width_bits = clog2(part.tpe.width) max 1
          ExprSlice(part, ExprInt(false, width_bits, m), ":", ExprInt(false, width_bits, l))
        }

        @tailrec def loop(concat_list: List[Expr],
                          rem_parts: List[Expr],
                          msb_rem: BigInt,
                          lsb_rem: BigInt): List[Expr] = {
          val next_part_width = rem_parts.head.tpe.width
          if (lsb_rem >= next_part_width) {
            // None of this part included in slice
            loop(concat_list, rem_parts.tail, msb_rem - next_part_width, lsb_rem - next_part_width)
          } else if (msb_rem < next_part_width) {
            // This is the final part included in slice
            slice_part(rem_parts.head, msb_rem, lsb_rem) :: concat_list
          } else {
            // Slice this part and continue
            val new_concat_list = slice_part(rem_parts.head, next_part_width - 1, lsb_rem) :: concat_list
            loop(new_concat_list, rem_parts.tail, msb_rem - next_part_width, 0)
          }
        }

        ExprCat(loop(List(), parts.reverse, msb, lsb)) regularize tree.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold constant index of repetitions
      ////////////////////////////////////////////////////////////////////////////

      case ExprIndex(ExprRep(_, expr), ExprInt(_, _, index)) => {
        val expr_width = expr.tpe.width
        ExprIndex(expr, ExprInt(false, clog2(expr_width) max 1, index % expr_width)) regularize tree.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold constant slice of repetitions
      ////////////////////////////////////////////////////////////////////////////

      case ExprSlice(ExprRep(_, expr), ExprInt(_, _, lidx), op, ExprInt(_, _, ridx)) => {
        val (msb, lsb) = op match {
          case ":"  => (lidx, ridx)
          case "+:" => (lidx + ridx - 1, lidx)
          case "-:" => (lidx, lidx - ridx + 1)
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
          val num_intermediate_reps = ((msb - lsb + 1) - ms_slice_width - ls_slice_width) / expr_width
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
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold built-in functions
      ////////////////////////////////////////////////////////////////////////////

      case call @ ExprCall(ExprSym(symbol), _) if symbol.isBuiltin => {
        val result = cc.foldBuiltinCall(call)
        if (result eq call) {
          call
        } else {
          result regularize tree.loc
        }
      }

      ////////////////////////////////////////////////////////////////////////////
      // Fold casts
      ////////////////////////////////////////////////////////////////////////////

      case ExprCast(TypeNum(signed), expr) => ExprNum(signed, expr.value.get) withLoc tree.loc

      case ExprCast(kind: TypeInt, ExprNum(signed, value)) => {
        val width = kind.width.toInt
        val lo = if (signed) -(BigInt(1) << (width - 1)) else BigInt(0)
        val hi = if (signed) BigInt.mask(width - 1) else BigInt.mask(width)
        if (value > hi || value < lo) {
          // TODO: should out of range width inference in the typer
          val signedness = if (signed) "signed" else "unsigned"
          cc.error(tree, s"Value ${value} cannot be represented with ${width} ${signedness} bits")
          ExprError() withLoc tree.loc
        } else {
          ExprInt(signed, width.toInt, value) withLoc tree.loc
        }
      }

      case cast @ ExprCast(_: TypeInt, expr) if expr.tpe.underlying.isNum => {
        // Expression is TypeNum but not ExprNum, but anything with TypeNum is
        // a compile time constant, so just simplify it. This will cause the
        // repeat walk of the rewritten cast to enter the case above when we
        // cast an ExprNum to a TypeInt. It is however possible that we don't
        // have bindings for TypeNum symbols yet, so only re-write if the
        // simplified argument is actually different
        expr.simplify match {
          case `expr`     => cast
          case simplified => cast.copy(expr = simplified) withLoc tree.loc
        }
      }

      case ExprCast(kind: TypeInt, expr) if expr.tpe.isPacked => {
        val kWidth = kind.width.toInt
        val eWidth = expr.tpe.width.toInt
        require(kWidth >= expr.tpe.width)
        val res = if (kWidth == eWidth) {
          expr
        } else if (expr.tpe.isSigned) {
          expr sx kWidth
        } else {
          expr zx kWidth
        }
        res.simplify
      }

      ////////////////////////////////////////////////////////////////////////////
      // Leave rest alone
      ////////////////////////////////////////////////////////////////////////////

      case _ => tree
    }

    if (foldRefs) {
      tgtTpe.pop()
    }

    // Recursively fold the resulting expression
    val result2 = if (result ne tree) walk(result) else result

    if (!result2.hasTpe) TypeAssigner(result2) else result2
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(tgtTpe.isEmpty)
    assert(!dontFoldNextSym)
  }

}

object FoldExpr extends PairTransformerPass {
  val name = "fold-expr"
  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) = {
    val transformer = new FoldExpr(foldRefs = false)(cc)
    (transformer(decl), transformer(defn))
  }
}
