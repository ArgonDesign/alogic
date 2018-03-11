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
// Constant folding
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.transform

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.util.unreachable

import scala.language.implicitConversions

final class ConstantFold(implicit cc: CompilerContext) extends TreeTransformer {

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

  override def transform(tree: Tree): Tree = tree match {

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
        // Valid cases
        case "+"           => expr
        case "-" if signed => ExprNum(true, -value)
        case "!"           => ExprNum(false, value == 0)
        // Invalid cases
        case "-" if !signed => {
          cc.error(tree, "Unary '-' is not well defined for unsigned values")
          ExprError()
        }
        case op => {
          cc.error(tree, s"Unary '${op}' is not well defined for unsized values")
          ExprError()
        }
      }
      if (num.hasLoc) num else num withLoc tree.loc
    }

    ////////////////////////////////////////////////////////////////////////////
    // Fold unary expressions with a sized operand
    ////////////////////////////////////////////////////////////////////////////

    case ExprUnary(op, expr @ ExprInt(signed, width, value)) => {
      lazy val mask = (BigInt(1) << width) - 1
      val num = op match {
        // Valid cases
        case "+"           => expr
        case "-" if signed => ExprInt(true, width, -value)
        case "~" if signed => ExprInt(true, width, ~value)
        case "~"           => ExprInt(false, width, ~value & mask)
        case "!"           => ExprInt(false, 1, value == 0)
        case "&"           => ExprInt(false, 1, (value & mask) == mask)
        case "~&"          => ExprInt(false, 1, (value & mask) != mask)
        case "|"           => ExprInt(false, 1, (value & mask) != 0)
        case "~|"          => ExprInt(false, 1, (value & mask) == 0)
        case "^"           => ExprInt(false, 1, ((value & mask).bitCount & 1) == 1)
        case "~^"          => ExprInt(false, 1, ((value & mask).bitCount & 1) != 1)
        // Invalid cases
        case "-" if !signed => {
          cc.error(tree, "Unary '-' is not well defined for unsigned values")
          ExprError()
        }
      }
      if (num.hasLoc) num else num withLoc tree.loc
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
    // Fold built-in functions
    ////////////////////////////////////////////////////////////////////////////

    // TODO: generalise handling of these somewhat
    case ExprAtCall("max", args) if args forall { _.isInstanceOf[ExprNum] } => {
      if (args.length >= 2) {
        val (s, v) = (args collect { case ExprNum(signed, value) => (signed, value) }).unzip
        ExprNum(s reduceLeft { _ && _ }, v reduceLeft { _ max _ }) withLoc tree.loc
      } else if (args.length == 1) {
        args.head
      } else {
        cc.error(tree, "Reslt of '@max()' is not well defined")
        ExprError() withLoc tree.loc
      }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Leave rest alone
    ////////////////////////////////////////////////////////////////////////////

    case _ => tree
  }

}
