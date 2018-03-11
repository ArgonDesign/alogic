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
import com.argondesign.alogic.util.FollowedBy

final class ConstantFold(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy {

  private implicit def boolean2BigInt(bool: Boolean) = if (bool) BigInt(1) else BigInt(0)

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
    // Fold expressions with only unsized literals
    ////////////////////////////////////////////////////////////////////////////

    case ExprUnary(op, expr @ ExprNum(signed, value)) => {
      val num = op match {
        // Valid cases
        case "+"           => expr
        case "-" if signed => ExprNum(true, -value)
        case "!"           => ExprNum(false, value == 0)
        // Invalid cases
        case "-" if !signed => ExprError() followedBy {
          cc.error(tree, "Unary operator '-' is not well defined for unsized unsigned values")
        }
        case op => ExprError() followedBy {
          cc.error(tree, s"Unary operator '${op}' is not well defined for unsized values")
        }
      }
      if (num.hasLoc) num else num withLoc tree.loc
    }

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
        case (_, ">", _)       => ExprNum(false, lv > rv)
        case (_, "<", _)       => ExprNum(false, lv < rv)
        case (_, ">=", _)      => ExprNum(false, lv >= rv)
        case (_, "<=", _)      => ExprNum(false, lv <= rv)
        case (_, "==", _)      => ExprNum(false, lv == rv)
        case (_, "!=", _)      => ExprNum(false, lv != rv)
        case (_, "&&", _)      => ExprNum(false, (lv != 0) && (rv != 0))
        case (_, "||", _)      => ExprNum(false, (lv != 0) || (rv != 0))

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

        // Shift
        case (_, op @ ("<<" | ">>" | "<<<" | ">>>"), _) if negr => ExprError() followedBy {
          cc.error(tree, "Negative shift amount")
        }
        case (true, ">>", _) if negl => ExprError() followedBy {
          cc.error(tree, "Logical right shift '>>' is not well defined for negative unsized values")
        }
        case (signed, "<<", _)  => ExprNum(signed, lv << rv.toInt)
        case (signed, ">>", _)  => ExprNum(signed, lv >> rv.toInt)
        case (signed, "<<<", _) => ExprNum(signed, lv << rv.toInt)
        case (signed, ">>>", _) => ExprNum(signed, lv >> rv.toInt)

        // Bitwise
        case (_, op @ ("&" | "^" | "|"), _) if nege => ExprError() followedBy {
          cc.error(tree, s"Bitwise '${op}' operator is not well defined for negative unsized values")
        }
        case (_, "~^", _) => ExprError() followedBy {
          cc.error(tree, "Bitwise '~^' operator is not well defined for unsized values")
        }
        case (true, "&", true) => ExprNum(true, lv & rv)
        case (true, "^", true) => ExprNum(true, lv ^ rv)
        case (true, "|", true) => ExprNum(true, lv | rv)
        case (_, "&", _)       => ExprNum(false, lv & rv)
        case (_, "^", _)       => ExprNum(false, lv ^ rv)
        case (_, "|", _)       => ExprNum(false, lv | rv)

        case _                 => unreachable
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
