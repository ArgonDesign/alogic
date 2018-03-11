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
// Namer tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.transform

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error

import org.scalatest.FreeSpec

final class ConstantFoldSpec extends FreeSpec with AlogicTest {

  implicit val cc = new CompilerContext
  val constantFold = new ConstantFold

  "ConstantFold should" - {
    "fold expressions containing only unsized literals" - {
      "unary" - {
        for {
          (expr, result, msg) <- List(
            // signed operand
            ("+2", ExprNum(true, 2), ""),
            ("-2", ExprNum(true, -2), ""),
            ("+(+2)", ExprNum(true, 2), ""),
            ("-(-2)", ExprNum(true, 2), ""),
            ("!2", ExprNum(false, 0), ""),
            ("!0", ExprNum(false, 1), ""),
            ("!!2", ExprNum(false, 1), ""),
            ("!!0", ExprNum(false, 0), ""),
            ("~2", ExprError(), "Unary operator '~' is not well defined for unsized values"),
            ("&2", ExprError(), "Unary operator '&' is not well defined for unsized values"),
            ("~&2", ExprError(), "Unary operator '~&' is not well defined for unsized values"),
            ("|2", ExprError(), "Unary operator '\\|' is not well defined for unsized values"),
            ("~|2", ExprError(), "Unary operator '~\\|' is not well defined for unsized values"),
            ("^2", ExprError(), "Unary operator '\\^' is not well defined for unsized values"),
            ("~^2", ExprError(), "Unary operator '~\\^' is not well defined for unsized values"),
            // unsigned operand
            ("+'d2", ExprNum(false, 2), ""),
            ("-'d2", ExprError(), "Unary operator '-' is not well defined for unsized unsigned values"),
            ("+(+'d2)", ExprNum(false, 2), ""),
            ("-(-'d2)", ExprError(), "Unary operator '-' is not well defined for unsized unsigned values"),
            ("!'d2", ExprNum(false, 0), ""),
            ("!'d0", ExprNum(false, 1), ""),
            ("!!'d2", ExprNum(false, 1), ""),
            ("!!'d0", ExprNum(false, 0), ""),
            ("~'d2", ExprError(), "Unary operator '~' is not well defined for unsized values"),
            ("&'d2", ExprError(), "Unary operator '&' is not well defined for unsized values"),
            ("~&'d2", ExprError(), "Unary operator '~&' is not well defined for unsized values"),
            ("|'d2", ExprError(), "Unary operator '\\|' is not well defined for unsized values"),
            ("~|'d2", ExprError(), "Unary operator '~\\|' is not well defined for unsized values"),
            ("^'d2", ExprError(), "Unary operator '\\^' is not well defined for unsized values"),
            ("~^'d2", ExprError(), "Unary operator '~\\^' is not well defined for unsized values")
          )
        } {
          expr in {
            expr.asTree[Expr] rewrite constantFold shouldBe result
            if (msg.isEmpty) {
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }
        }
      }

      "binary" - {
        for {
          (expr, result, msg) <- List(
            //////////////////////////////////////////////
            // signed signed
            //////////////////////////////////////////////
            // Always valid
            ("3 >  2", ExprNum(false, 1), ""),
            ("3 >= 2", ExprNum(false, 1), ""),
            ("3 <  2", ExprNum(false, 0), ""),
            ("3 <= 2", ExprNum(false, 0), ""),
            ("3 == 2", ExprNum(false, 0), ""),
            ("3 != 2", ExprNum(false, 1), ""),
            ("3 && 2", ExprNum(false, 1), ""),
            ("3 || 2", ExprNum(false, 1), ""),
            // Arith
            ("3 * 2", ExprNum(true, 6), ""),
            ("3 / 2", ExprNum(true, 1), ""),
            ("3 % 2", ExprNum(true, 1), ""),
            ("3 + 2", ExprNum(true, 5), ""),
            ("3 - 2", ExprNum(true, 1), ""),
            // Shifts
            (" 3 <<   2", ExprNum(true, 12), ""),
            (" 3 >>   2", ExprNum(true, 0), ""),
            (" 3 <<<  2", ExprNum(true, 12), ""),
            (" 3 >>>  2", ExprNum(true, 0), ""),
            (" 3 <<  -2", ExprError(), "Negative shift amount"),
            (" 3 >>  -2", ExprError(), "Negative shift amount"),
            (" 3 <<< -2", ExprError(), "Negative shift amount"),
            (" 3 >>> -2", ExprError(), "Negative shift amount"),
            ("-3 <<   2", ExprNum(true, -12), ""),
            ("-3 >>   2", ExprError(), "Logical right shift '>>' is not well defined for negative unsized values"),
            ("-3 <<<  2", ExprNum(true, -12), ""),
            ("-3 >>>  2", ExprNum(true, -1), ""),
            ("-3 <<  -2", ExprError(), "Negative shift amount"),
            ("-3 >>  -2", ExprError(), "Negative shift amount"), // ***
            ("-3 <<< -2", ExprError(), "Negative shift amount"),
            ("-3 >>> -2", ExprError(), "Negative shift amount"),
            // Bitwise
            (" 3 &   2", ExprNum(true, 2), ""),
            (" 3 ^   2", ExprNum(true, 1), ""),
            (" 3 ~^  2", ExprError(), "Bitwise '~\\^' operator is not well defined for unsized values"),
            (" 3 |   2", ExprNum(true, 3), ""),
            (" 3 &  -2", ExprError(), "Bitwise '&' operator is not well defined for negative unsized values"),
            (" 3 ^  -2", ExprError(), "Bitwise '\\^' operator is not well defined for negative unsized values"),
            (" 3 ~^ -2", ExprError(), "Bitwise '~\\^' operator is not well defined for unsized values"),
            (" 3 |  -2", ExprError(), "Bitwise '\\|' operator is not well defined for negative unsized values"),
            ("-3 &   2", ExprError(), "Bitwise '&' operator is not well defined for negative unsized values"),
            ("-3 ^   2", ExprError(), "Bitwise '\\^' operator is not well defined for negative unsized values"),
            ("-3 ~^  2", ExprError(), "Bitwise '~\\^' operator is not well defined for unsized values"),
            ("-3 |   2", ExprError(), "Bitwise '\\|' operator is not well defined for negative unsized values"),
            ("-3 &  -2", ExprError(), "Bitwise '&' operator is not well defined for negative unsized values"),
            ("-3 ^  -2", ExprError(), "Bitwise '\\^' operator is not well defined for negative unsized values"),
            ("-3 ~^ -2", ExprError(), "Bitwise '~\\^' operator is not well defined for unsized values"),
            ("-3 |  -2", ExprError(), "Bitwise '\\|' operator is not well defined for negative unsized values"),

            //////////////////////////////////////////////
            // signed unsigned
            //////////////////////////////////////////////
            // Always valid
            ("3 >  'd2", ExprNum(false, 1), ""),
            ("3 >= 'd2", ExprNum(false, 1), ""),
            ("3 <  'd2", ExprNum(false, 0), ""),
            ("3 <= 'd2", ExprNum(false, 0), ""),
            ("3 == 'd2", ExprNum(false, 0), ""),
            ("3 != 'd2", ExprNum(false, 1), ""),
            ("3 && 'd2", ExprNum(false, 1), ""),
            ("3 || 'd2", ExprNum(false, 1), ""),
            // Arith
            ("3 * 'd2", ExprNum(false, 6), ""),
            ("3 / 'd2", ExprNum(false, 1), ""),
            ("3 % 'd2", ExprNum(false, 1), ""),
            ("3 + 'd2", ExprNum(false, 5), ""),
            ("3 - 'd2", ExprNum(false, 1), ""),
            ("3 - 'd4", ExprError(), "Result of operator '-' is unsigned, but value is negative"),
            ("-3 * 'd2", ExprError(), "Result of operator '\\*' is unsigned, but value is negative"),
            ("-3 / 'd2", ExprError(), "Result of operator '/' is unsigned, but value is negative"),
            ("-3 % 'd2", ExprError(), "Result of operator '%' is unsigned, but value is negative"),
            ("-3 + 'd2", ExprError(), "Result of operator '\\+' is unsigned, but value is negative"),
            ("-3 - 'd2", ExprError(), "Result of operator '-' is unsigned, but value is negative"),
            ("-3 + 'd4", ExprNum(false, 1), ""),
            // Shifts
            (" 3 <<  'd2", ExprNum(true, 12), ""),
            (" 3 >>  'd2", ExprNum(true, 0), ""),
            (" 3 <<< 'd2", ExprNum(true, 12), ""),
            (" 3 >>> 'd2", ExprNum(true, 0), ""),
            ("-3 <<  'd2", ExprNum(true, -12), ""),
            ("-3 >>  'd2", ExprError(), "Logical right shift '>>' is not well defined for negative unsized values"),
            ("-3 <<< 'd2", ExprNum(true, -12), ""),
            ("-3 >>> 'd2", ExprNum(true, -1), ""),
            // Bitwise
            (" 3 &  'd2", ExprNum(false, 2), ""),
            (" 3 ^  'd2", ExprNum(false, 1), ""),
            (" 3 ~^ 'd2", ExprError(), "Bitwise '~\\^' operator is not well defined for unsized values"),
            (" 3 |  'd2", ExprNum(false, 3), ""),
            ("-3 &  'd2", ExprError(), "Bitwise '&' operator is not well defined for negative unsized values"),
            ("-3 ^  'd2", ExprError(), "Bitwise '\\^' operator is not well defined for negative unsized values"),
            ("-3 ~^ 'd2", ExprError(), "Bitwise '~\\^' operator is not well defined for unsized values"),
            ("-3 |  'd2", ExprError(), "Bitwise '\\|' operator is not well defined for negative unsized values"),

            //////////////////////////////////////////////
            // unsigned signed
            //////////////////////////////////////////////
            // Always valid
            ("'d3 >  2", ExprNum(false, 1), ""),
            ("'d3 >= 2", ExprNum(false, 1), ""),
            ("'d3 <  2", ExprNum(false, 0), ""),
            ("'d3 <= 2", ExprNum(false, 0), ""),
            ("'d3 == 2", ExprNum(false, 0), ""),
            ("'d3 != 2", ExprNum(false, 1), ""),
            ("'d3 && 2", ExprNum(false, 1), ""),
            ("'d3 || 2", ExprNum(false, 1), ""),
            // Arith
            ("'d3 * 2", ExprNum(false, 6), ""),
            ("'d3 / 2", ExprNum(false, 1), ""),
            ("'d3 % 2", ExprNum(false, 1), ""),
            ("'d3 + 2", ExprNum(false, 5), ""),
            ("'d3 - 2", ExprNum(false, 1), ""),
            ("'d3 - 4", ExprError(), "Result of operator '-' is unsigned, but value is negative"),
            ("'d3 * -2", ExprError(), "Result of operator '\\*' is unsigned, but value is negative"),
            ("'d3 / -2", ExprError(), "Result of operator '/' is unsigned, but value is negative"),
            ("'d3 % -2", ExprNum(false, 1), ""),
            ("'d3 + -2", ExprNum(false, 1), ""),
            ("'d3 - -2", ExprNum(false, 5), ""),
            ("'d3 + -4", ExprError(), "Result of operator '\\+' is unsigned, but value is negative"),
            // Shifts
            ("'d3 <<  2", ExprNum(false, 12), ""),
            ("'d3 >>  2", ExprNum(false, 0), ""),
            ("'d3 <<< 2", ExprNum(false, 12), ""),
            ("'d3 >>> 2", ExprNum(false, 0), ""),
            ("'d3 <<  -2", ExprError(), "Negative shift amount"),
            ("'d3 >>  -2", ExprError(), "Negative shift amount"),
            ("'d3 <<< -2", ExprError(), "Negative shift amount"),
            ("'d3 >>> -2", ExprError(), "Negative shift amount"),
            // Bitwise
            ("'d3 &  2", ExprNum(false, 2), ""),
            ("'d3 ^  2", ExprNum(false, 1), ""),
            ("'d3 ~^ 2", ExprError(), "Bitwise '~\\^' operator is not well defined for unsized values"),
            ("'d3 |  2", ExprNum(false, 3), ""),
            ("'d3 &  -2", ExprError(), "Bitwise '&' operator is not well defined for negative unsized values"),
            ("'d3 ^  -2", ExprError(), "Bitwise '\\^' operator is not well defined for negative unsized values"),
            ("'d3 ~^ -2", ExprError(), "Bitwise '~\\^' operator is not well defined for unsized values"),
            ("'d3 |  -2", ExprError(), "Bitwise '\\|' operator is not well defined for negative unsized values"),

            //////////////////////////////////////////////
            // unsigned unsigned
            //////////////////////////////////////////////
            // Always valid
            ("'d3 >  'd2", ExprNum(false, 1), ""),
            ("'d3 >= 'd2", ExprNum(false, 1), ""),
            ("'d3 <  'd2", ExprNum(false, 0), ""),
            ("'d3 <= 'd2", ExprNum(false, 0), ""),
            ("'d3 == 'd2", ExprNum(false, 0), ""),
            ("'d3 != 'd2", ExprNum(false, 1), ""),
            ("'d3 && 'd2", ExprNum(false, 1), ""),
            ("'d3 || 'd2", ExprNum(false, 1), ""),
            // Arith
            ("'d3 * 'd2", ExprNum(false, 6), ""),
            ("'d3 / 'd2", ExprNum(false, 1), ""),
            ("'d3 % 'd2", ExprNum(false, 1), ""),
            ("'d3 + 'd2", ExprNum(false, 5), ""),
            ("'d3 - 'd2", ExprNum(false, 1), ""),
            ("'d3 - 'd4", ExprError(), "Result of operator '-' is unsigned, but value is negative"),
            // Shifts
            ("'d3 <<  'd2", ExprNum(false, 12), ""),
            ("'d3 >>  'd2", ExprNum(false, 0), ""),
            ("'d3 <<< 'd2", ExprNum(false, 12), ""),
            ("'d3 >>> 'd2", ExprNum(false, 0), ""),
            // Bitwise
            ("'d3 &  'd2", ExprNum(false, 2), ""),
            ("'d3 ^  'd2", ExprNum(false, 1), ""),
            ("'d3 ~^ 'd2", ExprError(), "Bitwise '~\\^' operator is not well defined for unsized values"),
            ("'d3 |  'd2", ExprNum(false, 3), "")
          )
        } {
          val e = expr.trim.replaceAll(" +", " ")
          e in {
            e.asTree[Expr] rewrite constantFold shouldBe result
            if (msg.isEmpty) {
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }
        }
      }

      //      "ternary" - {
      //        for {
      //          (expr, result) <- List(
      //            ("'d3 |  'd2", ExprNum(false, 3))
      //          )
      //        } {
      //          val e = expr.trim.replaceAll(" +", " ")
      //          e in {
      //            e.asTree[Expr] rewrite constantFold shouldBe result
      //            cc.messages.loneElement should beThe[Warning]("Condition of )
      //          }
      //        }
      //      }

      "@max" - {
        for {
          (expr, result, msg) <- List(
            ("@max()", ExprError(), "Reslt of '@max\\(\\)' is not well defined"),
            ("@max(1)", ExprNum(true, 1), ""),
            ("@max('d1)", ExprNum(false, 1), ""),
            ("@max(1, 2)", ExprNum(true, 2), ""),
            ("@max(1, 'd2)", ExprNum(false, 2), ""),
            ("@max('d1, 2)", ExprNum(false, 2), ""),
            ("@max('d1, 'd2)", ExprNum(false, 2), ""),
            ("@max(0, 1)", ExprNum(true, 1), ""),
            ("@max(-2, -1)", ExprNum(true, -1), ""),
            ("@max(-2, 'd1)", ExprNum(false, 1), ""),
            ("@max(0, 1, 2, 3, 4, 5)", ExprNum(true, 5), "")
          )
        } {
          val e = expr.trim.replaceAll(" +", " ")
          e in {
            e.asTree[Expr] rewrite constantFold shouldBe result
            if (msg.isEmpty) {
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }
        }

      }
    }
  }
}
