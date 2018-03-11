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

import java.util.regex.Pattern

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
          (text, result, msg) <- List(
            // signed positive operand
            ("+2", ExprNum(true, 2), ""),
            ("-2", ExprNum(true, -2), ""),
            ("~2", ExprNum(true, -3), ""),
            ("!2", ExprInt(false, 1, 0), ""),
            ("&2", ExprInt(false, 1, 0), ""),
            ("|2", ExprInt(false, 1, 1), ""),
            ("^2", ExprInt(false, 1, 1), ""),
            // signed negative operand
            ("+(-2)", ExprNum(true, -2), ""),
            ("-(-2)", ExprNum(true, 2), ""),
            ("~(-2)", ExprNum(true, 1), ""),
            ("!(-2)", ExprInt(false, 1, 0), ""),
            ("&(-2)", ExprInt(false, 1, 0), ""),
            ("|(-2)", ExprInt(false, 1, 1), ""),
            ("^(-2)", ExprError(), "Unary '^' is not well defined for unsized negative values"),
            // signed 0 operand
            ("+0", ExprNum(true, 0), ""),
            ("-0", ExprNum(true, 0), ""),
            ("~0", ExprNum(true, -1), ""),
            ("!0", ExprInt(false, 1, 1), ""),
            ("&0", ExprInt(false, 1, 0), ""),
            ("|0", ExprInt(false, 1, 0), ""),
            ("^0", ExprInt(false, 1, 0), ""),
            // signed -1 operand
            ("+(-1)", ExprNum(true, -1), ""),
            ("-(-1)", ExprNum(true, 1), ""),
            ("~(-1)", ExprNum(true, 0), ""),
            ("!(-1)", ExprInt(false, 1, 0), ""),
            ("&(-1)", ExprInt(false, 1, 1), ""),
            ("|(-1)", ExprInt(false, 1, 1), ""),
            ("^(-1)", ExprError(), "Unary '^' is not well defined for unsized negative values"),
            // unsigned non-0 operand
            ("+'d2", ExprNum(false, 2), ""),
            ("-'d2", ExprError(), "Unary '-' is not well defined for unsigned values"),
            ("~'d2", ExprError(), "Unary '~' is not well defined for unsized unsigned values"),
            ("!'d2", ExprInt(false, 1, 0), ""),
            ("&'d2", ExprInt(false, 1, 0), ""),
            ("|'d2", ExprInt(false, 1, 1), ""),
            ("^'d2", ExprInt(false, 1, 1), ""),
            // unsigned 0 operand
            ("+'d0", ExprNum(false, 0), ""),
            ("-'d0", ExprNum(false, 0), ""),
            ("~'d0", ExprError(), "Unary '~' is not well defined for unsized unsigned values"),
            ("!'d0", ExprInt(false, 1, 1), ""),
            ("&'d0", ExprInt(false, 1, 0), ""),
            ("|'d0", ExprInt(false, 1, 0), ""),
            ("^'d0", ExprInt(false, 1, 0), "")
          )
        } {
          val expr = text.trim
          expr in {
            expr.asTree[Expr] rewrite constantFold shouldBe result
            if (msg.isEmpty) {
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](Pattern.quote(msg))
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
            ("-3 >>   2", ExprError(), "'>>' is not well defined for negative unsized values"),
            ("-3 <<<  2", ExprNum(true, -12), ""),
            ("-3 >>>  2", ExprNum(true, -1), ""),
            ("-3 <<  -2", ExprError(), "Negative shift amount"),
            ("-3 >>  -2", ExprError(), "Negative shift amount"), // ***
            ("-3 <<< -2", ExprError(), "Negative shift amount"),
            ("-3 >>> -2", ExprError(), "Negative shift amount"),
            // Bitwise
            (" 3 &   2", ExprNum(true, 2), ""),
            (" 3 ^   2", ExprNum(true, 1), ""),
            (" 3 |   2", ExprNum(true, 3), ""),
            (" 3 &  -2",
             ExprError(),
             "Bitwise '&' operator is not well defined for negative unsized values"),
            (" 3 ^  -2",
             ExprError(),
             "Bitwise '\\^' operator is not well defined for negative unsized values"),
            (" 3 |  -2",
             ExprError(),
             "Bitwise '\\|' operator is not well defined for negative unsized values"),
            ("-3 &   2",
             ExprError(),
             "Bitwise '&' operator is not well defined for negative unsized values"),
            ("-3 ^   2",
             ExprError(),
             "Bitwise '\\^' operator is not well defined for negative unsized values"),
            ("-3 |   2",
             ExprError(),
             "Bitwise '\\|' operator is not well defined for negative unsized values"),
            ("-3 &  -2",
             ExprError(),
             "Bitwise '&' operator is not well defined for negative unsized values"),
            ("-3 ^  -2",
             ExprError(),
             "Bitwise '\\^' operator is not well defined for negative unsized values"),
            ("-3 |  -2",
             ExprError(),
             "Bitwise '\\|' operator is not well defined for negative unsized values"),
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
            ("-3 * 'd2",
             ExprError(),
             "Result of operator '\\*' is unsigned, but value is negative"),
            ("-3 / 'd2", ExprError(), "Result of operator '/' is unsigned, but value is negative"),
            ("-3 % 'd2", ExprError(), "Result of operator '%' is unsigned, but value is negative"),
            ("-3 + 'd2",
             ExprError(),
             "Result of operator '\\+' is unsigned, but value is negative"),
            ("-3 - 'd2", ExprError(), "Result of operator '-' is unsigned, but value is negative"),
            ("-3 + 'd4", ExprNum(false, 1), ""),
            // Shifts
            (" 3 <<  'd2", ExprNum(true, 12), ""),
            (" 3 >>  'd2", ExprNum(true, 0), ""),
            (" 3 <<< 'd2", ExprNum(true, 12), ""),
            (" 3 >>> 'd2", ExprNum(true, 0), ""),
            ("-3 <<  'd2", ExprNum(true, -12), ""),
            ("-3 >>  'd2", ExprError(), "'>>' is not well defined for negative unsized values"),
            ("-3 <<< 'd2", ExprNum(true, -12), ""),
            ("-3 >>> 'd2", ExprNum(true, -1), ""),
            // Bitwise
            (" 3 &  'd2", ExprNum(false, 2), ""),
            (" 3 ^  'd2", ExprNum(false, 1), ""),
            (" 3 |  'd2", ExprNum(false, 3), ""),
            ("-3 &  'd2",
             ExprError(),
             "Bitwise '&' operator is not well defined for negative unsized values"),
            ("-3 ^  'd2",
             ExprError(),
             "Bitwise '\\^' operator is not well defined for negative unsized values"),
            ("-3 |  'd2",
             ExprError(),
             "Bitwise '\\|' operator is not well defined for negative unsized values"),
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
            ("'d3 * -2",
             ExprError(),
             "Result of operator '\\*' is unsigned, but value is negative"),
            ("'d3 / -2", ExprError(), "Result of operator '/' is unsigned, but value is negative"),
            ("'d3 % -2", ExprNum(false, 1), ""),
            ("'d3 + -2", ExprNum(false, 1), ""),
            ("'d3 - -2", ExprNum(false, 5), ""),
            ("'d3 + -4",
             ExprError(),
             "Result of operator '\\+' is unsigned, but value is negative"),
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
            ("'d3 |  2", ExprNum(false, 3), ""),
            ("'d3 &  -2",
             ExprError(),
             "Bitwise '&' operator is not well defined for negative unsized values"),
            ("'d3 ^  -2",
             ExprError(),
             "Bitwise '\\^' operator is not well defined for negative unsized values"),
            ("'d3 |  -2",
             ExprError(),
             "Bitwise '\\|' operator is not well defined for negative unsized values"),
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

    "fold shifts with an unsized left hand side and a sized right hand side" - {
      for {
        (expr, result, msg) <- List(
          //////////////////////////////////////////////
          // signed signed
          //////////////////////////////////////////////
          (" 3 <<   8'sd2", ExprNum(true, 12), ""),
          (" 3 >>   8'sd2", ExprNum(true, 0), ""),
          (" 3 <<<  8'sd2", ExprNum(true, 12), ""),
          (" 3 >>>  8'sd2", ExprNum(true, 0), ""),
          (" 3 <<  -8'sd2", ExprError(), "Negative shift amount"),
          (" 3 >>  -8'sd2", ExprError(), "Negative shift amount"),
          (" 3 <<< -8'sd2", ExprError(), "Negative shift amount"),
          (" 3 >>> -8'sd2", ExprError(), "Negative shift amount"),
          ("-3 <<   8'sd2", ExprNum(true, -12), ""),
          ("-3 >>   8'sd2", ExprError(), "'>>' is not well defined for negative unsized values"),
          ("-3 <<<  8'sd2", ExprNum(true, -12), ""),
          ("-3 >>>  8'sd2", ExprNum(true, -1), ""),
          ("-3 <<  -8'sd2", ExprError(), "Negative shift amount"),
          ("-3 >>  -8'sd2", ExprError(), "Negative shift amount"), // ***
          ("-3 <<< -8'sd2", ExprError(), "Negative shift amount"),
          ("-3 >>> -8'sd2", ExprError(), "Negative shift amount"),
          //////////////////////////////////////////////
          // signed unsigned
          //////////////////////////////////////////////
          (" 3 <<  8'd2", ExprNum(true, 12), ""),
          (" 3 >>  8'd2", ExprNum(true, 0), ""),
          (" 3 <<< 8'd2", ExprNum(true, 12), ""),
          (" 3 >>> 8'd2", ExprNum(true, 0), ""),
          ("-3 <<  8'd2", ExprNum(true, -12), ""),
          ("-3 >>  8'd2", ExprError(), "'>>' is not well defined for negative unsized values"),
          ("-3 <<< 8'd2", ExprNum(true, -12), ""),
          ("-3 >>> 8'd2", ExprNum(true, -1), ""),
          //////////////////////////////////////////////
          // unsigned signed
          //////////////////////////////////////////////
          ("'d3 <<  8'sd2", ExprNum(false, 12), ""),
          ("'d3 >>  8'sd2", ExprNum(false, 0), ""),
          ("'d3 <<< 8'sd2", ExprNum(false, 12), ""),
          ("'d3 >>> 8'sd2", ExprNum(false, 0), ""),
          ("'d3 <<  -8'sd2", ExprError(), "Negative shift amount"),
          ("'d3 >>  -8'sd2", ExprError(), "Negative shift amount"),
          ("'d3 <<< -8'sd2", ExprError(), "Negative shift amount"),
          ("'d3 >>> -8'sd2", ExprError(), "Negative shift amount"),
          //////////////////////////////////////////////
          // unsigned unsigned
          //////////////////////////////////////////////
          ("'d3 <<  8'd2", ExprNum(false, 12), ""),
          ("'d3 >>  8'd2", ExprNum(false, 0), ""),
          ("'d3 <<< 8'd2", ExprNum(false, 12), ""),
          ("'d3 >>> 8'd2", ExprNum(false, 0), "")
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

    "fold unary operators with a sized operand" - {
      for {
        (text, result, msg) <- List(
          // signed positive operand
          ("+8'sd2", ExprInt(true, 8, 2), ""),
          ("-8'sd2", ExprInt(true, 8, -2), ""),
          ("~8'sd2", ExprInt(true, 8, -3), ""),
          ("!8'sd2", ExprInt(false, 1, 0), ""),
          ("&8'sd2", ExprInt(false, 1, 0), ""),
          ("|8'sd2", ExprInt(false, 1, 1), ""),
          ("^8'sd2", ExprInt(false, 1, 1), ""),
          // signed negative operand
          ("+(-8'sd2)", ExprInt(true, 8, -2), ""),
          ("-(-8'sd2)", ExprInt(true, 8, 2), ""),
          ("~(-8'sd2)", ExprInt(true, 8, 1), ""),
          ("!(-8'sd2)", ExprInt(false, 1, 0), ""),
          ("&(-8'sd2)", ExprInt(false, 1, 0), ""),
          ("|(-8'sd2)", ExprInt(false, 1, 1), ""),
          ("^(-8'sd2)", ExprInt(false, 1, 1), ""),
          // signed 0 operand
          ("+8'sd0", ExprInt(true, 8, 0), ""),
          ("-8'sd0", ExprInt(true, 8, 0), ""),
          ("~8'sd0", ExprInt(true, 8, -1), ""),
          ("!8'sd0", ExprInt(false, 1, 1), ""),
          ("&8'sd0", ExprInt(false, 1, 0), ""),
          ("|8'sd0", ExprInt(false, 1, 0), ""),
          ("^8'sd0", ExprInt(false, 1, 0), ""),
          // signed -1 operand
          ("+(-8'sd1)", ExprInt(true, 8, -1), ""),
          ("-(-8'sd1)", ExprInt(true, 8, 1), ""),
          ("~(-8'sd1)", ExprInt(true, 8, 0), ""),
          ("!(-8'sd1)", ExprInt(false, 1, 0), ""),
          ("&(-8'sd1)", ExprInt(false, 1, 1), ""),
          ("|(-8'sd1)", ExprInt(false, 1, 1), ""),
          ("^(-8'sd1)", ExprInt(false, 1, 0), ""),
          // unsigned non-0 operand
          ("+8'd2", ExprInt(false, 8, 2), ""),
          ("-8'd2", ExprError(), "Unary '-' is not well defined for unsigned values"),
          ("~8'd2", ExprInt(false, 8, 253), ""),
          ("!8'd2", ExprInt(false, 1, 0), ""),
          ("&8'd2", ExprInt(false, 1, 0), ""),
          ("|8'd2", ExprInt(false, 1, 1), ""),
          ("^8'd2", ExprInt(false, 1, 1), ""),
          // unsigned 0 operand
          ("+8'd0", ExprInt(false, 8, 0), ""),
          ("-8'd0", ExprInt(false, 8, 0), ""),
          ("~8'd0", ExprInt(false, 8, 255), ""),
          ("!8'd0", ExprInt(false, 1, 1), ""),
          ("&8'd0", ExprInt(false, 1, 0), ""),
          ("|8'd0", ExprInt(false, 1, 0), ""),
          ("^8'd0", ExprInt(false, 1, 0), ""),
          // reductions ff
          ("&8'hff", ExprInt(false, 1, 1), ""),
          ("|8'hff", ExprInt(false, 1, 1), ""),
          ("^8'hff", ExprInt(false, 1, 0), ""),
          // reductions 0
          ("&8'h0", ExprInt(false, 1, 0), ""),
          ("|8'h0", ExprInt(false, 1, 0), ""),
          ("^8'h0", ExprInt(false, 1, 0), "")
        )
      } {
        val expr = text.trim
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
  }
}
