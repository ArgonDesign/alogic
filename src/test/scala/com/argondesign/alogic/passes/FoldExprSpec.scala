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
import com.argondesign.alogic.passes.Namer
import org.scalatest.FreeSpec

final class ConstantFoldSpec extends FreeSpec with AlogicTest {

  implicit val cc = new CompilerContext
  val namer = new Namer
  val constantFold = new ConstantFold

  "ConstantFold should" - {
    "fold expressions containing only unsized literals" - {
      "unary" - {
        for {
          (text, result, msg) <- List(
            // signed positive operand
            ("+('sd2)", ExprNum(true, 2), ""),
            ("-('sd2)", ExprNum(true, -2), ""),
            ("~('sd2)", ExprNum(true, -3), ""),
            ("!('sd2)", ExprInt(false, 1, 0), ""),
            ("&('sd2)", ExprInt(false, 1, 0), ""),
            ("|('sd2)", ExprInt(false, 1, 1), ""),
            ("^('sd2)", ExprInt(false, 1, 1), ""),
            // signed negative operand
            ("+(-'sd2)", ExprNum(true, -2), ""),
            ("-(-'sd2)", ExprNum(true, 2), ""),
            ("~(-'sd2)", ExprNum(true, 1), ""),
            ("!(-'sd2)", ExprInt(false, 1, 0), ""),
            ("&(-'sd2)", ExprInt(false, 1, 0), ""),
            ("|(-'sd2)", ExprInt(false, 1, 1), ""),
            ("^(-'sd2)", ExprError(), "Unary '^' is not well defined for unsized negative values"),
            // signed 0 operand
            ("+('sd0)", ExprNum(true, 0), ""),
            ("-('sd0)", ExprNum(true, 0), ""),
            ("~('sd0)", ExprNum(true, -1), ""),
            ("!('sd0)", ExprInt(false, 1, 1), ""),
            ("&('sd0)", ExprInt(false, 1, 0), ""),
            ("|('sd0)", ExprInt(false, 1, 0), ""),
            ("^('sd0)", ExprInt(false, 1, 0), ""),
            // signed -1 operand
            ("+(-'sd1)", ExprNum(true, -1), ""),
            ("-(-'sd1)", ExprNum(true, 1), ""),
            ("~(-'sd1)", ExprNum(true, 0), ""),
            ("!(-'sd1)", ExprInt(false, 1, 0), ""),
            ("&(-'sd1)", ExprInt(false, 1, 1), ""),
            ("|(-'sd1)", ExprInt(false, 1, 1), ""),
            ("^(-'sd1)", ExprError(), "Unary '^' is not well defined for unsized negative values"),
            // unsigned non-0 operand
            ("+(2)", ExprNum(false, 2), ""),
            ("-(2)", ExprError(), "Unary '-' is not well defined for unsigned values"),
            ("~(2)", ExprError(), "Unary '~' is not well defined for unsized unsigned values"),
            ("!(2)", ExprInt(false, 1, 0), ""),
            ("&(2)", ExprInt(false, 1, 0), ""),
            ("|(2)", ExprInt(false, 1, 1), ""),
            ("^(2)", ExprInt(false, 1, 1), ""),
            // unsigned 0 operand
            ("+(0)", ExprNum(false, 0), ""),
            ("-(0)", ExprNum(false, 0), ""),
            ("~(0)", ExprError(), "Unary '~' is not well defined for unsized unsigned values"),
            ("!(0)", ExprInt(false, 1, 1), ""),
            ("&(0)", ExprInt(false, 1, 0), ""),
            ("|(0)", ExprInt(false, 1, 0), ""),
            ("^(0)", ExprInt(false, 1, 0), "")
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
            ("'sd3 >  'sd2", ExprNum(false, 1), ""),
            ("'sd3 >= 'sd2", ExprNum(false, 1), ""),
            ("'sd3 <  'sd2", ExprNum(false, 0), ""),
            ("'sd3 <= 'sd2", ExprNum(false, 0), ""),
            ("'sd3 == 'sd2", ExprNum(false, 0), ""),
            ("'sd3 != 'sd2", ExprNum(false, 1), ""),
            ("'sd3 && 'sd2", ExprNum(false, 1), ""),
            ("'sd3 || 'sd2", ExprNum(false, 1), ""),
            // Arith
            ("'sd3 * 'sd2", ExprNum(true, 6), ""),
            ("'sd3 / 'sd2", ExprNum(true, 1), ""),
            ("'sd3 % 'sd2", ExprNum(true, 1), ""),
            ("'sd3 + 'sd2", ExprNum(true, 5), ""),
            ("'sd3 - 'sd2", ExprNum(true, 1), ""),
            // Shifts
            (" 'sd3 <<   'sd2", ExprNum(true, 12), ""),
            (" 'sd3 >>   'sd2", ExprNum(true, 0), ""),
            (" 'sd3 <<<  'sd2", ExprNum(true, 12), ""),
            (" 'sd3 >>>  'sd2", ExprNum(true, 0), ""),
            (" 'sd3 <<  -'sd2", ExprError(), "Negative shift amount"),
            (" 'sd3 >>  -'sd2", ExprError(), "Negative shift amount"),
            (" 'sd3 <<< -'sd2", ExprError(), "Negative shift amount"),
            (" 'sd3 >>> -'sd2", ExprError(), "Negative shift amount"),
            ("-'sd3 <<   'sd2", ExprNum(true, -12), ""),
            ("-'sd3 >>   'sd2",
             ExprError(),
             "'>>' is not well defined for negative unsized values"),
            ("-'sd3 <<<  'sd2", ExprNum(true, -12), ""),
            ("-'sd3 >>>  'sd2", ExprNum(true, -1), ""),
            ("-'sd3 <<  -'sd2", ExprError(), "Negative shift amount"),
            ("-'sd3 >>  -'sd2", ExprError(), "Negative shift amount"), // ***
            ("-'sd3 <<< -'sd2", ExprError(), "Negative shift amount"),
            ("-'sd3 >>> -'sd2", ExprError(), "Negative shift amount"),
            // Bitwise
            (" 'sd3 &   'sd2", ExprNum(true, 2), ""),
            (" 'sd3 ^   'sd2", ExprNum(true, 1), ""),
            (" 'sd3 |   'sd2", ExprNum(true, 3), ""),
            (" 'sd3 &  -'sd2",
             ExprError(),
             "Bitwise '&' operator is not well defined for negative unsized values"),
            (" 'sd3 ^  -'sd2",
             ExprError(),
             "Bitwise '\\^' operator is not well defined for negative unsized values"),
            (" 'sd3 |  -'sd2",
             ExprError(),
             "Bitwise '\\|' operator is not well defined for negative unsized values"),
            ("-'sd3 &   'sd2",
             ExprError(),
             "Bitwise '&' operator is not well defined for negative unsized values"),
            ("-'sd3 ^   'sd2",
             ExprError(),
             "Bitwise '\\^' operator is not well defined for negative unsized values"),
            ("-'sd3 |   'sd2",
             ExprError(),
             "Bitwise '\\|' operator is not well defined for negative unsized values"),
            ("-'sd3 &  -'sd2",
             ExprError(),
             "Bitwise '&' operator is not well defined for negative unsized values"),
            ("-'sd3 ^  -'sd2",
             ExprError(),
             "Bitwise '\\^' operator is not well defined for negative unsized values"),
            ("-'sd3 |  -'sd2",
             ExprError(),
             "Bitwise '\\|' operator is not well defined for negative unsized values"),
            //////////////////////////////////////////////
            // signed unsigned
            //////////////////////////////////////////////
            // Always valid
            ("'sd3 >  2", ExprNum(false, 1), ""),
            ("'sd3 >= 2", ExprNum(false, 1), ""),
            ("'sd3 <  2", ExprNum(false, 0), ""),
            ("'sd3 <= 2", ExprNum(false, 0), ""),
            ("'sd3 == 2", ExprNum(false, 0), ""),
            ("'sd3 != 2", ExprNum(false, 1), ""),
            ("'sd3 && 2", ExprNum(false, 1), ""),
            ("'sd3 || 2", ExprNum(false, 1), ""),
            // Arith
            ("'sd3 * 2", ExprNum(false, 6), ""),
            ("'sd3 / 2", ExprNum(false, 1), ""),
            ("'sd3 % 2", ExprNum(false, 1), ""),
            ("'sd3 + 2", ExprNum(false, 5), ""),
            ("'sd3 - 2", ExprNum(false, 1), ""),
            ("'sd3 - 4", ExprError(), "Result of operator '-' is unsigned, but value is negative"),
            ("-'sd3 * 2",
             ExprError(),
             "Result of operator '\\*' is unsigned, but value is negative"),
            ("-'sd3 / 2", ExprError(), "Result of operator '/' is unsigned, but value is negative"),
            ("-'sd3 % 2", ExprError(), "Result of operator '%' is unsigned, but value is negative"),
            ("-'sd3 + 2",
             ExprError(),
             "Result of operator '\\+' is unsigned, but value is negative"),
            ("-'sd3 - 2", ExprError(), "Result of operator '-' is unsigned, but value is negative"),
            ("-'sd3 + 4", ExprNum(false, 1), ""),
            // Shifts
            (" 'sd3 <<  2", ExprNum(true, 12), ""),
            (" 'sd3 >>  2", ExprNum(true, 0), ""),
            (" 'sd3 <<< 2", ExprNum(true, 12), ""),
            (" 'sd3 >>> 2", ExprNum(true, 0), ""),
            ("-'sd3 <<  2", ExprNum(true, -12), ""),
            ("-'sd3 >>  2", ExprError(), "'>>' is not well defined for negative unsized values"),
            ("-'sd3 <<< 2", ExprNum(true, -12), ""),
            ("-'sd3 >>> 2", ExprNum(true, -1), ""),
            // Bitwise
            (" 'sd3 &  2", ExprNum(false, 2), ""),
            (" 'sd3 ^  2", ExprNum(false, 1), ""),
            (" 'sd3 |  2", ExprNum(false, 3), ""),
            ("-'sd3 &  2",
             ExprError(),
             "Bitwise '&' operator is not well defined for negative unsized values"),
            ("-'sd3 ^  2",
             ExprError(),
             "Bitwise '\\^' operator is not well defined for negative unsized values"),
            ("-'sd3 |  2",
             ExprError(),
             "Bitwise '\\|' operator is not well defined for negative unsized values"),
            //////////////////////////////////////////////
            // unsigned signed
            //////////////////////////////////////////////
            // Always valid
            ("3 >  'sd2", ExprNum(false, 1), ""),
            ("3 >= 'sd2", ExprNum(false, 1), ""),
            ("3 <  'sd2", ExprNum(false, 0), ""),
            ("3 <= 'sd2", ExprNum(false, 0), ""),
            ("3 == 'sd2", ExprNum(false, 0), ""),
            ("3 != 'sd2", ExprNum(false, 1), ""),
            ("3 && 'sd2", ExprNum(false, 1), ""),
            ("3 || 'sd2", ExprNum(false, 1), ""),
            // Arith
            ("3 * 'sd2", ExprNum(false, 6), ""),
            ("3 / 'sd2", ExprNum(false, 1), ""),
            ("3 % 'sd2", ExprNum(false, 1), ""),
            ("3 + 'sd2", ExprNum(false, 5), ""),
            ("3 - 'sd2", ExprNum(false, 1), ""),
            ("3 - 'sd4", ExprError(), "Result of operator '-' is unsigned, but value is negative"),
            ("3 * -'sd2",
             ExprError(),
             "Result of operator '\\*' is unsigned, but value is negative"),
            ("3 / -'sd2", ExprError(), "Result of operator '/' is unsigned, but value is negative"),
            ("3 % -'sd2", ExprNum(false, 1), ""),
            ("3 + -'sd2", ExprNum(false, 1), ""),
            ("3 - -'sd2", ExprNum(false, 5), ""),
            ("3 + -'sd4",
             ExprError(),
             "Result of operator '\\+' is unsigned, but value is negative"),
            // Shifts
            ("3 <<  'sd2", ExprNum(false, 12), ""),
            ("3 >>  'sd2", ExprNum(false, 0), ""),
            ("3 <<< 'sd2", ExprNum(false, 12), ""),
            ("3 >>> 'sd2", ExprNum(false, 0), ""),
            ("3 <<  -'sd2", ExprError(), "Negative shift amount"),
            ("3 >>  -'sd2", ExprError(), "Negative shift amount"),
            ("3 <<< -'sd2", ExprError(), "Negative shift amount"),
            ("3 >>> -'sd2", ExprError(), "Negative shift amount"),
            // Bitwise
            ("3 &  'sd2", ExprNum(false, 2), ""),
            ("3 ^  'sd2", ExprNum(false, 1), ""),
            ("3 |  'sd2", ExprNum(false, 3), ""),
            ("3 &  -'sd2",
             ExprError(),
             "Bitwise '&' operator is not well defined for negative unsized values"),
            ("3 ^  -'sd2",
             ExprError(),
             "Bitwise '\\^' operator is not well defined for negative unsized values"),
            ("3 |  -'sd2",
             ExprError(),
             "Bitwise '\\|' operator is not well defined for negative unsized values"),
            //////////////////////////////////////////////
            // unsigned unsigned
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
            ("3 * 2", ExprNum(false, 6), ""),
            ("3 / 2", ExprNum(false, 1), ""),
            ("3 % 2", ExprNum(false, 1), ""),
            ("3 + 2", ExprNum(false, 5), ""),
            ("3 - 2", ExprNum(false, 1), ""),
            ("3 - 4", ExprError(), "Result of operator '-' is unsigned, but value is negative"),
            // Shifts
            ("3 <<  2", ExprNum(false, 12), ""),
            ("3 >>  2", ExprNum(false, 0), ""),
            ("3 <<< 2", ExprNum(false, 12), ""),
            ("3 >>> 2", ExprNum(false, 0), ""),
            // Bitwise
            ("3 &  2", ExprNum(false, 2), ""),
            ("3 ^  2", ExprNum(false, 1), ""),
            ("3 |  2", ExprNum(false, 3), "")
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
            ("@max('sd1)", ExprNum(true, 1), ""),
            ("@max(1)", ExprNum(false, 1), ""),
            ("@max('sd1, 'sd2)", ExprNum(true, 2), ""),
            ("@max('sd1, 2)", ExprNum(false, 2), ""),
            ("@max(1, 'sd2)", ExprNum(false, 2), ""),
            ("@max(1, 2)", ExprNum(false, 2), ""),
            ("@max('sd0, 'sd1)", ExprNum(true, 1), ""),
            ("@max(-'sd2, -'sd1)", ExprNum(true, -1), ""),
            ("@max(-'sd2, 1)", ExprNum(false, 1), ""),
            ("@max(0, 1, 2, 3, 4, 5)", ExprNum(false, 5), "")
          )
        } {
          val e = expr.trim.replaceAll(" +", " ")
          e in {
            e.asTree[Expr] rewrite namer rewrite constantFold shouldBe result
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
          (" 'sd3 <<   8'sd2", ExprNum(true, 12), ""),
          (" 'sd3 >>   8'sd2", ExprNum(true, 0), ""),
          (" 'sd3 <<<  8'sd2", ExprNum(true, 12), ""),
          (" 'sd3 >>>  8'sd2", ExprNum(true, 0), ""),
          (" 'sd3 <<  -8'sd2", ExprError(), "Negative shift amount"),
          (" 'sd3 >>  -8'sd2", ExprError(), "Negative shift amount"),
          (" 'sd3 <<< -8'sd2", ExprError(), "Negative shift amount"),
          (" 'sd3 >>> -8'sd2", ExprError(), "Negative shift amount"),
          ("-'sd3 <<   8'sd2", ExprNum(true, -12), ""),
          ("-'sd3 >>   8'sd2", ExprError(), "'>>' is not well defined for negative unsized values"),
          ("-'sd3 <<<  8'sd2", ExprNum(true, -12), ""),
          ("-'sd3 >>>  8'sd2", ExprNum(true, -1), ""),
          ("-'sd3 <<  -8'sd2", ExprError(), "Negative shift amount"),
          ("-'sd3 >>  -8'sd2", ExprError(), "Negative shift amount"), // ***
          ("-'sd3 <<< -8'sd2", ExprError(), "Negative shift amount"),
          ("-'sd3 >>> -8'sd2", ExprError(), "Negative shift amount"),
          //////////////////////////////////////////////
          // signed unsigned
          //////////////////////////////////////////////
          (" 'sd3 <<  8'd2", ExprNum(true, 12), ""),
          (" 'sd3 >>  8'd2", ExprNum(true, 0), ""),
          (" 'sd3 <<< 8'd2", ExprNum(true, 12), ""),
          (" 'sd3 >>> 8'd2", ExprNum(true, 0), ""),
          ("-'sd3 <<  8'd2", ExprNum(true, -12), ""),
          ("-'sd3 >>  8'd2", ExprError(), "'>>' is not well defined for negative unsized values"),
          ("-'sd3 <<< 8'd2", ExprNum(true, -12), ""),
          ("-'sd3 >>> 8'd2", ExprNum(true, -1), ""),
          //////////////////////////////////////////////
          // unsigned signed
          //////////////////////////////////////////////
          ("3 <<  8'sd2", ExprNum(false, 12), ""),
          ("3 >>  8'sd2", ExprNum(false, 0), ""),
          ("3 <<< 8'sd2", ExprNum(false, 12), ""),
          ("3 >>> 8'sd2", ExprNum(false, 0), ""),
          ("3 <<  -8'sd2", ExprError(), "Negative shift amount"),
          ("3 >>  -8'sd2", ExprError(), "Negative shift amount"),
          ("3 <<< -8'sd2", ExprError(), "Negative shift amount"),
          ("3 >>> -8'sd2", ExprError(), "Negative shift amount"),
          //////////////////////////////////////////////
          // unsigned unsigned
          //////////////////////////////////////////////
          ("3 <<  8'd2", ExprNum(false, 12), ""),
          ("3 >>  8'd2", ExprNum(false, 0), ""),
          ("3 <<< 8'd2", ExprNum(false, 12), ""),
          ("3 >>> 8'd2", ExprNum(false, 0), "")
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
          ("+(8'sd2)", ExprInt(true, 8, 2), ""),
          ("-(8'sd2)", ExprInt(true, 8, -2), ""),
          ("~(8'sd2)", ExprInt(true, 8, -3), ""),
          ("!(8'sd2)", ExprInt(false, 1, 0), ""),
          ("&(8'sd2)", ExprInt(false, 1, 0), ""),
          ("|(8'sd2)", ExprInt(false, 1, 1), ""),
          ("^(8'sd2)", ExprInt(false, 1, 1), ""),
          // signed negative operand
          ("+(-8'sd2)", ExprInt(true, 8, -2), ""),
          ("-(-8'sd2)", ExprInt(true, 8, 2), ""),
          ("~(-8'sd2)", ExprInt(true, 8, 1), ""),
          ("!(-8'sd2)", ExprInt(false, 1, 0), ""),
          ("&(-8'sd2)", ExprInt(false, 1, 0), ""),
          ("|(-8'sd2)", ExprInt(false, 1, 1), ""),
          ("^(-8'sd2)", ExprInt(false, 1, 1), ""),
          // signed 0 operand
          ("+(8'sd0)", ExprInt(true, 8, 0), ""),
          ("-(8'sd0)", ExprInt(true, 8, 0), ""),
          ("~(8'sd0)", ExprInt(true, 8, -1), ""),
          ("!(8'sd0)", ExprInt(false, 1, 1), ""),
          ("&(8'sd0)", ExprInt(false, 1, 0), ""),
          ("|(8'sd0)", ExprInt(false, 1, 0), ""),
          ("^(8'sd0)", ExprInt(false, 1, 0), ""),
          // signed -1 operand
          ("+(-8'sd1)", ExprInt(true, 8, -1), ""),
          ("-(-8'sd1)", ExprInt(true, 8, 1), ""),
          ("~(-8'sd1)", ExprInt(true, 8, 0), ""),
          ("!(-8'sd1)", ExprInt(false, 1, 0), ""),
          ("&(-8'sd1)", ExprInt(false, 1, 1), ""),
          ("|(-8'sd1)", ExprInt(false, 1, 1), ""),
          ("^(-8'sd1)", ExprInt(false, 1, 0), ""),
          // unsigned non-0 operand
          ("+(8'd2)", ExprInt(false, 8, 2), ""),
          ("-(8'd2)", ExprError(), "Unary '-' is not well defined for unsigned values"),
          ("~(8'd2)", ExprInt(false, 8, 253), ""),
          ("!(8'd2)", ExprInt(false, 1, 0), ""),
          ("&(8'd2)", ExprInt(false, 1, 0), ""),
          ("|(8'd2)", ExprInt(false, 1, 1), ""),
          ("^(8'd2)", ExprInt(false, 1, 1), ""),
          // unsigned 0 operand
          ("+(8'd0)", ExprInt(false, 8, 0), ""),
          ("-(8'd0)", ExprInt(false, 8, 0), ""),
          ("~(8'd0)", ExprInt(false, 8, 255), ""),
          ("!(8'd0)", ExprInt(false, 1, 1), ""),
          ("&(8'd0)", ExprInt(false, 1, 0), ""),
          ("|(8'd0)", ExprInt(false, 1, 0), ""),
          ("^(8'd0)", ExprInt(false, 1, 0), ""),
          // reductions ff
          ("&(8'hff)", ExprInt(false, 1, 1), ""),
          ("|(8'hff)", ExprInt(false, 1, 1), ""),
          ("^(8'hff)", ExprInt(false, 1, 0), ""),
          // reductions 0
          ("&(8'h0)", ExprInt(false, 1, 0), ""),
          ("|(8'h0)", ExprInt(false, 1, 0), ""),
          ("^(8'h0)", ExprInt(false, 1, 0), "")
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
