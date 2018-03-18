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
            ("+(2s)", ExprNum(true, 2), ""),
            ("-(2s)", ExprNum(true, -2), ""),
            ("~(2s)", ExprNum(true, -3), ""),
            ("!(2s)", ExprInt(false, 1, 0), ""),
            ("&(2s)", ExprInt(false, 1, 0), ""),
            ("|(2s)", ExprInt(false, 1, 1), ""),
            ("^(2s)", ExprInt(false, 1, 1), ""),
            // signed negative operand
            ("+(-2s)", ExprNum(true, -2), ""),
            ("-(-2s)", ExprNum(true, 2), ""),
            ("~(-2s)", ExprNum(true, 1), ""),
            ("!(-2s)", ExprInt(false, 1, 0), ""),
            ("&(-2s)", ExprInt(false, 1, 0), ""),
            ("|(-2s)", ExprInt(false, 1, 1), ""),
            ("^(-2s)", ExprError(), "Unary '^' is not well defined for unsized negative values"),
            // signed 0 operand
            ("+(0s)", ExprNum(true, 0), ""),
            ("-(0s)", ExprNum(true, 0), ""),
            ("~(0s)", ExprNum(true, -1), ""),
            ("!(0s)", ExprInt(false, 1, 1), ""),
            ("&(0s)", ExprInt(false, 1, 0), ""),
            ("|(0s)", ExprInt(false, 1, 0), ""),
            ("^(0s)", ExprInt(false, 1, 0), ""),
            // signed -1 operand
            ("+(-1s)", ExprNum(true, -1), ""),
            ("-(-1s)", ExprNum(true, 1), ""),
            ("~(-1s)", ExprNum(true, 0), ""),
            ("!(-1s)", ExprInt(false, 1, 0), ""),
            ("&(-1s)", ExprInt(false, 1, 1), ""),
            ("|(-1s)", ExprInt(false, 1, 1), ""),
            ("^(-1s)", ExprError(), "Unary '^' is not well defined for unsized negative values"),
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
            ("3s >  2s", ExprNum(false, 1), ""),
            ("3s >= 2s", ExprNum(false, 1), ""),
            ("3s <  2s", ExprNum(false, 0), ""),
            ("3s <= 2s", ExprNum(false, 0), ""),
            ("3s == 2s", ExprNum(false, 0), ""),
            ("3s != 2s", ExprNum(false, 1), ""),
            ("3s && 2s", ExprNum(false, 1), ""),
            ("3s || 2s", ExprNum(false, 1), ""),
            // Arith
            ("3s * 2s", ExprNum(true, 6), ""),
            ("3s / 2s", ExprNum(true, 1), ""),
            ("3s % 2s", ExprNum(true, 1), ""),
            ("3s + 2s", ExprNum(true, 5), ""),
            ("3s - 2s", ExprNum(true, 1), ""),
            // Shifts
            (" 3s <<   2s", ExprNum(true, 12), ""),
            (" 3s >>   2s", ExprNum(true, 0), ""),
            (" 3s <<<  2s", ExprNum(true, 12), ""),
            (" 3s >>>  2s", ExprNum(true, 0), ""),
            (" 3s <<  -2s", ExprError(), "Negative shift amount"),
            (" 3s >>  -2s", ExprError(), "Negative shift amount"),
            (" 3s <<< -2s", ExprError(), "Negative shift amount"),
            (" 3s >>> -2s", ExprError(), "Negative shift amount"),
            ("-3s <<   2s", ExprNum(true, -12), ""),
            ("-3s >>   2s", ExprError(), "'>>' is not well defined for negative unsized values"),
            ("-3s <<<  2s", ExprNum(true, -12), ""),
            ("-3s >>>  2s", ExprNum(true, -1), ""),
            ("-3s <<  -2s", ExprError(), "Negative shift amount"),
            ("-3s >>  -2s", ExprError(), "Negative shift amount"), // ***
            ("-3s <<< -2s", ExprError(), "Negative shift amount"),
            ("-3s >>> -2s", ExprError(), "Negative shift amount"),
            // Bitwise
            (" 3s &   2s", ExprNum(true, 2), ""),
            (" 3s ^   2s", ExprNum(true, 1), ""),
            (" 3s |   2s", ExprNum(true, 3), ""),
            (" 3s &  -2s",
             ExprError(),
             "Bitwise '&' operator is not well defined for negative unsized values"),
            (" 3s ^  -2s",
             ExprError(),
             "Bitwise '\\^' operator is not well defined for negative unsized values"),
            (" 3s |  -2s",
             ExprError(),
             "Bitwise '\\|' operator is not well defined for negative unsized values"),
            ("-3s &   2s",
             ExprError(),
             "Bitwise '&' operator is not well defined for negative unsized values"),
            ("-3s ^   2s",
             ExprError(),
             "Bitwise '\\^' operator is not well defined for negative unsized values"),
            ("-3s |   2s",
             ExprError(),
             "Bitwise '\\|' operator is not well defined for negative unsized values"),
            ("-3s &  -2s",
             ExprError(),
             "Bitwise '&' operator is not well defined for negative unsized values"),
            ("-3s ^  -2s",
             ExprError(),
             "Bitwise '\\^' operator is not well defined for negative unsized values"),
            ("-3s |  -2s",
             ExprError(),
             "Bitwise '\\|' operator is not well defined for negative unsized values"),
            //////////////////////////////////////////////
            // signed unsigned
            //////////////////////////////////////////////
            // Always valid
            ("3s >  2", ExprNum(false, 1), ""),
            ("3s >= 2", ExprNum(false, 1), ""),
            ("3s <  2", ExprNum(false, 0), ""),
            ("3s <= 2", ExprNum(false, 0), ""),
            ("3s == 2", ExprNum(false, 0), ""),
            ("3s != 2", ExprNum(false, 1), ""),
            ("3s && 2", ExprNum(false, 1), ""),
            ("3s || 2", ExprNum(false, 1), ""),
            // Arith
            ("3s * 2", ExprNum(false, 6), ""),
            ("3s / 2", ExprNum(false, 1), ""),
            ("3s % 2", ExprNum(false, 1), ""),
            ("3s + 2", ExprNum(false, 5), ""),
            ("3s - 2", ExprNum(false, 1), ""),
            ("3s - 4", ExprError(), "Result of operator '-' is unsigned, but value is negative"),
            ("-3s * 2", ExprError(), "Result of operator '\\*' is unsigned, but value is negative"),
            ("-3s / 2", ExprError(), "Result of operator '/' is unsigned, but value is negative"),
            ("-3s % 2", ExprError(), "Result of operator '%' is unsigned, but value is negative"),
            ("-3s + 2", ExprError(), "Result of operator '\\+' is unsigned, but value is negative"),
            ("-3s - 2", ExprError(), "Result of operator '-' is unsigned, but value is negative"),
            ("-3s + 4", ExprNum(false, 1), ""),
            // Shifts
            (" 3s <<  2", ExprNum(true, 12), ""),
            (" 3s >>  2", ExprNum(true, 0), ""),
            (" 3s <<< 2", ExprNum(true, 12), ""),
            (" 3s >>> 2", ExprNum(true, 0), ""),
            ("-3s <<  2", ExprNum(true, -12), ""),
            ("-3s >>  2", ExprError(), "'>>' is not well defined for negative unsized values"),
            ("-3s <<< 2", ExprNum(true, -12), ""),
            ("-3s >>> 2", ExprNum(true, -1), ""),
            // Bitwise
            (" 3s &  2", ExprNum(false, 2), ""),
            (" 3s ^  2", ExprNum(false, 1), ""),
            (" 3s |  2", ExprNum(false, 3), ""),
            ("-3s &  2",
             ExprError(),
             "Bitwise '&' operator is not well defined for negative unsized values"),
            ("-3s ^  2",
             ExprError(),
             "Bitwise '\\^' operator is not well defined for negative unsized values"),
            ("-3s |  2",
             ExprError(),
             "Bitwise '\\|' operator is not well defined for negative unsized values"),
            //////////////////////////////////////////////
            // unsigned signed
            //////////////////////////////////////////////
            // Always valid
            ("3 >  2s", ExprNum(false, 1), ""),
            ("3 >= 2s", ExprNum(false, 1), ""),
            ("3 <  2s", ExprNum(false, 0), ""),
            ("3 <= 2s", ExprNum(false, 0), ""),
            ("3 == 2s", ExprNum(false, 0), ""),
            ("3 != 2s", ExprNum(false, 1), ""),
            ("3 && 2s", ExprNum(false, 1), ""),
            ("3 || 2s", ExprNum(false, 1), ""),
            // Arith
            ("3 * 2s", ExprNum(false, 6), ""),
            ("3 / 2s", ExprNum(false, 1), ""),
            ("3 % 2s", ExprNum(false, 1), ""),
            ("3 + 2s", ExprNum(false, 5), ""),
            ("3 - 2s", ExprNum(false, 1), ""),
            ("3 - 4s", ExprError(), "Result of operator '-' is unsigned, but value is negative"),
            ("3 * -2s", ExprError(), "Result of operator '\\*' is unsigned, but value is negative"),
            ("3 / -2s", ExprError(), "Result of operator '/' is unsigned, but value is negative"),
            ("3 % -2s", ExprNum(false, 1), ""),
            ("3 + -2s", ExprNum(false, 1), ""),
            ("3 - -2s", ExprNum(false, 5), ""),
            ("3 + -4s", ExprError(), "Result of operator '\\+' is unsigned, but value is negative"),
            // Shifts
            ("3 <<  2s", ExprNum(false, 12), ""),
            ("3 >>  2s", ExprNum(false, 0), ""),
            ("3 <<< 2s", ExprNum(false, 12), ""),
            ("3 >>> 2s", ExprNum(false, 0), ""),
            ("3 <<  -2s", ExprError(), "Negative shift amount"),
            ("3 >>  -2s", ExprError(), "Negative shift amount"),
            ("3 <<< -2s", ExprError(), "Negative shift amount"),
            ("3 >>> -2s", ExprError(), "Negative shift amount"),
            // Bitwise
            ("3 &  2s", ExprNum(false, 2), ""),
            ("3 ^  2s", ExprNum(false, 1), ""),
            ("3 |  2s", ExprNum(false, 3), ""),
            ("3 &  -2s",
             ExprError(),
             "Bitwise '&' operator is not well defined for negative unsized values"),
            ("3 ^  -2s",
             ExprError(),
             "Bitwise '\\^' operator is not well defined for negative unsized values"),
            ("3 |  -2s",
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
            ("@max(1s)", ExprNum(true, 1), ""),
            ("@max(1)", ExprNum(false, 1), ""),
            ("@max(1s, 2s)", ExprNum(true, 2), ""),
            ("@max(1s, 2)", ExprNum(false, 2), ""),
            ("@max(1, 2s)", ExprNum(false, 2), ""),
            ("@max(1, 2)", ExprNum(false, 2), ""),
            ("@max(0s, 1s)", ExprNum(true, 1), ""),
            ("@max(-2s, -1s)", ExprNum(true, -1), ""),
            ("@max(-2s, 1)", ExprNum(false, 1), ""),
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
          (" 3s <<   8'd2s", ExprNum(true, 12), ""),
          (" 3s >>   8'd2s", ExprNum(true, 0), ""),
          (" 3s <<<  8'd2s", ExprNum(true, 12), ""),
          (" 3s >>>  8'd2s", ExprNum(true, 0), ""),
          (" 3s <<  -8'd2s", ExprError(), "Negative shift amount"),
          (" 3s >>  -8'd2s", ExprError(), "Negative shift amount"),
          (" 3s <<< -8'd2s", ExprError(), "Negative shift amount"),
          (" 3s >>> -8'd2s", ExprError(), "Negative shift amount"),
          ("-3s <<   8'd2s", ExprNum(true, -12), ""),
          ("-3s >>   8'd2s", ExprError(), "'>>' is not well defined for negative unsized values"),
          ("-3s <<<  8'd2s", ExprNum(true, -12), ""),
          ("-3s >>>  8'd2s", ExprNum(true, -1), ""),
          ("-3s <<  -8'd2s", ExprError(), "Negative shift amount"),
          ("-3s >>  -8'd2s", ExprError(), "Negative shift amount"), // ***
          ("-3s <<< -8'd2s", ExprError(), "Negative shift amount"),
          ("-3s >>> -8'd2s", ExprError(), "Negative shift amount"),
          //////////////////////////////////////////////
          // signed unsigned
          //////////////////////////////////////////////
          (" 3s <<  8'd2", ExprNum(true, 12), ""),
          (" 3s >>  8'd2", ExprNum(true, 0), ""),
          (" 3s <<< 8'd2", ExprNum(true, 12), ""),
          (" 3s >>> 8'd2", ExprNum(true, 0), ""),
          ("-3s <<  8'd2", ExprNum(true, -12), ""),
          ("-3s >>  8'd2", ExprError(), "'>>' is not well defined for negative unsized values"),
          ("-3s <<< 8'd2", ExprNum(true, -12), ""),
          ("-3s >>> 8'd2", ExprNum(true, -1), ""),
          //////////////////////////////////////////////
          // unsigned signed
          //////////////////////////////////////////////
          ("3 <<  8'd2s", ExprNum(false, 12), ""),
          ("3 >>  8'd2s", ExprNum(false, 0), ""),
          ("3 <<< 8'd2s", ExprNum(false, 12), ""),
          ("3 >>> 8'd2s", ExprNum(false, 0), ""),
          ("3 <<  -8'd2s", ExprError(), "Negative shift amount"),
          ("3 >>  -8'd2s", ExprError(), "Negative shift amount"),
          ("3 <<< -8'd2s", ExprError(), "Negative shift amount"),
          ("3 >>> -8'd2s", ExprError(), "Negative shift amount"),
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
          ("+(8'd2s)", ExprInt(true, 8, 2), ""),
          ("-(8'd2s)", ExprInt(true, 8, -2), ""),
          ("~(8'd2s)", ExprInt(true, 8, -3), ""),
          ("!(8'd2s)", ExprInt(false, 1, 0), ""),
          ("&(8'd2s)", ExprInt(false, 1, 0), ""),
          ("|(8'd2s)", ExprInt(false, 1, 1), ""),
          ("^(8'd2s)", ExprInt(false, 1, 1), ""),
          // signed negative operand
          ("+(-8'd2s)", ExprInt(true, 8, -2), ""),
          ("-(-8'd2s)", ExprInt(true, 8, 2), ""),
          ("~(-8'd2s)", ExprInt(true, 8, 1), ""),
          ("!(-8'd2s)", ExprInt(false, 1, 0), ""),
          ("&(-8'd2s)", ExprInt(false, 1, 0), ""),
          ("|(-8'd2s)", ExprInt(false, 1, 1), ""),
          ("^(-8'd2s)", ExprInt(false, 1, 1), ""),
          // signed 0 operand
          ("+(8'd0s)", ExprInt(true, 8, 0), ""),
          ("-(8'd0s)", ExprInt(true, 8, 0), ""),
          ("~(8'd0s)", ExprInt(true, 8, -1), ""),
          ("!(8'd0s)", ExprInt(false, 1, 1), ""),
          ("&(8'd0s)", ExprInt(false, 1, 0), ""),
          ("|(8'd0s)", ExprInt(false, 1, 0), ""),
          ("^(8'd0s)", ExprInt(false, 1, 0), ""),
          // signed -1 operand
          ("+(-8'd1s)", ExprInt(true, 8, -1), ""),
          ("-(-8'd1s)", ExprInt(true, 8, 1), ""),
          ("~(-8'd1s)", ExprInt(true, 8, 0), ""),
          ("!(-8'd1s)", ExprInt(false, 1, 0), ""),
          ("&(-8'd1s)", ExprInt(false, 1, 1), ""),
          ("|(-8'd1s)", ExprInt(false, 1, 1), ""),
          ("^(-8'd1s)", ExprInt(false, 1, 0), ""),
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
