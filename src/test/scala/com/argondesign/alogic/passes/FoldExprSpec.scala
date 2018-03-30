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

package com.argondesign.alogic.passes

import java.util.regex.Pattern

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.typer.Typer
import org.scalatest.FreeSpec

final class FoldExprSpec extends FreeSpec with AlogicTest {

  implicit val cc = new CompilerContext
  val namer = new Namer
  val typer = new Typer
  val fold = new FoldExpr(false)

  def xform(tree: Tree): Tree = {
    tree match {
      case root: Root     => cc.addGlobalEntity(root.entity)
      case entity: Entity => cc.addGlobalEntity(entity)
      case _              =>
    }
    tree rewrite namer rewrite typer rewrite fold
  }

  "FoldExpr should fold" - {
    "unary operators applied to unsized integer literals" - {
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
          expr.asTree[Expr] rewrite fold shouldBe result
          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](Pattern.quote(msg))
          }
        }
      }
    }

    "binary operators applied to unsized integer literals" - {
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
          ("-'sd3 >>   'sd2", ExprError(), "'>>' is not well defined for negative unsized values"),
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
          ("-'sd3 * 2", ExprError(), "Result of operator '\\*' is unsigned, but value is negative"),
          ("-'sd3 / 2", ExprError(), "Result of operator '/' is unsigned, but value is negative"),
          ("-'sd3 % 2", ExprError(), "Result of operator '%' is unsigned, but value is negative"),
          ("-'sd3 + 2", ExprError(), "Result of operator '\\+' is unsigned, but value is negative"),
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
          ("3 * -'sd2", ExprError(), "Result of operator '\\*' is unsigned, but value is negative"),
          ("3 / -'sd2", ExprError(), "Result of operator '/' is unsigned, but value is negative"),
          ("3 % -'sd2", ExprNum(false, 1), ""),
          ("3 + -'sd2", ExprNum(false, 1), ""),
          ("3 - -'sd2", ExprNum(false, 5), ""),
          ("3 + -'sd4", ExprError(), "Result of operator '\\+' is unsigned, but value is negative"),
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
          e.asTree[Expr] rewrite fold shouldBe result
          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](msg)
          }
        }
      }
    }

    "shifts with an unsized left hand side and a sized right hand side" - {
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
          e.asTree[Expr] rewrite fold shouldBe result
          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](msg)
          }
        }
      }
    }

    "unary operators applied to sized integer literals" - {
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
          expr.asTree[Expr] rewrite fold shouldBe result
          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](msg)
          }
        }
      }
    }

    "builtin functions" - {
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
            e.asTree[Expr] rewrite namer rewrite fold shouldBe result
            if (msg.isEmpty) {
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }
        }
      }

      "@zx" - {
        for {
          (expr, result, msg) <- List(
            ("@zx(3, 2'b00)", ExprInt(false, 3, 0), ""),
            ("@zx(3, 2'b01)", ExprInt(false, 3, 1), ""),
            ("@zx(3, 2'b10)", ExprInt(false, 3, 2), ""),
            ("@zx(3, 2'b11)", ExprInt(false, 3, 3), ""),
            ("@zx(3, 2'sb00)", ExprInt(true, 3, 0), ""),
            ("@zx(3, 2'sb01)", ExprInt(true, 3, 1), ""),
            ("@zx(3, 2'sb10)", ExprInt(true, 3, 2), ""),
            ("@zx(3, 2'sb11)", ExprInt(true, 3, 3), ""),
            ("@zx(2, 2'b00)", ExprInt(false, 2, 0), ""),
            ("@zx(2, 2'b01)", ExprInt(false, 2, 1), ""),
            ("@zx(2, 2'b10)", ExprInt(false, 2, 2), ""),
            ("@zx(2, 2'b11)", ExprInt(false, 2, 3), ""),
            ("@zx(2, 2'sb00)", ExprInt(true, 2, 0), ""),
            ("@zx(2, 2'sb01)", ExprInt(true, 2, 1), ""),
            ("@zx(2, 2'sb10)", ExprInt(true, 2, -2), ""),
            ("@zx(2, 2'sb11)", ExprInt(true, 2, -1), ""),
            ("@zx(1, 2'b00)",
             ExprError(),
             "Result width (1) of @zx is less than argument width (2)"),
            ("@zx(1, 2'b01)",
             ExprError(),
             "Result width (1) of @zx is less than argument width (2)"),
            ("@zx(1, 2'b10)",
             ExprError(),
             "Result width (1) of @zx is less than argument width (2)"),
            ("@zx(1, 2'b11)",
             ExprError(),
             "Result width (1) of @zx is less than argument width (2)"),
            ("@zx(1, 2'sb00)",
             ExprError(),
             "Result width (1) of @zx is less than argument width (2)"),
            ("@zx(1, 2'sb01)",
             ExprError(),
             "Result width (1) of @zx is less than argument width (2)"),
            ("@zx(1, 2'sb10)",
             ExprError(),
             "Result width (1) of @zx is less than argument width (2)"),
            ("@zx(1, 2'sb11)",
             ExprError(),
             "Result width (1) of @zx is less than argument width (2)")
          )
        } {
          val e = expr.trim.replaceAll(" +", " ")
          e in {
            xform(e.asTree[Expr]) shouldBe result
            val errors = cc.messages filter { _.isInstanceOf[Error] }
            if (msg.isEmpty) {
              errors shouldBe empty
            } else {
              errors.loneElement should beThe[Error](Pattern.quote(msg))
            }
          }
        }
      }

      "@sx" - {
        for {
          (expr, result, msg) <- List(
            ("@sx(3, 2'b00)", ExprInt(false, 3, 0), ""),
            ("@sx(3, 2'b01)", ExprInt(false, 3, 1), ""),
            ("@sx(3, 2'b10)", ExprInt(false, 3, 6), ""),
            ("@sx(3, 2'b11)", ExprInt(false, 3, 7), ""),
            ("@sx(3, 2'sb00)", ExprInt(true, 3, 0), ""),
            ("@sx(3, 2'sb01)", ExprInt(true, 3, 1), ""),
            ("@sx(3, 2'sb10)", ExprInt(true, 3, -2), ""),
            ("@sx(3, 2'sb11)", ExprInt(true, 3, -1), ""),
            ("@sx(2, 2'b00)", ExprInt(false, 2, 0), ""),
            ("@sx(2, 2'b01)", ExprInt(false, 2, 1), ""),
            ("@sx(2, 2'b10)", ExprInt(false, 2, 2), ""),
            ("@sx(2, 2'b11)", ExprInt(false, 2, 3), ""),
            ("@sx(2, 2'sb00)", ExprInt(true, 2, 0), ""),
            ("@sx(2, 2'sb01)", ExprInt(true, 2, 1), ""),
            ("@sx(2, 2'sb10)", ExprInt(true, 2, -2), ""),
            ("@sx(2, 2'sb11)", ExprInt(true, 2, -1), ""),
            ("@sx(1, 2'b00)",
             ExprError(),
             "Result width (1) of @sx is less than argument width (2)"),
            ("@sx(1, 2'b01)",
             ExprError(),
             "Result width (1) of @sx is less than argument width (2)"),
            ("@sx(1, 2'b10)",
             ExprError(),
             "Result width (1) of @sx is less than argument width (2)"),
            ("@sx(1, 2'b11)",
             ExprError(),
             "Result width (1) of @sx is less than argument width (2)"),
            ("@sx(1, 2'sb00)",
             ExprError(),
             "Result width (1) of @sx is less than argument width (2)"),
            ("@sx(1, 2'sb01)",
             ExprError(),
             "Result width (1) of @sx is less than argument width (2)"),
            ("@sx(1, 2'sb10)",
             ExprError(),
             "Result width (1) of @sx is less than argument width (2)"),
            ("@sx(1, 2'sb11)",
             ExprError(),
             "Result width (1) of @sx is less than argument width (2)")
          )
        } {
          val e = expr.trim.replaceAll(" +", " ")
          e in {
            xform(e.asTree[Expr]) shouldBe result
            val errors = cc.messages filter { _.isInstanceOf[Error] }
            if (msg.isEmpty) {
              errors shouldBe empty
            } else {
              errors.loneElement should beThe[Error](Pattern.quote(msg))
            }
          }
        }
      }

      "@bits" - {
        for {
          (expr, result, msg) <- List(
            ("@bits(1'b0)", ExprNum(false, 1), ""),
            ("@bits(2'b0)", ExprNum(false, 2), ""),
            ("@bits(2'sb0)", ExprNum(false, 2), ""),
            ("@bits(bool)", ExprNum(false, 1), ""),
            ("@bits(u3)", ExprNum(false, 3), ""),
            ("@bits(i3)", ExprNum(false, 3), ""),
            ("@bits(a)", ExprNum(false, 10), ""),
            ("@bits(a.f0)", ExprNum(false, 1), ""),
            ("@bits(a.f1)", ExprNum(false, 7), ""),
            ("@bits(a.f2)", ExprNum(false, 2), ""),
            ("@bits(a.f2.f0)", ExprNum(false, 2), ""),
            ("@bits(a_t)", ExprNum(false, 10), ""),
            ("@bits(a_t.f0)", ExprNum(false, 1), ""),
            ("@bits(a_t.f1)", ExprNum(false, 7), ""),
            ("@bits(a_t.f2)", ExprNum(false, 2), ""),
            ("@bits(a_t.f2.f0)", ExprNum(false, 2), "")
          )
        } {
          val e = expr.trim.replaceAll(" +", " ")
          e in {
            val tree = s"""|struct b_t {
                           |  u2 f0;
                           |};
                           |
                           |struct a_t {
                           |  bool f0;
                           |  i7   f1;
                           |  b_t  f2;
                           |};
                           |
                           |fsm x {
                           |  a_t a;
                           |  fence {
                           |    ${expr};
                           |  }
                           |}""".stripMargin.asTree[Root]
            val exprOpt = xform(tree) collectFirst { case StmtExpr(e) => e }
            val errors = cc.messages filter { _.isInstanceOf[Error] }
            if (msg.isEmpty) {
              exprOpt.value shouldBe result
              errors shouldBe empty
            } else {
              errors.loneElement should beThe[Error](Pattern.quote(msg))
            }
          }
        }
      }
    }
  }
}
