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
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Warning
import com.argondesign.alogic.typer.Typer
import org.scalatest.FreeSpec

final class FoldExprSpec extends FreeSpec with AlogicTest {

  implicit val cc = new CompilerContext
  val namer = new Namer
  val typer = new Typer
  val rpoly = new ResolvePolyFunc
  val aics = new AddCasts
  val fold = new FoldExpr(foldRefs = false)

  def xform(tree: Tree): Tree = {
    tree match {
      case Root(_, entity: Entity) => cc.addGlobalEntity(entity)
      case entity: Entity          => cc.addGlobalEntity(entity)
      case _                       =>
    }
    val node = tree rewrite namer match {
      case Root(_, entity) => entity
      case other           => other
    }
    node rewrite typer rewrite rpoly rewrite aics rewrite fold
  }

  "FoldExpr should fold" - {
    "unary operators applied to unsized integer literals" - {
      for {
        (text, result, msg) <- List(
          // signed positive operand
          ("+(2s)", ExprNum(true, 2), ""),
          ("-(2s)", ExprNum(true, -2), ""),
          ("~(2s)", ExprNum(true, -3), ""),
          ("!(2s)", ExprInt(false, 1, 0), ""),
          // signed negative operand
          ("+(-2s)", ExprNum(true, -2), ""),
          ("-(-2s)", ExprNum(true, 2), ""),
          ("~(-2s)", ExprNum(true, 1), ""),
          ("!(-2s)", ExprInt(false, 1, 0), ""),
          // signed 0 operand
          ("+(0s)", ExprNum(true, 0), ""),
          ("-(0s)", ExprNum(true, 0), ""),
          ("~(0s)", ExprNum(true, -1), ""),
          ("!(0s)", ExprInt(false, 1, 1), ""),
          // signed -1 operand
          ("+(-1s)", ExprNum(true, -1), ""),
          ("-(-1s)", ExprNum(true, 1), ""),
          ("~(-1s)", ExprNum(true, 0), ""),
          ("!(-1s)", ExprInt(false, 1, 0), ""),
          // unsigned non-0 operand
          ("+(2)", ExprNum(false, 2), ""),
          ("-(2)", ExprError(), "Unary '-' is not well defined for unsigned values"),
          ("~(2)", ExprError(), "Unary '~' is not well defined for unsized unsigned values"),
          ("!(2)", ExprInt(false, 1, 0), ""),
          // unsigned 0 operand
          ("+(0)", ExprNum(false, 0), ""),
          ("-(0)", ExprNum(false, 0), ""),
          ("~(0)", ExprError(), "Unary '~' is not well defined for unsized unsigned values"),
          ("!(0)", ExprInt(false, 1, 1), "")
        )
      } {
        val expr = text.trim
        expr in {
          xform(expr.asTree[Expr]) shouldBe result
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
          // format: off
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
          (" 3s &  2s", ExprNum(true, 2), ""),
          (" 3s ^  2s", ExprNum(true, 1), ""),
          (" 3s |  2s", ExprNum(true, 3), ""),
          (" 3s & -2s", ExprError(), "Bitwise '&' operator is not well defined for negative unsized values"),
          (" 3s ^ -2s", ExprError(), "Bitwise '\\^' operator is not well defined for negative unsized values"),
          (" 3s | -2s", ExprError(), "Bitwise '\\|' operator is not well defined for negative unsized values"),
          ("-3s &  2s", ExprError(), "Bitwise '&' operator is not well defined for negative unsized values"),
          ("-3s ^  2s", ExprError(), "Bitwise '\\^' operator is not well defined for negative unsized values"),
          ("-3s |  2s", ExprError(), "Bitwise '\\|' operator is not well defined for negative unsized values"),
          ("-3s & -2s", ExprError(), "Bitwise '&' operator is not well defined for negative unsized values"),
          ("-3s ^ -2s", ExprError(), "Bitwise '\\^' operator is not well defined for negative unsized values"),
          ("-3s | -2s", ExprError(), "Bitwise '\\|' operator is not well defined for negative unsized values"),
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
          (" 3s * 2", ExprNum(false, 6), ""),
          (" 3s / 2", ExprNum(false, 1), ""),
          (" 3s % 2", ExprNum(false, 1), ""),
          (" 3s + 2", ExprNum(false, 5), ""),
          (" 3s - 2", ExprNum(false, 1), ""),
          (" 3s - 4", ExprError(), "Result of operator '-' is unsigned, but value is negative"),
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
          (" 3s & 2", ExprNum(false, 2), ""),
          (" 3s ^ 2", ExprNum(false, 1), ""),
          (" 3s | 2", ExprNum(false, 3), ""),
          ("-3s & 2", ExprError(), "Bitwise '&' operator is not well defined for negative unsized values"),
          ("-3s ^ 2", ExprError(), "Bitwise '\\^' operator is not well defined for negative unsized values"),
          ("-3s | 2", ExprError(), "Bitwise '\\|' operator is not well defined for negative unsized values"),
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
          ("3 *  2s", ExprNum(false, 6), ""),
          ("3 /  2s", ExprNum(false, 1), ""),
          ("3 %  2s", ExprNum(false, 1), ""),
          ("3 +  2s", ExprNum(false, 5), ""),
          ("3 -  2s", ExprNum(false, 1), ""),
          ("3 -  4s", ExprError(), "Result of operator '-' is unsigned, but value is negative"),
          ("3 * -2s", ExprError(), "Result of operator '\\*' is unsigned, but value is negative"),
          ("3 / -2s", ExprError(), "Result of operator '/' is unsigned, but value is negative"),
          ("3 % -2s", ExprNum(false, 1), ""),
          ("3 + -2s", ExprNum(false, 1), ""),
          ("3 - -2s", ExprNum(false, 5), ""),
          ("3 + -4s", ExprError(), "Result of operator '\\+' is unsigned, but value is negative"),
          // Shifts
          ("3 <<   2s", ExprNum(false, 12), ""),
          ("3 >>   2s", ExprNum(false, 0), ""),
          ("3 <<<  2s", ExprNum(false, 12), ""),
          ("3 >>>  2s", ExprNum(false, 0), ""),
          ("3 <<  -2s", ExprError(), "Negative shift amount"),
          ("3 >>  -2s", ExprError(), "Negative shift amount"),
          ("3 <<< -2s", ExprError(), "Negative shift amount"),
          ("3 >>> -2s", ExprError(), "Negative shift amount"),
          // Bitwise
          ("3 &  2s", ExprNum(false, 2), ""),
          ("3 ^  2s", ExprNum(false, 1), ""),
          ("3 |  2s", ExprNum(false, 3), ""),
          ("3 & -2s", ExprError(), "Bitwise '&' operator is not well defined for negative unsized values"),
          ("3 ^ -2s", ExprError(), "Bitwise '\\^' operator is not well defined for negative unsized values"),
          ("3 | -2s", ExprError(), "Bitwise '\\|' operator is not well defined for negative unsized values"),
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
          // format: on
        )
      } {
        val e = expr.trim.replaceAll(" +", " ")
        e in {
          xform(e.asTree[Expr]) shouldBe result
          if (msg.isEmpty) {
            cc.messages filterNot { _.isInstanceOf[Warning] } shouldBe empty
          } else {
            (cc.messages filterNot { _.isInstanceOf[Warning] }).loneElement should beThe[Error](msg)
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
          (" 3s <<   8'sd2", ExprNum(true, 12), ""),
          (" 3s >>   8'sd2", ExprNum(true, 0), ""),
          (" 3s <<<  8'sd2", ExprNum(true, 12), ""),
          (" 3s >>>  8'sd2", ExprNum(true, 0), ""),
          (" 3s <<  -8'sd2", ExprError(), "Negative shift amount"),
          (" 3s >>  -8'sd2", ExprError(), "Negative shift amount"),
          (" 3s <<< -8'sd2", ExprError(), "Negative shift amount"),
          (" 3s >>> -8'sd2", ExprError(), "Negative shift amount"),
          ("-3s <<   8'sd2", ExprNum(true, -12), ""),
          ("-3s >>   8'sd2", ExprError(), "'>>' is not well defined for negative unsized values"),
          ("-3s <<<  8'sd2", ExprNum(true, -12), ""),
          ("-3s >>>  8'sd2", ExprNum(true, -1), ""),
          ("-3s <<  -8'sd2", ExprError(), "Negative shift amount"),
          ("-3s >>  -8'sd2", ExprError(), "Negative shift amount"), // ***
          ("-3s <<< -8'sd2", ExprError(), "Negative shift amount"),
          ("-3s >>> -8'sd2", ExprError(), "Negative shift amount"),
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
          ("3 <<   8'sd2", ExprNum(false, 12), ""),
          ("3 >>   8'sd2", ExprNum(false, 0), ""),
          ("3 <<<  8'sd2", ExprNum(false, 12), ""),
          ("3 >>>  8'sd2", ExprNum(false, 0), ""),
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
          xform(e.asTree[Expr]) shouldBe result
          if (msg.isEmpty) {
            cc.messages filterNot { _.isInstanceOf[Warning] } shouldBe empty
          } else {
            (cc.messages filterNot { _.isInstanceOf[Warning] }).loneElement should beThe[Error](msg)
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
          xform(expr.asTree[Expr]) shouldBe result
          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](msg)
          }
        }
      }
    }

    "binary operators applied to equally sized integer literals" - {
      for {
        (expr, result) <- List(
          //////////////////////////////////////////////
          // signed signed
          //////////////////////////////////////////////
          // Always valid
          ("4'sd3 >  4'sd2", ExprInt(false, 1, 1)),
          ("4'sd3 >= 4'sd2", ExprInt(false, 1, 1)),
          ("4'sd3 <  4'sd2", ExprInt(false, 1, 0)),
          ("4'sd3 <= 4'sd2", ExprInt(false, 1, 0)),
          ("4'sd3 == 4'sd2", ExprInt(false, 1, 0)),
          ("4'sd3 != 4'sd2", ExprInt(false, 1, 1)),
          ("4'sd3 && 4'sd2", ExprInt(false, 1, 1)),
          ("4'sd3 || 4'sd2", ExprInt(false, 1, 1)),
          // Arith
          ("4'sd3 * 4'sd2", ExprInt(true, 4, 6)),
          ("4'sd3 / 4'sd2", ExprInt(true, 4, 1)),
          ("4'sd3 % 4'sd2", ExprInt(true, 4, 1)),
          ("4'sd3 + 4'sd2", ExprInt(true, 4, 5)),
          ("4'sd3 - 4'sd2", ExprInt(true, 4, 1)),
          ("4'sd3 - 4'sd4", ExprInt(true, 4, -1)),
          // Bitwise
          (" 4'sd3 &   4'sd2", ExprInt(true, 4, 2)),
          (" 4'sd3 ^   4'sd2", ExprInt(true, 4, 1)),
          (" 4'sd3 |   4'sd2", ExprInt(true, 4, 3)),
          (" 4'sd3 &  -4'sd2", ExprInt(true, 4, 2)),
          (" 4'sd3 ^  -4'sd2", ExprInt(true, 4, -3)),
          (" 4'sd3 |  -4'sd2", ExprInt(true, 4, -1)),
          ("-4'sd3 &   4'sd2", ExprInt(true, 4, 0)),
          ("-4'sd3 ^   4'sd2", ExprInt(true, 4, -1)),
          ("-4'sd3 |   4'sd2", ExprInt(true, 4, -1)),
          ("-4'sd3 &  -4'sd2", ExprInt(true, 4, -4)),
          ("-4'sd3 ^  -4'sd2", ExprInt(true, 4, 3)),
          ("-4'sd3 |  -4'sd2", ExprInt(true, 4, -1)),
          //////////////////////////////////////////////
          // signed unsigned
          //////////////////////////////////////////////
          // Always valid
          ("4'sd3 >  4'd2", ExprInt(false, 1, 1)),
          ("4'sd3 >= 4'd2", ExprInt(false, 1, 1)),
          ("4'sd3 <  4'd2", ExprInt(false, 1, 0)),
          ("4'sd3 <= 4'd2", ExprInt(false, 1, 0)),
          ("4'sd3 == 4'd2", ExprInt(false, 1, 0)),
          ("4'sd3 != 4'd2", ExprInt(false, 1, 1)),
          ("4'sd3 && 4'd2", ExprInt(false, 1, 1)),
          ("4'sd3 || 4'd2", ExprInt(false, 1, 1)),
          // Bitwise
          (" 4'sd3 &  4'd2", ExprInt(false, 4, 2)),
          (" 4'sd3 ^  4'd2", ExprInt(false, 4, 1)),
          (" 4'sd3 |  4'd2", ExprInt(false, 4, 3)),
          ("-4'sd3 &  4'd2", ExprInt(false, 4, 0)),
          ("-4'sd3 ^  4'd2", ExprInt(false, 4, 15)),
          ("-4'sd3 |  4'd2", ExprInt(false, 4, 15)),
          //////////////////////////////////////////////
          // unsigned signed
          //////////////////////////////////////////////
          // Always valid
          ("4'd3 >  4'sd2", ExprInt(false, 1, 1)),
          ("4'd3 >= 4'sd2", ExprInt(false, 1, 1)),
          ("4'd3 <  4'sd2", ExprInt(false, 1, 0)),
          ("4'd3 <= 4'sd2", ExprInt(false, 1, 0)),
          ("4'd3 == 4'sd2", ExprInt(false, 1, 0)),
          ("4'd3 != 4'sd2", ExprInt(false, 1, 1)),
          ("4'd3 && 4'sd2", ExprInt(false, 1, 1)),
          ("4'd3 || 4'sd2", ExprInt(false, 1, 1)),
          // Bitwise
          ("4'd3 &  4'sd2", ExprInt(false, 4, 2)),
          ("4'd3 ^  4'sd2", ExprInt(false, 4, 1)),
          ("4'd3 |  4'sd2", ExprInt(false, 4, 3)),
          ("4'd3 & -4'sd2", ExprInt(false, 4, 2)),
          ("4'd3 ^ -4'sd2", ExprInt(false, 4, 13)),
          ("4'd3 | -4'sd2", ExprInt(false, 4, 15)),
          //////////////////////////////////////////////
          // unsigned unsigned
          //////////////////////////////////////////////
          // Always valid
          ("4'd3 >  4'd2", ExprInt(false, 1, 1)),
          ("4'd3 >= 4'd2", ExprInt(false, 1, 1)),
          ("4'd3 <  4'd2", ExprInt(false, 1, 0)),
          ("4'd3 <= 4'd2", ExprInt(false, 1, 0)),
          ("4'd3 == 4'd2", ExprInt(false, 1, 0)),
          ("4'd3 != 4'd2", ExprInt(false, 1, 1)),
          ("4'd3 && 4'd2", ExprInt(false, 1, 1)),
          ("4'd3 || 4'd2", ExprInt(false, 1, 1)),
          // Arith
          ("4'd3 * 4'd2", ExprInt(false, 4, 6)),
          ("4'd3 / 4'd2", ExprInt(false, 4, 1)),
          ("4'd3 % 4'd2", ExprInt(false, 4, 1)),
          ("4'd3 + 4'd2", ExprInt(false, 4, 5)),
          ("4'd3 - 4'd2", ExprInt(false, 4, 1)),
          ("4'd3 - 4'd4", ExprInt(false, 4, 15)),
          // Bitwise
          ("4'd3 &  4'd2", ExprInt(false, 4, 2)),
          ("4'd3 ^  4'd2", ExprInt(false, 4, 1)),
          ("4'd3 |  4'd2", ExprInt(false, 4, 3))
        )
      } {
        val e = expr.trim.replaceAll(" +", " ")
        e in {
          xform(e.asTree[Expr]) shouldBe result
          cc.messages filterNot { _.isInstanceOf[Warning] } shouldBe empty
        }
      }
    }

    "ternary operator" - {
      for {
        (text, pattern, msg) <- List[(String, PartialFunction[Any, Unit], String)](
          ("0 ? 1 : 2", { case ExprNum(false, v) if v == 2                                     => }, ""),
          ("1 ? 1 : 2", { case ExprNum(false, v) if v == 1                                     => }, ""),
          ("@randbit() ? 1 : 1", { case ExprNum(false, v) if v == 1                            => }, ""),
          ("@randbit() ? 2 : 2", { case ExprNum(false, v) if v == 2                            => }, ""),
          ("@randbit() ? 8'd0 : 8'd0", { case ExprInt(false, 8, v) if v == 0                   => }, ""),
          ("@randbit() ? 8'd0 : 8'd1", { case ExprTernary(_: ExprCall, _: ExprInt, _: ExprInt) => },
           ""),
          ("@randbit() ? 8'd0 : 8'sd0", { case ExprTernary(_: ExprCall, _: ExprInt, _: ExprInt) => },
           "")
        )
      } {
        val expr = text.trim
        expr in {
          xform(expr.asTree[Expr]) should matchPattern(pattern)
          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](msg)
          }
        }
      }
    }

    "index into sized integer literals" - {
      for {
        (text, result, msg) <- List(
          // signed operand
          ("4'sd2[0]", ExprInt(false, 1, 0), ""),
          ("4'sd2[1]", ExprInt(false, 1, 1), ""),
          ("4'sd2[2]", ExprInt(false, 1, 0), ""),
          ("4'sd2[3]", ExprInt(false, 1, 0), ""),
          // unsigned operand
          ("4'd2[0]", ExprInt(false, 1, 0), ""),
          ("4'd2[1]", ExprInt(false, 1, 1), ""),
          ("4'd2[2]", ExprInt(false, 1, 0), ""),
          ("4'd2[3]", ExprInt(false, 1, 0), "")
        )
      } {
        val expr = text.trim
        expr in {
          xform(expr.asTree[Expr]) shouldBe result
          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](msg)
          }
        }
      }
    }

    "slice into sized integer literals" - {
      for {
        (text, result, msg) <- List(
          // signed operand
          ("4'sb0101[1 :0]", ExprInt(false, 2, 1), ""),
          ("4'sb0101[2 :0]", ExprInt(false, 3, 5), ""),
          ("4'sb0101[3 :0]", ExprInt(false, 4, 5), ""),
          ("4'sb0101[1+:1]", ExprInt(false, 1, 0), ""),
          ("4'sb0101[1+:2]", ExprInt(false, 2, 2), ""),
          ("4'sb0101[1+:3]", ExprInt(false, 3, 2), ""),
          ("4'sb0101[3-:1]", ExprInt(false, 1, 0), ""),
          ("4'sb0101[3-:2]", ExprInt(false, 2, 1), ""),
          ("4'sb0101[3-:3]", ExprInt(false, 3, 2), ""),
          // unsigned operand
          ("4'b0101[1 :0]", ExprInt(false, 2, 1), ""),
          ("4'b0101[2 :0]", ExprInt(false, 3, 5), ""),
          ("4'b0101[3 :0]", ExprInt(false, 4, 5), ""),
          ("4'b0101[1+:1]", ExprInt(false, 1, 0), ""),
          ("4'b0101[1+:2]", ExprInt(false, 2, 2), ""),
          ("4'b0101[1+:3]", ExprInt(false, 3, 2), ""),
          ("4'b0101[3-:1]", ExprInt(false, 1, 0), ""),
          ("4'b0101[3-:2]", ExprInt(false, 2, 1), ""),
          ("4'b0101[3-:3]", ExprInt(false, 3, 2), ""),
          // long range
          ("36'hf0f0f0f0f[35:0]", ExprInt(false, 36, BigInt("f0f0f0f0f", 16)), ""),
          ("68'hf0f0f0f0f0f0f0f0f[67:0]", ExprInt(false, 68, BigInt("f0f0f0f0f0f0f0f0f", 16)), "")
        )
      } {
        val expr = text.trim
        expr in {
          xform(expr.asTree[Expr]) shouldBe result
          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](msg)
          }
        }
      }
    }

    "index into unsized integer literals" - {
      for {
        (text, result, msg) <- List(
          // signed operand
          (" 2s[0]", ExprInt(false, 1, 0), ""),
          (" 2s[1]", ExprInt(false, 1, 1), ""),
          (" 2s[2]", ExprInt(false, 1, 0), ""),
          (" 2s[3]", ExprInt(false, 1, 0), ""),
          ("-2s[0]", ExprInt(false, 1, 0), ""),
          ("-2s[1]", ExprInt(false, 1, 1), ""),
          ("-2s[2]", ExprInt(false, 1, 1), ""),
          ("-2s[3]", ExprInt(false, 1, 1), ""),
          // unsigned operand
          (" 2[0]", ExprInt(false, 1, 0), ""),
          (" 2[1]", ExprInt(false, 1, 1), ""),
          (" 2[2]", ExprInt(false, 1, 0), ""),
          (" 2[3]", ExprInt(false, 1, 0), "")
        )
      } {
        val expr = text.trim
        expr in {
          xform(expr.asTree[Expr]) shouldBe result
          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](msg)
          }
        }
      }
    }

    "slice into unsized integer literals" - {
      for {
        (text, result, msg) <- List(
          // signed operand
          (" 5s[1 :0]", ExprInt(false, 2, 1), ""),
          (" 5s[2 :0]", ExprInt(false, 3, 5), ""),
          (" 5s[3 :0]", ExprInt(false, 4, 5), ""),
          (" 5s[1+:1]", ExprInt(false, 1, 0), ""),
          (" 5s[1+:2]", ExprInt(false, 2, 2), ""),
          (" 5s[1+:3]", ExprInt(false, 3, 2), ""),
          (" 5s[3-:1]", ExprInt(false, 1, 0), ""),
          (" 5s[3-:2]", ExprInt(false, 2, 1), ""),
          (" 5s[3-:3]", ExprInt(false, 3, 2), ""),
          ("-5s[1 :0]", ExprInt(false, 2, 3), ""),
          ("-5s[2 :0]", ExprInt(false, 3, 3), ""),
          ("-5s[3 :0]", ExprInt(false, 4, 11), ""),
          ("-5s[1+:1]", ExprInt(false, 1, 1), ""),
          ("-5s[1+:2]", ExprInt(false, 2, 1), ""),
          ("-5s[1+:3]", ExprInt(false, 3, 5), ""),
          ("-5s[3-:1]", ExprInt(false, 1, 1), ""),
          ("-5s[3-:2]", ExprInt(false, 2, 2), ""),
          ("-5s[3-:3]", ExprInt(false, 3, 5), ""),
          // unsigned operand
          ("5[1 :0]", ExprInt(false, 2, 1), ""),
          ("5[2 :0]", ExprInt(false, 3, 5), ""),
          ("5[3 :0]", ExprInt(false, 4, 5), ""),
          ("5[1+:1]", ExprInt(false, 1, 0), ""),
          ("5[1+:2]", ExprInt(false, 2, 2), ""),
          ("5[1+:3]", ExprInt(false, 3, 2), ""),
          ("5[3-:1]", ExprInt(false, 1, 0), ""),
          ("5[3-:2]", ExprInt(false, 2, 1), ""),
          ("5[3-:3]", ExprInt(false, 3, 2), "")
        )
      } {
        val expr = text.trim
        expr in {
          xform(expr.asTree[Expr]) shouldBe result
          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](msg)
          }
        }
      }
    }

    "index over a slice" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          // format: off
          ("a[8  : 3][0]", { case ExprIndex(ExprSym(s), ExprInt(false, 4, v)) if s.name == "a" && v == 3 => }),
          ("a[9  : 3][0]", { case ExprIndex(ExprSym(s), ExprInt(false, 4, v)) if s.name == "a" && v == 3 => }),
          ("a[8  : 3][2]", { case ExprIndex(ExprSym(s), ExprInt(false, 4, v)) if s.name == "a" && v == 5 => }),
          ("a[9  : 3][2]", { case ExprIndex(ExprSym(s), ExprInt(false, 4, v)) if s.name == "a" && v == 5 => }),
          ("a[8 +: 3][0]", { case ExprIndex(ExprSym(s), ExprInt(false, 4, v)) if s.name == "a" && v == 8 => }),
          ("a[9 +: 3][0]", { case ExprIndex(ExprSym(s), ExprInt(false, 4, v)) if s.name == "a" && v == 9 => }),
          ("a[8 +: 3][2]", { case ExprIndex(ExprSym(s), ExprInt(false, 4, v)) if s.name == "a" && v == 10 => }),
          ("a[9 +: 3][2]", { case ExprIndex(ExprSym(s), ExprInt(false, 4, v)) if s.name == "a" && v == 11 => }),
          ("a[8 -: 3][0]", { case ExprIndex(ExprSym(s), ExprInt(false, 4, v)) if s.name == "a" && v == 6 => }),
          ("a[9 -: 3][0]", { case ExprIndex(ExprSym(s), ExprInt(false, 4, v)) if s.name == "a" && v == 7 => }),
          ("a[8 -: 3][2]", { case ExprIndex(ExprSym(s), ExprInt(false, 4, v)) if s.name == "a" && v == 8 => }),
          ("a[9 -: 3][2]", { case ExprIndex(ExprSym(s), ExprInt(false, 4, v)) if s.name == "a" && v == 9 => })
          // format: on
        )
      } {
        text in {
          val src = s"""|fsm f {
                        |  in u10 a;
                        |  out u1 b;
                        |  fence { b = ${text}; }
                        |}""".stripMargin
          val tree = xform(src.asTree[Root])
          val expr = tree getFirst { case StmtAssign(_, rhs) => rhs }
          expr should matchPattern(pattern)
          cc.messages shouldBe empty
        }
      }
    }

    "slice over a slice" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          // format: off
          ("a[8  : 4][1  : 0]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if s.name == "a" && l == 5 && r == 4 => }),
          ("a[9  : 4][1  : 0]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if s.name == "a" && l == 5 && r == 4 => }),
          ("a[8  : 4][2  : 1]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if s.name == "a" && l == 6 && r == 5 => }),
          ("a[9  : 4][2  : 1]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if s.name == "a" && l == 6 && r == 5 => }),
          ("a[8  : 4][1 +: 2]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if s.name == "a" && l == 5 && r == 2 => }),
          ("a[9  : 4][1 +: 2]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if s.name == "a" && l == 5 && r == 2 => }),
          ("a[8  : 4][2 +: 2]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if s.name == "a" && l == 6 && r == 2 => }),
          ("a[9  : 4][2 +: 2]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if s.name == "a" && l == 6 && r == 2 => }),
          ("a[8  : 4][1 -: 2]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if s.name == "a" && l == 5 && r == 2 => }),
          ("a[9  : 4][1 -: 2]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if s.name == "a" && l == 5 && r == 2 => }),
          ("a[8  : 4][2 -: 2]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if s.name == "a" && l == 6 && r == 2 => }),
          ("a[9  : 4][2 -: 2]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if s.name == "a" && l == 6 && r == 2 => }),
          ("a[8 +: 4][1  : 0]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if s.name == "a" && l == 8 && r == 2 => }),
          ("a[9 +: 4][1  : 0]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if s.name == "a" && l == 9 && r == 2 => }),
          ("a[8 +: 4][2  : 1]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if s.name == "a" && l == 9 && r == 2 => }),
          ("a[9 +: 4][2  : 1]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if s.name == "a" && l == 10 && r == 2 => }),
          ("a[8 +: 4][1 +: 2]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if s.name == "a" && l == 9 && r == 2 => }),
          ("a[9 +: 4][1 +: 2]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if s.name == "a" && l == 10 && r == 2 => }),
          ("a[8 +: 4][2 +: 2]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if s.name == "a" && l == 10 && r == 2 => }),
          ("a[9 +: 4][2 +: 2]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if s.name == "a" && l == 11 && r == 2 => }),
          ("a[8 +: 4][1 -: 2]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if s.name == "a" && l == 9 && r == 2 => }),
          ("a[9 +: 4][1 -: 2]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if s.name == "a" && l == 10 && r == 2 => }),
          ("a[8 +: 4][2 -: 2]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if s.name == "a" && l == 10 && r == 2 => }),
          ("a[9 +: 4][2 -: 2]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if s.name == "a" && l == 11 && r == 2 => }),
          ("a[8 -: 4][1  : 0]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if s.name == "a" && l == 6 && r == 2 => }),
          ("a[9 -: 4][1  : 0]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if s.name == "a" && l == 7 && r == 2 => }),
          ("a[8 -: 4][2  : 1]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if s.name == "a" && l == 7 && r == 2 => }),
          ("a[9 -: 4][2  : 1]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if s.name == "a" && l == 8 && r == 2 => }),
          ("a[8 -: 4][1 +: 2]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if s.name == "a" && l == 6 && r == 2 => }),
          ("a[9 -: 4][1 +: 2]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if s.name == "a" && l == 7 && r == 2 => }),
          ("a[8 -: 4][2 +: 2]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if s.name == "a" && l == 7 && r == 2 => }),
          ("a[9 -: 4][2 +: 2]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if s.name == "a" && l == 8 && r == 2 => }),
          ("a[8 -: 4][1 -: 2]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if s.name == "a" && l == 6 && r == 2 => }),
          ("a[9 -: 4][1 -: 2]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if s.name == "a" && l == 7 && r == 2 => }),
          ("a[8 -: 4][2 -: 2]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if s.name == "a" && l == 7 && r == 2 => }),
          ("a[9 -: 4][2 -: 2]", { case ExprSlice(ExprSym(s), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if s.name == "a" && l == 8 && r == 2 => })
          // format: on
        )
      } {
        text in {
          val src = s"""|fsm f {
                        |  in u10 a;
                        |  out u2 b;
                        |  fence { b = ${text}; }
                        |}""".stripMargin
          val tree = xform(src.asTree[Root])
          val expr = tree getFirst { case StmtAssign(_, rhs) => rhs }
          expr should matchPattern(pattern)
          cc.messages shouldBe empty
        }
      }
    }

    "index over $signed/$unsigned" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          ("$signed(a)[3'd0]", {
            case ExprIndex(ExprSym(term), ExprInt(false, 3, v)) if term.name == "a" && v == 0 =>
          }),
          ("$signed(a)[3'd1]", {
            case ExprIndex(ExprSym(term), ExprInt(false, 3, v)) if term.name == "a" && v == 1 =>
          }),
          ("$unsigned(a)[3'd0]", {
            case ExprIndex(ExprSym(term), ExprInt(false, 3, v)) if term.name == "a" && v == 0 =>
          }),
          ("$unsigned(a)[3'd1]", {
            case ExprIndex(ExprSym(term), ExprInt(false, 3, v)) if term.name == "a" && v == 1 =>
          }),
        )
      } {
        val expr = text.trim
        expr in {
          val src = s"""|fsm f {
                        |  in u8 a;
                        |  out u1 b;
                        |  fence { b = ${expr}; }
                        |}""".stripMargin
          val res = (xform(src.asTree[Root]) collectFirst { case StmtAssign(_, rhs) => rhs }).value
          res should matchPattern(pattern)
          cc.messages shouldBe empty
        }
      }
    }

    "slice over $signed/$unsigned" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          ("$signed(a)[3'd1  : 3'd0]", {
            case ExprSlice(ExprSym(term), ExprInt(false, 3, l), ":", ExprInt(false, 3, r))
                if term.name == "a" && l == 1 && r == 0 =>
          }),
          ("$signed(a)[3'd2  : 3'd1]", {
            case ExprSlice(ExprSym(term), ExprInt(false, 3, l), ":", ExprInt(false, 3, r))
                if term.name == "a" && l == 2 && r == 1 =>
          }),
          ("$signed(a)[3'd0 +: 4'd2]", {
            case ExprSlice(ExprSym(term), ExprInt(false, 3, l), "+:", ExprInt(false, 4, r))
                if term.name == "a" && l == 0 && r == 2 =>
          }),
          ("$signed(a)[3'd1 +: 4'd2]", {
            case ExprSlice(ExprSym(term), ExprInt(false, 3, l), "+:", ExprInt(false, 4, r))
                if term.name == "a" && l == 1 && r == 2 =>
          }),
          ("$signed(a)[3'd2 -: 4'd2]", {
            case ExprSlice(ExprSym(term), ExprInt(false, 3, l), "-:", ExprInt(false, 4, r))
                if term.name == "a" && l == 2 && r == 2 =>
          }),
          ("$signed(a)[3'd1 -: 4'd2]", {
            case ExprSlice(ExprSym(term), ExprInt(false, 3, l), "-:", ExprInt(false, 4, r))
                if term.name == "a" && l == 1 && r == 2 =>
          }),
          ("$unsigned(a)[3'd1  : 3'd0]", {
            case ExprSlice(ExprSym(term), ExprInt(false, 3, l), ":", ExprInt(false, 3, r))
                if term.name == "a" && l == 1 && r == 0 =>
          }),
          ("$unsigned(a)[3'd2  : 3'd1]", {
            case ExprSlice(ExprSym(term), ExprInt(false, 3, l), ":", ExprInt(false, 3, r))
                if term.name == "a" && l == 2 && r == 1 =>
          }),
          ("$unsigned(a)[3'd0 +: 4'd2]", {
            case ExprSlice(ExprSym(term), ExprInt(false, 3, l), "+:", ExprInt(false, 4, r))
                if term.name == "a" && l == 0 && r == 2 =>
          }),
          ("$unsigned(a)[3'd1 +: 4'd2]", {
            case ExprSlice(ExprSym(term), ExprInt(false, 3, l), "+:", ExprInt(false, 4, r))
                if term.name == "a" && l == 1 && r == 2 =>
          }),
          ("$unsigned(a)[3'd2 -: 4'd2]", {
            case ExprSlice(ExprSym(term), ExprInt(false, 3, l), "-:", ExprInt(false, 4, r))
                if term.name == "a" && l == 2 && r == 2 =>
          }),
          ("$unsigned(a)[3'd1 -: 4'd2]", {
            case ExprSlice(ExprSym(term), ExprInt(false, 3, l), "-:", ExprInt(false, 4, r))
                if term.name == "a" && l == 1 && r == 2 =>
          })
        )
      } {
        val expr = text.trim
        expr in {
          val src = s"""|fsm f {
                        |  in u8 a;
                        |  out u2 b;
                        |  fence { b = ${expr}; }
                        |}""".stripMargin
          val res = (xform(src.asTree[Root]) collectFirst { case StmtAssign(_, rhs) => rhs }).value
          res should matchPattern(pattern)
          cc.messages shouldBe empty
        }
      }
    }

    "width 1 slices" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          // format: off
          ("c = a[7:7]", { case ExprIndex(ExprSym(a), ExprInt(false, 3, i)) if a.name == "a" && i == 7 => }),
          ("c = a[6 +: 1]", { case ExprIndex(ExprSym(a), ExprInt(false, 3, i)) if a.name == "a" && i == 6 => }),
          ("c = a[5 +: 1]", { case ExprIndex(ExprSym(a), ExprInt(false, 3, i)) if a.name == "a" && i == 5 => }),
          ("c = a[4 -: 1]", { case ExprIndex(ExprSym(a), ExprInt(false, 3, i)) if a.name == "a" && i == 4 => }),
          ("c = a[3 -: 1]", { case ExprIndex(ExprSym(a), ExprInt(false, 3, i)) if a.name == "a" && i == 3 => }),
          ("d = a[2  : 1]", { case ExprSlice(ExprSym(a), ExprInt(false, 3, l), ":", ExprInt(false, 3, r)) if a.name == "a" && l == 2 && r == 1 => }),
          ("c = a[b +: 1]", { case ExprIndex(ExprSym(a), ExprSym(b)) if a.name == "a" && b.name == "b" => }),
          ("c = a[b -: 1]", { case ExprIndex(ExprSym(a), ExprSym(b)) if a.name == "a" && b.name == "b" => })
          // format: on
        )
      } {
        text in {
          val src = s"""|fsm f {
                        |  in u8 a;
                        |  (* unused *) in u3 b;
                        |  (* unused *) out u1 c;
                        |  (* unused *) out u2 d;
                        |  fence { ${text}; }
                        |}""".stripMargin
          val tree = xform(src.asTree[Root])
          val expr = tree getFirst { case StmtAssign(_, rhs) => rhs }
          expr should matchPattern(pattern)
          cc.messages shouldBe empty
        }
      }
    }

    "full width slices" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          // format: off
          ("c = a[6:0]", { case ExprSym(a) if a.name == "a" => }),
          ("c = a[6-:7]", { case ExprSym(a) if a.name == "a" => }),
          ("c = a[0+:7]", { case ExprSym(a) if a.name == "a" => }),
          ("d = b[0:0]", { case ExprSym(b) if b.name == "b" => })
          // format: on
        )
      } {
        text in {
          val src =
            s"""|fsm f {
                |  (* unused *) in u7 a;
                |  (* unused *) in u1 b;
                |  (* unused *) out u7 c;
                |  (* unused *) out u1 d;
                |  fence { ${text}; }
                |}""".stripMargin
          val tree = xform(src.asTree[Root])
          val expr = tree getFirst { case StmtAssign(_, rhs) => rhs }
          expr should matchPattern(pattern)
          cc.messages shouldBe empty
        }
      }
    }

    "repetitions of count 1" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          // format: off
          ("c = {1{a}}", { case ExprSym(a) if a.name == "a" => }),
          ("c = {1'd1{a}}", { case ExprSym(a) if a.name == "a" => })
          // format: on
        )
      } {
        text in {
          val src =
            s"""|fsm f {
                |  in u8 a;
                |  out u8 c;
                |  fence { ${text}; }
                |}""".stripMargin
          val tree = xform(src.asTree[Root])
          val expr = tree getFirst { case StmtAssign(_, rhs) => rhs }
          expr should matchPattern(pattern)
          cc.messages shouldBe empty
        }
      }
    }

    "concatenation of sized integer literals" - {
      for {
        (text, result, msg) <- List(
          ("{4'd2, 4'd2}", ExprInt(false, 8, 34), ""),
          ("{4'd2, 4'sd2}", ExprInt(false, 8, 34), ""),
          ("{4'sd2, 4'd2}", ExprInt(false, 8, 34), ""),
          ("{4'sd2, 4'sd2}", ExprInt(false, 8, 34), ""),
          ("{1'b1, 2'b11, 3'b111, 4'b1111}", ExprInt(false, 10, 1023), ""),
          ("{-1'sd1, -2'sd1, -3'sd1, -4'sd1}", ExprInt(false, 10, 1023), "")
        )
      } {
        val expr = text.trim
        expr in {
          xform(expr.asTree[Expr]) shouldBe result
          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](msg)
          }
        }
      }
    }

    "repetition of sized integer literals" - {
      for {
        (text, result, msg) <- List(
          ("{4{1'b1}}", ExprInt(false, 4, 15), ""),
          ("{4{1'b0}}", ExprInt(false, 4, 0), ""),
          ("{4{-1'sb1}}", ExprInt(false, 4, 15), ""),
          ("{2{2'b1}}", ExprInt(false, 4, 5), ""),
          ("{2{2'b0}}", ExprInt(false, 4, 0), ""),
          ("{2{-4'sb1}}", ExprInt(false, 8, 255), ""),
          ("{4{2'b10}}", ExprInt(false, 8, 170), "")
        )
      } {
        val expr = text.trim
        expr in {
          xform(expr.asTree[Expr]) shouldBe result
          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](msg)
          }
        }
      }
    }

    "constant index of concatenation" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          ("c = {a,b}[3]", {
            case ExprIndex(ExprSym(b), ExprInt(false, 2, i)) if b.name == "b" && i == 3 =>
          }),
          ("c = {a,b}[4]", {
            case ExprIndex(ExprSym(a), ExprInt(false, 2, i)) if a.name == "a" && i == 0 =>
          }),
          ("d = @sx(28, {a,10'd0})", {
            case ExprCat(
                List(ExprRep(ExprNum(false, rep), ExprIndex(ExprSym(a1), ExprInt(false, 2, msb))),
                     ExprSym(a2),
                     ExprInt(false, 10, i)))
                if rep == 14 && a1.name == "a" && msb == 3 && a2.name == "a" && i == 0 =>
          })
        )
      } {
        text in {
          val src = s"""|fsm f {
                        |  in u4 a;
                        |  (* unused *) in u4 b;
                        |  (* unused *) out u1 c;
                        |  (* unused *) out u28 d;
                        |  fence { ${text}; }
                        |}""".stripMargin
          val tree = xform(src.asTree[Root])
          val expr = tree getFirst { case StmtAssign(_, rhs) => rhs }
          expr should matchPattern(pattern)
          cc.messages shouldBe empty
        }
      }
    }

    "constant index of repetition" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          ("c = {2{a}}[3]", {
            case ExprIndex(ExprSym(a), ExprInt(false, 2, i)) if a.name == "a" && i == 3 =>
          }),
          ("c = {2{a}}[4]", {
            case ExprIndex(ExprSym(a), ExprInt(false, 2, i)) if a.name == "a" && i == 0 =>
          })
        )
      } {
        text in {
          val src = s"""|fsm f {
                        |  in u4 a;
                        |  out u1 c;
                        |  fence { ${text}; }
                        |}""".stripMargin
          val tree = xform(src.asTree[Root])
          val expr = tree getFirst { case StmtAssign(_, rhs) => rhs }
          expr should matchPattern(pattern)
          cc.messages shouldBe empty
        }
      }
    }

    "constant slice of concatenation" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          ("d = {a,b}[2:0]", {
            case ExprSlice(ExprSym(b), ExprInt(false, 2, m), ":", ExprInt(false, 2, l))
                if b.name == "b" && m == 2 && l == 0 =>
          }),
          ("d = {a,b}[6:4]", {
            case ExprSlice(ExprSym(a), ExprInt(false, 2, m), ":", ExprInt(false, 2, l))
                if a.name == "a" && m == 2 && l == 0 =>
          }),
          ("d = {a,b}[4+:3]", {
            case ExprSlice(ExprSym(a), ExprInt(false, 2, m), ":", ExprInt(false, 2, l))
                if a.name == "a" && m == 2 && l == 0 =>
          }),
          ("d = {a,b}[6-:3]", {
            case ExprSlice(ExprSym(a), ExprInt(false, 2, m), ":", ExprInt(false, 2, l))
                if a.name == "a" && m == 2 && l == 0 =>
          }),
          ("d = {a,b}[4:2]", {
            case ExprCat(
                List(ExprIndex(ExprSym(a), ExprInt(false, 2, i)),
                     ExprSlice(ExprSym(b), ExprInt(false, 2, m), ":", ExprInt(false, 2, l))))
                if a.name == "a" && i == 0 && b.name == "b" && m == 3 && l == 2 =>
          }),
          ("e = {a,b,c}[8:2]", {
            case ExprCat(
                List(ExprIndex(ExprSym(a), ExprInt(false, 2, i)),
                     ExprSym(b),
                     ExprSlice(ExprSym(c), ExprInt(false, 2, m), ":", ExprInt(false, 2, l))))
                if a.name == "a" && i == 0 && b.name == "b" && m == 3 && l == 2 && c.name == "c" =>
          })
        )
      } {
        text in {
          val src =
            s"""|fsm f {
                |  in u4 a;
                |  in u4 b;
                |  (* unused *) in u4 c;
                |  (* unused *) out u3 d;
                |  (* unused *) out u7 e;
                |  fence { ${text}; }
                |}""".stripMargin
          val tree = xform(src.asTree[Root])
          val expr = tree getFirst { case StmtAssign(_, rhs) => rhs }
          expr should matchPattern(pattern)
          cc.messages shouldBe empty
        }
      }
    }

    "constant slice of repetition" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          ("c = {2{a}}[2:0]", {
            case ExprSlice(ExprSym(a), ExprInt(false, 2, m), ":", ExprInt(false, 2, l))
                if a.name == "a" && m == 2 && l == 0 =>
          }),
          ("c = {2{a}}[6:4]", {
            case ExprSlice(ExprSym(a), ExprInt(false, 2, m), ":", ExprInt(false, 2, l))
                if a.name == "a" && m == 2 && l == 0 =>
          }),
          ("c = {2{a}}[4:2]", {
            case ExprCat(
                List(ExprIndex(ExprSym(a1), ExprInt(false, 2, i)),
                     ExprSlice(ExprSym(a2), ExprInt(false, 2, m), ":", ExprInt(false, 2, l))))
                if a1.name == "a" && i == 0 && a2.name == "a" && m == 3 && l == 2 =>
          }),
          ("d = {3{a}}[8:2]", {
            case ExprCat(
                List(ExprIndex(ExprSym(a1), ExprInt(false, 2, i)),
                     ExprSym(a2),
                     ExprSlice(ExprSym(a3), ExprInt(false, 2, m), ":", ExprInt(false, 2, l))))
                if a1.name == "a" && i == 0 && a2.name == "a" && a3.name == "a" && m == 3 && l == 2 =>
          }),
          ("e = {4{a}}[12:2]", {
            case ExprCat(
                List(ExprIndex(ExprSym(a1), ExprInt(false, 2, i)),
                     ExprRep(ExprNum(false, r), ExprSym(a2)),
                     ExprSlice(ExprSym(a3), ExprInt(false, 2, m), ":", ExprInt(false, 2, l))))
                if a1.name == "a" && i == 0 && r == 2 && a2.name == "a" && a3.name == "a" && m == 3 && l == 2 =>
          }),
          ("e = {4{a}}[11:1]", {
            case ExprCat(
                List(ExprRep(ExprNum(false, r), ExprSym(a2)),
                     ExprSlice(ExprSym(a3), ExprInt(false, 2, m), ":", ExprInt(false, 2, l))))
                if r == 2 && a2.name == "a" && a3.name == "a" && m == 3 && l == 1 =>
          }),
          ("e = {4{a}}[14:4]", {
            case ExprCat(
                List(ExprSlice(ExprSym(a1), ExprInt(false, 2, m), ":", ExprInt(false, 2, l)),
                     ExprRep(ExprNum(false, r), ExprSym(a2))))
                if a1.name == "a" && m == 2 && l == 0 && r == 2 && a2.name == "a" =>
          })
        )
      } {
        text in {
          val src = s"""|fsm f {
                        |  in u4 a;
                        |  (* unused *) out u3 c;
                        |  (* unused *) out u7 d;
                        |  (* unused *) out u11 e;
                        |  fence { ${text}; }
                        |}""".stripMargin
          val tree = xform(src.asTree[Root])
          val expr = tree getFirst { case StmtAssign(_, rhs) => rhs }
          expr should matchPattern(pattern)
          cc.messages shouldBe empty
        }
      }
    }

    "builtin functions" - {
      "@max" - {
        for {
          (expr, result, msg) <- List(
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
            xform(e.asTree[Expr]) shouldBe result
            if (msg.isEmpty) {
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }
        }
      }

      // TODO: @ex

      // TODO: @msb

      "@zx" - {
        for {
          (expr, result, msg) <- List(
            // format: off
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
            ("@zx(1, 2'b00)", ExprError(), "Result width 1 of extension is less than argument width 2"),
            ("@zx(1, 2'b01)", ExprError(), "Result width 1 of extension is less than argument width 2"),
            ("@zx(1, 2'b10)", ExprError(), "Result width 1 of extension is less than argument width 2"),
            ("@zx(1, 2'b11)", ExprError(), "Result width 1 of extension is less than argument width 2"),
            ("@zx(1, 2'sb00)", ExprError(), "Result width 1 of extension is less than argument width 2"),
            ("@zx(1, 2'sb01)", ExprError(), "Result width 1 of extension is less than argument width 2"),
            ("@zx(1, 2'sb10)", ExprError(), "Result width 1 of extension is less than argument width 2"),
            ("@zx(1, 2'sb11)", ExprError(), "Result width 1 of extension is less than argument width 2")
            // format: on
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
            // format: off
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
            ("@sx(1, 2'b00)", ExprError(), "Result width 1 of extension is less than argument width 2"),
            ("@sx(1, 2'b01)", ExprError(), "Result width 1 of extension is less than argument width 2"),
            ("@sx(1, 2'b10)", ExprError(), "Result width 1 of extension is less than argument width 2"),
            ("@sx(1, 2'b11)", ExprError(), "Result width 1 of extension is less than argument width 2"),
            ("@sx(1, 2'sb00)", ExprError(), "Result width 1 of extension is less than argument width 2"),
            ("@sx(1, 2'sb01)", ExprError(), "Result width 1 of extension is less than argument width 2"),
            ("@sx(1, 2'sb10)", ExprError(), "Result width 1 of extension is less than argument width 2"),
            ("@sx(1, 2'sb11)", ExprError(), "Result width 1 of extension is less than argument width 2")
            // format: on
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
          (text, result, msg) <- List(
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
          text in {
            val tree = s"""|struct b_t {
                           |  u2 f0;
                           |}
                           |
                           |struct a_t {
                           |  bool f0;
                           |  i7   f1;
                           |  b_t  f2;
                           |}
                           |
                           |fsm x {
                           |  a_t a;
                           |  fence {
                           |    $$display("", ${text});
                           |  }
                           |}""".stripMargin.asTree[Root]
            val expr = xform(tree) getFirst { case ExprCall(_, List(_, e)) => e }
            val errors = cc.messages filter { _.isInstanceOf[Error] }
            if (msg.isEmpty) {
              expr shouldBe result
              errors shouldBe empty
            } else {
              errors.loneElement should beThe[Error](Pattern.quote(msg))
            }
          }
        }
      }
    }

    "reference to constant" in {
      val entity = """|fsm a {
                      |  const u36 A = {{24{1'b0}}, {12{1'b1}}};
                      |  const u41 B = {5'h1, A[35:0]};
                      |
                      |  out u41 o;
                      |
                      |  void main() {
                      |    o = B;
                      |    fence;
                      |  }
                      |}""".stripMargin.asTree[Entity]

      val expr = xform(entity) collectFirst {
        case StmtAssign(_, rhs) => rhs
      }

      expr.value.simplify shouldBe ExprInt(false, 41, 0x1000000fffL)
    }

    "cast" - {
      for {
        (exprSrc, kindSrc, pattern, err) <- List[(String,
                                                  String,
                                                  PartialFunction[Any, Unit],
                                                  String)](
          // format: off
          ("32", "u8", { case ExprInt(false, 8, v) if v == 32 => }, ""),
          ("32s", "i8", { case ExprInt(true, 8, v) if v == 32=> }, ""),
          ("-1s", "i8", { case ExprInt(true, 8, v) if v == -1 => }, ""),
          ("32", "u4", { case _ => }, "Value 32 cannot be represented with 4 unsigned bits"),
          ("32s", "i4", { case _ => }, "Value 32 cannot be represented with 4 signed bits"),
          ("31", "u5", { case ExprInt(false, 5, v) if v == 31 => }, ""),
          ("31s", "i5", { case ExprError() => }, "Value 31 cannot be represented with 5 signed bits"),
          ("10'sd12",  "int", { case ExprNum(true, v) if v == 12 => }, ""),
          ("10'd12",  "uint", { case ExprNum(false, v) if v == 12 => }, ""),
          ("-10'sd1",  "int", { case ExprNum(true, v) if v == -1 => }, ""),
          ("a",  "u10", { case ExprCat(List(ExprInt(false, 2, z), ExprSym(a))) if z == 0 && a.name == "a" => }, ""),
          ("b",  "i10", { case ExprCall(
                                ExprSym(s),
                                List(ExprCat(List(
                                      ExprRep(
                                        Expr(3),
                                        ExprIndex(
                                          ExprSym(b0),
                                          ExprInt(false, 3, i))),
                                      ExprSym(b1))))) if s.name == "$signed" && b0.name == "b" && i == 6 && b1.name == "b" =>
          }, ""),
          ("a",  "u8", { case ExprSym(a) if a.name == "a" => }, ""),
          ("b",  "i7", { case ExprSym(b) if b.name == "b" => }, ""),
          ("c",  "u10", { case ExprInt(false, 10, v) if v == 7 => }, ""),
          ("c", "uint", { case ExprNum(false, v) if v == 7 => }, ""),
          ("d",  "i10", { case ExprInt(true, 10, v) if v == -3 => }, ""),
          ("d",  "int", { case ExprNum(true, v) if v == -3 => }, "")
          // format: on
        )
      } {
        s"(${kindSrc})(${exprSrc})" in {
          val tree = s"""|fsm f {
                         |  (* unused *) u8 a;
                         |  (* unused *) i7 b;
                         |  (* unused *) const u8 c =  7;
                         |  (* unused *) const i8 d = -3s;
                         |  void main() {
                         |    $$display("", ${exprSrc});
                         |    fence;
                         |  }
                         |}""".stripMargin.asTree[Entity]
          val expr = xform(tree) getFirst { case ExprCall(_, List(_, e)) => e }
          val kind = kindSrc.asTree[Expr] match {
            case ExprType(kind) => kind
            case _              => fail()
          }
          cc.messages shouldBe empty
          val result = xform(ExprCast(kind, expr) withLoc Loc.synthetic)
          if (err.isEmpty) {
            result should matchPattern(pattern)
            cc.messages shouldBe empty
          } else {
            result shouldBe ExprError()
            cc.messages.loneElement should beThe[Error](err)
          }
        }
      }
    }
  }
}
