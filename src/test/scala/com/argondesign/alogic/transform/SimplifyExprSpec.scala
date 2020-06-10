////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018-2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// SimplifyExpr tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.transform

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.passes._
import com.argondesign.alogic.typer.Typer
import org.scalatest.freespec.AnyFreeSpec

final class SimplifyExprSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  protected def fold(text: String): Thicket = Thicket {
    transformWithPass(
      Namer andThen
        Elaborate andThen
        TypeCheck andThen
        ResolvePolyFunc andThen
        AddCasts andThen
        Fold,
      text
    ).value flatMap {
      case (decl, defn) =>
        List(decl, defn)
    }
  }

  protected def simplify(text: String): Expr =
    (text.asTree[Expr] rewrite new Namer rewrite new Typer).simplify

  "FoldExpr should fold" - {
    "unary operators applied to unsized integer literals" - {
      for {
        (text, result, err) <- List(
          // format: off
          // signed positive operand
          ("+(2s)", ExprNum(true, 2), Nil),
          ("-(2s)", ExprNum(true, -2), Nil),
          ("~(2s)", ExprNum(true, -3), Nil),
          ("!(2s)", ExprInt(false, 1, 0), Nil),
          // signed negative operand
          ("+(-2s)", ExprNum(true, -2), Nil),
          ("-(-2s)", ExprNum(true, 2), Nil),
          ("~(-2s)", ExprNum(true, 1), Nil),
          ("!(-2s)", ExprInt(false, 1, 0), Nil),
          // signed 0 operand
          ("+(0s)", ExprNum(true, 0), Nil),
          ("-(0s)", ExprNum(true, 0), Nil),
          ("~(0s)", ExprNum(true, -1), Nil),
          ("!(0s)", ExprInt(false, 1, 1), Nil),
          // signed -1 operand
          ("+(-1s)", ExprNum(true, -1), Nil),
          ("-(-1s)", ExprNum(true, 1), Nil),
          ("~(-1s)", ExprNum(true, 0), Nil),
          ("!(-1s)", ExprInt(false, 1, 0), Nil),
          // unsigned non-0 operand
          ("+(2)", ExprNum(false, 2), Nil),
          ("-(2)", ExprError(), "Unary '-' is not well defined for unsigned values" :: Nil),
          ("~(2)", ExprError(), "Unary '~' is not well defined for unsized unsigned values" :: Nil),
          ("!(2)", ExprInt(false, 1, 0), Nil),
          // unsigned 0 operand
          ("+(0)", ExprNum(false, 0), Nil),
          ("-(0)", ExprNum(false, 0), Nil),
          ("~(0)", ExprError(), "Unary '~' is not well defined for unsized unsigned values" :: Nil),
          ("!(0)", ExprInt(false, 1, 1), Nil)
          // format: on
        )
      } {
        text in {
          simplify(text) shouldBe result
          checkSingleError(err)
        }
      }
    }

    "binary operators applied to unsized integer literals" - {
      for {
        (text, result, err) <- List(
          // format: off
          //////////////////////////////////////////////
          // signed signed
          //////////////////////////////////////////////
          // Always valid
          ("3s >  2s", ExprInt(false, 1, 1), Nil),
          ("3s >= 2s", ExprInt(false, 1, 1), Nil),
          ("3s <  2s", ExprInt(false, 1, 0), Nil),
          ("3s <= 2s", ExprInt(false, 1, 0), Nil),
          ("3s == 2s", ExprInt(false, 1, 0), Nil),
          ("3s != 2s", ExprInt(false, 1, 1), Nil),
          ("3s && 2s", ExprInt(false, 1, 1), Nil),
          ("3s || 2s", ExprInt(false, 1, 1), Nil),
          // Arith
          ("3s * 2s", ExprNum(true, 6), Nil),
          ("3s / 2s", ExprNum(true, 1), Nil),
          ("3s % 2s", ExprNum(true, 1), Nil),
          ("3s + 2s", ExprNum(true, 5), Nil),
          ("3s - 2s", ExprNum(true, 1), Nil),
          // Shifts
          (" 3s <<   2s", ExprNum(true, 12), Nil),
          (" 3s >>   2s", ExprNum(true, 0), Nil),
          (" 3s <<<  2s", ExprNum(true, 12), Nil),
          (" 3s >>>  2s", ExprNum(true, 0), Nil),
          (" 3s <<  -2s", ExprError(), "Negative shift amount" :: Nil),
          (" 3s >>  -2s", ExprError(), "Negative shift amount" :: Nil),
          (" 3s <<< -2s", ExprError(), "Negative shift amount" :: Nil),
          (" 3s >>> -2s", ExprError(), "Negative shift amount" :: Nil),
          ("-3s <<   2s", ExprNum(true, -12), Nil),
          ("-3s >>   2s", ExprError(), "'>>' is not well defined for negative unsized values" :: Nil),
          ("-3s <<<  2s", ExprNum(true, -12), Nil),
          ("-3s >>>  2s", ExprNum(true, -1), Nil),
          ("-3s <<  -2s", ExprError(), "Negative shift amount" :: Nil),
          ("-3s >>  -2s", ExprError(), "Negative shift amount" :: Nil),
          ("-3s <<< -2s", ExprError(), "Negative shift amount" :: Nil),
          ("-3s >>> -2s", ExprError(), "Negative shift amount" :: Nil),
          // Bitwise
          (" 3s &  2s", ExprNum(true, 2), Nil),
          (" 3s ^  2s", ExprNum(true, 1), Nil),
          (" 3s |  2s", ExprNum(true, 3), Nil),
          (" 3s & -2s", ExprError(), "Bitwise '&' operator is not well defined for negative unsized values" :: Nil),
          (" 3s ^ -2s", ExprError(), "Bitwise '^' operator is not well defined for negative unsized values" :: Nil),
          (" 3s | -2s", ExprError(), "Bitwise '|' operator is not well defined for negative unsized values" :: Nil),
          ("-3s &  2s", ExprError(), "Bitwise '&' operator is not well defined for negative unsized values" :: Nil),
          ("-3s ^  2s", ExprError(), "Bitwise '^' operator is not well defined for negative unsized values" :: Nil),
          ("-3s |  2s", ExprError(), "Bitwise '|' operator is not well defined for negative unsized values" :: Nil),
          ("-3s & -2s", ExprError(), "Bitwise '&' operator is not well defined for negative unsized values" :: Nil),
          ("-3s ^ -2s", ExprError(), "Bitwise '^' operator is not well defined for negative unsized values" :: Nil),
          ("-3s | -2s", ExprError(), "Bitwise '|' operator is not well defined for negative unsized values" :: Nil),
          //////////////////////////////////////////////
          // signed unsigned
          //////////////////////////////////////////////
          // Always valid
          ("3s >  2", ExprInt(false, 1, 1), Nil),
          ("3s >= 2", ExprInt(false, 1, 1), Nil),
          ("3s <  2", ExprInt(false, 1, 0), Nil),
          ("3s <= 2", ExprInt(false, 1, 0), Nil),
          ("3s == 2", ExprInt(false, 1, 0), Nil),
          ("3s != 2", ExprInt(false, 1, 1), Nil),
          ("3s && 2", ExprInt(false, 1, 1), Nil),
          ("3s || 2", ExprInt(false, 1, 1), Nil),
          // Arith
          (" 3s * 2", ExprNum(false, 6), Nil),
          (" 3s / 2", ExprNum(false, 1), Nil),
          (" 3s % 2", ExprNum(false, 1), Nil),
          (" 3s + 2", ExprNum(false, 5), Nil),
          (" 3s - 2", ExprNum(false, 1), Nil),
          (" 3s - 4", ExprError(), "Result of operator '-' is unsigned, but value is negative: -1" :: Nil),
          ("-3s * 2", ExprError(), "Result of operator '*' is unsigned, but value is negative: -6" :: Nil),
          ("-3s / 2", ExprError(), "Result of operator '/' is unsigned, but value is negative: -1" :: Nil),
          ("-3s % 2", ExprError(), "Result of operator '%' is unsigned, but value is negative: -1" :: Nil),
          ("-3s + 2", ExprError(), "Result of operator '+' is unsigned, but value is negative: -1" :: Nil),
          ("-3s - 2", ExprError(), "Result of operator '-' is unsigned, but value is negative: -5" :: Nil),
          ("-3s + 4", ExprNum(false, 1), Nil),
          // Shifts
          (" 3s <<  2", ExprNum(true, 12), Nil),
          (" 3s >>  2", ExprNum(true, 0), Nil),
          (" 3s <<< 2", ExprNum(true, 12), Nil),
          (" 3s >>> 2", ExprNum(true, 0), Nil),
          ("-3s <<  2", ExprNum(true, -12), Nil),
          ("-3s >>  2", ExprError(), "'>>' is not well defined for negative unsized values" :: Nil),
          ("-3s <<< 2", ExprNum(true, -12), Nil),
          ("-3s >>> 2", ExprNum(true, -1), Nil),
          // Bitwise
          (" 3s & 2", ExprNum(false, 2), Nil),
          (" 3s ^ 2", ExprNum(false, 1), Nil),
          (" 3s | 2", ExprNum(false, 3), Nil),
          ("-3s & 2", ExprError(), "Bitwise '&' operator is not well defined for negative unsized values" :: Nil),
          ("-3s ^ 2", ExprError(), "Bitwise '^' operator is not well defined for negative unsized values" :: Nil),
          ("-3s | 2", ExprError(), "Bitwise '|' operator is not well defined for negative unsized values" :: Nil),
          //////////////////////////////////////////////
          // unsigned signed
          //////////////////////////////////////////////
          // Always valid
          ("3 >  2s", ExprInt(false, 1, 1), Nil),
          ("3 >= 2s", ExprInt(false, 1, 1), Nil),
          ("3 <  2s", ExprInt(false, 1, 0), Nil),
          ("3 <= 2s", ExprInt(false, 1, 0), Nil),
          ("3 == 2s", ExprInt(false, 1, 0), Nil),
          ("3 != 2s", ExprInt(false, 1, 1), Nil),
          ("3 && 2s", ExprInt(false, 1, 1), Nil),
          ("3 || 2s", ExprInt(false, 1, 1), Nil),
          // Arith
          ("3 *  2s", ExprNum(false, 6), Nil),
          ("3 /  2s", ExprNum(false, 1), Nil),
          ("3 %  2s", ExprNum(false, 1), Nil),
          ("3 +  2s", ExprNum(false, 5), Nil),
          ("3 -  2s", ExprNum(false, 1), Nil),
          ("3 -  4s", ExprError(), "Result of operator '-' is unsigned, but value is negative: -1" :: Nil),
          ("3 * -2s", ExprError(), "Result of operator '*' is unsigned, but value is negative: -6" :: Nil),
          ("3 / -2s", ExprError(), "Result of operator '/' is unsigned, but value is negative: -1" :: Nil),
          ("3 % -2s", ExprNum(false, 1), Nil),
          ("3 + -2s", ExprNum(false, 1), Nil),
          ("3 - -2s", ExprNum(false, 5), Nil),
          ("3 + -4s", ExprError(), "Result of operator '+' is unsigned, but value is negative: -1" :: Nil),
          // Shifts
          ("3 <<   2s", ExprNum(false, 12), Nil),
          ("3 >>   2s", ExprNum(false, 0), Nil),
          ("3 <<<  2s", ExprNum(false, 12), Nil),
          ("3 >>>  2s", ExprNum(false, 0), Nil),
          ("3 <<  -2s", ExprError(), "Negative shift amount" :: Nil),
          ("3 >>  -2s", ExprError(), "Negative shift amount" :: Nil),
          ("3 <<< -2s", ExprError(), "Negative shift amount" :: Nil),
          ("3 >>> -2s", ExprError(), "Negative shift amount" :: Nil),
          // Bitwise
          ("3 &  2s", ExprNum(false, 2), Nil),
          ("3 ^  2s", ExprNum(false, 1), Nil),
          ("3 |  2s", ExprNum(false, 3), Nil),
          ("3 & -2s", ExprError(), "Bitwise '&' operator is not well defined for negative unsized values" :: Nil),
          ("3 ^ -2s", ExprError(), "Bitwise '^' operator is not well defined for negative unsized values" :: Nil),
          ("3 | -2s", ExprError(), "Bitwise '|' operator is not well defined for negative unsized values" :: Nil),
          //////////////////////////////////////////////
          // unsigned unsigned
          //////////////////////////////////////////////
          // Always valid
          ("3 >  2", ExprInt(false, 1, 1), Nil),
          ("3 >= 2", ExprInt(false, 1, 1), Nil),
          ("3 <  2", ExprInt(false, 1, 0), Nil),
          ("3 <= 2", ExprInt(false, 1, 0), Nil),
          ("3 == 2", ExprInt(false, 1, 0), Nil),
          ("3 != 2", ExprInt(false, 1, 1), Nil),
          ("3 && 2", ExprInt(false, 1, 1), Nil),
          ("3 || 2", ExprInt(false, 1, 1), Nil),
          // Arith
          ("3 * 2", ExprNum(false, 6), Nil),
          ("3 / 2", ExprNum(false, 1), Nil),
          ("3 % 2", ExprNum(false, 1), Nil),
          ("3 + 2", ExprNum(false, 5), Nil),
          ("3 - 2", ExprNum(false, 1), Nil),
          ("3 - 4", ExprError(), "Result of operator '-' is unsigned, but value is negative: -1" :: Nil),
          // Shifts
          ("3 <<  2", ExprNum(false, 12), Nil),
          ("3 >>  2", ExprNum(false, 0), Nil),
          ("3 <<< 2", ExprNum(false, 12), Nil),
          ("3 >>> 2", ExprNum(false, 0), Nil),
          // Bitwise
          ("3 &  2", ExprNum(false, 2), Nil),
          ("3 ^  2", ExprNum(false, 1), Nil),
          ("3 |  2", ExprNum(false, 3), Nil)
          // format: on
        )
      } {
        text in {
          simplify(text) shouldBe result
          checkSingleError(err)
        }
      }
    }

    "shifts with an unsized left hand side and a sized right hand side" - {
      for {
        (text, result, err) <- List(
          // format: off
          //////////////////////////////////////////////
          // signed signed
          //////////////////////////////////////////////
          (" 3s <<   8'sd2", ExprNum(true, 12), Nil),
          (" 3s >>   8'sd2", ExprNum(true, 0), Nil),
          (" 3s <<<  8'sd2", ExprNum(true, 12), Nil),
          (" 3s >>>  8'sd2", ExprNum(true, 0), Nil),
          (" 3s <<  -8'sd2", ExprError(), "Negative shift amount" :: Nil),
          (" 3s >>  -8'sd2", ExprError(), "Negative shift amount" :: Nil),
          (" 3s <<< -8'sd2", ExprError(), "Negative shift amount" :: Nil),
          (" 3s >>> -8'sd2", ExprError(), "Negative shift amount" :: Nil),
          ("-3s <<   8'sd2", ExprNum(true, -12), Nil),
          ("-3s >>   8'sd2", ExprError(), "'>>' is not well defined for negative unsized values" :: Nil),
          ("-3s <<<  8'sd2", ExprNum(true, -12), Nil),
          ("-3s >>>  8'sd2", ExprNum(true, -1), Nil),
          ("-3s <<  -8'sd2", ExprError(), "Negative shift amount" :: Nil),
          ("-3s >>  -8'sd2", ExprError(), "Negative shift amount" :: Nil),
          ("-3s <<< -8'sd2", ExprError(), "Negative shift amount" :: Nil),
          ("-3s >>> -8'sd2", ExprError(), "Negative shift amount" :: Nil),
          //////////////////////////////////////////////
          // signed unsigned
          //////////////////////////////////////////////
          (" 3s <<  8'd2", ExprNum(true, 12), Nil),
          (" 3s >>  8'd2", ExprNum(true, 0), Nil),
          (" 3s <<< 8'd2", ExprNum(true, 12), Nil),
          (" 3s >>> 8'd2", ExprNum(true, 0), Nil),
          ("-3s <<  8'd2", ExprNum(true, -12), Nil),
          ("-3s >>  8'd2", ExprError(), "'>>' is not well defined for negative unsized values" :: Nil),
          ("-3s <<< 8'd2", ExprNum(true, -12), Nil),
          ("-3s >>> 8'd2", ExprNum(true, -1), Nil),
          //////////////////////////////////////////////
          // unsigned signed
          //////////////////////////////////////////////
          ("3 <<   8'sd2", ExprNum(false, 12), Nil),
          ("3 >>   8'sd2", ExprNum(false, 0), Nil),
          ("3 <<<  8'sd2", ExprNum(false, 12), Nil),
          ("3 >>>  8'sd2", ExprNum(false, 0), Nil),
          ("3 <<  -8'sd2", ExprError(), "Negative shift amount" :: Nil),
          ("3 >>  -8'sd2", ExprError(), "Negative shift amount" :: Nil),
          ("3 <<< -8'sd2", ExprError(), "Negative shift amount" :: Nil),
          ("3 >>> -8'sd2", ExprError(), "Negative shift amount" :: Nil),
          //////////////////////////////////////////////
          // unsigned unsigned
          //////////////////////////////////////////////
          ("3 <<  8'd2", ExprNum(false, 12), Nil),
          ("3 >>  8'd2", ExprNum(false, 0), Nil),
          ("3 <<< 8'd2", ExprNum(false, 12), Nil),
          ("3 >>> 8'd2", ExprNum(false, 0), Nil)
          // format: on
        )
      } {
        text in {
          simplify(text) shouldBe result
          checkSingleError(err)
        }
      }
    }

    "unary operators applied to sized integer literals" - {
      for {
        (text, result, err) <- List(
          // format: off
          // signed positive operand
          ("+(8'sd2)", ExprInt(true, 8, 2), Nil),
          ("-(8'sd2)", ExprInt(true, 8, -2), Nil),
          ("~(8'sd2)", ExprInt(true, 8, -3), Nil),
          ("!(8'sd2)", ExprInt(false, 1, 0), Nil),
          ("&(8'sd2)", ExprInt(false, 1, 0), Nil),
          ("|(8'sd2)", ExprInt(false, 1, 1), Nil),
          ("^(8'sd2)", ExprInt(false, 1, 1), Nil),
          // signed negative operand
          ("+(-8'sd2)", ExprInt(true, 8, -2), Nil),
          ("-(-8'sd2)", ExprInt(true, 8, 2), Nil),
          ("~(-8'sd2)", ExprInt(true, 8, 1), Nil),
          ("!(-8'sd2)", ExprInt(false, 1, 0), Nil),
          ("&(-8'sd2)", ExprInt(false, 1, 0), Nil),
          ("|(-8'sd2)", ExprInt(false, 1, 1), Nil),
          ("^(-8'sd2)", ExprInt(false, 1, 1), Nil),
          // signed 0 operand
          ("+(8'sd0)", ExprInt(true, 8, 0), Nil),
          ("-(8'sd0)", ExprInt(true, 8, 0), Nil),
          ("~(8'sd0)", ExprInt(true, 8, -1), Nil),
          ("!(8'sd0)", ExprInt(false, 1, 1), Nil),
          ("&(8'sd0)", ExprInt(false, 1, 0), Nil),
          ("|(8'sd0)", ExprInt(false, 1, 0), Nil),
          ("^(8'sd0)", ExprInt(false, 1, 0), Nil),
          // signed -1 operand
          ("+(-8'sd1)", ExprInt(true, 8, -1), Nil),
          ("-(-8'sd1)", ExprInt(true, 8, 1), Nil),
          ("~(-8'sd1)", ExprInt(true, 8, 0), Nil),
          ("!(-8'sd1)", ExprInt(false, 1, 0), Nil),
          ("&(-8'sd1)", ExprInt(false, 1, 1), Nil),
          ("|(-8'sd1)", ExprInt(false, 1, 1), Nil),
          ("^(-8'sd1)", ExprInt(false, 1, 0), Nil),
          // unsigned non-0 operand
          ("+(8'd2)", ExprInt(false, 8, 2), Nil),
          ("-(8'd2)", ExprError(), "Unary '-' is not well defined for unsigned values" :: Nil),
          ("~(8'd2)", ExprInt(false, 8, 253), Nil),
          ("!(8'd2)", ExprInt(false, 1, 0), Nil),
          ("&(8'd2)", ExprInt(false, 1, 0), Nil),
          ("|(8'd2)", ExprInt(false, 1, 1), Nil),
          ("^(8'd2)", ExprInt(false, 1, 1), Nil),
          // unsigned 0 operand
          ("+(8'd0)", ExprInt(false, 8, 0), Nil),
          ("-(8'd0)", ExprInt(false, 8, 0), Nil),
          ("~(8'd0)", ExprInt(false, 8, 255), Nil),
          ("!(8'd0)", ExprInt(false, 1, 1), Nil),
          ("&(8'd0)", ExprInt(false, 1, 0), Nil),
          ("|(8'd0)", ExprInt(false, 1, 0), Nil),
          ("^(8'd0)", ExprInt(false, 1, 0), Nil),
          // reductions ff
          ("&(8'hff)", ExprInt(false, 1, 1), Nil),
          ("|(8'hff)", ExprInt(false, 1, 1), Nil),
          ("^(8'hff)", ExprInt(false, 1, 0), Nil),
          // reductions 0
          ("&(8'h0)", ExprInt(false, 1, 0), Nil),
          ("|(8'h0)", ExprInt(false, 1, 0), Nil),
          ("^(8'h0)", ExprInt(false, 1, 0), Nil)
          // format: on
        )
      } {
        text in {
          simplify(text) shouldBe result
          checkSingleError(err)
        }
      }
    }

    "binary operators applied to equally sized integer literals" - {
      for {
        (text, result) <- List(
          // format: off
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
          // format: on
        )
      } {
        text in {
          simplify(text) shouldBe result
          checkSingleError(Nil)
        }
      }
    }

    "ternary operator" - {
      for {
        (text, pattern, err) <- List[(String, PartialFunction[Any, Unit], List[String])](
          // format: off
          ("0 ? 1 : 2", { case ExprNum(false, v) if v == 2                                      => }, Nil),
          ("1 ? 1 : 2", { case ExprNum(false, v) if v == 1                                      => }, Nil),
          ("@randbit() ? 1 : 1", { case ExprNum(false, v) if v == 1                             => }, Nil),
          ("@randbit() ? 2 : 2", { case ExprNum(false, v) if v == 2                             => }, Nil),
          ("@randbit() ? 8'd0 : 8'd0", { case ExprInt(false, 8, v) if v == 0                    => }, Nil),
          ("@randbit() ? 8'd0 : 8'd1", { case ExprTernary(_: ExprCall, _: ExprInt, _: ExprInt)  => }, Nil),
          ("@randbit() ? 8'd0 : 8'sd0", { case ExprTernary(_: ExprCall, _: ExprInt, _: ExprInt) => }, Nil),
          ("1 ? 1 : 0 - 2", { case ExprNum(false, v) if v == 1                                  => }, Nil),
          ("1 ? 1s : 0", { case ExprNum(false, v) if v == 1                                     => }, Nil),
          ("@randbit() ? 1 - 1 : 2 - 2", { case ExprNum(false, v) if v == 0                     => }, Nil)
          // format: on
        )
      } {
        text in {
          simplify(text) should matchPattern(pattern)
          checkSingleError(err)
        }
      }
    }

    "binary operators with one side known" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          // format: off
          // Arithmetic *
          ("8'd0   * x_u8",    { case ExprInt(false, 8, v) if v == 0 => }),
          ("8'sd0  * x_u8",    { case ExprInt(false, 8, v) if v == 0 => }),
          ("8'd0   * x_i8",    { case ExprInt(false, 8, v) if v == 0 => }),
          ("8'sd0  * x_i8",    { case ExprInt(true,  8, v) if v == 0 => }),
          ("x_u8   * 8'd0",    { case ExprInt(false, 8, v) if v == 0 => }),
          ("x_u8   * 8'sd0",   { case ExprInt(false, 8, v) if v == 0 => }),
          ("x_i8   * 8'd0",    { case ExprInt(false, 8, v) if v == 0 => }),
          ("x_i8   * 8'sd0",   { case ExprInt(true,  8, v) if v == 0 => }),
          ("8'd1   * x_u8",    { case ExprSym(Symbol("x_u8")) => }),
          ("8'sd1  * x_u8",    { case ExprSym(Symbol("x_u8")) => }),
          ("8'd1   * x_i8",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("x_i8"))) :: Nil) => }),
          ("8'sd1  * x_i8",    { case ExprSym(Symbol("x_i8")) => }),
          ("x_u8   * 8'd1",    { case ExprSym(Symbol("x_u8")) => }),
          ("x_u8   * 8'sd1",   { case ExprSym(Symbol("x_u8")) => }),
          ("x_i8   * 8'd1",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("x_i8"))) :: Nil) => }),
          ("x_i8   * 8'sd1",   { case ExprSym(Symbol("x_i8")) => }),
          // Arithmetic /
          ("8'd0   / x_u8",    { case ExprInt(false, 8, v) if v == 0 => }),
          ("8'sd0  / x_u8",    { case ExprInt(false, 8, v) if v == 0 => }),
          ("8'd0   / x_i8",    { case ExprInt(false, 8, v) if v == 0 => }),
          ("8'sd0  / x_i8",    { case ExprInt(true,  8, v) if v == 0 => }),
          ("x_u8   / 8'd1",    { case ExprSym(Symbol("x_u8")) => }),
          ("x_u8   / 8'sd1",   { case ExprSym(Symbol("x_u8")) => }),
          ("x_i8   / 8'd1",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("x_i8"))) :: Nil) => }),
          ("x_i8   / 8'sd1",   { case ExprSym(Symbol("x_i8")) => }),
          // Arithmetic /
          ("8'd0   % x_u8",    { case ExprInt(false, 8, v) if v == 0 => }),
          ("8'sd0  % x_u8",    { case ExprInt(false, 8, v) if v == 0 => }),
          ("8'd0   % x_i8",    { case ExprInt(false, 8, v) if v == 0 => }),
          ("8'sd0  % x_i8",    { case ExprInt(true,  8, v) if v == 0 => }),
          ("x_u8   % 8'd1",    { case ExprInt(false, 8, v) if v == 0 => }),
          ("x_u8   % 8'sd1",   { case ExprInt(false, 8, v) if v == 0 => }),
          ("x_i8   % 8'd1",    { case ExprInt(false, 8, v) if v == 0 => }),
          ("x_i8   % 8'sd1",   { case ExprInt(true,  8, v) if v == 0 => }),
          // Arithmetic +
          ("8'd0   + x_u8",    { case ExprSym(Symbol("x_u8")) => }),
          ("8'sd0  + x_u8",    { case ExprSym(Symbol("x_u8")) => }),
          ("8'd0   + x_i8",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("x_i8"))) :: Nil) => }),
          ("8'sd0  + x_i8",    { case ExprSym(Symbol("x_i8")) => }),
          ("x_u8   + 8'd0",    { case ExprSym(Symbol("x_u8")) => }),
          ("x_u8   + 8'sd0",   { case ExprSym(Symbol("x_u8")) => }),
          ("x_i8   + 8'd0",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("x_i8"))) :: Nil) => }),
          ("x_i8   + 8'sd0",   { case ExprSym(Symbol("x_i8")) => }),
          // Arithmetic -
          ("8'd0   - x_u8",    { case ExprUnary("-", ExprSym(Symbol("x_u8"))) => }),
          ("8'sd0  - x_u8",    { case ExprUnary("-", ExprSym(Symbol("x_u8"))) => }),
          ("8'd0   - x_i8",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprUnary("-", ExprSym(Symbol("x_i8")))) :: Nil) => }),
          ("8'sd0  - x_i8",    { case ExprUnary("-", ExprSym(Symbol("x_i8"))) => }),
          ("x_u8   - 8'd0",    { case ExprSym(Symbol("x_u8")) => }),
          ("x_u8   - 8'sd0",   { case ExprSym(Symbol("x_u8")) => }),
          ("x_i8   - 8'd0",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("x_i8"))) :: Nil) => }),
          ("x_i8   - 8'sd0",   { case ExprSym(Symbol("x_i8")) => }),
          // Shift >>
          ("8'd0   >> x_u8",    { case ExprInt(false, 8, v) if v == 0 => }),
          ("8'sd0  >> x_u8",    { case ExprInt(true,  8, v) if v == 0 => }),
          ("8'd0   >> x_i8",    { case ExprInt(false, 8, v) if v == 0 => }),
          ("8'sd0  >> x_i8",    { case ExprInt(true,  8, v) if v == 0 => }),
          ("x_u8   >> 8'd0",    { case ExprSym(Symbol("x_u8")) => }),
          ("x_u8   >> 8'sd0",   { case ExprSym(Symbol("x_u8")) => }),
          ("x_i8   >> 8'd0",    { case ExprSym(Symbol("x_i8")) => }),
          ("x_i8   >> 8'sd0",   { case ExprSym(Symbol("x_i8")) => }),
          // Shift >>>
          ("8'd0   >>> x_u8",   { case ExprInt(false, 8, v) if v == 0 => }),
          ("8'sd0  >>> x_u8",   { case ExprInt(true,  8, v) if v == 0 => }),
          ("8'd0   >>> x_i8",   { case ExprInt(false, 8, v) if v == 0 => }),
          ("8'sd0  >>> x_i8",   { case ExprInt(true,  8, v) if v == 0 => }),
          ("x_u8   >>> 8'd0",   { case ExprSym(Symbol("x_u8")) => }),
          ("x_u8   >>> 8'sd0",  { case ExprSym(Symbol("x_u8")) => }),
          ("x_i8   >>> 8'd0",   { case ExprSym(Symbol("x_i8")) => }),
          ("x_i8   >>> 8'sd0",  { case ExprSym(Symbol("x_i8")) => }),
          // Shift <<
          ("8'd0   << x_u8",    { case ExprInt(false, 8, v) if v == 0 => }),
          ("8'sd0  << x_u8",    { case ExprInt(true,  8, v) if v == 0 => }),
          ("8'd0   << x_i8",    { case ExprInt(false, 8, v) if v == 0 => }),
          ("8'sd0  << x_i8",    { case ExprInt(true,  8, v) if v == 0 => }),
          ("x_u8   << 8'd0",    { case ExprSym(Symbol("x_u8")) => }),
          ("x_u8   << 8'sd0",   { case ExprSym(Symbol("x_u8")) => }),
          ("x_i8   << 8'd0",    { case ExprSym(Symbol("x_i8")) => }),
          ("x_i8   << 8'sd0",   { case ExprSym(Symbol("x_i8")) => }),
          // Shift <<<
          ("8'd0   <<< x_u8",   { case ExprInt(false, 8, v) if v == 0 => }),
          ("8'sd0  <<< x_u8",   { case ExprInt(true,  8, v) if v == 0 => }),
          ("8'd0   <<< x_i8",   { case ExprInt(false, 8, v) if v == 0 => }),
          ("8'sd0  <<< x_i8",   { case ExprInt(true,  8, v) if v == 0 => }),
          ("x_u8   <<< 8'd0",   { case ExprSym(Symbol("x_u8")) => }),
          ("x_u8   <<< 8'sd0",  { case ExprSym(Symbol("x_u8")) => }),
          ("x_i8   <<< 8'd0",   { case ExprSym(Symbol("x_i8")) => }),
          ("x_i8   <<< 8'sd0",  { case ExprSym(Symbol("x_i8")) => }),
          // Logical &&
          ("1    && x_u1",    { case ExprSym(Symbol("x_u1")) => }),
          ("1'd1 && x_u1",    { case ExprSym(Symbol("x_u1")) => }),
          ("2'd2 && x_u1",    { case ExprSym(Symbol("x_u1")) => }),
          ("x_u1 && 1   ",    { case ExprSym(Symbol("x_u1")) => }),
          ("x_u1 && 1'd1",    { case ExprSym(Symbol("x_u1")) => }),
          ("x_u1 && 2'd2",    { case ExprSym(Symbol("x_u1")) => }),
          ("1    && x_i1",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("x_i1"))) :: Nil) => }),
          ("1'd1 && x_i1",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("x_i1"))) :: Nil) => }),
          ("2'd2 && x_i1",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("x_i1"))) :: Nil) => }),
          ("x_i1 && 1   ",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("x_i1"))) :: Nil) => }),
          ("x_i1 && 1'd1",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("x_i1"))) :: Nil) => }),
          ("x_i1 && 2'd2",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("x_i1"))) :: Nil) => }),
          ("0    && x_u1",    { case ExprInt(false, 1, v) if v == 0 => }),
          ("1'd0 && x_u1",    { case ExprInt(false, 1, v) if v == 0 => }),
          ("2'd0 && x_u1",    { case ExprInt(false, 1, v) if v == 0 => }),
          ("x_u1 && 0   ",    { case ExprInt(false, 1, v) if v == 0 => }),
          ("x_u1 && 1'd0",    { case ExprInt(false, 1, v) if v == 0 => }),
          ("x_u1 && 2'd0",    { case ExprInt(false, 1, v) if v == 0 => }),
          ("0    && x_i1",    { case ExprInt(false, 1, v) if v == 0 => }),
          ("1'd0 && x_i1",    { case ExprInt(false, 1, v) if v == 0 => }),
          ("2'd0 && x_i1",    { case ExprInt(false, 1, v) if v == 0 => }),
          ("x_i1 && 0   ",    { case ExprInt(false, 1, v) if v == 0 => }),
          ("x_i1 && 1'd0",    { case ExprInt(false, 1, v) if v == 0 => }),
          ("x_i1 && 2'd0",    { case ExprInt(false, 1, v) if v == 0 => }),
          // Logical ||
          ("1    || x_u1",    { case ExprInt(false, 1, v) if v == 1 => }),
          ("1'd1 || x_u1",    { case ExprInt(false, 1, v) if v == 1 => }),
          ("2'd2 || x_u1",    { case ExprInt(false, 1, v) if v == 1 => }),
          ("x_u1 || 1   ",    { case ExprInt(false, 1, v) if v == 1 => }),
          ("x_u1 || 1'd1",    { case ExprInt(false, 1, v) if v == 1 => }),
          ("x_u1 || 2'd2",    { case ExprInt(false, 1, v) if v == 1 => }),
          ("1    || x_i1",    { case ExprInt(false, 1, v) if v == 1 => }),
          ("1'd1 || x_i1",    { case ExprInt(false, 1, v) if v == 1 => }),
          ("2'd2 || x_i1",    { case ExprInt(false, 1, v) if v == 1 => }),
          ("x_i1 || 1   ",    { case ExprInt(false, 1, v) if v == 1 => }),
          ("x_i1 || 1'd1",    { case ExprInt(false, 1, v) if v == 1 => }),
          ("x_i1 || 2'd2",    { case ExprInt(false, 1, v) if v == 1 => }),
          ("0    || x_u1",    { case ExprSym(Symbol("x_u1")) => }),
          ("1'd0 || x_u1",    { case ExprSym(Symbol("x_u1")) => }),
          ("2'd0 || x_u1",    { case ExprSym(Symbol("x_u1")) => }),
          ("x_u1 || 0   ",    { case ExprSym(Symbol("x_u1")) => }),
          ("x_u1 || 1'd0",    { case ExprSym(Symbol("x_u1")) => }),
          ("x_u1 || 2'd0",    { case ExprSym(Symbol("x_u1")) => }),
          ("0    || x_i1",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("x_i1"))) :: Nil) => }),
          ("1'd0 || x_i1",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("x_i1"))) :: Nil) => }),
          ("2'd0 || x_i1",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("x_i1"))) :: Nil) => }),
          ("x_i1 || 0   ",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("x_i1"))) :: Nil) => }),
          ("x_i1 || 1'd0",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("x_i1"))) :: Nil) => }),
          ("x_i1 || 2'd0",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("x_i1"))) :: Nil) => }),
          // Binary &
          ("8'd0   & x_u8",    { case ExprInt(false, 8, v) if v == 0 => }),
          ("8'sd0  & x_u8",    { case ExprInt(false,  8, v) if v == 0 => }),
          ("8'd0   & x_i8",    { case ExprInt(false, 8, v) if v == 0 => }),
          ("8'sd0  & x_i8",    { case ExprInt(true,  8, v) if v == 0 => }),
          ("x_u8   & 8'd0",    { case ExprInt(false, 8, v) if v == 0 => }),
          ("x_u8   & 8'sd0",   { case ExprInt(false,  8, v) if v == 0 => }),
          ("x_i8   & 8'd0",    { case ExprInt(false, 8, v) if v == 0 => }),
          ("x_i8   & 8'sd0",   { case ExprInt(true,  8, v) if v == 0 => }),
          ("8'hff  & x_u8",    { case ExprSym(Symbol("x_u8")) => }),
          ("-8'sd1 & x_u8",    { case ExprSym(Symbol("x_u8")) => }),
          ("8'hff  & x_i8",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("x_i8"))) :: Nil) => }),
          ("-8'sd1 & x_i8",    { case ExprSym(Symbol("x_i8")) => }),
          ("x_u8   & 8'hff",   { case ExprSym(Symbol("x_u8")) => }),
          ("x_u8   & -8'sd1",  { case ExprSym(Symbol("x_u8")) => }),
          ("x_i8   & 8'hff",   { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("x_i8"))) :: Nil) => }),
          ("x_i8   & -8'sd1",  { case ExprSym(Symbol("x_i8")) => }),
          // Binary |
          ("8'd0   | x_u8",    { case ExprSym(Symbol("x_u8")) => }),
          ("8'sd0  | x_u8",    { case ExprSym(Symbol("x_u8")) => }),
          ("8'd0   | x_i8",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("x_i8"))) :: Nil) => }),
          ("8'sd0  | x_i8",    { case ExprSym(Symbol("x_i8")) => }),
          ("x_u8   | 8'd0",    { case ExprSym(Symbol("x_u8")) => }),
          ("x_u8   | 8'sd0",   { case ExprSym(Symbol("x_u8")) => }),
          ("x_i8   | 8'd0",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("x_i8"))) :: Nil) => }),
          ("x_i8   | 8'sd0",   { case ExprSym(Symbol("x_i8")) => }),
          ("8'hff  | x_u8",    { case ExprInt(false, 8, v) if v == 0 => }),
          ("-8'sd1 | x_u8",    { case ExprInt(false,  8, v) if v == 0 => }),
          ("8'hff  | x_i8",    { case ExprInt(false, 8, v) if v == 0 => }),
          ("-8'sd1 | x_i8",    { case ExprInt(true,  8, v) if v == 0 => }),
          ("x_u8   | 8'hff",   { case ExprInt(false, 8, v) if v == 0 => }),
          ("x_u8   | -8'sd1",  { case ExprInt(false,  8, v) if v == 0 => }),
          ("x_i8   | 8'hff",   { case ExprInt(false, 8, v) if v == 0 => }),
          ("x_i8   | -8'sd1",  { case ExprInt(true,  8, v) if v == 0 => }),
          // Binary ^
          ("8'd0   ^ x_u8",    { case ExprSym(Symbol("x_u8")) => }),
          ("8'sd0  ^ x_u8",    { case ExprSym(Symbol("x_u8")) => }),
          ("8'd0   ^ x_i8",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("x_i8"))) :: Nil) => }),
          ("8'sd0  ^ x_i8",    { case ExprSym(Symbol("x_i8")) => }),
          ("x_u8   ^ 8'd0",    { case ExprSym(Symbol("x_u8")) => }),
          ("x_u8   ^ 8'sd0",   { case ExprSym(Symbol("x_u8")) => }),
          ("x_i8   ^ 8'd0",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("x_i8"))) :: Nil) => }),
          ("x_i8   ^ 8'sd0",   { case ExprSym(Symbol("x_i8")) => }),
          ("8'hff  ^ x_u8",    { case ExprUnary("~", ExprSym(Symbol("x_u8"))) => }),
          ("-8'sd1 ^ x_u8",    { case ExprUnary("~", ExprSym(Symbol("x_u8"))) => }),
          ("8'hff  ^ x_i8",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprUnary("~", ExprSym(Symbol("x_i8")))) :: Nil) => }),
          ("-8'sd1 ^ x_i8",    { case ExprUnary("~", ExprSym(Symbol("x_i8"))) => }),
          ("x_u8   ^ 8'hff",   { case ExprUnary("~", ExprSym(Symbol("x_u8"))) => }),
          ("x_u8   ^ -8'sd1",  { case ExprUnary("~", ExprSym(Symbol("x_u8"))) => }),
          ("x_i8   ^ 8'hff",   { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprUnary("~", ExprSym(Symbol("x_i8")))) :: Nil) => }),
          ("x_i8   ^ -8'sd1",  { case ExprUnary("~", ExprSym(Symbol("x_i8"))) => }),
          // format: on
        )
      } {
        text in {
          fold {
            s"""
            |fsm f {
            |  u1 x_u1;
            |  i1 x_i1;
            |  u8 x_u8;
            |  i8 x_i8;
            |  void main() {
            |    $$display("", $text);
            |    fence;
            |  }
            |}"""
          } getFirst {
            case ExprCall(_, _ :: ArgP(expr) :: Nil) => expr
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages filter { _.isInstanceOf[Error] } shouldBe empty
        }
      }
    }

    "index into sized integer literals" - {
      for {
        (text, result, err) <- List(
          // format: off
          // signed operand
          ("4'sd2[0]", ExprInt(false, 1, 0), Nil),
          ("4'sd2[1]", ExprInt(false, 1, 1), Nil),
          ("4'sd2[2]", ExprInt(false, 1, 0), Nil),
          ("4'sd2[3]", ExprInt(false, 1, 0), Nil),
          // unsigned operand
          ("4'd2[0]", ExprInt(false, 1, 0), Nil),
          ("4'd2[1]", ExprInt(false, 1, 1), Nil),
          ("4'd2[2]", ExprInt(false, 1, 0), Nil),
          ("4'd2[3]", ExprInt(false, 1, 0), Nil)
          // format: on
        )
      } {
        text in {
          simplify(text) shouldBe result
          checkSingleError(err)
        }
      }
    }

    "slice into sized integer literals" - {
      for {
        (text, result, err) <- List(
          // format: off
          // signed operand
          ("4'sb0101[1 :0]", ExprInt(false, 2, 1), Nil),
          ("4'sb0101[2 :0]", ExprInt(false, 3, 5), Nil),
          ("4'sb0101[3 :0]", ExprInt(false, 4, 5), Nil),
          ("4'sb0101[1+:1]", ExprInt(false, 1, 0), Nil),
          ("4'sb0101[1+:2]", ExprInt(false, 2, 2), Nil),
          ("4'sb0101[1+:3]", ExprInt(false, 3, 2), Nil),
          ("4'sb0101[3-:1]", ExprInt(false, 1, 0), Nil),
          ("4'sb0101[3-:2]", ExprInt(false, 2, 1), Nil),
          ("4'sb0101[3-:3]", ExprInt(false, 3, 2), Nil),
          // unsigned operand
          ("4'b0101[1 :0]", ExprInt(false, 2, 1), Nil),
          ("4'b0101[2 :0]", ExprInt(false, 3, 5), Nil),
          ("4'b0101[3 :0]", ExprInt(false, 4, 5), Nil),
          ("4'b0101[1+:1]", ExprInt(false, 1, 0), Nil),
          ("4'b0101[1+:2]", ExprInt(false, 2, 2), Nil),
          ("4'b0101[1+:3]", ExprInt(false, 3, 2), Nil),
          ("4'b0101[3-:1]", ExprInt(false, 1, 0), Nil),
          ("4'b0101[3-:2]", ExprInt(false, 2, 1), Nil),
          ("4'b0101[3-:3]", ExprInt(false, 3, 2), Nil),
          // long range
          ("36'hf0f0f0f0f[35:0]", ExprInt(false, 36, BigInt("f0f0f0f0f", 16)), Nil),
          ("68'hf0f0f0f0f0f0f0f0f[67:0]", ExprInt(false, 68, BigInt("f0f0f0f0f0f0f0f0f", 16)), Nil)
          // format: on
        )
      } {
        text in {
          simplify(text) shouldBe result
          checkSingleError(err)
        }
      }
    }

    "index into unsized integer literals" - {
      for {
        (text, result, err) <- List(
          // format: off
          // signed operand
          (" 2s[0]", ExprInt(false, 1, 0), Nil),
          (" 2s[1]", ExprInt(false, 1, 1), Nil),
          (" 2s[2]", ExprInt(false, 1, 0), Nil),
          (" 2s[3]", ExprInt(false, 1, 0), Nil),
          ("-2s[0]", ExprInt(false, 1, 0), Nil),
          ("-2s[1]", ExprInt(false, 1, 1), Nil),
          ("-2s[2]", ExprInt(false, 1, 1), Nil),
          ("-2s[3]", ExprInt(false, 1, 1), Nil),
          // unsigned operand
          (" 2[0]", ExprInt(false, 1, 0), Nil),
          (" 2[1]", ExprInt(false, 1, 1), Nil),
          (" 2[2]", ExprInt(false, 1, 0), Nil),
          (" 2[3]", ExprInt(false, 1, 0), Nil)
          // format: on
        )
      } {
        text in {
          simplify(text) shouldBe result
          checkSingleError(err)
        }
      }
    }

    "slice into unsized integer literals" - {
      for {
        (text, result, err) <- List(
          // format: off
          // signed operand
          (" 5s[1 :0]", ExprInt(false, 2, 1), Nil),
          (" 5s[2 :0]", ExprInt(false, 3, 5), Nil),
          (" 5s[3 :0]", ExprInt(false, 4, 5), Nil),
          (" 5s[1+:1]", ExprInt(false, 1, 0), Nil),
          (" 5s[1+:2]", ExprInt(false, 2, 2), Nil),
          (" 5s[1+:3]", ExprInt(false, 3, 2), Nil),
          (" 5s[3-:1]", ExprInt(false, 1, 0), Nil),
          (" 5s[3-:2]", ExprInt(false, 2, 1), Nil),
          (" 5s[3-:3]", ExprInt(false, 3, 2), Nil),
          ("-5s[1 :0]", ExprInt(false, 2, 3), Nil),
          ("-5s[2 :0]", ExprInt(false, 3, 3), Nil),
          ("-5s[3 :0]", ExprInt(false, 4, 11), Nil),
          ("-5s[1+:1]", ExprInt(false, 1, 1), Nil),
          ("-5s[1+:2]", ExprInt(false, 2, 1), Nil),
          ("-5s[1+:3]", ExprInt(false, 3, 5), Nil),
          ("-5s[3-:1]", ExprInt(false, 1, 1), Nil),
          ("-5s[3-:2]", ExprInt(false, 2, 2), Nil),
          ("-5s[3-:3]", ExprInt(false, 3, 5), Nil),
          // unsigned operand
          ("5[1 :0]", ExprInt(false, 2, 1), Nil),
          ("5[2 :0]", ExprInt(false, 3, 5), Nil),
          ("5[3 :0]", ExprInt(false, 4, 5), Nil),
          ("5[1+:1]", ExprInt(false, 1, 0), Nil),
          ("5[1+:2]", ExprInt(false, 2, 2), Nil),
          ("5[1+:3]", ExprInt(false, 3, 2), Nil),
          ("5[3-:1]", ExprInt(false, 1, 0), Nil),
          ("5[3-:2]", ExprInt(false, 2, 1), Nil),
          ("5[3-:3]", ExprInt(false, 3, 2), Nil)
          // format: on
        )
      } {
        text in {
          simplify(text) shouldBe result
          checkSingleError(err)
        }
      }
    }

    "index over a slice" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          // format: off
          ("a[8  : 3][0]", { case ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 4, v)) if v == 3 => }),
          ("a[9  : 3][0]", { case ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 4, v)) if v == 3 => }),
          ("a[8  : 3][2]", { case ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 4, v)) if v == 5 => }),
          ("a[9  : 3][2]", { case ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 4, v)) if v == 5 => }),
          ("a[8 +: 3][0]", { case ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 4, v)) if v == 8 => }),
          ("a[9 +: 3][0]", { case ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 4, v)) if v == 9 => }),
          ("a[8 +: 3][2]", { case ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 4, v)) if v == 10 => }),
          ("a[9 +: 3][2]", { case ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 4, v)) if v == 11 => }),
          ("a[8 -: 3][0]", { case ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 4, v)) if v == 6 => }),
          ("a[9 -: 3][0]", { case ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 4, v)) if v == 7 => }),
          ("a[8 -: 3][2]", { case ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 4, v)) if v == 8 => }),
          ("a[9 -: 3][2]", { case ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 4, v)) if v == 9 => })
          // format: on
        )
      } {
        text in {
          fold {
            s"""
            |fsm f {
            |  in u10 a;
            |  out u1 b;
            |  fence {
            |    b = $text;
            |  }
            |}"""
          } getFirst {
            case StmtAssign(_, rhs) => rhs
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages shouldBe empty
        }
      }
    }

    "slice over a slice" - {
      for {
        (descr, in_type, out_type) <- List(
          ("of non-vectors", "u10", "u2"),
          ("of vectors", "u8[10]", "u8[2]")
        )
      } {
        descr - {
          for {
            (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
              // format: off
              ("a[8  : 4][1  : 0]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 5 && r == 4 => }),
              ("a[9  : 4][1  : 0]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 5 && r == 4 => }),
              ("a[8  : 4][2  : 1]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 6 && r == 5 => }),
              ("a[9  : 4][2  : 1]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 6 && r == 5 => }),
              ("a[8  : 4][1 +: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if l == 5 && r == 2 => }),
              ("a[9  : 4][1 +: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if l == 5 && r == 2 => }),
              ("a[8  : 4][2 +: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if l == 6 && r == 2 => }),
              ("a[9  : 4][2 +: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if l == 6 && r == 2 => }),
              ("a[8  : 4][1 -: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if l == 5 && r == 2 => }),
              ("a[9  : 4][1 -: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if l == 5 && r == 2 => }),
              ("a[8  : 4][2 -: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if l == 6 && r == 2 => }),
              ("a[9  : 4][2 -: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if l == 6 && r == 2 => }),
              ("a[8 +: 4][1  : 0]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if l == 8 && r == 2 => }),
              ("a[9 +: 4][1  : 0]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if l == 9 && r == 2 => }),
              ("a[8 +: 4][2  : 1]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if l == 9 && r == 2 => }),
              ("a[9 +: 4][2  : 1]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if l == 10 && r == 2 => }),
              ("a[8 +: 4][1 +: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if l == 9 && r == 2 => }),
              ("a[9 +: 4][1 +: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if l == 10 && r == 2 => }),
              ("a[8 +: 4][2 +: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if l == 10 && r == 2 => }),
              ("a[9 +: 4][2 +: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if l == 11 && r == 2 => }),
              ("a[8 +: 4][1 -: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if l == 9 && r == 2 => }),
              ("a[9 +: 4][1 -: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if l == 10 && r == 2 => }),
              ("a[8 +: 4][2 -: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if l == 10 && r == 2 => }),
              ("a[9 +: 4][2 -: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if l == 11 && r == 2 => }),
              ("a[8 -: 4][1  : 0]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if l == 6 && r == 2 => }),
              ("a[9 -: 4][1  : 0]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if l == 7 && r == 2 => }),
              ("a[8 -: 4][2  : 1]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if l == 7 && r == 2 => }),
              ("a[9 -: 4][2  : 1]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if l == 8 && r == 2 => }),
              ("a[8 -: 4][1 +: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if l == 6 && r == 2 => }),
              ("a[9 -: 4][1 +: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if l == 7 && r == 2 => }),
              ("a[8 -: 4][2 +: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if l == 7 && r == 2 => }),
              ("a[9 -: 4][2 +: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "+:", ExprInt(false, 4, r)) if l == 8 && r == 2 => }),
              ("a[8 -: 4][1 -: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if l == 6 && r == 2 => }),
              ("a[9 -: 4][1 -: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if l == 7 && r == 2 => }),
              ("a[8 -: 4][2 -: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if l == 7 && r == 2 => }),
              ("a[9 -: 4][2 -: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), "-:", ExprInt(false, 4, r)) if l == 8 && r == 2 => })
              // format: on
            )
          } {
            text in {
              fold {
                s"""
                |fsm f {
                |  in  $in_type  a;
                |  out $out_type b;
                |  fence {
                |    b = $text;
                |  }
                |}"""
              } getFirst {
                case StmtAssign(_, rhs) => rhs
              } tap {
                _ should matchPattern(pattern)
              }
              cc.messages shouldBe empty
            }
          }
        }
      }
    }

    "index over $signed/$unsigned" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          // format: off
          ("$signed(a)[3'd0]",   { case ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 3, v)) if v == 0 => }),
          ("$signed(a)[3'd1]",   { case ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 3, v)) if v == 1 => }),
          ("$unsigned(a)[3'd0]", { case ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 3, v)) if v == 0 => }),
          ("$unsigned(a)[3'd1]", { case ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 3, v)) if v == 1 => }),
          // format: on
        )
      } {
        text in {
          fold {
            s"""
            |fsm f {
            |  in u8 a;
            |  out u1 b;
            |  fence {
            |    b = $text;
            |  }
            |}"""
          } getFirst {
            case StmtAssign(_, rhs) => rhs
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages shouldBe empty
        }
      }
    }

    "slice over $signed/$unsigned" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          // format: off
          ("$signed(a)[3'd1  : 3'd0]",   { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l),  ":", ExprInt(false, 3, r)) if l == 1 && r == 0 => }),
          ("$signed(a)[3'd2  : 3'd1]",   { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l),  ":", ExprInt(false, 3, r)) if l == 2 && r == 1 => }),
          ("$signed(a)[3'd0 +: 4'd2]",   { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l), "+:", ExprInt(false, 4, r)) if l == 0 && r == 2 => }),
          ("$signed(a)[3'd1 +: 4'd2]",   { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l), "+:", ExprInt(false, 4, r)) if l == 1 && r == 2 => }),
          ("$signed(a)[3'd2 -: 4'd2]",   { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l), "-:", ExprInt(false, 4, r)) if l == 2 && r == 2 => }),
          ("$signed(a)[3'd1 -: 4'd2]",   { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l), "-:", ExprInt(false, 4, r)) if l == 1 && r == 2 => }),
          ("$unsigned(a)[3'd1  : 3'd0]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l),  ":", ExprInt(false, 3, r)) if l == 1 && r == 0 => }),
          ("$unsigned(a)[3'd2  : 3'd1]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l),  ":", ExprInt(false, 3, r)) if l == 2 && r == 1 => }),
          ("$unsigned(a)[3'd0 +: 4'd2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l), "+:", ExprInt(false, 4, r)) if l == 0 && r == 2 => }),
          ("$unsigned(a)[3'd1 +: 4'd2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l), "+:", ExprInt(false, 4, r)) if l == 1 && r == 2 => }),
          ("$unsigned(a)[3'd2 -: 4'd2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l), "-:", ExprInt(false, 4, r)) if l == 2 && r == 2 => }),
          ("$unsigned(a)[3'd1 -: 4'd2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l), "-:", ExprInt(false, 4, r)) if l == 1 && r == 2 => })
          // format: on
        )
      } {
        text in {
          fold {
            s"""
            |fsm f {
            |  in u8 a;
            |  out u2 b;
            |  fence {
            |    b = $text;
            |  }
            |}"""
          } getFirst {
            case StmtAssign(_, rhs) => rhs
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages shouldBe empty
        }
      }
    }

    "width 1 slices except vectors" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          // format: off
          ("c = a[7:7]",    { case ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 3, i)) if i == 7 => }),
          ("c = a[6 +: 1]", { case ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 3, i)) if i == 6 => }),
          ("c = a[5 +: 1]", { case ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 3, i)) if i == 5 => }),
          ("c = a[4 -: 1]", { case ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 3, i)) if i == 4 => }),
          ("c = a[3 -: 1]", { case ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 3, i)) if i == 3 => }),
          ("d = a[2  : 1]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l), ":", ExprInt(false, 3, r)) if l == 2 && r == 1 => }),
          ("c = a[b +: 1]", { case ExprIndex(ExprSym(Symbol("a")), ExprSym(Symbol("b"))) => }),
          ("c = a[b -: 1]", { case ExprIndex(ExprSym(Symbol("a")), ExprSym(Symbol("b"))) => }),
          ("d = v[0  : 0]", { case ExprSlice(ExprSym(Symbol("v")), ExprInt(false, 3, l), ":", ExprInt(false, 3, r))  if l == 0 && r == 0 => }),
          ("d = v[5 +: 1]", { case ExprSlice(ExprSym(Symbol("v")), ExprInt(false, 3, l), "+:", ExprInt(false, 4, r)) if l == 5 && r == 1 => }),
          ("d = v[5 -: 1]", { case ExprSlice(ExprSym(Symbol("v")), ExprInt(false, 3, l), "-:", ExprInt(false, 4, r)) if l == 5 && r == 1 => }),
          ("d = v[b +: 1]", { case ExprSlice(ExprSym(Symbol("v")), ExprSym(Symbol("b")), "+:", ExprInt(false, 4, r)) if r == 1 => })
          // format: on
        )
      } {
        text in {
          fold {
            s"""
            |fsm f {
            |  in u8 a;
            |  in u3 b;
            |  in u2[8] v;
            |  out u1 c;
            |  out u2 d;
            |  fence {
            |    $text;
            |  }
            |}"""
          } getFirst {
            case StmtAssign(_, rhs) => rhs
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages shouldBe empty
        }
      }
    }

    "full width slices" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          // format: off
          ("c = a[6:0]",  { case ExprSym(Symbol("a")) => }),
          ("c = a[6-:7]", { case ExprSym(Symbol("a")) => }),
          ("c = a[0+:7]", { case ExprSym(Symbol("a")) => }),
          ("d = b[0:0]",  { case ExprSym(Symbol("b")) => }),
          ("c = e[6:0] + 7'd1",  { case ExprBinary(ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("e"))) :: Nil),
                                                   "+", ExprInt(false, 7, v)) if v == 1 => }),
          ("c = e[0+:7] + 7'd1", { case ExprBinary(ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("e"))) :: Nil),
                                                   "+", ExprInt(false, 7, v)) if v == 1 => }),
          ("c = e[6-:7] + 7'd1", { case ExprBinary(ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("e"))) :: Nil),
                                                   "+", ExprInt(false, 7, v)) if v == 1 => })
          // format: on
        )
      } {
        text in {
          fold {
            s"""
            |fsm f {
            |  in u7 a;
            |  in u1 b;
            |  out u7 c;
            |  out u1 d;
            |  in i7 e;
            |  fence {
            |    $text;
            |  }
            |}"""
          } getFirst {
            case StmtAssign(_, rhs) => rhs
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages shouldBe empty
        }
      }
    }

    "index zero of width zero" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          // format: off
          ("c = a[0]", { case ExprSym(Symbol("a")) => }),
          ("c = b[3][0]", { case ExprIndex(ExprSym(Symbol("b")), ExprInt(false, _, idx)) if idx == 3 => }),
          ("d = mem[0]", { case ExprIndex(ExprSym(Symbol("mem")), ExprInt(false, _, idx)) if idx == 0 => })
          // format: on
        )
      } {
        text in {
          fold {
            s"""
             |fsm f {
             |  in u1 a;
             |  in u7 b;
             |  out u1 c;
             |  out u8 d;
             |  u8 mem[8];
             |  fence { $text; }
             |}"""
          } getFirst {
            case StmtAssign(_, rhs) => rhs
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages shouldBe empty
        }
      }
    }

    "repetitions of count 1" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          // format: off
          ("{1{a}}",    { case ExprSym(Symbol("a")) => }),
          ("{1'd1{a}}", { case ExprSym(Symbol("a"))  => }),
          ("{1{b}}",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("b"))) :: Nil) => }),
          ("{1'd1{b}}", { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("b"))) :: Nil) => })
          // format: on
        )
      } {
        text in {
          fold {
            s"""
            |fsm f {
            |  in u8 a;
            |  in i8 b;
            |  void main() {
            |    $$display("", $text);
            |    fence;
            |  }
            |}"""
          } getFirst {
            case ExprCall(_, _ :: ArgP(expr) :: Nil) => expr
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages shouldBe empty
        }
      }
    }

    "concatenation of size 1" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          // format: off
          ("{a}",    { case ExprSym(Symbol("a")) => }),
          ("{b}",    { case ExprCall(ExprSym(Symbol("$unsigned")), ArgP(ExprSym(Symbol("b"))) :: Nil) => }),
          // format: on
        )
      } {
        text in {
          fold {
            s"""
            |fsm f {
            |  in u8 a;
            |  in i8 b;
            |  out u8 c;
            |  void main() {
            |    $$display("", $text);
            |    fence;
            |  }
            |}"""
          } getFirst {
            case ExprCall(_, _ :: ArgP(expr) :: Nil) => expr
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages shouldBe empty
        }
      }
    }

    "concatenation of sized integer literals" - {
      for {
        (text, result, err) <- List(
          // format: off
          ("{4'd2, 4'd2}", ExprInt(false, 8, 34), Nil),
          ("{4'd2, 4'sd2}", ExprInt(false, 8, 34), Nil),
          ("{4'sd2, 4'd2}", ExprInt(false, 8, 34), Nil),
          ("{4'sd2, 4'sd2}", ExprInt(false, 8, 34), Nil),
          ("{1'b1, 2'b11, 3'b111, 4'b1111}", ExprInt(false, 10, 1023), Nil),
          ("{-1'sd1, -2'sd1, -3'sd1, -4'sd1}", ExprInt(false, 10, 1023), Nil)
          // format: on
        )
      } {
        text in {
          simplify(text) shouldBe result
          checkSingleError(err)
        }
      }
    }

    "repetition of sized integer literals" - {
      for {
        (text, result, err) <- List(
          // format: off
          ("{4{1'b1}}", ExprInt(false, 4, 15), Nil),
          ("{4{1'b0}}", ExprInt(false, 4, 0), Nil),
          ("{4{-1'sb1}}", ExprInt(false, 4, 15), Nil),
          ("{2{2'b1}}", ExprInt(false, 4, 5), Nil),
          ("{2{2'b0}}", ExprInt(false, 4, 0), Nil),
          ("{2{-4'sb1}}", ExprInt(false, 8, 255), Nil),
          ("{4{2'b10}}", ExprInt(false, 8, 170), Nil)
          // format: on
        )
      } {
        text in {
          simplify(text) shouldBe result
          checkSingleError(err)
        }
      }
    }

    "constant index of concatenation" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          // format: off
          ("c = {a,b}[3]", { case ExprIndex(ExprSym(Symbol("b")), ExprInt(false, 2, i)) if i == 3 => }),
          ("c = {a,b}[4]", { case ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 2, i)) if i == 0 => }),
          ("d = @sx(28, {a,10'd0})", {
            case ExprCat(
                List(ExprRep(ExprNum(false, rep), ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 2, msb))),
                     ExprSym(Symbol("a")),
                     ExprInt(false, 10, i)))
                if rep == 14 && msb == 3 && i == 0 =>
          })
          // format: on
        )
      } {
        text in {
          fold {
            s"""
            |fsm f {
            |  in u4 a;
            |  in u4 b;
            |  out u1 c;
            |  out u28 d;
            |  fence {
            |    $text;
            |  }
            |}"""
          } getFirst {
            case StmtAssign(_, rhs) => rhs
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages shouldBe empty
        }
      }
    }

    "constant index of repetition" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          // format: off
          ("c = {2{a}}[3]", { case ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 2, i)) if i == 3 => }),
          ("c = {2{a}}[4]", { case ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 2, i)) if i == 0 => })
          // format: on
        )
      } {
        text in {
          fold {
            s"""
            |fsm f {
            |  in u4 a;
            |  out u1 c;
            |  fence {
            |    $text;
            |  }
            |}"""
          } getFirst {
            case StmtAssign(_, rhs) => rhs
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages shouldBe empty
        }
      }
    }

    "constant slice of concatenation" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          // format: off
          ("d = {a,b}[2:0]",  { case ExprSlice(ExprSym(Symbol("b")), ExprInt(false, 2, m), ":", ExprInt(false, 2, l)) if m == 2 && l == 0 => }),
          ("d = {a,b}[6:4]",  { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 2, m), ":", ExprInt(false, 2, l)) if m == 2 && l == 0 => }),
          ("d = {a,b}[4+:3]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 2, m), ":", ExprInt(false, 2, l)) if m == 2 && l == 0 => }),
          ("d = {a,b}[6-:3]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 2, m), ":", ExprInt(false, 2, l)) if m == 2 && l == 0 => }),
          ("d = {a,b}[4:2]", {
            case ExprCat(
                List(
                  ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 2, i)),
                  ExprSlice(ExprSym(Symbol("b")), ExprInt(false, 2, m), ":", ExprInt(false, 2, l))))
                if i == 0 && m == 3 && l == 2 =>
          }),
          ("e = {a,b,c}[8:2]", {
            case ExprCat(
                List(
                  ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 2, i)),
                  ExprSym(Symbol("b")),
                  ExprSlice(ExprSym(Symbol("c")), ExprInt(false, 2, m), ":", ExprInt(false, 2, l))))
                if i == 0 && m == 3 && l == 2 =>
          })
          // format: on
        )
      } {
        text in {
          fold {
            s"""
            |fsm f {
            |  in u4 a;
            |  in u4 b;
            |  in u4 c;
            |  out u3 d;
            |  out u7 e;
            |  fence {
            |    $text;
            |  }
            |}"""
          } getFirst {
            case StmtAssign(_, rhs) => rhs
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages shouldBe empty
        }
      }
    }

    "constant slice of repetition" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          // format: off
          ("c = {2{a}}[2:0]", {
            case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 2, m), ":", ExprInt(false, 2, l))
                if m == 2 && l == 0 =>
          }),
          ("c = {2{a}}[6:4]", {
            case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 2, m), ":", ExprInt(false, 2, l))
                if m == 2 && l == 0 =>
          }),
          ("c = {2{a}}[4:2]", {
            case ExprCat(
                List(ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 2, i)),
                     ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 2, m), ":", ExprInt(false, 2, l))))
                if i == 0 && m == 3 && l == 2 =>
          }),
          ("d = {3{a}}[8:2]", {
            case ExprCat(
                List(ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 2, i)),
                     ExprSym(Symbol("a")),
                     ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 2, m), ":", ExprInt(false, 2, l))))
                if  i == 0 && m == 3 && l == 2 =>
          }),
          ("e = {4{a}}[12:2]", {
            case ExprCat(
                List(ExprIndex(ExprSym(Symbol("a")), ExprInt(false, 2, i)),
                     ExprRep(ExprNum(false, r), ExprSym(Symbol("a"))),
                     ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 2, m), ":", ExprInt(false, 2, l))))
                if i == 0 && r == 2 && m == 3 && l == 2 =>
          }),
          ("e = {4{a}}[11:1]", {
            case ExprCat(
                List(ExprRep(ExprNum(false, r), ExprSym(Symbol("a"))),
                     ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 2, m), ":", ExprInt(false, 2, l))))
                if r == 2 && m == 3 && l == 1 =>
          }),
          ("e = {4{a}}[14:4]", {
            case ExprCat(
                List(ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 2, m), ":", ExprInt(false, 2, l)),
                     ExprRep(ExprNum(false, r), ExprSym(Symbol("a")))))
                if m == 2 && l == 0 && r == 2 =>
          })
          // format: on
        )
      } {
        text in {
          fold {
            s"""
            |fsm f {
            |  in u4 a;
            |  out u3 c;
            |  out u7 d;
            |  out u11 e;
            |  fence {
            |    $text;
            |  }
            |}"""
          } getFirst {
            case StmtAssign(_, rhs) => rhs
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages shouldBe empty
        }
      }
    }

    "builtin functions" - {
      "@max" - {
        for {
          (text, result, err) <- List(
            // format: off
            ("@max(1s)", ExprNum(true, 1), Nil),
            ("@max(1)", ExprNum(false, 1), Nil),
            ("@max(1s, 2s)", ExprNum(true, 2), Nil),
            ("@max(1s, 2)", ExprNum(false, 2), Nil),
            ("@max(1, 2s)", ExprNum(false, 2), Nil),
            ("@max(1, 2)", ExprNum(false, 2), Nil),
            ("@max(0s, 1s)", ExprNum(true, 1), Nil),
            ("@max(-2s, -1s)", ExprNum(true, -1), Nil),
            ("@max(-2s, 1)", ExprNum(false, 1), Nil),
            ("@max(0, 1, 2, 3, 4, 5)", ExprNum(false, 5), Nil)
            // format: on
          )
        } {
          text in {
            simplify(text) shouldBe result
            checkSingleError(err)
          }
        }
      }

      // TODO: @ex

      // TODO: @msb

      "@zx" - {
        for {
          (text, result, err) <- List(
            // format: off
            ("@zx(3, 2'b00)", ExprInt(false, 3, 0), Nil),
            ("@zx(3, 2'b01)", ExprInt(false, 3, 1), Nil),
            ("@zx(3, 2'b10)", ExprInt(false, 3, 2), Nil),
            ("@zx(3, 2'b11)", ExprInt(false, 3, 3), Nil),
            ("@zx(3, 2'sb00)", ExprInt(true, 3, 0), Nil),
            ("@zx(3, 2'sb01)", ExprInt(true, 3, 1), Nil),
            ("@zx(3, 2'sb10)", ExprInt(true, 3, 2), Nil),
            ("@zx(3, 2'sb11)", ExprInt(true, 3, 3), Nil),
            ("@zx(2, 2'b00)", ExprInt(false, 2, 0), Nil),
            ("@zx(2, 2'b01)", ExprInt(false, 2, 1), Nil),
            ("@zx(2, 2'b10)", ExprInt(false, 2, 2), Nil),
            ("@zx(2, 2'b11)", ExprInt(false, 2, 3), Nil),
            ("@zx(2, 2'sb00)", ExprInt(true, 2, 0), Nil),
            ("@zx(2, 2'sb01)", ExprInt(true, 2, 1), Nil),
            ("@zx(2, 2'sb10)", ExprInt(true, 2, -2), Nil),
            ("@zx(2, 2'sb11)", ExprInt(true, 2, -1), Nil),
            ("@zx(1, 2'b00)", ExprError(), "Result width 1 of extension is less than argument width 2" :: Nil),
            ("@zx(1, 2'b01)", ExprError(), "Result width 1 of extension is less than argument width 2" :: Nil),
            ("@zx(1, 2'b10)", ExprError(), "Result width 1 of extension is less than argument width 2" :: Nil),
            ("@zx(1, 2'b11)", ExprError(), "Result width 1 of extension is less than argument width 2" :: Nil),
            ("@zx(1, 2'sb00)", ExprError(), "Result width 1 of extension is less than argument width 2" :: Nil),
            ("@zx(1, 2'sb01)", ExprError(), "Result width 1 of extension is less than argument width 2" :: Nil),
            ("@zx(1, 2'sb10)", ExprError(), "Result width 1 of extension is less than argument width 2" :: Nil),
            ("@zx(1, 2'sb11)", ExprError(), "Result width 1 of extension is less than argument width 2" :: Nil)
            // format: on
          )
        } {
          text in {
            simplify(text) shouldBe result
            checkSingleError(err)
          }
        }
      }

      "@sx" - {
        for {
          (text, result, err) <- List(
            // format: off
            ("@sx(3, 2'b00)", ExprInt(false, 3, 0), Nil),
            ("@sx(3, 2'b01)", ExprInt(false, 3, 1), Nil),
            ("@sx(3, 2'b10)", ExprInt(false, 3, 6), Nil),
            ("@sx(3, 2'b11)", ExprInt(false, 3, 7), Nil),
            ("@sx(3, 2'sb00)", ExprInt(true, 3, 0), Nil),
            ("@sx(3, 2'sb01)", ExprInt(true, 3, 1), Nil),
            ("@sx(3, 2'sb10)", ExprInt(true, 3, -2), Nil),
            ("@sx(3, 2'sb11)", ExprInt(true, 3, -1), Nil),
            ("@sx(2, 2'b00)", ExprInt(false, 2, 0), Nil),
            ("@sx(2, 2'b01)", ExprInt(false, 2, 1), Nil),
            ("@sx(2, 2'b10)", ExprInt(false, 2, 2), Nil),
            ("@sx(2, 2'b11)", ExprInt(false, 2, 3), Nil),
            ("@sx(2, 2'sb00)", ExprInt(true, 2, 0), Nil),
            ("@sx(2, 2'sb01)", ExprInt(true, 2, 1), Nil),
            ("@sx(2, 2'sb10)", ExprInt(true, 2, -2), Nil),
            ("@sx(2, 2'sb11)", ExprInt(true, 2, -1), Nil),
            ("@sx(1, 2'b00)", ExprError(), "Result width 1 of extension is less than argument width 2" :: Nil),
            ("@sx(1, 2'b01)", ExprError(), "Result width 1 of extension is less than argument width 2" :: Nil),
            ("@sx(1, 2'b10)", ExprError(), "Result width 1 of extension is less than argument width 2" :: Nil),
            ("@sx(1, 2'b11)", ExprError(), "Result width 1 of extension is less than argument width 2" :: Nil),
            ("@sx(1, 2'sb00)", ExprError(), "Result width 1 of extension is less than argument width 2" :: Nil),
            ("@sx(1, 2'sb01)", ExprError(), "Result width 1 of extension is less than argument width 2" :: Nil),
            ("@sx(1, 2'sb10)", ExprError(), "Result width 1 of extension is less than argument width 2" :: Nil),
            ("@sx(1, 2'sb11)", ExprError(), "Result width 1 of extension is less than argument width 2" :: Nil)
            // format: on
          )
        } {
          text in {
            simplify(text) shouldBe result
            checkSingleError(err)
          }
        }
      }

      "@bits" - {
        for {
          (text, result) <- List(
            // format: off
            ("@bits(1'b0)", ExprNum(false, 1)),
            ("@bits(2'b0)", ExprNum(false, 2)),
            ("@bits(2'sb0)", ExprNum(false, 2)),
            ("@bits(bool)", ExprNum(false, 1)),
            ("@bits(u3)", ExprNum(false, 3)),
            ("@bits(i3)", ExprNum(false, 3)),
            ("@bits(a)", ExprNum(false, 10)),
            ("@bits(a.f0)", ExprNum(false, 1)),
            ("@bits(a.f1)", ExprNum(false, 7)),
            ("@bits(a.f2)", ExprNum(false, 2)),
            ("@bits(a.f2.f0)", ExprNum(false, 2)),
            ("@bits(a_t)", ExprNum(false, 10)),
            ("@bits(a_t.f0)", ExprNum(false, 1)),
            ("@bits(a_t.f1)", ExprNum(false, 7)),
            ("@bits(a_t.f2)", ExprNum(false, 2)),
            ("@bits(a_t.f2.f0)", ExprNum(false, 2))
            // format: on
          )
        } {
          text in {
            fold {
              s"""
              |struct b_t {
              |  u2 f0;
              |}
              |
              |struct a_t {
              |  bool f0;
              |  i7   f1;
              |  b_t  f2;
              |}
              |
              |(* toplevel *)
              |fsm x {
              |  a_t a;
              |  void main() {
              |    $$display("", $text);
              |    fence;
              |  }
              |}"""
            } getFirst {
              case ExprCall(_, List(_, ArgP(e))) => e
            } tap {
              _ shouldBe result
            }
            cc.messages shouldBe empty
          }
        }
      }

      "$signed" - {
        for {
          (text, result) <- List(
            // format: off
            ("$signed(1)",      ExprNum(true,  1)),
            ("$signed(1s)",     ExprNum(true,  1)),
            ("$signed(-1s)",    ExprNum(true, -1)),
            ("$signed(2'd0)",   ExprInt(true, 2,    0)),
            ("$signed(2'd1)",   ExprInt(true, 2,    1)),
            ("$signed(2'd3)",   ExprInt(true, 2,   -1)),
            ("$signed(2'sd0)",  ExprInt(true, 2,    0)),
            ("$signed(2'sd1)",  ExprInt(true, 2,    1)),
            ("$signed(-2'sd1)", ExprInt(true, 2,   -1)),
            ("$signed(8'h7f)",  ExprInt(true, 8,  127)),
            ("$signed(8'h80)",  ExprInt(true, 8, -128)),
            ("$signed(8'hff)",  ExprInt(true, 8,   -1)),
            ("$signed({1'd0, {31{1'd1}}})", ExprInt(true, 32,  2147483647)),
            ("$signed({1'd1, {31{1'd0}}})", ExprInt(true, 32, -2147483648))
            // format: on
          )
        } {
          text in {
            simplify(text) shouldBe result
          }
        }
      }

      "$unsigned" - {
        for {
          (text, result, err) <- List(
            // format: off
            ("$unsigned(1)",        ExprNum(false,  1), Nil),
            ("$unsigned(1s)",       ExprNum(false,  1), Nil),
            ("$unsigned(-1s)",      ExprError(), "Cannot cast negative unsized integer to unsigned" :: Nil),
            ("$unsigned(2'd0)",     ExprInt(false, 2,   0), Nil),
            ("$unsigned(2'd1)",     ExprInt(false, 2,   1), Nil),
            ("$unsigned(2'd3)",     ExprInt(false, 2,   3), Nil),
            ("$unsigned(2'sd0)",    ExprInt(false, 2,   0), Nil),
            ("$unsigned(2'sd1)",    ExprInt(false, 2,   1), Nil),
            ("$unsigned(-2'sd1)",   ExprInt(false, 2,   3), Nil),
            ("$unsigned(8'sd127)",  ExprInt(false, 8, 127), Nil),
            ("$unsigned(-8'sd128)", ExprInt(false, 8, 128), Nil),
            ("$unsigned(-8'sd1)",   ExprInt(false, 8, 255), Nil),
            ("$unsigned({1'd0, {31{1'd1}}})", ExprInt(false, 32, 2147483647), Nil),
            ("$unsigned({1'd1, {31{1'd0}}})", ExprInt(false, 32, 2147483648L), Nil)
            // format: on
          )
        } {
          text in {
            simplify(text) shouldBe result
            checkSingleError(err)

          }
        }
      }
    }

    "reference to constant" - {
      for {
        (expr, value) <- List(
          // format: off
          ("A", ExprInt(false, 36, 0x0000000fffL)),
          ("B", ExprInt(false, 41, 0x1000000fffL)),
          ("C", ExprInt(false,  8, 2)),
          ("D", ExprInt(false,  7, 3)),
          ("E", ExprInt(true,   6, 4)),
          ("F", ExprInt(true,   5, 5))
          // format: on
        )
      } expr in {
        fold {
          s"""
          |fsm a {
          |  const u36 A = {{24{1'b0}}, {12{1'b1}}};
          |  const u41 B = {5'h1, A[35:0]};
          |  const u8  C = 2;
          |  const u7  D = 3s;
          |  const i6  E = 4;
          |  const i5  F = 5s;
          |
          |  void main() {
          |    $$display("", $expr);
          |    fence;
          |  }
          |}"""
        } getFirst {
          case ExprCall(_, _ :: ArgP(expr) :: Nil) => expr
        } tap {
          _ shouldBe value
        }
      }
    }

    "sliced or indexed refs when slice or index is const" - {
      for {
        (stmt, pattern) <- List[(String, PartialFunction[Any, Unit])](
          // format: off
          ("o_2 = B[1]",                { case ExprInt(false, 2, v) if v == 1 => }),
          ("os_2 = Bs[1]",              { case ExprInt(true, 2, v) if v == -1 => }),
          ("o_4 = B[1+:2]",             { case ExprInt(false, 4, v) if v == 9 => }),
          ("o_1 = i_2[i_3[B[1]+2'd1]]", { case ExprIndex(ExprSym(Symbol("i_2")), ExprIndex(ExprSym(Symbol("i_3")), ExprInt(false, 2, w))) if w == 2 => }),
          ("o_1 = i_3[i_2 +: B[1]]",    { case ExprIndex(ExprSym(Symbol("i_3")), ExprSym(Symbol("i_2"))) => }),
          ("o_8 = A[B[1]]",             { case ExprInt(false, 8, v) if v == 123 => }),
          ("o_16 = A[B[1] +: 2]",       { case ExprInt(false, 16, v) if v == ((5 << 8) + 123) => }),
          ("o_8 = A[2:1][0]",           { case ExprInt(false, 8, v) if v == 123 => }),
          ("o_2 = A[2][2:1]",           { case ExprInt(false, 2, v) if v == 2 => }),
          ("o_8 = C.a",                 { case ExprInt(false, 8, v) if v == 47 => }),
          ("o_8 = C.b",                 { case ExprInt(false, 8, v) if v == 23 => }),
          ("o_4 = C.c[1]",              { case ExprInt(false, 4, v) if v == 3 =>  })
          // format: on
        )
      } {
        stmt in {
          fold {
            s"""
            |fsm a {
            |  struct struct_t {
            |    u8 a;
            |    u8 b;
            |    u4[2] c;
            |  }
            |  const u8[3] A = {8'd5, 8'd123, 8'd7};
            |  const u2[3] B = {2'd2, 2'd1, 2'd0};
            |  const i2[3] Bs= {-2'sd1, -2'sd1, 2'sd0};
            |  const struct_t C = {8'd47, 8'd23, {4'd3, 4'd4}};
            |
            |  in  u2 i_2;
            |  in  u3 i_3;
            |  out u1 o_1;
            |  out u2 o_2;
            |  out i2 os_2;
            |  out u4 o_4;
            |  out u8 o_8;
            |  out u16 o_16;
            |
            |  fence {
            |    $stmt;
            |  }
            |}"""
          } getFirst {
            case StmtAssign(_, rhs) => rhs.simplify
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages shouldBe empty
        }
      }
    }

    "no sliced or indexed refs when slice or index is not const" - {
      for {
        (stmt, pattern) <- List[(String, PartialFunction[Any, Unit])](
          // format: off
          ("o_1 = A[i_3]",       { case ExprIndex(ExprSym(Symbol("A")), ExprSym(Symbol("i_3"))) => }),
          ("o_2 = B[i_2]",       { case ExprIndex(ExprSym(Symbol("B")), ExprSym(Symbol("i_2"))) => }),
          ("o_2 = A[i_3 +: 2]",  { case ExprSlice(ExprSym(Symbol("A")), ExprSym(Symbol("i_3")), "+:", ExprInt(false, 4, w)) if w == 2 => }),
          ("o_4 = B[i_2 +: 2]",  { case ExprSlice(ExprSym(Symbol("B")), ExprSym(Symbol("i_2")), "+:", ExprInt(false, 2, w)) if w == 2 => }),
          ("o_2 = B[1:0][i_1]",  { case ExprIndex(ExprSym(Symbol("B")), ExprCat(List(ExprInt(false, 1, p), ExprSym(Symbol("i_1"))))) if p == 0 => }),
          ("o_2 = B[2:1][i_1]",  { case ExprIndex(ExprSym(Symbol("B")), ExprBinary(ExprInt(false, 2, l), "+", ExprCat(List(ExprInt(false, 1, p), ExprSym(Symbol("i_1")))))) if l == 1 && p == 0 => }),
          ("o_2 = B[2+:1][i_1]", { case ExprIndex(ExprSym(Symbol("B")), ExprBinary(ExprInt(false, 2, l), "+", ExprCat(List(ExprInt(false, 1, p), ExprSym(Symbol("i_1")))))) if l == 2 && p == 0 => })
          // format: on
        )
      } {
        stmt in {
          fold {
            s"""
            |fsm a {
            |  const u8    A = 8'b10101100;
            |  const u2[3] B = {2'd2, 2'd1, 2'd0};
            |
            |  in  u1 i_1;
            |  in  u2 i_2;
            |  in  u3 i_3;
            |  out u1 o_1;
            |  out u2 o_2;
            |  out u4 o_4;
            |
            |  fence {
            |    $stmt;
            |  }
            |}"""
          } getFirst {
            case StmtAssign(_, rhs) => rhs
          } tap {
            _ should matchPattern(pattern)
          }
        }
      }
    }

    "cast" - {
      for {
        (exprSrc, kindSrc, pattern, err) <-
          List[(String, String, PartialFunction[Any, Unit], List[String])](
            // format: off
          ("32", "u8", { case ExprInt(false, 8, v) if v == 32 => }, Nil),
          ("32s", "i8", { case ExprInt(true, 8, v) if v == 32=> }, Nil),
          ("-1s", "i8", { case ExprInt(true, 8, v) if v == -1 => }, Nil),
          ("32", "u4", { case ExprError() => }, "Value 32 cannot be represented with 4 unsigned bits" :: Nil),
          ("32s", "i4", { case ExprError() => }, "Value 32 cannot be represented with 4 signed bits" :: Nil),
          ("31", "u5", { case ExprInt(false, 5, v) if v == 31 => }, Nil),
          ("31s", "i5", { case ExprError() => }, "Value 31 cannot be represented with 5 signed bits" :: Nil),
          ("10'sd12",  "int", { case ExprNum(true, v) if v == 12 => }, Nil),
          ("10'd12",  "uint", { case ExprNum(false, v) if v == 12 => }, Nil),
          ("-10'sd1",  "int", { case ExprNum(true, v) if v == -1 => }, Nil),
          ("a",  "u10", { case ExprCat(List(ExprInt(false, 2, z), ExprSym(Symbol("a")))) if z == 0  => }, Nil),
          ("b",  "i10", { case ExprCall(
                                ExprSym(Symbol("$signed")),
                                List(ArgP(ExprCat(List(
                                      ExprRep(
                                        Expr(3),
                                        ExprIndex(
                                          ExprSym(Symbol("b")),
                                          ExprInt(false, 3, i))),
                                      ExprSym(Symbol("b"))))))) if i == 6 =>
          }, Nil),
          ("a",  "u8", { case ExprSym(Symbol("a")) => }, Nil),
          ("b",  "i7", { case ExprSym(Symbol("b")) => }, Nil),
          ("c",  "u10", { case ExprInt(false, 10, v) if v == 7 => }, Nil),
          ("c", "uint", { case ExprNum(false, v) if v == 7 => }, Nil),
          ("d",  "i10", { case ExprInt(true, 10, v) if v == -3 => }, Nil),
          ("d",  "int", { case ExprNum(true, v) if v == -3 => }, Nil)
          // format: on
          )
      } {
        s"($kindSrc)($exprSrc)" in {
          fold {
            s"""
            |fsm f {
            |  u8 a;
            |  i7 b;
            |  const u8 c =  7;
            |  const i8 d = -3s;
            |  void main() {
            |    $$display("", $exprSrc);
            |    fence;
            |  }
            |}"""
          } getFirst {
            case ExprCall(_, List(_, ArgP(e))) => e
          } tap { expr =>
            cc.messages shouldBe empty
            val kind = kindSrc.asTree[Expr] match {
              case ExprType(kind) => kind
              case _              => fail
            }
            ExprCast(kind, expr) withLoc Loc.synthetic rewrite {
              cc.simpifyExpr
            } should matchPattern(pattern)
            checkSingleError(err)
          }
        }
      }
    }

    "binary '" - {
      for {
        (expr, pattern) <- List[(String, PartialFunction[Any, Unit])](
          // format: off
          ("20'10'sd12", { case ExprInt(true, 20, v) if v == 12 => }),
          ("20'10'd12", { case ExprInt(false, 20, v) if v == 12 => }),
          ("20'-10'sd1", { case ExprInt(true, 20, v) if v == -1 => }),
          ("10'a", { case ExprCat(List(ExprInt(false, 2, z), ExprSym(Symbol("a")))) if z == 0 => }),
          ("10'b", { case ExprCall(
                            ExprSym(Symbol("$signed")),
                            List(ArgP(
                              ExprCat(List(
                                ExprRep(Expr(3), ExprIndex(ExprSym(Symbol("b")), ExprInt(false, 3, i))),
                                ExprSym(Symbol("b"))
                              ))
                            ))
                          ) if i == 6 => }),
          ("8'a", { case ExprSym(Symbol("a")) => }),
          ("7'b", { case ExprSym(Symbol("b")) => }),
          ("10'c", { case ExprInt(false, 10, v) if v == 7 => }),
          ("10'd", { case ExprInt(true, 10, v) if v == -3 => })
          // format: on
        )
      } {
        expr in {
          fold {
            s"""
               |fsm f {
               |  u8 a;
               |  i7 b;
               |  const u8 c =  7;
               |  const i8 d = -3s;
               |  void main() {
               |    $$display("", $expr);
               |    fence;
               |  }
               |}"""
          } getFirst {
            case ExprCall(_, List(_, ArgP(e))) => e
          } tap { expr =>
            cc.messages shouldBe empty
            expr should matchPattern(pattern)
          }
        }
      }
    }

    "reference to type" - {
      for {
        (decl, pattern) <- List[(String, PartialFunction[Any, Unit])](
          // format: off
          ("in bool_t x;",      { case ExprType(TypeUInt(w)) if w == 1 => }),
          ("in i2_t x;",        { case ExprType(TypeSInt(w)) if w == 2 => }),
          ("in v3u2_t x;",      { case ExprType(TypeVector(TypeUInt(w), s)) if w == 2 && s == 3=> }),
//          ("const int x = 0;",  { case ExprType(TypeNum(true)) => }), // These are dropped by Fold
//          ("const uint x = 0;", { case ExprType(TypeNum(false)) => }),// These are dropped by Fold
          ("in sync void_t x;", { case ExprType(TypeVoid) => }),
          ("in s_t x;",         { case ExprSym(Symbol("s_t")) => }),
          ("x = new e_t;",      { case ExprSym(Symbol("e_t")) => }),
          ("in ts_t x;",        { case ExprSym(Symbol("s_t")) => })
          // format: on
        )
      } {
        decl in {
          fold {
            s"""
            |struct s_t {
            |  bool x;
            |}
            |
            |network e_t {
            |}
            |
            |(* toplevel *)
            |network a {
            |  typedef bool  bool_t;
            |  typedef i2    i2_t;
            |  typedef u2[3] v3u2_t;
            |  typedef void  void_t;
            |  typedef s_t   ts_t;
            |
            |  $decl
            |}"""
          } getFirst {
            case DeclIn(_, spec, _)    => spec
            case DeclConst(_, spec)    => spec
            case DeclInstance(_, spec) => spec
          } tap {
            _ should matchPattern(pattern)
          }
        }
      }
    }
  }
}
