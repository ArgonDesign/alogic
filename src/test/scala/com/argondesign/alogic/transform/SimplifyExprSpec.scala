////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.transform

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Messages.Warning
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.frontend.Clarify
import com.argondesign.alogic.frontend.Complete
import com.argondesign.alogic.frontend.Finished
import com.argondesign.alogic.frontend.Frontend
import com.argondesign.alogic.passes._
import com.argondesign.alogic.ExprExtractors._
import com.argondesign.alogic.builtins.Builtins
import com.argondesign.alogic.core.Messages.Fatal
import com.argondesign.alogic.frontend.Elaborate
import org.scalatest.freespec.AnyFreeSpec

final class SimplifyExprSpec extends AnyFreeSpec with AlogicTest {

  implicit private val cc: CompilerContext = new CompilerContext

  private def fold(text: String): Thicket = Thicket {
    transformWithPass(
      FrontendPass andThen
        DropPackageAndParametrizedDescs andThen
        DescToDeclDefn andThen
        Fold,
      text
    ).value.iterator.toList flatMap {
      case (decl, defn) => List(decl, defn)
    }
  }

  private def simplify(text: String): Expr = {
    implicit val fe: Frontend = new Frontend
    val tree = text.asTree[Expr]()
    assert(cc.messages.forall(_.isInstanceOf[Warning]))
    val expr = Elaborate
      .elaborate(tree, Builtins.symbolTable)
      .proceed(e => { fe.typeCheck(e) map { _ => e } })
      .map(Clarify.apply)
      .pipe {
        case Complete(result) => result
        case Finished(result) => result
        case _                => fail()
      }
    expr.simplify
  }

  private def checkExact(tests: (String, Expr)*): Unit = tests foreach {
    case (text, result) => text in { simplify(text) shouldBe result }
  }

  private def checkPattern(tests: (String, PartialFunction[Any, Unit])*): Unit = tests foreach {
    case (text, pattern) => text in { simplify(text) should matchPattern(pattern) }
  }

  "FoldExpr should fold" - {
    "unary operators" - {
      "with a known operand" - {
        "+" - {
          checkExact(
            //// Unsized
            // - unsigned
            ("+(9)  ", ExprNum(false, 9)),
            ("+(0)  ", ExprNum(false, 0)),
            // - signed
            ("+( 9s)", ExprNum(true, 9)),
            ("+(-9s)", ExprNum(true, -9)),
            ("+( 0s)", ExprNum(true, 0)),
            ("+(-1s)", ExprNum(true, -1)),
            //// Sized
            // - unsigned
            ("+(8'd9)  ", ExprInt(false, 8, 9)),
            ("+(8'd0)  ", ExprInt(false, 8, 0)),
            ("+(8'd255)", ExprInt(false, 8, 255)),
            // -  signed
            ("+( 8'sd9)", ExprInt(true, 8, 9)),
            ("+(-8'sd9)", ExprInt(true, 8, -9)),
            ("+( 8'sd0)", ExprInt(true, 8, 0)),
            ("+(-8'sd1)", ExprInt(true, 8, -1))
          )
        }

        "-" - {
          checkExact(
            //// Unsized
            // - unsigned
            ("-(0)  ", ExprNum(false, 0)),
            // - signed
            ("-( 9s)", ExprNum(true, -9)),
            ("-(-9s)", ExprNum(true, 9)),
            ("-( 0s)", ExprNum(true, 0)),
            ("-(-1s)", ExprNum(true, 1)),
            //// Sized
            // - unsigned
            ("-(8'd9)  ", ExprInt(false, 8, 247)),
            ("-(8'd0)  ", ExprInt(false, 8, 0)),
            ("-(8'd255)", ExprInt(false, 8, 1)),
            // -  signed
            ("-( 8'sd9)", ExprInt(true, 8, -9)),
            ("-(-8'sd9)", ExprInt(true, 8, 9)),
            ("-( 8'sd0)", ExprInt(true, 8, 0)),
            ("-(-8'sd1)", ExprInt(true, 8, 1))
          )
        }

        "~" - {
          checkExact(
            //// Unsized
            // - unsigned
            // None are valid as they all yield negative values
            // - signed
            ("~( 9s)", ExprNum(true, -10)),
            ("~(-9s)", ExprNum(true, 8)),
            ("~( 0s)", ExprNum(true, -1)),
            ("~(-1s)", ExprNum(true, 0)),
            //// Sized
            // - unsigned
            ("~(8'd9)  ", ExprInt(false, 8, 246)),
            ("~(8'd0)  ", ExprInt(false, 8, 255)),
            ("~(8'd255)", ExprInt(false, 8, 0)),
            // -  signed
            ("~( 8'sd9)", ExprInt(true, 8, -10)),
            ("~(-8'sd9)", ExprInt(true, 8, 8)),
            ("~( 8'sd0)", ExprInt(true, 8, -1)),
            ("~(-8'sd1)", ExprInt(true, 8, 0))
          )
        }

        "&" - {
          checkExact(
            //// Unsized
            // - unsigned
            ("&(9)  ", ExprInt(false, 1, 0)),
            ("&(0)  ", ExprInt(false, 1, 0)),
            // - signed
            ("&( 9s)", ExprInt(false, 1, 0)),
            ("&(-9s)", ExprInt(false, 1, 0)),
            ("&( 0s)", ExprInt(false, 1, 0)),
            ("&(-1s)", ExprInt(false, 1, 1)),
            //// Sized
            // - unsigned
            ("&(8'd9)  ", ExprInt(false, 1, 0)),
            ("&(8'd0)  ", ExprInt(false, 1, 0)),
            ("&(8'd255)", ExprInt(false, 1, 1)),
            // -  signed
            ("&( 8'sd9)", ExprInt(false, 1, 0)),
            ("&(-8'sd9)", ExprInt(false, 1, 0)),
            ("&( 8'sd0)", ExprInt(false, 1, 0)),
            ("&(-8'sd1)", ExprInt(false, 1, 1))
          )
        }

        "|" - {
          checkExact(
            //// Unsized
            // - unsigned
            ("|(9)  ", ExprInt(false, 1, 1)),
            ("|(0)  ", ExprInt(false, 1, 0)),
            // - signed
            ("|( 9s)", ExprInt(false, 1, 1)),
            ("|(-9s)", ExprInt(false, 1, 1)),
            ("|( 0s)", ExprInt(false, 1, 0)),
            ("|(-1s)", ExprInt(false, 1, 1)),
            //// Sized
            // - unsigned
            ("|(8'd9)  ", ExprInt(false, 1, 1)),
            ("|(8'd0)  ", ExprInt(false, 1, 0)),
            ("|(8'd255)", ExprInt(false, 1, 1)),
            // -  signed
            ("|( 8'sd9)", ExprInt(false, 1, 1)),
            ("|(-8'sd9)", ExprInt(false, 1, 1)),
            ("|( 8'sd0)", ExprInt(false, 1, 0)),
            ("|(-8'sd1)", ExprInt(false, 1, 1))
          )
        }

        "^" - {
          checkExact(
            //// Unsized
            // - unsigned
            ("^(9) ", ExprInt(false, 1, 0)),
            ("^(8) ", ExprInt(false, 1, 1)),
            ("^(0) ", ExprInt(false, 1, 0)),
            // - signed
            ("^(9s)", ExprInt(false, 1, 0)),
            ("^(8s)", ExprInt(false, 1, 1)),
            ("^(0s)", ExprInt(false, 1, 0)),
            //// Sized
            // - unsigned
            ("^(8'd9)  ", ExprInt(false, 1, 0)),
            ("^(8'd8)  ", ExprInt(false, 1, 1)),
            ("^(8'd0)  ", ExprInt(false, 1, 0)),
            ("^(8'd255)", ExprInt(false, 1, 0)),
            // -  signed
            ("^( 8'sd9)", ExprInt(false, 1, 0)),
            ("^(-8'sd9)", ExprInt(false, 1, 1)),
            ("^( 8'sd0)", ExprInt(false, 1, 0)),
            ("^(-8'sd1)", ExprInt(false, 1, 0))
          )
        }

        "!" - {
          checkExact(
            // - unsigned
            ("!( 1'd0 )", ExprInt(false, 1, 1)),
            ("!( 1'd1 )", ExprInt(false, 1, 0)),
            // -  signed
            ("!( 1'sd0)", ExprInt(false, 1, 1)),
            ("!(-1'sd1)", ExprInt(false, 1, 0))
          )
        }
      }

      "special cases" - {
        "+ is identity" - {
          checkPattern(
            // unsigned
            ("+@unknownu(1)", { case UnknownU(1) => }),
            ("+@unknownu(2)", { case UnknownU(2) => }),
            // signed
            ("+@unknowni(1)", { case UnknownI(1) => }),
            ("+@unknowni(2)", { case UnknownI(2) => })
          )
        }

        "unary over 1 bit values" - {
          checkPattern(
            // unsigned
            ("-@unknownu(1)", { case UnknownU(1) => }),
            ("&@unknownu(1)", { case UnknownU(1) => }),
            ("|@unknownu(1)", { case UnknownU(1) => }),
            ("^@unknownu(1)", { case UnknownU(1) => }),
            // signed
            ("-@unknowni(1)", { case UnknownI(1) => }),
            ("&@unknowni(1)", { case Unsigned(UnknownI(1)) => }),
            ("|@unknowni(1)", { case Unsigned(UnknownI(1)) => }),
            ("^@unknowni(1)", { case Unsigned(UnknownI(1)) => })
          )
        }

        "unary over unary" - {
          checkPattern(
            // ~~
            ("~~@unknownu(4)", { case UnknownU(4) => }),
            ("~~@unknownu(1)", { case UnknownU(1) => }),
            ("~~@unknowni(4)", { case UnknownI(4) => }),
            ("~~@unknowni(1)", { case UnknownI(1) => }),
            // !!
            ("!!@unknownu(1)", { case UnknownU(1) => }),
            ("!!@unknowni(1)", { case Unsigned(UnknownI(1)) => }),
            // ! and ~ combinations
            ("~!@unknownu(1)", { case UnknownU(1) => }),
            ("!~@unknownu(1)", { case UnknownU(1) => }),
            ("~!@unknowni(1)", { case Unsigned(UnknownI(1)) => }),
            ("!~@unknowni(1)", { case Unsigned(UnknownI(1)) => }),
            // --
            ("-(-@unknownu(4))", { case UnknownU(4) => }),
            ("-(-@unknowni(4))", { case UnknownI(4) => })
          )
        }
      }
    }

    "binary operators" - {
      "with both operands known" - {
        "+" - {
          checkExact(
            //// Unsized
            // - unsigned unsigned
            (" 10  +  3 ", ExprNum(false, 13)),
            // - unsigned signed
            (" 10  +  3s", ExprNum(false, 13)),
            (" 10  + -3s", ExprNum(false, 7)),
            // - signed unsigned
            (" 10s +  3 ", ExprNum(false, 13)),
            // - signed signed
            (" 10s +  3s", ExprNum(true, 13)),
            (" 10s + -3s", ExprNum(true, 7)),
            ("-10s +  3s", ExprNum(true, -7)),
            ("-10s + -3s", ExprNum(true, -13)),
            //// Sized
            // - unsigned unsigned
            ("  8'd10  +   8'd3 ", ExprInt(false, 8, 13)),
            ("  8'd10  + -(8'd3)", ExprInt(false, 8, 7)),
            ("-(8'd10) +   8'd3 ", ExprInt(false, 8, 249)),
            ("-(8'd10) + -(8'd3)", ExprInt(false, 8, 243)),
            // - unsigned signed
            ("  8'd10  +   8'sd3", ExprInt(false, 8, 13)),
            ("  8'd10  +  -8'sd3", ExprInt(false, 8, 7)),
            ("-(8'd10) +   8'sd3", ExprInt(false, 8, 249)),
            ("-(8'd10) +  -8'sd3", ExprInt(false, 8, 243)),
            // - signed unsigned
            ("  8'sd10 +   8'd3 ", ExprInt(false, 8, 13)),
            ("  8'sd10 + -(8'd3)", ExprInt(false, 8, 7)),
            (" -8'sd10 +   8'd3 ", ExprInt(false, 8, 249)),
            (" -8'sd10 + -(8'd3)", ExprInt(false, 8, 243)),
            // - signed signed
            ("  8'sd10 +   8'sd3", ExprInt(true, 8, 13)),
            ("  8'sd10 +  -8'sd3", ExprInt(true, 8, 7)),
            (" -8'sd10 +   8'sd3", ExprInt(true, 8, -7)),
            (" -8'sd10 +  -8'sd3", ExprInt(true, 8, -13))
          )
        }

        "-" - {
          checkExact(
            //// Unsized
            // - unsigned unsigned
            (" 10  -  3 ", ExprNum(false, 7)),
            // - unsigned signed
            (" 10  -  3s", ExprNum(false, 7)),
            (" 10  - -3s", ExprNum(false, 13)),
            // - signed unsigned
            (" 10s -  3 ", ExprNum(false, 7)),
            // - signed signed
            (" 10s -  3s", ExprNum(true, 7)),
            (" 10s - -3s", ExprNum(true, 13)),
            ("-10s -  3s", ExprNum(true, -13)),
            ("-10s - -3s", ExprNum(true, -7)),
            //// Sized
            // - unsigned unsigned
            ("  8'd10  -   8'd3 ", ExprInt(false, 8, 7)),
            ("  8'd10  - -(8'd3)", ExprInt(false, 8, 13)),
            ("-(8'd10) -   8'd3 ", ExprInt(false, 8, 243)),
            ("-(8'd10) - -(8'd3)", ExprInt(false, 8, 249)),
            // - unsigned signed
            ("  8'd10  -   8'sd3", ExprInt(false, 8, 7)),
            ("  8'd10  -  -8'sd3", ExprInt(false, 8, 13)),
            ("-(8'd10) -   8'sd3", ExprInt(false, 8, 243)),
            ("-(8'd10) -  -8'sd3", ExprInt(false, 8, 249)),
            // - signed unsigned
            ("  8'sd10 -   8'd3 ", ExprInt(false, 8, 7)),
            ("  8'sd10 - -(8'd3)", ExprInt(false, 8, 13)),
            (" -8'sd10 -   8'd3 ", ExprInt(false, 8, 243)),
            (" -8'sd10 - -(8'd3)", ExprInt(false, 8, 249)),
            // - signed signed
            ("  8'sd10 -   8'sd3", ExprInt(true, 8, 7)),
            ("  8'sd10 -  -8'sd3", ExprInt(true, 8, 13)),
            (" -8'sd10 -   8'sd3", ExprInt(true, 8, -13)),
            (" -8'sd10 -  -8'sd3", ExprInt(true, 8, -7))
          )
        }

        "*" - {
          checkExact(
            //// Unsized
            // - unsigned unsigned
            (" 10  *  3 ", ExprNum(false, 30)),
            // - unsigned signed
            (" 10  *  3s", ExprNum(false, 30)),
            // - signed unsigned
            (" 10s *  3 ", ExprNum(false, 30)),
            // - signed signed
            (" 10s *  3s", ExprNum(true, 30)),
            (" 10s * -3s", ExprNum(true, -30)),
            ("-10s *  3s", ExprNum(true, -30)),
            ("-10s * -3s", ExprNum(true, 30)),
            //// Sized
            // - unsigned unsigned
            ("  8'd10  *   8'd3 ", ExprInt(false, 8, 30)),
            ("  8'd10  * -(8'd3)", ExprInt(false, 8, 226)),
            ("-(8'd10) *   8'd3 ", ExprInt(false, 8, 226)),
            ("-(8'd10) * -(8'd3)", ExprInt(false, 8, 30)),
            // - unsigned signed
            ("  8'd10  *   8'sd3", ExprInt(false, 8, 30)),
            ("  8'd10  *  -8'sd3", ExprInt(false, 8, 226)),
            ("-(8'd10) *   8'sd3", ExprInt(false, 8, 226)),
            ("-(8'd10) *  -8'sd3", ExprInt(false, 8, 30)),
            // - signed unsigned
            ("  8'sd10 *   8'd3 ", ExprInt(false, 8, 30)),
            ("  8'sd10 * -(8'd3)", ExprInt(false, 8, 226)),
            (" -8'sd10 *   8'd3 ", ExprInt(false, 8, 226)),
            (" -8'sd10 * -(8'd3)", ExprInt(false, 8, 30)),
            // - signed signed
            ("  8'sd10 *   8'sd3", ExprInt(true, 8, 30)),
            ("  8'sd10 *  -8'sd3", ExprInt(true, 8, -30)),
            (" -8'sd10 *   8'sd3", ExprInt(true, 8, -30)),
            (" -8'sd10 *  -8'sd3", ExprInt(true, 8, 30))
          )
        }

        "/" - {
          checkExact(
            //// Unsized
            // - unsigned unsigned
            (" 10  /  3 ", ExprNum(false, 3)),
            // - unsigned signed
            (" 10  /  3s", ExprNum(false, 3)),
            // - signed unsigned
            (" 10s /  3 ", ExprNum(false, 3)),
            // - signed signed
            (" 10s /  3s", ExprNum(true, 3)),
            (" 10s / -3s", ExprNum(true, -3)),
            ("-10s /  3s", ExprNum(true, -3)),
            ("-10s / -3s", ExprNum(true, 3)),
            //// Sized
            // - unsigned unsigned
            ("  8'd10  /   8'd3 ", ExprInt(false, 8, 3)),
            ("  8'd10  / -(8'd3)", ExprInt(false, 8, 0)),
            ("-(8'd10) /   8'd3 ", ExprInt(false, 8, 82)),
            ("-(8'd10) / -(8'd3)", ExprInt(false, 8, 0)),
            // - unsigned signed
            ("  8'd10  /   8'sd3", ExprInt(false, 8, 3)),
            ("  8'd10  /  -8'sd3", ExprInt(false, 8, 0)),
            ("-(8'd10) /   8'sd3", ExprInt(false, 8, 82)),
            ("-(8'd10) /  -8'sd3", ExprInt(false, 8, 0)),
            // - signed unsigned
            ("  8'sd10 /   8'd3 ", ExprInt(false, 8, 3)),
            ("  8'sd10 / -(8'd3)", ExprInt(false, 8, 0)),
            (" -8'sd10 /   8'd3 ", ExprInt(false, 8, 82)),
            (" -8'sd10 / -(8'd3)", ExprInt(false, 8, 0)),
            // - signed signed
            ("  8'sd10 /   8'sd3", ExprInt(true, 8, 3)),
            ("  8'sd10 /  -8'sd3", ExprInt(true, 8, -3)),
            (" -8'sd10 /   8'sd3", ExprInt(true, 8, -3)),
            (" -8'sd10 /  -8'sd3", ExprInt(true, 8, 3))
          )
        }

        "%" - {
          checkExact(
            // Unsized
            // - unsigned unsigned
            ("  10  %  3 ", ExprNum(false, 1)),
            // - unsigned signed
            ("  10  %  3s", ExprNum(false, 1)),
            // - signed unsigned
            ("  10s %  3 ", ExprNum(false, 1)),
            // - signed signed
            ("  10s %  3s", ExprNum(true, 1)),
            ("  10s % -3s", ExprNum(true, 1)),
            (" -10s %  3s", ExprNum(true, -1)),
            (" -10s % -3s", ExprNum(true, -1)),
            // Sized
            // - unsigned unsigned
            ("  8'd10  %   8'd3 ", ExprInt(false, 8, 1)),
            ("  8'd10  % -(8'd3)", ExprInt(false, 8, 10)),
            ("-(8'd10) %   8'd3 ", ExprInt(false, 8, 0)),
            ("-(8'd10) % -(8'd3)", ExprInt(false, 8, 246)),
            // - unsigned signed
            ("  8'd10  %   8'sd3", ExprInt(false, 8, 1)),
            ("  8'd10  %  -8'sd3", ExprInt(false, 8, 10)),
            ("-(8'd10) %   8'sd3", ExprInt(false, 8, 0)),
            ("-(8'd10) %  -8'sd3", ExprInt(false, 8, 246)),
            // - signed unsigned
            ("  8'sd10 %   8'd3 ", ExprInt(false, 8, 1)),
            ("  8'sd10 % -(8'd3)", ExprInt(false, 8, 10)),
            (" -8'sd10 %   8'd3 ", ExprInt(false, 8, 0)),
            (" -8'sd10 % -(8'd3)", ExprInt(false, 8, 246)),
            // - signed signed
            ("  8'sd10 %   8'sd3", ExprInt(true, 8, 1)),
            ("  8'sd10 %  -8'sd3", ExprInt(true, 8, 1)),
            (" -8'sd10 %   8'sd3", ExprInt(true, 8, -1)),
            (" -8'sd10 %  -8'sd3", ExprInt(true, 8, -1))
          )
        }

        "&" - {
          checkExact(
            //// Unsized
            // - unsigned unsigned
            (" 10  &  7 ", ExprNum(false, 2)),
            // - unsigned signed
            (" 10  &  7s", ExprNum(false, 2)),
            (" 10  & -7s", ExprNum(false, 8)),
            // - signed unsigned
            (" 10s &  7 ", ExprNum(false, 2)),
            ("-10s &  7 ", ExprNum(false, 6)),
            // - signed signed
            (" 10s &  7s", ExprNum(true, 2)),
            (" 10s & -7s", ExprNum(true, 8)),
            ("-10s &  7s", ExprNum(true, 6)),
            ("-10s & -7s", ExprNum(true, -16)),
            //// Sized
            // - unsigned unsigned
            ("  8'd10  &   8'd7 ", ExprInt(false, 8, 2)),
            ("  8'd10  & -(8'd7)", ExprInt(false, 8, 8)),
            ("-(8'd10) &   8'd7 ", ExprInt(false, 8, 6)),
            ("-(8'd10) & -(8'd7)", ExprInt(false, 8, 240)),
            // - unsigned signed
            ("  8'd10  &   8'sd7", ExprInt(false, 8, 2)),
            ("  8'd10  &  -8'sd7", ExprInt(false, 8, 8)),
            ("-(8'd10) &   8'sd7", ExprInt(false, 8, 6)),
            ("-(8'd10) &  -8'sd7", ExprInt(false, 8, 240)),
            // - signed unsigned
            ("  8'sd10 &   8'd7 ", ExprInt(false, 8, 2)),
            ("  8'sd10 & -(8'd7)", ExprInt(false, 8, 8)),
            (" -8'sd10 &   8'd7 ", ExprInt(false, 8, 6)),
            (" -8'sd10 & -(8'd7)", ExprInt(false, 8, 240)),
            // - signed signed
            ("  8'sd10 &   8'sd7", ExprInt(true, 8, 2)),
            ("  8'sd10 &  -8'sd7", ExprInt(true, 8, 8)),
            (" -8'sd10 &   8'sd7", ExprInt(true, 8, 6)),
            (" -8'sd10 &  -8'sd7", ExprInt(true, 8, -16))
          )
        }

        "|" - {
          checkExact(
            // Unsized
            // - unsigned unsigned
            ("  10  |  4 ", ExprNum(false, 14)),
            // - unsigned signed
            ("  10  |  4s", ExprNum(false, 14)),
            // - signed unsigned
            ("  10s |  4 ", ExprNum(false, 14)),
            // - signed signed
            ("  10s |  4s", ExprNum(true, 14)),
            ("  10s | -4s", ExprNum(true, -2)),
            (" -10s |  4s", ExprNum(true, -10)),
            (" -10s | -4s", ExprNum(true, -2)),
            // Sized
            // - unsigned unsigned
            ("  8'd10  |   8'd4 ", ExprInt(false, 8, 14)),
            ("  8'd10  | -(8'd4)", ExprInt(false, 8, 254)),
            ("-(8'd10) |   8'd4 ", ExprInt(false, 8, 246)),
            ("-(8'd10) | -(8'd4)", ExprInt(false, 8, 254)),
            // - unsigned signed
            ("  8'd10  |   8'sd4", ExprInt(false, 8, 14)),
            ("  8'd10  |  -8'sd4", ExprInt(false, 8, 254)),
            ("-(8'd10) |   8'sd4", ExprInt(false, 8, 246)),
            ("-(8'd10) |  -8'sd4", ExprInt(false, 8, 254)),
            // - signed unsigned
            ("  8'sd10 |   8'd4 ", ExprInt(false, 8, 14)),
            ("  8'sd10 | -(8'd4)", ExprInt(false, 8, 254)),
            (" -8'sd10 |   8'd4 ", ExprInt(false, 8, 246)),
            (" -8'sd10 | -(8'd4)", ExprInt(false, 8, 254)),
            // - signed signed
            ("  8'sd10 |   8'sd4", ExprInt(true, 8, 14)),
            ("  8'sd10 |  -8'sd4", ExprInt(true, 8, -2)),
            (" -8'sd10 |   8'sd4", ExprInt(true, 8, -10)),
            (" -8'sd10 |  -8'sd4", ExprInt(true, 8, -2))
          )
        }

        "^" - {
          checkExact(
            // Unsized
            // - unsigned unsigned
            ("  10  ^  4 ", ExprNum(false, 14)),
            // - unsigned signed
            ("  10  ^  4s", ExprNum(false, 14)),
            // - signed unsigned
            ("  10s ^  4 ", ExprNum(false, 14)),
            // - signed signed
            ("  10s ^  4s", ExprNum(true, 14)),
            ("  10s ^ -4s", ExprNum(true, -10)),
            (" -10s ^  4s", ExprNum(true, -14)),
            (" -10s ^ -4s", ExprNum(true, 10)),
            // Sized
            // - unsigned unsigned
            ("  8'd10  ^   8'd4 ", ExprInt(false, 8, 14)),
            ("  8'd10  ^ -(8'd4)", ExprInt(false, 8, 246)),
            ("-(8'd10) ^   8'd4 ", ExprInt(false, 8, 242)),
            ("-(8'd10) ^ -(8'd4)", ExprInt(false, 8, 10)),
            // - unsigned signed
            ("  8'd10  ^   8'sd4", ExprInt(false, 8, 14)),
            ("  8'd10  ^  -8'sd4", ExprInt(false, 8, 246)),
            ("-(8'd10) ^   8'sd4", ExprInt(false, 8, 242)),
            ("-(8'd10) ^  -8'sd4", ExprInt(false, 8, 10)),
            // - signed unsigned
            ("  8'sd10 ^   8'd4 ", ExprInt(false, 8, 14)),
            ("  8'sd10 ^ -(8'd4)", ExprInt(false, 8, 246)),
            (" -8'sd10 ^   8'd4 ", ExprInt(false, 8, 242)),
            (" -8'sd10 ^ -(8'd4)", ExprInt(false, 8, 10)),
            // - signed signed
            ("  8'sd10 ^   8'sd4", ExprInt(true, 8, 14)),
            ("  8'sd10 ^  -8'sd4", ExprInt(true, 8, -10)),
            (" -8'sd10 ^   8'sd4", ExprInt(true, 8, -14)),
            (" -8'sd10 ^  -8'sd4", ExprInt(true, 8, 10))
          )
        }

        ">" - {
          checkExact(
            //// Unsized
            // - unsigned unsigned
            (" 10  >   7 ", ExprInt(false, 1, 1)),
            ("  7  >  10 ", ExprInt(false, 1, 0)),
            ("  5  >   5 ", ExprInt(false, 1, 0)),
            // - unsigned signed
            (" 10  >  7s ", ExprInt(false, 1, 1)),
            (" 10  > -7s ", ExprInt(false, 1, 1)),
            ("  7  >  10s", ExprInt(false, 1, 0)),
            ("  7  > -10s", ExprInt(false, 1, 1)),
            ("  5  >  5s ", ExprInt(false, 1, 0)),
            // - signed unsigned
            (" 10s >   7 ", ExprInt(false, 1, 1)),
            ("-10s >   7 ", ExprInt(false, 1, 0)),
            ("  7s >  10 ", ExprInt(false, 1, 0)),
            (" -7s >  10 ", ExprInt(false, 1, 0)),
            ("  5s >   5 ", ExprInt(false, 1, 0)),
            // - signed signed
            (" 10s >  7s ", ExprInt(false, 1, 1)),
            (" 10s > -7s ", ExprInt(false, 1, 1)),
            ("-10s >  7s ", ExprInt(false, 1, 0)),
            ("-10s > -7s ", ExprInt(false, 1, 0)),
            ("  7s >  10s", ExprInt(false, 1, 0)),
            ("  7s > -10s", ExprInt(false, 1, 1)),
            (" -7s >  10s", ExprInt(false, 1, 0)),
            (" -7s > -10s", ExprInt(false, 1, 1)),
            ("  5s >  5s ", ExprInt(false, 1, 0)),
            (" -5s > -5s ", ExprInt(false, 1, 0)),
            //// Sized
            // - unsigned unsigned
            ("  8'd10  >   8'd7  ", ExprInt(false, 1, 1)),
            ("  8'd10  > -(8'd7) ", ExprInt(false, 1, 0)),
            ("-(8'd10) >   8'd7  ", ExprInt(false, 1, 1)),
            ("-(8'd10) > -(8'd7) ", ExprInt(false, 1, 0)),
            ("  8'd7   >   8'd10 ", ExprInt(false, 1, 0)),
            ("  8'd7   > -(8'd10)", ExprInt(false, 1, 0)),
            ("-(8'd7)  >   8'd10 ", ExprInt(false, 1, 1)),
            ("-(8'd7)  > -(8'd10)", ExprInt(false, 1, 1)),
            ("  8'd5   >   8'd5  ", ExprInt(false, 1, 0)),
            // - unsigned signed
            ("  8'd10  >   8'sd7 ", ExprInt(false, 1, 1)),
            ("  8'd10  >  -8'sd7 ", ExprInt(false, 1, 0)),
            ("-(8'd10) >   8'sd7 ", ExprInt(false, 1, 1)),
            ("-(8'd10) >  -8'sd7 ", ExprInt(false, 1, 0)),
            ("  8'd7   >   8'sd10", ExprInt(false, 1, 0)),
            ("  8'd7   >  -8'sd10", ExprInt(false, 1, 0)),
            ("-(8'd7)  >   8'sd10", ExprInt(false, 1, 1)),
            ("-(8'd7)  >  -8'sd10", ExprInt(false, 1, 1)),
            ("  8'd5   >   8'sd5 ", ExprInt(false, 1, 0)),
            // - signed unsigned
            ("  8'sd10 >   8'd7  ", ExprInt(false, 1, 1)),
            ("  8'sd10 > -(8'd7) ", ExprInt(false, 1, 0)),
            (" -8'sd10 >   8'd7  ", ExprInt(false, 1, 1)),
            (" -8'sd10 > -(8'd7) ", ExprInt(false, 1, 0)),
            ("  8'sd7  >   8'd10 ", ExprInt(false, 1, 0)),
            ("  8'sd7  > -(8'd10)", ExprInt(false, 1, 0)),
            (" -8'sd7  >   8'd10 ", ExprInt(false, 1, 1)),
            (" -8'sd7  > -(8'd10)", ExprInt(false, 1, 1)),
            ("  8'sd5  >   8'd5  ", ExprInt(false, 1, 0)),
            // - signed signed
            ("  8'sd10 >   8'sd7 ", ExprInt(false, 1, 1)),
            ("  8'sd10 >  -8'sd7 ", ExprInt(false, 1, 1)),
            (" -8'sd10 >   8'sd7 ", ExprInt(false, 1, 0)),
            (" -8'sd10 >  -8'sd7 ", ExprInt(false, 1, 0)),
            ("  8'sd7  >   8'sd10", ExprInt(false, 1, 0)),
            ("  8'sd7  >  -8'sd10", ExprInt(false, 1, 1)),
            (" -8'sd7  >   8'sd10", ExprInt(false, 1, 0)),
            (" -8'sd7  >  -8'sd10", ExprInt(false, 1, 1)),
            ("  8'sd5  >   8'sd5 ", ExprInt(false, 1, 0)),
            (" -8'sd5  >  -8'sd5 ", ExprInt(false, 1, 0))
          )
        }

        ">=" - {
          checkExact(
            //// Unsized
            // - unsigned unsigned
            (" 10  >=   7 ", ExprInt(false, 1, 1)),
            ("  7  >=  10 ", ExprInt(false, 1, 0)),
            ("  5  >=   5 ", ExprInt(false, 1, 1)),
            // - unsigned signed
            (" 10  >=  7s ", ExprInt(false, 1, 1)),
            (" 10  >= -7s ", ExprInt(false, 1, 1)),
            ("  7  >=  10s", ExprInt(false, 1, 0)),
            ("  7  >= -10s", ExprInt(false, 1, 1)),
            ("  5  >=  5s ", ExprInt(false, 1, 1)),
            // - signed unsigned
            (" 10s >=   7 ", ExprInt(false, 1, 1)),
            ("-10s >=   7 ", ExprInt(false, 1, 0)),
            ("  7s >=  10 ", ExprInt(false, 1, 0)),
            (" -7s >=  10 ", ExprInt(false, 1, 0)),
            ("  5s >=   5 ", ExprInt(false, 1, 1)),
            // - signed signed
            (" 10s >=  7s ", ExprInt(false, 1, 1)),
            (" 10s >= -7s ", ExprInt(false, 1, 1)),
            ("-10s >=  7s ", ExprInt(false, 1, 0)),
            ("-10s >= -7s ", ExprInt(false, 1, 0)),
            ("  7s >=  10s", ExprInt(false, 1, 0)),
            ("  7s >= -10s", ExprInt(false, 1, 1)),
            (" -7s >=  10s", ExprInt(false, 1, 0)),
            (" -7s >= -10s", ExprInt(false, 1, 1)),
            ("  5s >=  5s ", ExprInt(false, 1, 1)),
            (" -5s >= -5s ", ExprInt(false, 1, 1)),
            //// Sized
            // - unsigned unsigned
            ("  8'd10  >=   8'd7  ", ExprInt(false, 1, 1)),
            ("  8'd10  >= -(8'd7) ", ExprInt(false, 1, 0)),
            ("-(8'd10) >=   8'd7  ", ExprInt(false, 1, 1)),
            ("-(8'd10) >= -(8'd7) ", ExprInt(false, 1, 0)),
            ("  8'd7   >=   8'd10 ", ExprInt(false, 1, 0)),
            ("  8'd7   >= -(8'd10)", ExprInt(false, 1, 0)),
            ("-(8'd7)  >=   8'd10 ", ExprInt(false, 1, 1)),
            ("-(8'd7)  >= -(8'd10)", ExprInt(false, 1, 1)),
            ("  8'd5   >=   8'd5  ", ExprInt(false, 1, 1)),
            // - unsigned signed
            ("  8'd10  >=   8'sd7 ", ExprInt(false, 1, 1)),
            ("  8'd10  >=  -8'sd7 ", ExprInt(false, 1, 0)),
            ("-(8'd10) >=   8'sd7 ", ExprInt(false, 1, 1)),
            ("-(8'd10) >=  -8'sd7 ", ExprInt(false, 1, 0)),
            ("  8'd7   >=   8'sd10", ExprInt(false, 1, 0)),
            ("  8'd7   >=  -8'sd10", ExprInt(false, 1, 0)),
            ("-(8'd7)  >=   8'sd10", ExprInt(false, 1, 1)),
            ("-(8'd7)  >=  -8'sd10", ExprInt(false, 1, 1)),
            ("  8'd5   >=   8'sd5 ", ExprInt(false, 1, 1)),
            // - signed unsigned
            ("  8'sd10 >=   8'd7  ", ExprInt(false, 1, 1)),
            ("  8'sd10 >= -(8'd7) ", ExprInt(false, 1, 0)),
            (" -8'sd10 >=   8'd7  ", ExprInt(false, 1, 1)),
            (" -8'sd10 >= -(8'd7) ", ExprInt(false, 1, 0)),
            ("  8'sd7  >=   8'd10 ", ExprInt(false, 1, 0)),
            ("  8'sd7  >= -(8'd10)", ExprInt(false, 1, 0)),
            (" -8'sd7  >=   8'd10 ", ExprInt(false, 1, 1)),
            (" -8'sd7  >= -(8'd10)", ExprInt(false, 1, 1)),
            ("  8'sd5  >=   8'd5  ", ExprInt(false, 1, 1)),
            // - signed signed
            ("  8'sd10 >=   8'sd7 ", ExprInt(false, 1, 1)),
            ("  8'sd10 >=  -8'sd7 ", ExprInt(false, 1, 1)),
            (" -8'sd10 >=   8'sd7 ", ExprInt(false, 1, 0)),
            (" -8'sd10 >=  -8'sd7 ", ExprInt(false, 1, 0)),
            ("  8'sd7  >=   8'sd10", ExprInt(false, 1, 0)),
            ("  8'sd7  >=  -8'sd10", ExprInt(false, 1, 1)),
            (" -8'sd7  >=   8'sd10", ExprInt(false, 1, 0)),
            (" -8'sd7  >=  -8'sd10", ExprInt(false, 1, 1)),
            ("  8'sd5  >=   8'sd5 ", ExprInt(false, 1, 1)),
            (" -8'sd5  >=  -8'sd5 ", ExprInt(false, 1, 1))
          )
        }

        "<" - {
          checkExact(
            //// Unsized
            // - unsigned unsigned
            (" 10  <   7 ", ExprInt(false, 1, 0)),
            ("  7  <  10 ", ExprInt(false, 1, 1)),
            ("  5  <   5 ", ExprInt(false, 1, 0)),
            // - unsigned signed
            (" 10  <  7s ", ExprInt(false, 1, 0)),
            (" 10  < -7s ", ExprInt(false, 1, 0)),
            ("  7  <  10s", ExprInt(false, 1, 1)),
            ("  7  < -10s", ExprInt(false, 1, 0)),
            ("  5  <  5s ", ExprInt(false, 1, 0)),
            // - signed unsigned
            (" 10s <   7 ", ExprInt(false, 1, 0)),
            ("-10s <   7 ", ExprInt(false, 1, 1)),
            ("  7s <  10 ", ExprInt(false, 1, 1)),
            (" -7s <  10 ", ExprInt(false, 1, 1)),
            ("  5s <   5 ", ExprInt(false, 1, 0)),
            // - signed signed
            (" 10s <  7s ", ExprInt(false, 1, 0)),
            (" 10s < -7s ", ExprInt(false, 1, 0)),
            ("-10s <  7s ", ExprInt(false, 1, 1)),
            ("-10s < -7s ", ExprInt(false, 1, 1)),
            ("  7s <  10s", ExprInt(false, 1, 1)),
            ("  7s < -10s", ExprInt(false, 1, 0)),
            (" -7s <  10s", ExprInt(false, 1, 1)),
            (" -7s < -10s", ExprInt(false, 1, 0)),
            ("  5s <  5s ", ExprInt(false, 1, 0)),
            (" -5s < -5s ", ExprInt(false, 1, 0)),
            //// Sized
            // - unsigned unsigned
            ("  8'd10  <   8'd7  ", ExprInt(false, 1, 0)),
            ("  8'd10  < -(8'd7) ", ExprInt(false, 1, 1)),
            ("-(8'd10) <   8'd7  ", ExprInt(false, 1, 0)),
            ("-(8'd10) < -(8'd7) ", ExprInt(false, 1, 1)),
            ("  8'd7   <   8'd10 ", ExprInt(false, 1, 1)),
            ("  8'd7   < -(8'd10)", ExprInt(false, 1, 1)),
            ("-(8'd7)  <   8'd10 ", ExprInt(false, 1, 0)),
            ("-(8'd7)  < -(8'd10)", ExprInt(false, 1, 0)),
            ("  8'd5   <   8'd5  ", ExprInt(false, 1, 0)),
            // - unsigned signed
            ("  8'd10  <   8'sd7 ", ExprInt(false, 1, 0)),
            ("  8'd10  <  -8'sd7 ", ExprInt(false, 1, 1)),
            ("-(8'd10) <   8'sd7 ", ExprInt(false, 1, 0)),
            ("-(8'd10) <  -8'sd7 ", ExprInt(false, 1, 1)),
            ("  8'd7   <   8'sd10", ExprInt(false, 1, 1)),
            ("  8'd7   <  -8'sd10", ExprInt(false, 1, 1)),
            ("-(8'd7)  <   8'sd10", ExprInt(false, 1, 0)),
            ("-(8'd7)  <  -8'sd10", ExprInt(false, 1, 0)),
            ("  8'd5   <   8'sd5 ", ExprInt(false, 1, 0)),
            // - signed unsigned
            ("  8'sd10 <   8'd7  ", ExprInt(false, 1, 0)),
            ("  8'sd10 < -(8'd7) ", ExprInt(false, 1, 1)),
            (" -8'sd10 <   8'd7  ", ExprInt(false, 1, 0)),
            (" -8'sd10 < -(8'd7) ", ExprInt(false, 1, 1)),
            ("  8'sd7  <   8'd10 ", ExprInt(false, 1, 1)),
            ("  8'sd7  < -(8'd10)", ExprInt(false, 1, 1)),
            (" -8'sd7  <   8'd10 ", ExprInt(false, 1, 0)),
            (" -8'sd7  < -(8'd10)", ExprInt(false, 1, 0)),
            ("  8'sd5  <   8'd5  ", ExprInt(false, 1, 0)),
            // - signed signed
            ("  8'sd10 <   8'sd7 ", ExprInt(false, 1, 0)),
            ("  8'sd10 <  -8'sd7 ", ExprInt(false, 1, 0)),
            (" -8'sd10 <   8'sd7 ", ExprInt(false, 1, 1)),
            (" -8'sd10 <  -8'sd7 ", ExprInt(false, 1, 1)),
            ("  8'sd7  <   8'sd10", ExprInt(false, 1, 1)),
            ("  8'sd7  <  -8'sd10", ExprInt(false, 1, 0)),
            (" -8'sd7  <   8'sd10", ExprInt(false, 1, 1)),
            (" -8'sd7  <  -8'sd10", ExprInt(false, 1, 0)),
            ("  8'sd5  <   8'sd5 ", ExprInt(false, 1, 0)),
            (" -8'sd5  <  -8'sd5 ", ExprInt(false, 1, 0))
          )
        }

        "<=" - {
          checkExact(
            //// Unsized
            // - unsigned unsigned
            (" 10  <=   7 ", ExprInt(false, 1, 0)),
            ("  7  <=  10 ", ExprInt(false, 1, 1)),
            ("  5  <=   5 ", ExprInt(false, 1, 1)),
            // - unsigned signed
            (" 10  <=  7s ", ExprInt(false, 1, 0)),
            (" 10  <= -7s ", ExprInt(false, 1, 0)),
            ("  7  <=  10s", ExprInt(false, 1, 1)),
            ("  7  <= -10s", ExprInt(false, 1, 0)),
            ("  5  <=  5s ", ExprInt(false, 1, 1)),
            // - signed unsigned
            (" 10s <=   7 ", ExprInt(false, 1, 0)),
            ("-10s <=   7 ", ExprInt(false, 1, 1)),
            ("  7s <=  10 ", ExprInt(false, 1, 1)),
            (" -7s <=  10 ", ExprInt(false, 1, 1)),
            ("  5s <=   5 ", ExprInt(false, 1, 1)),
            // - signed signed
            (" 10s <=  7s ", ExprInt(false, 1, 0)),
            (" 10s <= -7s ", ExprInt(false, 1, 0)),
            ("-10s <=  7s ", ExprInt(false, 1, 1)),
            ("-10s <= -7s ", ExprInt(false, 1, 1)),
            ("  7s <=  10s", ExprInt(false, 1, 1)),
            ("  7s <= -10s", ExprInt(false, 1, 0)),
            (" -7s <=  10s", ExprInt(false, 1, 1)),
            (" -7s <= -10s", ExprInt(false, 1, 0)),
            ("  5s <=  5s ", ExprInt(false, 1, 1)),
            (" -5s <= -5s ", ExprInt(false, 1, 1)),
            //// Sized
            // - unsigned unsigned
            ("  8'd10  <=   8'd7  ", ExprInt(false, 1, 0)),
            ("  8'd10  <= -(8'd7) ", ExprInt(false, 1, 1)),
            ("-(8'd10) <=   8'd7  ", ExprInt(false, 1, 0)),
            ("-(8'd10) <= -(8'd7) ", ExprInt(false, 1, 1)),
            ("  8'd7   <=   8'd10 ", ExprInt(false, 1, 1)),
            ("  8'd7   <= -(8'd10)", ExprInt(false, 1, 1)),
            ("-(8'd7)  <=   8'd10 ", ExprInt(false, 1, 0)),
            ("-(8'd7)  <= -(8'd10)", ExprInt(false, 1, 0)),
            ("  8'd5   <=   8'd5  ", ExprInt(false, 1, 1)),
            // - unsigned signed
            ("  8'd10  <=   8'sd7 ", ExprInt(false, 1, 0)),
            ("  8'd10  <=  -8'sd7 ", ExprInt(false, 1, 1)),
            ("-(8'd10) <=   8'sd7 ", ExprInt(false, 1, 0)),
            ("-(8'd10) <=  -8'sd7 ", ExprInt(false, 1, 1)),
            ("  8'd7   <=   8'sd10", ExprInt(false, 1, 1)),
            ("  8'd7   <=  -8'sd10", ExprInt(false, 1, 1)),
            ("-(8'd7)  <=   8'sd10", ExprInt(false, 1, 0)),
            ("-(8'd7)  <=  -8'sd10", ExprInt(false, 1, 0)),
            ("  8'd5   <=   8'sd5 ", ExprInt(false, 1, 1)),
            // - signed unsigned
            ("  8'sd10 <=   8'd7  ", ExprInt(false, 1, 0)),
            ("  8'sd10 <= -(8'd7) ", ExprInt(false, 1, 1)),
            (" -8'sd10 <=   8'd7  ", ExprInt(false, 1, 0)),
            (" -8'sd10 <= -(8'd7) ", ExprInt(false, 1, 1)),
            ("  8'sd7  <=   8'd10 ", ExprInt(false, 1, 1)),
            ("  8'sd7  <= -(8'd10)", ExprInt(false, 1, 1)),
            (" -8'sd7  <=   8'd10 ", ExprInt(false, 1, 0)),
            (" -8'sd7  <= -(8'd10)", ExprInt(false, 1, 0)),
            ("  8'sd5  <=   8'd5  ", ExprInt(false, 1, 1)),
            // - signed signed
            ("  8'sd10 <=   8'sd7 ", ExprInt(false, 1, 0)),
            ("  8'sd10 <=  -8'sd7 ", ExprInt(false, 1, 0)),
            (" -8'sd10 <=   8'sd7 ", ExprInt(false, 1, 1)),
            (" -8'sd10 <=  -8'sd7 ", ExprInt(false, 1, 1)),
            ("  8'sd7  <=   8'sd10", ExprInt(false, 1, 1)),
            ("  8'sd7  <=  -8'sd10", ExprInt(false, 1, 0)),
            (" -8'sd7  <=   8'sd10", ExprInt(false, 1, 1)),
            (" -8'sd7  <=  -8'sd10", ExprInt(false, 1, 0)),
            ("  8'sd5  <=   8'sd5 ", ExprInt(false, 1, 1)),
            (" -8'sd5  <=  -8'sd5 ", ExprInt(false, 1, 1))
          )
        }

        "==" - {
          checkExact(
            //// Unsized
            // - unsigned unsigned
            (" 10  ==   7 ", ExprInt(false, 1, 0)),
            ("  7  ==  10 ", ExprInt(false, 1, 0)),
            ("  5  ==   5 ", ExprInt(false, 1, 1)),
            // - unsigned signed
            (" 10  ==  7s ", ExprInt(false, 1, 0)),
            (" 10  == -7s ", ExprInt(false, 1, 0)),
            ("  7  ==  10s", ExprInt(false, 1, 0)),
            ("  7  == -10s", ExprInt(false, 1, 0)),
            ("  5  ==  5s ", ExprInt(false, 1, 1)),
            // - signed unsigned
            (" 10s ==   7 ", ExprInt(false, 1, 0)),
            ("-10s ==   7 ", ExprInt(false, 1, 0)),
            ("  7s ==  10 ", ExprInt(false, 1, 0)),
            (" -7s ==  10 ", ExprInt(false, 1, 0)),
            ("  5s ==   5 ", ExprInt(false, 1, 1)),
            // - signed signed
            (" 10s ==  7s ", ExprInt(false, 1, 0)),
            (" 10s == -7s ", ExprInt(false, 1, 0)),
            ("-10s ==  7s ", ExprInt(false, 1, 0)),
            ("-10s == -7s ", ExprInt(false, 1, 0)),
            ("  7s ==  10s", ExprInt(false, 1, 0)),
            ("  7s == -10s", ExprInt(false, 1, 0)),
            (" -7s ==  10s", ExprInt(false, 1, 0)),
            (" -7s == -10s", ExprInt(false, 1, 0)),
            ("  5s ==  5s ", ExprInt(false, 1, 1)),
            (" -5s == -5s ", ExprInt(false, 1, 1)),
            //// Sized
            // - unsigned unsigned
            ("  8'd10  ==   8'd7  ", ExprInt(false, 1, 0)),
            ("  8'd10  == -(8'd7) ", ExprInt(false, 1, 0)),
            ("-(8'd10) ==   8'd7  ", ExprInt(false, 1, 0)),
            ("-(8'd10) == -(8'd7) ", ExprInt(false, 1, 0)),
            ("  8'd7   ==   8'd10 ", ExprInt(false, 1, 0)),
            ("  8'd7   == -(8'd10)", ExprInt(false, 1, 0)),
            ("-(8'd7)  ==   8'd10 ", ExprInt(false, 1, 0)),
            ("-(8'd7)  == -(8'd10)", ExprInt(false, 1, 0)),
            ("  8'd5   ==   8'd5  ", ExprInt(false, 1, 1)),
            // - unsigned signed
            ("  8'd10  ==   8'sd7 ", ExprInt(false, 1, 0)),
            ("  8'd10  ==  -8'sd7 ", ExprInt(false, 1, 0)),
            ("-(8'd10) ==   8'sd7 ", ExprInt(false, 1, 0)),
            ("-(8'd10) ==  -8'sd7 ", ExprInt(false, 1, 0)),
            ("  8'd7   ==   8'sd10", ExprInt(false, 1, 0)),
            ("  8'd7   ==  -8'sd10", ExprInt(false, 1, 0)),
            ("-(8'd7)  ==   8'sd10", ExprInt(false, 1, 0)),
            ("-(8'd7)  ==  -8'sd10", ExprInt(false, 1, 0)),
            ("  8'd5   ==   8'sd5 ", ExprInt(false, 1, 1)),
            // - signed unsigned
            ("  8'sd10 ==   8'd7  ", ExprInt(false, 1, 0)),
            ("  8'sd10 == -(8'd7) ", ExprInt(false, 1, 0)),
            (" -8'sd10 ==   8'd7  ", ExprInt(false, 1, 0)),
            (" -8'sd10 == -(8'd7) ", ExprInt(false, 1, 0)),
            ("  8'sd7  ==   8'd10 ", ExprInt(false, 1, 0)),
            ("  8'sd7  == -(8'd10)", ExprInt(false, 1, 0)),
            (" -8'sd7  ==   8'd10 ", ExprInt(false, 1, 0)),
            (" -8'sd7  == -(8'd10)", ExprInt(false, 1, 0)),
            ("  8'sd5  ==   8'd5  ", ExprInt(false, 1, 1)),
            // - signed signed
            ("  8'sd10 ==   8'sd7 ", ExprInt(false, 1, 0)),
            ("  8'sd10 ==  -8'sd7 ", ExprInt(false, 1, 0)),
            (" -8'sd10 ==   8'sd7 ", ExprInt(false, 1, 0)),
            (" -8'sd10 ==  -8'sd7 ", ExprInt(false, 1, 0)),
            ("  8'sd7  ==   8'sd10", ExprInt(false, 1, 0)),
            ("  8'sd7  ==  -8'sd10", ExprInt(false, 1, 0)),
            (" -8'sd7  ==   8'sd10", ExprInt(false, 1, 0)),
            (" -8'sd7  ==  -8'sd10", ExprInt(false, 1, 0)),
            ("  8'sd5  ==   8'sd5 ", ExprInt(false, 1, 1)),
            (" -8'sd5  ==  -8'sd5 ", ExprInt(false, 1, 1))
          )
        }

        "!=" - {
          checkExact(
            //// Unsized
            // - unsigned unsigned
            (" 10  !=   7 ", ExprInt(false, 1, 1)),
            ("  7  !=  10 ", ExprInt(false, 1, 1)),
            ("  5  !=   5 ", ExprInt(false, 1, 0)),
            // - unsigned signed
            (" 10  !=  7s ", ExprInt(false, 1, 1)),
            (" 10  != -7s ", ExprInt(false, 1, 1)),
            ("  7  !=  10s", ExprInt(false, 1, 1)),
            ("  7  != -10s", ExprInt(false, 1, 1)),
            ("  5  !=  5s ", ExprInt(false, 1, 0)),
            // - signed unsigned
            (" 10s !=   7 ", ExprInt(false, 1, 1)),
            ("-10s !=   7 ", ExprInt(false, 1, 1)),
            ("  7s !=  10 ", ExprInt(false, 1, 1)),
            (" -7s !=  10 ", ExprInt(false, 1, 1)),
            ("  5s !=   5 ", ExprInt(false, 1, 0)),
            // - signed signed
            (" 10s !=  7s ", ExprInt(false, 1, 1)),
            (" 10s != -7s ", ExprInt(false, 1, 1)),
            ("-10s !=  7s ", ExprInt(false, 1, 1)),
            ("-10s != -7s ", ExprInt(false, 1, 1)),
            ("  7s !=  10s", ExprInt(false, 1, 1)),
            ("  7s != -10s", ExprInt(false, 1, 1)),
            (" -7s !=  10s", ExprInt(false, 1, 1)),
            (" -7s != -10s", ExprInt(false, 1, 1)),
            ("  5s !=  5s ", ExprInt(false, 1, 0)),
            (" -5s != -5s ", ExprInt(false, 1, 0)),
            //// Sized
            // - unsigned unsigned
            ("  8'd10  !=   8'd7  ", ExprInt(false, 1, 1)),
            ("  8'd10  != -(8'd7) ", ExprInt(false, 1, 1)),
            ("-(8'd10) !=   8'd7  ", ExprInt(false, 1, 1)),
            ("-(8'd10) != -(8'd7) ", ExprInt(false, 1, 1)),
            ("  8'd7   !=   8'd10 ", ExprInt(false, 1, 1)),
            ("  8'd7   != -(8'd10)", ExprInt(false, 1, 1)),
            ("-(8'd7)  !=   8'd10 ", ExprInt(false, 1, 1)),
            ("-(8'd7)  != -(8'd10)", ExprInt(false, 1, 1)),
            ("  8'd5   !=   8'd5  ", ExprInt(false, 1, 0)),
            // - unsigned signed
            ("  8'd10  !=   8'sd7 ", ExprInt(false, 1, 1)),
            ("  8'd10  !=  -8'sd7 ", ExprInt(false, 1, 1)),
            ("-(8'd10) !=   8'sd7 ", ExprInt(false, 1, 1)),
            ("-(8'd10) !=  -8'sd7 ", ExprInt(false, 1, 1)),
            ("  8'd7   !=   8'sd10", ExprInt(false, 1, 1)),
            ("  8'd7   !=  -8'sd10", ExprInt(false, 1, 1)),
            ("-(8'd7)  !=   8'sd10", ExprInt(false, 1, 1)),
            ("-(8'd7)  !=  -8'sd10", ExprInt(false, 1, 1)),
            ("  8'd5   !=   8'sd5 ", ExprInt(false, 1, 0)),
            // - signed unsigned
            ("  8'sd10 !=   8'd7  ", ExprInt(false, 1, 1)),
            ("  8'sd10 != -(8'd7) ", ExprInt(false, 1, 1)),
            (" -8'sd10 !=   8'd7  ", ExprInt(false, 1, 1)),
            (" -8'sd10 != -(8'd7) ", ExprInt(false, 1, 1)),
            ("  8'sd7  !=   8'd10 ", ExprInt(false, 1, 1)),
            ("  8'sd7  != -(8'd10)", ExprInt(false, 1, 1)),
            (" -8'sd7  !=   8'd10 ", ExprInt(false, 1, 1)),
            (" -8'sd7  != -(8'd10)", ExprInt(false, 1, 1)),
            ("  8'sd5  !=   8'd5  ", ExprInt(false, 1, 0)),
            // - signed signed
            ("  8'sd10 !=   8'sd7 ", ExprInt(false, 1, 1)),
            ("  8'sd10 !=  -8'sd7 ", ExprInt(false, 1, 1)),
            (" -8'sd10 !=   8'sd7 ", ExprInt(false, 1, 1)),
            (" -8'sd10 !=  -8'sd7 ", ExprInt(false, 1, 1)),
            ("  8'sd7  !=   8'sd10", ExprInt(false, 1, 1)),
            ("  8'sd7  !=  -8'sd10", ExprInt(false, 1, 1)),
            (" -8'sd7  !=   8'sd10", ExprInt(false, 1, 1)),
            (" -8'sd7  !=  -8'sd10", ExprInt(false, 1, 1)),
            ("  8'sd5  !=   8'sd5 ", ExprInt(false, 1, 0)),
            (" -8'sd5  !=  -8'sd5 ", ExprInt(false, 1, 0))
          )
        }

        "&&" - {
          checkExact(
            // - unsigned unsigned
            (" 1'd0  &&  1'd0 ", ExprInt(false, 1, 0)),
            (" 1'd0  &&  1'd1 ", ExprInt(false, 1, 0)),
            (" 1'd1  &&  1'd0 ", ExprInt(false, 1, 0)),
            (" 1'd1  &&  1'd1 ", ExprInt(false, 1, 1)),
            // - unsigned signed
            (" 1'd0  &&  1'sd0", ExprInt(false, 1, 0)),
            (" 1'd0  && -1'sd1", ExprInt(false, 1, 0)),
            (" 1'd1  &&  1'sd0", ExprInt(false, 1, 0)),
            (" 1'd1  && -1'sd1", ExprInt(false, 1, 1)),
            // - signed unsigned
            (" 1'sd0 &&  1'd0 ", ExprInt(false, 1, 0)),
            (" 1'sd0 &&  1'd1 ", ExprInt(false, 1, 0)),
            ("-1'sd1 &&  1'd0 ", ExprInt(false, 1, 0)),
            ("-1'sd1 &&  1'd1 ", ExprInt(false, 1, 1)),
            // - signed signed
            (" 1'sd0 &&  1'sd0  ", ExprInt(false, 1, 0)),
            (" 1'sd0 && -1'sd1  ", ExprInt(false, 1, 0)),
            ("-1'sd1 &&  1'sd0  ", ExprInt(false, 1, 0)),
            ("-1'sd1 && -1'sd1  ", ExprInt(false, 1, 1))
          )
        }

        "||" - {
          checkExact(
            // - unsigned unsigned
            (" 1'd0  ||  1'd0 ", ExprInt(false, 1, 0)),
            (" 1'd0  ||  1'd1 ", ExprInt(false, 1, 1)),
            (" 1'd1  ||  1'd0 ", ExprInt(false, 1, 1)),
            (" 1'd1  ||  1'd1 ", ExprInt(false, 1, 1)),
            // - unsigned signed
            (" 1'd0  ||  1'sd0", ExprInt(false, 1, 0)),
            (" 1'd0  || -1'sd1", ExprInt(false, 1, 1)),
            (" 1'd1  ||  1'sd0", ExprInt(false, 1, 1)),
            (" 1'd1  || -1'sd1", ExprInt(false, 1, 1)),
            // - signed unsigned
            (" 1'sd0 ||  1'd0 ", ExprInt(false, 1, 0)),
            (" 1'sd0 ||  1'd1 ", ExprInt(false, 1, 1)),
            ("-1'sd1 ||  1'd0 ", ExprInt(false, 1, 1)),
            ("-1'sd1 ||  1'd1 ", ExprInt(false, 1, 1)),
            // - signed signed
            (" 1'sd0 ||  1'sd0  ", ExprInt(false, 1, 0)),
            (" 1'sd0 || -1'sd1  ", ExprInt(false, 1, 1)),
            ("-1'sd1 ||  1'sd0  ", ExprInt(false, 1, 1)),
            ("-1'sd1 || -1'sd1  ", ExprInt(false, 1, 1))
          )
        }

        "<<" - {
          checkExact(
            //// Unsized unsized
            // - unsigned unsigned
            (" 10  << 2 ", ExprNum(false, 40)),
            // - unsigned signed
            (" 10  << 2s", ExprNum(false, 40)),
            // - signed unsigned
            (" 10s << 2 ", ExprNum(true, 40)),
            ("-10s << 2 ", ExprNum(true, -40)),
            // - signed signed
            (" 10s << 2s", ExprNum(true, 40)),
            ("-10s << 2s", ExprNum(true, -40)),
            //// Unsized sized
            // - unsigned unsigned
            (" 10  << 8'd2 ", ExprNum(false, 40)),
            // - unsigned signed
            (" 10  << 8'sd2", ExprNum(false, 40)),
            // - signed unsigned
            (" 10s << 8'd2 ", ExprNum(true, 40)),
            ("-10s << 8'd2 ", ExprNum(true, -40)),
            // - signed signed
            (" 10s << 8'sd2", ExprNum(true, 40)),
            ("-10s << 8'sd2", ExprNum(true, -40)),
            //// Sized unsized
            // - unsigned unsigned
            ("  8'd10  << 2 ", ExprInt(false, 8, 40)),
            ("-(8'd10) << 2 ", ExprInt(false, 8, 216)),
            // - unsigned signed
            ("  8'd10  << 2s", ExprInt(false, 8, 40)),
            ("-(8'd10) << 2s", ExprInt(false, 8, 216)),
            // - signed unsigned
            ("  8'sd10 << 2 ", ExprInt(true, 8, 40)),
            (" -8'sd10 << 2 ", ExprInt(true, 8, -40)),
            // - signed signed
            ("  8'sd10 << 2s", ExprInt(true, 8, 40)),
            (" -8'sd10 << 2s", ExprInt(true, 8, -40)),
            //// Sized sized
            // - unsigned unsigned
            ("  8'd10  << 8'd2 ", ExprInt(false, 8, 40)),
            ("-(8'd10) << 8'd2 ", ExprInt(false, 8, 216)),
            // - unsigned signed
            ("  8'd10  << 8'sd2", ExprInt(false, 8, 40)),
            ("-(8'd10) << 8'sd2", ExprInt(false, 8, 216)),
            // - signed unsigned
            ("  8'sd10 << 8'd2 ", ExprInt(true, 8, 40)),
            (" -8'sd10 << 8'd2 ", ExprInt(true, 8, -40)),
            // - signed signed
            ("  8'sd10 << 8'sd2", ExprInt(true, 8, 40)),
            (" -8'sd10 << 8'sd2", ExprInt(true, 8, -40))
          )
        }

        "<<<" - {
          checkExact(
            //// Unsized unsized
            // - unsigned unsigned
            (" 10  <<< 2 ", ExprNum(false, 40)),
            // - unsigned signed
            (" 10  <<< 2s", ExprNum(false, 40)),
            // - signed unsigned
            (" 10s <<< 2 ", ExprNum(true, 40)),
            ("-10s <<< 2 ", ExprNum(true, -40)),
            // - signed signed
            (" 10s <<< 2s", ExprNum(true, 40)),
            ("-10s <<< 2s", ExprNum(true, -40)),
            //// Unsized sized
            // - unsigned unsigned
            (" 10  <<< 8'd2 ", ExprNum(false, 40)),
            // - unsigned signed
            (" 10  <<< 8'sd2", ExprNum(false, 40)),
            // - signed unsigned
            (" 10s <<< 8'd2 ", ExprNum(true, 40)),
            ("-10s <<< 8'd2 ", ExprNum(true, -40)),
            // - signed signed
            (" 10s <<< 8'sd2", ExprNum(true, 40)),
            ("-10s <<< 8'sd2", ExprNum(true, -40)),
            //// Sized unsized
            // - unsigned unsigned
            ("  8'd10  <<< 2 ", ExprInt(false, 8, 40)),
            ("-(8'd10) <<< 2 ", ExprInt(false, 8, 216)),
            // - unsigned signed
            ("  8'd10  <<< 2s", ExprInt(false, 8, 40)),
            ("-(8'd10) <<< 2s", ExprInt(false, 8, 216)),
            // - signed unsigned
            ("  8'sd10 <<< 2 ", ExprInt(true, 8, 40)),
            (" -8'sd10 <<< 2 ", ExprInt(true, 8, -40)),
            // - signed signed
            ("  8'sd10 <<< 2s", ExprInt(true, 8, 40)),
            (" -8'sd10 <<< 2s", ExprInt(true, 8, -40)),
            //// Sized sized
            // - unsigned unsigned
            ("  8'd10  <<< 8'd2 ", ExprInt(false, 8, 40)),
            ("-(8'd10) <<< 8'd2 ", ExprInt(false, 8, 216)),
            // - unsigned signed
            ("  8'd10  <<< 8'sd2", ExprInt(false, 8, 40)),
            ("-(8'd10) <<< 8'sd2", ExprInt(false, 8, 216)),
            // - signed unsigned
            ("  8'sd10 <<< 8'd2 ", ExprInt(true, 8, 40)),
            (" -8'sd10 <<< 8'd2 ", ExprInt(true, 8, -40)),
            // - signed signed
            ("  8'sd10 <<< 8'sd2", ExprInt(true, 8, 40)),
            (" -8'sd10 <<< 8'sd2", ExprInt(true, 8, -40))
          )
        }

        ">>" - {
          checkExact(
            //// Unsized unsized
            // - unsigned unsigned
            (" 10  >> 2 ", ExprNum(false, 2)),
            // - unsigned signed
            (" 10  >> 2s", ExprNum(false, 2)),
            // - signed unsigned
            (" 10s >> 2 ", ExprNum(true, 2)),
            // - signed signed
            (" 10s >> 2s", ExprNum(true, 2)),
            //// Unsized sized
            // - unsigned unsigned
            (" 10  >> 8'd2 ", ExprNum(false, 2)),
            // - unsigned signed
            (" 10  >> 8'sd2", ExprNum(false, 2)),
            // - signed unsigned
            (" 10s >> 8'd2 ", ExprNum(true, 2)),
            // - signed signed
            (" 10s >> 8'sd2", ExprNum(true, 2)),
            //// Sized unsized
            // - unsigned unsigned
            ("  8'd10  >> 2 ", ExprInt(false, 8, 2)),
            ("-(8'd10) >> 2 ", ExprInt(false, 8, 61)),
            // - unsigned signed
            ("  8'd10  >> 2s", ExprInt(false, 8, 2)),
            ("-(8'd10) >> 2s", ExprInt(false, 8, 61)),
            // - signed unsigned
            ("  8'sd10 >> 2 ", ExprInt(true, 8, 2)),
            (" -8'sd10 >> 2 ", ExprInt(true, 8, 61)),
            // - signed signed
            ("  8'sd10 >> 2s", ExprInt(true, 8, 2)),
            (" -8'sd10 >> 2s", ExprInt(true, 8, 61)),
            //// Sized sized
            // - unsigned unsigned
            ("  8'd10  >> 8'd2 ", ExprInt(false, 8, 2)),
            ("-(8'd10) >> 8'd2 ", ExprInt(false, 8, 61)),
            // - unsigned signed
            ("  8'd10  >> 8'sd2", ExprInt(false, 8, 2)),
            ("-(8'd10) >> 8'sd2", ExprInt(false, 8, 61)),
            // - signed unsigned
            ("  8'sd10 >> 8'd2 ", ExprInt(true, 8, 2)),
            (" -8'sd10 >> 8'd2 ", ExprInt(true, 8, 61)),
            // - signed signed
            ("  8'sd10 >> 8'sd2", ExprInt(true, 8, 2)),
            (" -8'sd10 >> 8'sd2", ExprInt(true, 8, 61))
          )
        }

        ">>>" - {
          checkExact(
            //// Unsized unsized
            // - unsigned unsigned
            (" 10  >>> 2 ", ExprNum(false, 2)),
            // - unsigned signed
            (" 10  >>> 2s", ExprNum(false, 2)),
            // - signed unsigned
            (" 10s >>> 2 ", ExprNum(true, 2)),
            ("-10s >>> 2 ", ExprNum(true, -3)),
            // - signed signed
            (" 10s >>> 2s", ExprNum(true, 2)),
            ("-10s >>> 2s", ExprNum(true, -3)),
            //// Unsized sized
            // - unsigned unsigned
            (" 10  >>> 8'd2 ", ExprNum(false, 2)),
            // - unsigned signed
            (" 10  >>> 8'sd2", ExprNum(false, 2)),
            // - signed unsigned
            (" 10s >>> 8'd2 ", ExprNum(true, 2)),
            ("-10s >>> 8'd2 ", ExprNum(true, -3)),
            // - signed signed
            (" 10s >>> 8'sd2", ExprNum(true, 2)),
            ("-10s >>> 8'sd2", ExprNum(true, -3)),
            //// Sized unsized
            // - unsigned unsigned
            ("  8'd10  >>> 2 ", ExprInt(false, 8, 2)),
            ("-(8'd10) >>> 2 ", ExprInt(false, 8, 61)),
            // - unsigned signed
            ("  8'd10  >>> 2s", ExprInt(false, 8, 2)),
            ("-(8'd10) >>> 2s", ExprInt(false, 8, 61)),
            // - signed unsigned
            ("  8'sd10 >>> 2 ", ExprInt(true, 8, 2)),
            (" -8'sd10 >>> 2 ", ExprInt(true, 8, -3)),
            // - signed signed
            ("  8'sd10 >>> 2s", ExprInt(true, 8, 2)),
            (" -8'sd10 >>> 2s", ExprInt(true, 8, -3)),
            //// Sized sized
            // - unsigned unsigned
            ("  8'd10  >>> 8'd2 ", ExprInt(false, 8, 2)),
            ("-(8'd10) >>> 8'd2 ", ExprInt(false, 8, 61)),
            // - unsigned signed
            ("  8'd10  >>> 8'sd2", ExprInt(false, 8, 2)),
            ("-(8'd10) >>> 8'sd2", ExprInt(false, 8, 61)),
            // - signed unsigned
            ("  8'sd10 >>> 8'd2 ", ExprInt(true, 8, 2)),
            (" -8'sd10 >>> 8'd2 ", ExprInt(true, 8, -3)),
            // - signed signed
            ("  8'sd10 >>> 8'sd2", ExprInt(true, 8, 2)),
            (" -8'sd10 >>> 8'sd2", ExprInt(true, 8, -3))
          )
        }
      }

      "special cases" - {
        "shifts" - {
          "zero left hand side" - {
            checkExact(
              /// Unsized
              // unsigned
              ("0 >>  @unknownu(4)", ExprNum(false, 0)),
              ("0 >>> @unknownu(4)", ExprNum(false, 0)),
              ("0 <<  @unknownu(4)", ExprNum(false, 0)),
              ("0 <<< @unknownu(4)", ExprNum(false, 0)),
              // signed
              ("0s >>  @unknownu(4)", ExprNum(true, 0)),
              ("0s >>> @unknownu(4)", ExprNum(true, 0)),
              ("0s <<  @unknownu(4)", ExprNum(true, 0)),
              ("0s <<< @unknownu(4)", ExprNum(true, 0)),
              // Sized
              // unsigned
              ("8'd0 >>  @unknownu(4)", ExprInt(false, 8, 0)),
              ("8'd0 >>> @unknownu(4)", ExprInt(false, 8, 0)),
              ("8'd0 <<  @unknownu(4)", ExprInt(false, 8, 0)),
              ("8'd0 <<< @unknownu(4)", ExprInt(false, 8, 0)),
              // signed
              ("8'sd0 >>  @unknownu(4)", ExprInt(true, 8, 0)),
              ("8'sd0 >>> @unknownu(4)", ExprInt(true, 8, 0)),
              ("8'sd0 <<  @unknownu(4)", ExprInt(true, 8, 0)),
              ("8'sd0 <<< @unknownu(4)", ExprInt(true, 8, 0))
            )
          }

          "zero shift amount" - {
            checkPattern(
              /// Unsized
              // unsigned unsigned
              ("@unknownu(4) >>  0 ", { case UnknownU(4) => }),
              ("@unknownu(4) >>> 0 ", { case UnknownU(4) => }),
              ("@unknownu(4) <<  0 ", { case UnknownU(4) => }),
              ("@unknownu(4) <<< 0 ", { case UnknownU(4) => }),
              // unsigned signed
              ("@unknownu(4) >>  0s", { case UnknownU(4) => }),
              ("@unknownu(4) >>> 0s", { case UnknownU(4) => }),
              ("@unknownu(4) <<  0s", { case UnknownU(4) => }),
              ("@unknownu(4) <<< 0s", { case UnknownU(4) => }),
              // signed unsigned
              ("@unknowni(4) >>  0 ", { case UnknownI(4) => }),
              ("@unknowni(4) >>> 0 ", { case UnknownI(4) => }),
              ("@unknowni(4) <<  0 ", { case UnknownI(4) => }),
              ("@unknowni(4) <<< 0 ", { case UnknownI(4) => }),
              // signed signed
              ("@unknowni(4) >>  0s", { case UnknownI(4) => }),
              ("@unknowni(4) >>> 0s", { case UnknownI(4) => }),
              ("@unknowni(4) <<  0s", { case UnknownI(4) => }),
              ("@unknowni(4) <<< 0s", { case UnknownI(4) => }),
              // Sized
              // unsigne unsigned
              ("@unknownu(4) >>  8'd0 ", { case UnknownU(4) => }),
              ("@unknownu(4) >>> 8'd0 ", { case UnknownU(4) => }),
              ("@unknownu(4) <<  8'd0 ", { case UnknownU(4) => }),
              ("@unknownu(4) <<< 8'd0 ", { case UnknownU(4) => }),
              // unsigned signed
              ("@unknownu(4) >>  8'sd0", { case UnknownU(4) => }),
              ("@unknownu(4) >>> 8'sd0", { case UnknownU(4) => }),
              ("@unknownu(4) <<  8'sd0", { case UnknownU(4) => }),
              ("@unknownu(4) <<< 8'sd0", { case UnknownU(4) => }),
              // signed unsigned
              ("@unknowni(4) >>  8'd0 ", { case UnknownI(4) => }),
              ("@unknowni(4) >>> 8'd0 ", { case UnknownI(4) => }),
              ("@unknowni(4) <<  8'd0 ", { case UnknownI(4) => }),
              ("@unknowni(4) <<< 8'd0 ", { case UnknownI(4) => }),
              // signed signed
              ("@unknowni(4) >>  8'sd0", { case UnknownI(4) => }),
              ("@unknowni(4) >>> 8'sd0", { case UnknownI(4) => }),
              ("@unknowni(4) <<  8'sd0", { case UnknownI(4) => }),
              ("@unknowni(4) <<< 8'sd0", { case UnknownI(4) => })
            )
          }

          "shift amount >= width of left hand side" - {
            checkExact(
              // unsigned unsigned
              ("@unknownu(4) >>  8'd4 ", ExprInt(false, 4, 0)),
              ("@unknownu(4) >>> 8'd4 ", ExprInt(false, 4, 0)),
              ("@unknownu(4) <<  8'd4 ", ExprInt(false, 4, 0)),
              ("@unknownu(4) <<< 8'd4 ", ExprInt(false, 4, 0)),
              // unsigned signed
              ("@unknownu(4) >>  8'sd4", ExprInt(false, 4, 0)),
              ("@unknownu(4) >>> 8'sd4", ExprInt(false, 4, 0)),
              ("@unknownu(4) <<  8'sd4", ExprInt(false, 4, 0)),
              ("@unknownu(4) <<< 8'sd4", ExprInt(false, 4, 0)),
              // signed unsigned
              ("@unknowni(4) >>  8'd4 ", ExprInt(true, 4, 0)),
              ("@unknowni(4) >>> 8'd4 ", ExprInt(true, 4, -1)),
              ("@unknowni(4) <<  8'd4 ", ExprInt(true, 4, 0)),
              ("@unknowni(4) <<< 8'd4 ", ExprInt(true, 4, 0)),
              // signed signed
              ("@unknowni(4) >>  8'sd4", ExprInt(true, 4, 0)),
              ("@unknowni(4) >>> 8'sd4", ExprInt(true, 4, -1)),
              ("@unknowni(4) <<  8'sd4", ExprInt(true, 4, 0)),
              ("@unknowni(4) <<< 8'sd4", ExprInt(true, 4, 0))
            )
          }
        }

        "comparison operators with one side known" - {
          val tests = List[(String, String, String, PartialFunction[Any, Unit])](
            // _ < min value
            ("@unknownu(4)", "<", " 4'd0 ", { case ExprInt(false, 1, v) if v == 0 => }),
            ("@unknownu(4)", "<", " 4'sd0", { case ExprInt(false, 1, v) if v == 0 => }),
            ("@unknowni(4)", "<", " 4'd0 ", { case ExprInt(false, 1, v) if v == 0 => }),
            ("@unknowni(4)", "<", "-4'sd8", { case ExprInt(false, 1, v) if v == 0 => }),
            // _ > max value
            ("@unknownu(4)", ">", " 4'd15", { case ExprInt(false, 1, v) if v == 0 => }),
            ("@unknownu(4)", ">", "-4'sd1", { case ExprInt(false, 1, v) if v == 0 => }),
            ("@unknowni(4)", ">", " 4'd15", { case ExprInt(false, 1, v) if v == 0 => }),
            ("@unknowni(4)", ">", " 4'sd7", { case ExprInt(false, 1, v) if v == 0 => }),
            // _ <= max value
            ("@unknownu(4)", "<=", " 4'd15", { case ExprInt(false, 1, v) if v == 1 => }),
            ("@unknownu(4)", "<=", "-4'sd1", { case ExprInt(false, 1, v) if v == 1 => }),
            ("@unknowni(4)", "<=", " 4'd15", { case ExprInt(false, 1, v) if v == 1 => }),
            ("@unknowni(4)", "<=", " 4'sd7", { case ExprInt(false, 1, v) if v == 1 => }),
            // _ >= min value
            ("@unknownu(4)", ">=", " 4'd0 ", { case ExprInt(false, 1, v) if v == 1 => }),
            ("@unknownu(4)", ">=", " 4'sd0", { case ExprInt(false, 1, v) if v == 1 => }),
            ("@unknowni(4)", ">=", " 4'd0 ", { case ExprInt(false, 1, v) if v == 1 => }),
            ("@unknowni(4)", ">=", "-4'sd8", { case ExprInt(false, 1, v) if v == 1 => })
          )

          "unknown on left" - {
            checkPattern(tests map { case (a, op, b, p) => (s"$a $op $b", p) }: _*)
          }

          "unknown on right" - {
            val swap = Map("<" -> ">", "<=" -> ">=", ">" -> "<", ">=" -> "<=")
            checkPattern(tests map { case (a, op, b, p) => (s"$b ${swap(op)} $a", p) }: _*)
          }
        }

        "commutative operators with one side known" - {
          val tests = List[(String, String, String, PartialFunction[Any, Unit])](
            // _ + 0
            ("@unknownu(4)", "+", "4'd0 ", { case UnknownU(4) => }),
            ("@unknownu(4)", "+", "4'sd0", { case UnknownU(4) => }),
            ("@unknowni(4)", "+", "4'd0 ", { case Unsigned(UnknownI(4)) => }),
            ("@unknowni(4)", "+", "4'sd0", { case UnknownI(4) => }),
            // _ * 0
            ("@unknownu(4)", "*", "4'd0 ", { case ExprInt(false, 4, v) if v == 0 => }),
            ("@unknownu(4)", "*", "4'sd0", { case ExprInt(false, 4, v) if v == 0 => }),
            ("@unknowni(4)", "*", "4'd0 ", { case ExprInt(false, 4, v) if v == 0 => }),
            ("@unknowni(4)", "*", "4'sd0", { case ExprInt(true, 4, v) if v == 0 => }),
            // _ * 1
            ("@unknownu(4)", "*", "4'd1 ", { case UnknownU(4) => }),
            ("@unknownu(4)", "*", "4'sd1", { case UnknownU(4) => }),
            ("@unknowni(4)", "*", "4'd1 ", { case Unsigned(UnknownI(4)) => }),
            ("@unknowni(4)", "*", "4'sd1", { case UnknownI(4) => }),
            // _ & 0
            ("@unknownu(4)", "&", "4'd0 ", { case ExprInt(false, 4, v) if v == 0 => }),
            ("@unknownu(4)", "&", "4'sd0", { case ExprInt(false, 4, v) if v == 0 => }),
            ("@unknowni(4)", "&", "4'd0 ", { case ExprInt(false, 4, v) if v == 0 => }),
            ("@unknowni(4)", "&", "4'sd0", { case ExprInt(true, 4, v) if v == 0 => }),
            // _ & ones
            ("@unknownu(4)", "&", "4'd15 ", { case UnknownU(4) => }),
            ("@unknownu(4)", "&", "-4'sd1", { case UnknownU(4) => }),
            ("@unknowni(4)", "&", "4'd15 ", { case Unsigned(UnknownI(4)) => }),
            ("@unknowni(4)", "&", "-4'sd1", { case UnknownI(4) => }),
            // _ | 0
            ("@unknownu(4)", "|", "4'd0 ", { case UnknownU(4) => }),
            ("@unknownu(4)", "|", "4'sd0", { case UnknownU(4) => }),
            ("@unknowni(4)", "|", "4'd0 ", { case Unsigned(UnknownI(4)) => }),
            ("@unknowni(4)", "|", "4'sd0", { case UnknownI(4) => }),
            // _ | ones
            ("@unknownu(4)", "|", "4'd15 ", { case ExprInt(false, 4, v) if v == 15 => }),
            ("@unknownu(4)", "|", "-4'sd1", { case ExprInt(false, 4, v) if v == 15 => }),
            ("@unknowni(4)", "|", "4'd15 ", { case ExprInt(false, 4, v) if v == 15 => }),
            ("@unknowni(4)", "|", "-4'sd1", { case ExprInt(true, 4, v) if v == -1 => }),
            // _ ^ 0
            ("@unknownu(4)", "^", "4'd0 ", { case UnknownU(4) => }),
            ("@unknownu(4)", "^", "4'sd0", { case UnknownU(4) => }),
            ("@unknowni(4)", "^", "4'd0 ", { case Unsigned(UnknownI(4)) => }),
            ("@unknowni(4)", "^", "4'sd0", { case UnknownI(4) => }),
            // _ ^ ones
            ("@unknownu(4)", "^", "4'd15 ", { case ExprUnary("~", UnknownU(4)) => }),
            ("@unknownu(4)", "^", "-4'sd1", { case ExprUnary("~", UnknownU(4)) => }),
            ("@unknowni(4)", "^", "4'd15 ", { case Unsigned(ExprUnary("~", UnknownI(4))) => }),
            ("@unknowni(4)", "^", "-4'sd1", { case ExprUnary("~", UnknownI(4)) => }),
            // _ && 0
            ("@unknownu(1)", "&&", "1'd0 ", { case ExprInt(false, 1, v) if v == 0 => }),
            ("@unknownu(1)", "&&", "1'sd0", { case ExprInt(false, 1, v) if v == 0 => }),
            ("@unknowni(1)", "&&", "1'd0 ", { case ExprInt(false, 1, v) if v == 0 => }),
            ("@unknowni(1)", "&&", "1'sd0", { case ExprInt(false, 1, v) if v == 0 => }),
            // _ && 1
            ("@unknownu(1)", "&&", "1'd1  ", { case UnknownU(1) => }),
            ("@unknownu(1)", "&&", "-1'sd1", { case UnknownU(1) => }),
            ("@unknowni(1)", "&&", "1'd1  ", { case Unsigned(UnknownI(1)) => }),
            ("@unknowni(1)", "&&", "-1'sd1", { case Unsigned(UnknownI(1)) => }),
            // _ || 0
            ("@unknownu(1)", "||", "1'd0 ", { case UnknownU(1) => }),
            ("@unknownu(1)", "||", "1'sd0", { case UnknownU(1) => }),
            ("@unknowni(1)", "||", "1'd0 ", { case Unsigned(UnknownI(1)) => }),
            ("@unknowni(1)", "||", "1'sd0", { case Unsigned(UnknownI(1)) => }),
            // _ || 1
            ("@unknownu(1)", "||", "1'd1  ", { case ExprInt(false, 1, v) if v == 1 => }),
            ("@unknownu(1)", "||", "-1'sd1", { case ExprInt(false, 1, v) if v == 1 => }),
            ("@unknowni(1)", "||", "1'd1  ", { case ExprInt(false, 1, v) if v == 1 => }),
            ("@unknowni(1)", "||", "-1'sd1", { case ExprInt(false, 1, v) if v == 1 => })
          )

          "unknown on left" - {
            checkPattern(tests map { case (a, op, b, p) => (s"$a $op $b", p) }: _*)
          }

          "unknown on right" - {
            checkPattern(tests map { case (a, op, b, p) => (s"$b $op $a", p) }: _*)
          }
        }

        "non commutative operators with one side known" - {
          checkPattern(
            // 0 / _
            ("4'd0  / @unknownu(4)", { case ExprInt(false, 4, v) if v == 0 => }),
            ("4'sd0 / @unknownu(4)", { case ExprInt(false, 4, v) if v == 0 => }),
            ("4'd0  / @unknowni(4)", { case ExprInt(false, 4, v) if v == 0 => }),
            ("4'sd0 / @unknowni(4)", { case ExprInt(true, 4, v) if v == 0 => }),
            // _ / 1
            ("@unknownu(4) / 4'd1 ", { case UnknownU(4) => }),
            ("@unknownu(4) / 4'sd1", { case UnknownU(4) => }),
            ("@unknowni(4) / 4'd1 ", { case Unsigned(UnknownI(4)) => }),
            ("@unknowni(4) / 4'sd1", { case UnknownI(4) => }),
            // 0 % _
            ("4'd0  % @unknownu(4)", { case ExprInt(false, 4, v) if v == 0 => }),
            ("4'sd0 % @unknownu(4)", { case ExprInt(false, 4, v) if v == 0 => }),
            ("4'd0  % @unknowni(4)", { case ExprInt(false, 4, v) if v == 0 => }),
            ("4'sd0 % @unknowni(4)", { case ExprInt(true, 4, v) if v == 0 => }),
            // _ % 1
            ("@unknownu(4) % 4'd1 ", { case ExprInt(false, 4, v) if v == 0 => }),
            ("@unknownu(4) % 4'sd1", { case ExprInt(false, 4, v) if v == 0 => }),
            ("@unknowni(4) % 4'd1 ", { case ExprInt(false, 4, v) if v == 0 => }),
            ("@unknowni(4) % 4'sd1", { case ExprInt(true, 4, v) if v == 0 => }),
            // 0 - _
            ("4'd0  - @unknownu(4)", { case ExprUnary("-", UnknownU(4)) => }),
            ("4'sd0 - @unknownu(4)", { case ExprUnary("-", UnknownU(4)) => }),
            ("4'd0  - @unknowni(4)", { case Unsigned(ExprUnary("-", UnknownI(4))) => }),
            ("4'sd0 - @unknowni(4)", { case ExprUnary("-", UnknownI(4)) => }),
            // _ - 0
            ("@unknownu(4) - 4'd0 ", { case UnknownU(4) => }),
            ("@unknownu(4) - 4'sd0", { case UnknownU(4) => }),
            ("@unknowni(4) - 4'd0 ", { case Unsigned(UnknownI(4)) => }),
            ("@unknowni(4) - 4'sd0", { case UnknownI(4) => })
          )
        }
      }
    }

    "ternary operator" - {
      for {
        (text, pattern, err) <- List[(String, PartialFunction[Any, Unit], List[String])](
          // format: off
          ("1'd0 ? 1 : 2", { case ExprNum(false, v) if v == 2                                  => }, Nil),
          ("1'd1 ? 1 : 2", { case ExprNum(false, v) if v == 1                                  => }, Nil),
          ("@unknownu(1) ? 1 : 1", { case ExprNum(false, v) if v == 1                          => }, Nil),
          ("@unknownu(1) ? 2 : 2", { case ExprNum(false, v) if v == 2                          => }, Nil),
          ("@unknownu(1) ? 8'd0 : 8'd0", { case ExprInt(false, 8, v) if v == 0                 => }, Nil),
          ("@unknownu(1) ? 8'd0 : 8'd1", { case ExprCond(UnknownU(1), _: ExprInt, _: ExprInt)  => }, Nil),
          ("@unknownu(1) ? 8'd0 : 8'sd0", { case ExprCond(UnknownU(1), _: ExprInt, _: ExprInt) => }, Nil),
          ("1'd1 ? 1s : 0s - 2s", { case ExprNum(true, v) if v == 1                            => }, Nil),
          ("1'd1 ? 1s : 0", { case ExprNum(false, v) if v == 1                                 => }, Nil),
          ("@unknownu(1) ? 1 - 1 : 2 - 2", { case ExprNum(false, v) if v == 0                  => }, Nil)
          // format: on
        )
      } {
        text in {
          simplify(text) should matchPattern(pattern)
          checkSingleError(err)
        }
      }
    }

    "index into known sized values" - {
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

    "slice into known sized values" - {
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

    "index into unsized values" - {
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

    "slice into unsized values" - {
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
               |}""".stripMargin
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
              ("a[8  : 4][1 +: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 6 && r == 5 => }),
              ("a[9  : 4][1 +: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 6 && r == 5 => }),
              ("a[8  : 4][2 +: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 7 && r == 6 => }),
              ("a[9  : 4][2 +: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 7 && r == 6 => }),
              ("a[8  : 4][1 -: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 5 && r == 4 => }),
              ("a[9  : 4][1 -: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 5 && r == 4 => }),
              ("a[8  : 4][2 -: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 6 && r == 5 => }),
              ("a[9  : 4][2 -: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 6 && r == 5 => }),
              ("a[8 +: 4][1  : 0]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 9 && r == 8 => }),
              ("a[9 +: 4][1  : 0]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 10 && r == 9 => }),
              ("a[8 +: 4][2  : 1]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 10 && r == 9 => }),
              ("a[9 +: 4][2  : 1]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 11 && r == 10 => }),
              ("a[8 +: 4][1 +: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 10 && r == 9 => }),
              ("a[9 +: 4][1 +: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 11 && r == 10 => }),
              ("a[8 +: 4][2 +: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 11 && r == 10 => }),
              ("a[9 +: 4][2 +: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 12 && r == 11 => }),
              ("a[8 +: 4][1 -: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 9 && r == 8 => }),
              ("a[9 +: 4][1 -: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 10 && r == 9 => }),
              ("a[8 +: 4][2 -: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 10 && r == 9 => }),
              ("a[9 +: 4][2 -: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 11 && r == 10 => }),
              ("a[8 -: 4][1  : 0]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 6 && r == 5 => }),
              ("a[9 -: 4][1  : 0]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 7 && r == 6 => }),
              ("a[8 -: 4][2  : 1]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 7 && r == 6 => }),
              ("a[9 -: 4][2  : 1]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 8 && r == 7 => }),
              ("a[8 -: 4][1 +: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 7 && r == 6 => }),
              ("a[9 -: 4][1 +: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 8 && r == 7 => }),
              ("a[8 -: 4][2 +: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 8 && r == 7 => }),
              ("a[9 -: 4][2 +: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 9 && r == 8 => }),
              ("a[8 -: 4][1 -: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 6 && r == 5 => }),
              ("a[9 -: 4][1 -: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 7 && r == 6 => }),
              ("a[8 -: 4][2 -: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 7 && r == 6 => }),
              ("a[9 -: 4][2 -: 2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 4, l), ":", ExprInt(false, 4, r)) if l == 8 && r == 7 => })
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
                   |}""".stripMargin
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
               |}""".stripMargin
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
          ("$signed(a)[3'd1  : 3'd0]",   { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l), ":", ExprInt(false, 3, r)) if l == 1 && r == 0 => }),
          ("$signed(a)[3'd2  : 3'd1]",   { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l), ":", ExprInt(false, 3, r)) if l == 2 && r == 1 => }),
          ("$signed(a)[3'd0 +: 4'd2]",   { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l), ":", ExprInt(false, 3, r)) if l == 1 && r == 0 => }),
          ("$signed(a)[3'd1 +: 4'd2]",   { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l), ":", ExprInt(false, 3, r)) if l == 2 && r == 1 => }),
          ("$signed(a)[3'd2 -: 4'd2]",   { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l), ":", ExprInt(false, 3, r)) if l == 2 && r == 1 => }),
          ("$signed(a)[3'd1 -: 4'd2]",   { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l), ":", ExprInt(false, 3, r)) if l == 1 && r == 0 => }),
          ("$unsigned(a)[3'd1  : 3'd0]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l), ":", ExprInt(false, 3, r)) if l == 1 && r == 0 => }),
          ("$unsigned(a)[3'd2  : 3'd1]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l), ":", ExprInt(false, 3, r)) if l == 2 && r == 1 => }),
          ("$unsigned(a)[3'd0 +: 4'd2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l), ":", ExprInt(false, 3, r)) if l == 1 && r == 0 => }),
          ("$unsigned(a)[3'd1 +: 4'd2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l), ":", ExprInt(false, 3, r)) if l == 2 && r == 1 => }),
          ("$unsigned(a)[3'd2 -: 4'd2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l), ":", ExprInt(false, 3, r)) if l == 2 && r == 1 => }),
          ("$unsigned(a)[3'd1 -: 4'd2]", { case ExprSlice(ExprSym(Symbol("a")), ExprInt(false, 3, l), ":", ExprInt(false, 3, r)) if l == 1 && r == 0 => })
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
               |}""".stripMargin
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
               |}""".stripMargin
          } getFirst {
            case StmtAssign(_, rhs) => rhs
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages filterNot { _.isInstanceOf[Warning] } shouldBe empty
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
          ("c = e[6:0] + 7'd1",  { case ExprBinary(Unsigned(ExprSym(Symbol("e"))), "+", ExprInt(false, 7, v)) if v == 1 => }),
          ("c = e[0+:7] + 7'd1", { case ExprBinary(Unsigned(ExprSym(Symbol("e"))), "+", ExprInt(false, 7, v)) if v == 1 => }),
          ("c = e[6-:7] + 7'd1", { case ExprBinary(Unsigned(ExprSym(Symbol("e"))), "+", ExprInt(false, 7, v)) if v == 1 => })
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
               |}""".stripMargin
          } getFirst {
            case StmtAssign(_, rhs) => rhs
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages filterNot { _.isInstanceOf[Warning] } shouldBe empty
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
               |}""".stripMargin
          } getFirst {
            case StmtAssign(_, rhs) => rhs
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages filterNot { _.isInstanceOf[Warning] } shouldBe empty
        }
      }
    }

    "repetitions of count 1" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          ("{1{a}}", { case ExprSym(Symbol("a")) => }),
          ("{1'd1{a}}", { case ExprSym(Symbol("a")) => }),
          ("{1{b}}", { case Unsigned(ExprSym(Symbol("b"))) => }),
          ("{1'd1{b}}", { case Unsigned(ExprSym(Symbol("b"))) => })
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
               |}""".stripMargin
          } getFirst {
            case ExprBuiltin(_, _ :: ArgP(expr) :: Nil) => expr
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages filterNot { _.isInstanceOf[Warning] } shouldBe empty
        }
      }
    }

    "concatenation of size 1" - {
      for {
        (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
          ("{a}", { case ExprSym(Symbol("a")) => }),
          ("{b}", { case Unsigned(ExprSym(Symbol("b"))) => })
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
               |}""".stripMargin
          } getFirst {
            case ExprBuiltin(_, _ :: ArgP(expr) :: Nil) => expr
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages filterNot { _.isInstanceOf[Warning] } shouldBe empty
        }
      }
    }

    "concatenation of known sized values" - {
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

    "repetition of known sized values" - {
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
               |}""".stripMargin
          } getFirst {
            case StmtAssign(_, rhs) => rhs
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages filterNot { _.isInstanceOf[Warning] } shouldBe empty
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
               |}""".stripMargin
          } getFirst {
            case StmtAssign(_, rhs) => rhs
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages filterNot { _.isInstanceOf[Warning] } shouldBe empty
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
               |}""".stripMargin
          } getFirst {
            case StmtAssign(_, rhs) => rhs
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages filterNot { _.isInstanceOf[Warning] } shouldBe empty
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
               |}""".stripMargin
          } getFirst {
            case StmtAssign(_, rhs) => rhs
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages filterNot { _.isInstanceOf[Warning] } shouldBe empty
        }
      }
    }

    "builtin functions" - {
      "@max" - {
        checkExact(
          ("@max(1s)", ExprNum(true, 1)),
          ("@max(1)", ExprNum(false, 1)),
          ("@max(1s, 2s)", ExprNum(true, 2)),
          ("@max(1s, 2)", ExprNum(false, 2)),
          ("@max(1, 2s)", ExprNum(false, 2)),
          ("@max(1, 2)", ExprNum(false, 2)),
          ("@max(0s, 1s)", ExprNum(true, 1)),
          ("@max(-2s, -1s)", ExprNum(true, -1)),
          ("@max(-2s, 1)", ExprNum(false, 1)),
          ("@max(0, 1, 2, 3, 4, 5)", ExprNum(false, 5))
        )
      }

      // TODO: @ex

      // TODO: @msb

      "@zx" - {
        checkExact(
          ("@zx(3, 2'b00)", ExprInt(false, 3, 0)),
          ("@zx(3, 2'b01)", ExprInt(false, 3, 1)),
          ("@zx(3, 2'b10)", ExprInt(false, 3, 2)),
          ("@zx(3, 2'b11)", ExprInt(false, 3, 3)),
          ("@zx(3, 2'sb00)", ExprInt(true, 3, 0)),
          ("@zx(3, 2'sb01)", ExprInt(true, 3, 1)),
          ("@zx(3, 2'sb10)", ExprInt(true, 3, 2)),
          ("@zx(3, 2'sb11)", ExprInt(true, 3, 3)),
          ("@zx(2, 2'b00)", ExprInt(false, 2, 0)),
          ("@zx(2, 2'b01)", ExprInt(false, 2, 1)),
          ("@zx(2, 2'b10)", ExprInt(false, 2, 2)),
          ("@zx(2, 2'b11)", ExprInt(false, 2, 3)),
          ("@zx(2, 2'sb00)", ExprInt(true, 2, 0)),
          ("@zx(2, 2'sb01)", ExprInt(true, 2, 1)),
          ("@zx(2, 2'sb10)", ExprInt(true, 2, -2)),
          ("@zx(2, 2'sb11)", ExprInt(true, 2, -1))
        )
      }

      "@sx" - {
        checkExact(
          ("@sx(3, 2'b00)", ExprInt(false, 3, 0)),
          ("@sx(3, 2'b01)", ExprInt(false, 3, 1)),
          ("@sx(3, 2'b10)", ExprInt(false, 3, 6)),
          ("@sx(3, 2'b11)", ExprInt(false, 3, 7)),
          ("@sx(3, 2'sb00)", ExprInt(true, 3, 0)),
          ("@sx(3, 2'sb01)", ExprInt(true, 3, 1)),
          ("@sx(3, 2'sb10)", ExprInt(true, 3, -2)),
          ("@sx(3, 2'sb11)", ExprInt(true, 3, -1)),
          ("@sx(2, 2'b00)", ExprInt(false, 2, 0)),
          ("@sx(2, 2'b01)", ExprInt(false, 2, 1)),
          ("@sx(2, 2'b10)", ExprInt(false, 2, 2)),
          ("@sx(2, 2'b11)", ExprInt(false, 2, 3)),
          ("@sx(2, 2'sb00)", ExprInt(true, 2, 0)),
          ("@sx(2, 2'sb01)", ExprInt(true, 2, 1)),
          ("@sx(2, 2'sb10)", ExprInt(true, 2, -2)),
          ("@sx(2, 2'sb11)", ExprInt(true, 2, -1))
        )
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
                 |fsm x {
                 |  a_t a;
                 |  void main() {
                 |    $$display("", $text);
                 |    fence;
                 |  }
                 |}""".stripMargin
            } getFirst {
              case ExprBuiltin(_, List(_, ArgP(e))) => e
            } tap {
              _ shouldBe result
            }
            cc.messages filterNot { _.isInstanceOf[Warning] } shouldBe empty
          }
        }
      }

      "$signed" - {
        checkExact(
          ("$signed(1)", ExprNum(true, 1)),
          ("$signed(1s)", ExprNum(true, 1)),
          ("$signed(-1s)", ExprNum(true, -1)),
          ("$signed(2'd0)", ExprInt(true, 2, 0)),
          ("$signed(2'd1)", ExprInt(true, 2, 1)),
          ("$signed(2'd3)", ExprInt(true, 2, -1)),
          ("$signed(2'sd0)", ExprInt(true, 2, 0)),
          ("$signed(2'sd1)", ExprInt(true, 2, 1)),
          ("$signed(-2'sd1)", ExprInt(true, 2, -1)),
          ("$signed(8'h7f)", ExprInt(true, 8, 127)),
          ("$signed(8'h80)", ExprInt(true, 8, -128)),
          ("$signed(8'hff)", ExprInt(true, 8, -1)),
          ("$signed({1'd0, {31{1'd1}}})", ExprInt(true, 32, 2147483647)),
          ("$signed({1'd1, {31{1'd0}}})", ExprInt(true, 32, -2147483648))
        )
      }

      "$unsigned" - {
        checkExact(
          ("$unsigned(1)", ExprNum(false, 1)),
          ("$unsigned(1s)", ExprNum(false, 1)),
          ("$unsigned(2'd0)", ExprInt(false, 2, 0)),
          ("$unsigned(2'd1)", ExprInt(false, 2, 1)),
          ("$unsigned(2'd3)", ExprInt(false, 2, 3)),
          ("$unsigned(2'sd0)", ExprInt(false, 2, 0)),
          ("$unsigned(2'sd1)", ExprInt(false, 2, 1)),
          ("$unsigned(-2'sd1)", ExprInt(false, 2, 3)),
          ("$unsigned(8'sd127)", ExprInt(false, 8, 127)),
          ("$unsigned(-8'sd128)", ExprInt(false, 8, 128)),
          ("$unsigned(-8'sd1)", ExprInt(false, 8, 255)),
          ("$unsigned({1'd0, {31{1'd1}}})", ExprInt(false, 32, 2147483647)),
          ("$unsigned({1'd1, {31{1'd0}}})", ExprInt(false, 32, 2147483648L))
        )
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
             |}""".stripMargin
        } getFirst {
          case ExprBuiltin(_, _ :: ArgP(expr) :: Nil) => expr
        } tap {
          _ shouldBe value
        }
      }
    }

    "reference to val" - {
      for {
        (expr, pattern) <- List[(String, PartialFunction[Any, Unit])](
          // format: off
          ("A", { case ExprInt(false, 36, v) if v == 0x0000000fffL => }),
          ("B", { case ExprInt(false, 41, v) if v == 0x1000000fffL => }),
          ("C", { case ExprInt(false,  8, v) if v == 2 => }),
          ("D", { case ExprInt(false,  7, v) if v == 3 => }),
          ("E", { case ExprInt(true,   6, v) if v == 4 => }),
          ("F", { case ExprInt(true,   5, v) if v == 5 => }),
          ("G", { case ExprSym(Symbol("G")) => })
          // format: on
        )
      } expr in {
        fold {
          s"""
             |fsm a {
             |
             |  void main() {
             |    const u36 A = {{24{1'b0}}, {12{1'b1}}};
             |    const u41 B = {5'h1, A[35:0]};
             |    const u8  C = 2;
             |    const u7  D = 3s;
             |    const i6  E = 4;
             |    const i5  F = 5s;
             |    const u1  G = @unknownu(1);
             |    $$display("", $expr);
             |    fence;
             |  }
             |}""".stripMargin
        } getFirst {
          case ExprBuiltin(_, _ :: ArgP(expr) :: Nil) => expr
        } tap {
          _ should matchPattern(pattern)
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
               |}""".stripMargin
          } getFirst {
            case StmtAssign(_, rhs) => rhs.simplify
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages filterNot { _.isInstanceOf[Warning] } shouldBe empty
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
               |}""".stripMargin
          } getFirst {
            case StmtAssign(_, rhs) => rhs
          } tap {
            _ should matchPattern(pattern)
          }
        }
      }
    }

    "+: and -: slices with a known base index" - {
      List(
        ("@unknownu(4)[0 +: 3]", 2, 0),
        ("@unknownu(4)[1 +: 2]", 2, 1),
        ("@unknownu(4)[1 +: 3]", 3, 1),
        ("@unknownu(4)[3 -: 3]", 3, 1),
        ("@unknownu(4)[2 -: 2]", 2, 1),
        ("@unknownu(4)[2 -: 3]", 2, 0)
      ) foreach {
        case (text, lIdx, rIdx) =>
          text in {
            simplify(text) should matchPattern {
              case ExprSlice(
                    UnknownU(4),
                    ExprInt(false, 2, l),
                    ":",
                    ExprInt(false, 2, r)
                  ) if l == lIdx && r == rIdx =>
            }
          }
      }

    }

    "cast" - {
      for {
        (exprSrc, kindSrc, errorOrPattern) <-
          List[(String, String, Either[String, PartialFunction[Any, Unit]])](
            // format: off
            ("32", "u8", Right({ case ExprInt(false, 8, v) if v == 32 => })),
            ("32s", "i8",Right({ case ExprInt(true,  8, v) if v == 32 => })),
            ("-1s", "i8",Right({ case ExprInt(true,  8, v) if v == -1 => })),
            ("32", "u4", Left("Value 32 cannot be represented with 4 unsigned bits")),
            ("32s", "i4", Left("Value 32 cannot be represented with 4 signed bits")),
            ("31", "u5", Right({ case ExprInt(false, 5, v) if v == 31 => })),
            ("31s", "i5",Left("Value 31 cannot be represented with 5 signed bits")),
            ("10'sd12",  "int", Right({ case ExprNum(true, v) if v == 12 => })),
            ("10'd12",  "uint", Right({ case ExprNum(false, v) if v == 12 => })),
            ("-10'sd1",  "int", Right({ case ExprNum(true, v) if v == -1 => })),
            ("a",  "u10", Right({ case ExprCat(List(ExprInt(false, 2, z), ExprSym(Symbol("a")))) if z == 0  => })),
            ("b",  "i10", Right({ case Signed(
                                  ExprCat(List(
                                        ExprRep(
                                          Expr(3),
                                          ExprIndex(
                                            ExprSym(Symbol("b")),
                                            ExprInt(false, 3, i))),
                                        ExprSym(Symbol("b"))))) if i == 6 =>
            })),
            ("a",  "u8", Right({ case ExprSym(Symbol("a")) => })),
            ("b",  "i7", Right({ case ExprSym(Symbol("b")) => })),
            ("c",  "u10", Right({ case ExprInt(false, 10, v) if v == 7 => })),
            ("c", "uint", Right({ case ExprNum(false, v) if v == 7 => })),
            ("d",  "i10", Right({ case ExprInt(true, 10, v) if v == -3 => })),
            ("d",  "int", Right({ case ExprNum(true, v) if v == -3 => }))
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
               |}""".stripMargin
          } getFirst {
            case ExprBuiltin(_, List(_, ArgP(e))) => e
          } tap { expr =>
            cc.messages.filterNot(_.isInstanceOf[Warning]) shouldBe empty
            val kind = kindSrc.asTree[Expr]() match {
              case ExprType(kind) => kind
              case _              => fail()
            }
            val call = TypeAssigner(ExprCast(kind, expr) withLoc Loc.synthetic)
            errorOrPattern match {
              case Left(error)    => (the[Fatal] thrownBy SimplifyExpr(call)).msg shouldBe Seq(error)
              case Right(pattern) => SimplifyExpr(call) should matchPattern(pattern)
            }
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
          ("10'b", { case Signed(
                            ExprCat(List(
                              ExprRep(Expr(3), ExprIndex(ExprSym(Symbol("b")), ExprInt(false, 3, i))), 
                              ExprSym(Symbol("b"))
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
               |}""".stripMargin
          } getFirst {
            case ExprBuiltin(_, List(_, ArgP(e))) => e
          } tap { expr =>
            cc.messages filterNot { _.isInstanceOf[Warning] } shouldBe empty
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
               |network a {
               |  typedef bool  bool_t;
               |  typedef i2    i2_t;
               |  typedef u2[3] v3u2_t;
               |  typedef void  void_t;
               |  typedef s_t   ts_t;
               |
               |  $decl
               |}""".stripMargin
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
