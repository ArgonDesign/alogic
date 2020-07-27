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
// Tests for ExprOps
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.ast.Trees.Expr._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.Types.TypeUInt
import org.scalatest.freespec.AnyFreeSpec

final class ExprOpsSpec extends AnyFreeSpec with AlogicTest {
  implicit val cc: CompilerContext = new CompilerContext

  "ExprOps" - {
    "infix operator methods should build the appropriate binary expression node" - {
      "Expr rhs" - {
        "*" in { Expr(1) * Expr(2) shouldBe ExprBinary(Expr(1), "*", Expr(2)) }
        "/" in { Expr(1) / Expr(2) shouldBe ExprBinary(Expr(1), "/", Expr(2)) }
        "%" in { Expr(1) % Expr(2) shouldBe ExprBinary(Expr(1), "%", Expr(2)) }
        "+" in { Expr(1) + Expr(2) shouldBe ExprBinary(Expr(1), "+", Expr(2)) }
        "-" in { Expr(1) - Expr(2) shouldBe ExprBinary(Expr(1), "-", Expr(2)) }
        "<<" in { Expr(1) << Expr(2) shouldBe ExprBinary(Expr(1), "<<", Expr(2)) }
        ">>" in { Expr(1) >> Expr(2) shouldBe ExprBinary(Expr(1), ">>", Expr(2)) }
        "<<<" in { Expr(1) <<< Expr(2) shouldBe ExprBinary(Expr(1), "<<<", Expr(2)) }
        ">>>" in { Expr(1) >>> Expr(2) shouldBe ExprBinary(Expr(1), ">>>", Expr(2)) }
        "&" in { Expr(1) & Expr(2) shouldBe ExprBinary(Expr(1), "&", Expr(2)) }
        "^" in { Expr(1) ^ Expr(2) shouldBe ExprBinary(Expr(1), "^", Expr(2)) }
        "|" in { Expr(1) | Expr(2) shouldBe ExprBinary(Expr(1), "|", Expr(2)) }
        "&&" in { Expr(1) && Expr(2) shouldBe ExprBinary(Expr(1), "&&", Expr(2)) }
        "||" in { Expr(1) || Expr(2) shouldBe ExprBinary(Expr(1), "||", Expr(2)) }
        "<" in { Expr(1) < Expr(2) shouldBe ExprBinary(Expr(1), "<", Expr(2)) }
        "<=" in { Expr(1) <= Expr(2) shouldBe ExprBinary(Expr(1), "<=", Expr(2)) }
        ">" in { Expr(1) > Expr(2) shouldBe ExprBinary(Expr(1), ">", Expr(2)) }
        ">=" in { Expr(1) >= Expr(2) shouldBe ExprBinary(Expr(1), ">=", Expr(2)) }
      }

      "unsized lhs, Int rhs" - {
        "*" in { TypeAssigner(Expr(1)) * 2 shouldBe ExprBinary(Expr(1), "*", Expr(2)) }
        "/" in { TypeAssigner(Expr(1)) / 2 shouldBe ExprBinary(Expr(1), "/", Expr(2)) }
        "%" in { TypeAssigner(Expr(1)) % 2 shouldBe ExprBinary(Expr(1), "%", Expr(2)) }
        "+" in { TypeAssigner(Expr(1)) + 2 shouldBe ExprBinary(Expr(1), "+", Expr(2)) }
        "-" in { TypeAssigner(Expr(1)) - 2 shouldBe ExprBinary(Expr(1), "-", Expr(2)) }
        "<<" in { TypeAssigner(Expr(1)) << 2 shouldBe ExprBinary(Expr(1), "<<", Expr(2)) }
        ">>" in { TypeAssigner(Expr(1)) >> 2 shouldBe ExprBinary(Expr(1), ">>", Expr(2)) }
        "<<<" in { TypeAssigner(Expr(1)) <<< 2 shouldBe ExprBinary(Expr(1), "<<<", Expr(2)) }
        ">>>" in { TypeAssigner(Expr(1)) >>> 2 shouldBe ExprBinary(Expr(1), ">>>", Expr(2)) }
        "&" in { TypeAssigner(Expr(1)) & 2 shouldBe ExprBinary(Expr(1), "&", Expr(2)) }
        "^" in { TypeAssigner(Expr(1)) ^ 2 shouldBe ExprBinary(Expr(1), "^", Expr(2)) }
        "|" in { TypeAssigner(Expr(1)) | 2 shouldBe ExprBinary(Expr(1), "|", Expr(2)) }
        "&&" in { TypeAssigner(Expr(1)) && 2 shouldBe ExprBinary(Expr(1), "&&", Expr(2)) }
        "||" in { TypeAssigner(Expr(1)) || 2 shouldBe ExprBinary(Expr(1), "||", Expr(2)) }
        "<" in { TypeAssigner(Expr(1)) < 2 shouldBe ExprBinary(Expr(1), "<", Expr(2)) }
        "<=" in { TypeAssigner(Expr(1)) <= 2 shouldBe ExprBinary(Expr(1), "<=", Expr(2)) }
        ">" in { TypeAssigner(Expr(1)) > 2 shouldBe ExprBinary(Expr(1), ">", Expr(2)) }
        ">=" in { TypeAssigner(Expr(1)) >= 2 shouldBe ExprBinary(Expr(1), ">=", Expr(2)) }
      }

      "sized lhs, Int rhs" - {
        // format: off
        "*" in { TypeAssigner(ExprInt(false, 8, 1)) * 2 shouldBe ExprBinary(ExprInt(false, 8, 1), "*", ExprInt(false, 8, 2)) }
        "/" in { TypeAssigner(ExprInt(false, 8, 1)) / 2 shouldBe ExprBinary(ExprInt(false, 8, 1), "/", ExprInt(false, 8, 2)) }
        "%" in { TypeAssigner(ExprInt(false, 8, 1)) % 2 shouldBe ExprBinary(ExprInt(false, 8, 1), "%", ExprInt(false, 8, 2)) }
        "+" in { TypeAssigner(ExprInt(false, 8, 1)) + 2 shouldBe ExprBinary(ExprInt(false, 8, 1), "+", ExprInt(false, 8, 2)) }
        "-" in { TypeAssigner(ExprInt(false, 8, 1)) - 2 shouldBe ExprBinary(ExprInt(false, 8, 1), "-", ExprInt(false, 8, 2)) }
        "<<" in { TypeAssigner(ExprInt(false, 8, 1)) << 2 shouldBe ExprBinary(ExprInt(false, 8, 1), "<<", Expr(2)) }
        ">>" in { TypeAssigner(ExprInt(false, 8, 1)) >> 2 shouldBe ExprBinary(ExprInt(false, 8, 1), ">>", Expr(2)) }
        "<<<" in { TypeAssigner(ExprInt(false, 8, 1)) <<< 2 shouldBe ExprBinary(ExprInt(false, 8, 1), "<<<", Expr(2)) }
        ">>>" in { TypeAssigner(ExprInt(false, 8, 1)) >>> 2 shouldBe ExprBinary(ExprInt(false, 8, 1), ">>>", Expr(2)) }
        "&" in { TypeAssigner(ExprInt(false, 8, 1)) & 2 shouldBe ExprBinary(ExprInt(false, 8, 1), "&", ExprInt(false, 8, 2)) }
        "^" in { TypeAssigner(ExprInt(false, 8, 1)) ^ 2 shouldBe ExprBinary(ExprInt(false, 8, 1), "^", ExprInt(false, 8, 2)) }
        "|" in { TypeAssigner(ExprInt(false, 8, 1)) | 2 shouldBe ExprBinary(ExprInt(false, 8, 1), "|", ExprInt(false, 8, 2)) }
        "&&" in { TypeAssigner(ExprInt(false, 8, 1)) && 2 shouldBe ExprBinary(ExprInt(false, 8, 1), "&&", Expr(2)) }
        "||" in { TypeAssigner(ExprInt(false, 8, 1)) || 2 shouldBe ExprBinary(ExprInt(false, 8, 1), "||", Expr(2)) }
        "<" in { TypeAssigner(ExprInt(false, 8, 1)) < 2 shouldBe ExprBinary(ExprInt(false, 8, 1), "<", ExprInt(false, 8, 2)) }
        "<=" in { TypeAssigner(ExprInt(false, 8, 1)) <= 2 shouldBe ExprBinary(ExprInt(false, 8, 1), "<=", ExprInt(false, 8, 2)) }
        ">" in { TypeAssigner(ExprInt(false, 8, 1)) > 2 shouldBe ExprBinary(ExprInt(false, 8, 1), ">", ExprInt(false, 8, 2)) }
        ">=" in { TypeAssigner(ExprInt(false, 8, 1)) >= 2 shouldBe ExprBinary(ExprInt(false, 8, 1), ">=", ExprInt(false, 8, 2)) }
        // format: on
      }
    }

    "binary operator extractors should work naturally" - {
      "*" in { Expr(1) * Expr(2) should matchPattern { case Expr(1) * Expr(2) => } }
      "/" in { Expr(1) / Expr(2) should matchPattern { case Expr(1) / Expr(2) => } }
      "%" in { Expr(1) % Expr(2) should matchPattern { case Expr(1) % Expr(2) => } }
      "+" in { Expr(1) + Expr(2) should matchPattern { case Expr(1) + Expr(2) => } }
      "-" in { Expr(1) - Expr(2) should matchPattern { case Expr(1) - Expr(2) => } }
      "<<" in { Expr(1) << Expr(2) should matchPattern { case Expr(1) << Expr(2) => } }
      ">>" in { Expr(1) >> Expr(2) should matchPattern { case Expr(1) >> Expr(2) => } }
      "<<<" in { Expr(1) <<< Expr(2) should matchPattern { case Expr(1) <<< Expr(2) => } }
      ">>>" in { Expr(1) >>> Expr(2) should matchPattern { case Expr(1) >>> Expr(2) => } }
      "&" in { Expr(1) & Expr(2) should matchPattern { case Expr(1) & Expr(2) => } }
      "^" in { Expr(1) ^ Expr(2) should matchPattern { case Expr(1) ^ Expr(2) => } }
      "|" in { Expr(1) | Expr(2) should matchPattern { case Expr(1) `|` Expr(2) => } }
      "&&" in { Expr(1) && Expr(2) should matchPattern { case Expr(1) && Expr(2) => } }
      "||" in { Expr(1) || Expr(2) should matchPattern { case Expr(1) || Expr(2) => } }
      "<" in { Expr(1) < Expr(2) should matchPattern { case Expr(1) < Expr(2) => } }
      "<=" in { Expr(1) <= Expr(2) should matchPattern { case Expr(1) <= Expr(2) => } }
      ">" in { Expr(1) > Expr(2) should matchPattern { case Expr(1) > Expr(2) => } }
      ">=" in { Expr(1) >= Expr(2) should matchPattern { case Expr(1) >= Expr(2) => } }
    }
  }

  "unary operator methods should build the appropriate Unary expression node" - {
    "+" in {
      val expr = Expr(1)
      +expr should be theSameInstanceAs expr
    }
    "-" in { -Expr(1) shouldBe ExprUnary("-", Expr(1)) }
    "~" in { ~Expr(1) shouldBe ExprUnary("~", Expr(1)) }
    "!" in { !Expr(1) shouldBe ExprUnary("!", Expr(1)) }
  }

  "other combiner methods" - {
    "index Expr" in {
      ExprInt(false, 10, 0) index Expr(1) shouldBe ExprIndex(ExprInt(false, 10, 0), Expr(1))
    }
    "index Int" in {
      TypeAssigner(ExprInt(false, 10, 0)) index 1 should matchPattern {
        case ExprIndex(ExprInt(false, 10, v), ExprInt(false, 4, i)) if v == 0 && i == 1 =>
      }
    }

    "slice Expr : Expr" in {
      ExprInt(false, 8, 0).slice(Expr(2), ":", Expr(1)) shouldBe {
        ExprSlice(ExprInt(false, 8, 0), Expr(2), ":", Expr(1))
      }
    }
    "slice Expr : Int" in {
      TypeAssigner(ExprInt(false, 8, 0))
        .slice(2, ":", TypeAssigner(Expr(1))) should matchPattern {
        case ExprSlice(ExprInt(false, 8, v), ExprInt(false, 3, b), ":", Expr(1))
            if v == 0 && b == 2 =>
      }
    }
    "slice Int : Expr" in {
      TypeAssigner(ExprInt(false, 8, 0))
        .slice(TypeAssigner(Expr(2)), ":", 1) should matchPattern {
        case ExprSlice(ExprInt(false, 8, v), Expr(2), ":", ExprInt(false, 3, a))
            if v == 0 && a == 1 =>
      }
    }
    "slice Int : Int" in {
      TypeAssigner(ExprInt(false, 8, 0)).slice(2, ":", 1) should matchPattern {
        case ExprSlice(ExprInt(false, 8, v), ExprInt(false, 3, a), ":", ExprInt(false, 3, b))
            if v == 0 && a == 2 && b == 1 =>
      }
    }

    "slice Expr +: Expr" in {
      ExprInt(false, 8, 0).slice(Expr(1), "+:", Expr(2)) shouldBe {
        ExprSlice(ExprInt(false, 8, 0), Expr(1), "+:", Expr(2))
      }
    }
    "slice Expr +: Int" in {
      TypeAssigner(ExprInt(false, 8, 0))
        .slice(TypeAssigner(Expr(1)), "+:", 2) should matchPattern {
        case ExprSlice(ExprInt(false, 8, v), Expr(1), "+:", ExprInt(false, 4, b))
            if v == 0 && b == 2 =>
      }
    }
    "slice Int +: Expr" in {
      TypeAssigner(ExprInt(false, 8, 0))
        .slice(1, "+:", TypeAssigner(Expr(2))) should matchPattern {
        case ExprSlice(ExprInt(false, 8, v), ExprInt(false, 3, a), "+:", Expr(2))
            if v == 0 && a == 1 =>
      }
    }
    "slice Int +: Int" in {
      TypeAssigner(ExprInt(false, 8, 0)).slice(1, "+:", 2) should matchPattern {
        case ExprSlice(ExprInt(false, 8, v), ExprInt(false, 3, a), "+:", ExprInt(false, 4, b))
            if v == 0 && a == 1 && b == 2 =>
      }
    }

    "slice Expr -: Expr" in {
      ExprInt(false, 7, 0).slice(Expr(1), "-:", Expr(2)) shouldBe {
        ExprSlice(ExprInt(false, 7, 0), Expr(1), "-:", Expr(2))
      }
    }
    "slice Expr -: Int" in {
      TypeAssigner(ExprInt(false, 7, 0))
        .slice(TypeAssigner(Expr(1)), "-:", 2) should matchPattern {
        case ExprSlice(ExprInt(false, 7, v), Expr(1), "-:", ExprInt(false, 3, b))
            if v == 0 && b == 2 =>
      }
    }
    "slice Int -: Expr" in {
      TypeAssigner(ExprInt(false, 7, 0))
        .slice(1, "-:", TypeAssigner(Expr(2))) should matchPattern {
        case ExprSlice(ExprInt(false, 7, v), ExprInt(false, 3, a), "-:", Expr(2))
            if v == 0 && a == 1 =>
      }
    }
    "slice Int -: Int" in {
      TypeAssigner(ExprInt(false, 7, 0)).slice(1, "-:", 2) should matchPattern {
        case ExprSlice(ExprInt(false, 7, v), ExprInt(false, 3, a), "-:", ExprInt(false, 3, b))
            if v == 0 && a == 1 && b == 2 =>
      }
    }

    "select" in {
      Expr(1) sel "b" shouldBe ExprSel(Expr(1), "b")
    }

    "call" in {
      Expr(1) call List(ArgP(Expr(2)), ArgP(Expr(3))) shouldBe {
        ExprCall(Expr(1), List(ArgP(Expr(2)), ArgP(Expr(3))))
      }
    }

    "cat" in {
      Expr(1) cat Expr(2) shouldBe ExprCat(List(Expr(1), Expr(2)))
    }

    "rep Expr" in {
      Expr(1) rep Expr(2) shouldBe ExprRep(Expr(2), Expr(1))
    }
    "rep Int" in {
      Expr(1) rep 2 shouldBe ExprRep(Expr(2), Expr(1))
    }

    "cast" in {
      Expr(1) cast TypeUInt(8) shouldBe ExprCast(TypeUInt(8), Expr(1))
    }

    "inc 1-bit" in {
      TypeAssigner(ExprInt(false, 1, 0)).inc shouldBe ~ExprInt(false, 1, 0)
    }

    "inc n-bit" in {
      TypeAssigner(ExprInt(false, 2, 0)).inc shouldBe ExprInt(false, 2, 0) + ExprInt(
        false,
        2,
        1
      )
    }

    "inc num" in {
      TypeAssigner(Expr(0)).inc shouldBe Expr(0) + Expr(1)
    }

    "dec num" in {
      TypeAssigner(Expr(0)).dec shouldBe Expr(0) - Expr(1)
    }
  }
}
