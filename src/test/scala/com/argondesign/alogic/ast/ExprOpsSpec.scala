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
import org.scalatest.FreeSpec

final class ExprOpsSpec extends FreeSpec with AlogicTest {
  implicit val cc: CompilerContext = new CompilerContext

  "ExprOps" - {
    "infix operator methods should build the appropriate binary expression node" - {
      "*" in { Expr(1) * Expr(2) shouldBe ExprBinary(Expr(1), "*", Expr(2)) }
      "/" in { Expr(1) / Expr(2) shouldBe ExprBinary(Expr(1), "/", Expr(2)) }
      "%" in { Expr(1) % Expr(2) shouldBe ExprBinary(Expr(1), "%", Expr(2)) }
      "+" in { Expr(1) + Expr(2) shouldBe ExprBinary(Expr(1), "+", Expr(2)) }
      "-" in { Expr(1) - Expr(2) shouldBe ExprBinary(Expr(1), "-", Expr(2)) }
      "<<" in { Expr(1) << Expr(2) shouldBe ExprBinary(Expr(1), "<<", Expr(2)) }
      ">>" in { Expr(1) >> Expr(2) shouldBe ExprBinary(Expr(1), ">>", Expr(2)) }
      ">>>" in { Expr(1) >>> Expr(2) shouldBe ExprBinary(Expr(1), ">>>", Expr(2)) }
      "&" in { Expr(1) & Expr(2) shouldBe ExprBinary(Expr(1), "&", Expr(2)) }
      "^" in { Expr(1) ^ Expr(2) shouldBe ExprBinary(Expr(1), "^", Expr(2)) }
      "|" in { Expr(1) | Expr(2) shouldBe ExprBinary(Expr(1), "|", Expr(2)) }
      "&&" in { Expr(1) && Expr(2) shouldBe ExprBinary(Expr(1), "&&", Expr(2)) }
      "||" in { Expr(1) || Expr(2) shouldBe ExprBinary(Expr(1), "||", Expr(2)) }
    }

    "operator extractors should work naturally" - {
      "*" in { Expr(1) * Expr(2) should matchPattern { case Expr(1) * Expr(2)       => } }
      "/" in { Expr(1) / Expr(2) should matchPattern { case Expr(1) / Expr(2)       => } }
      "%" in { Expr(1) % Expr(2) should matchPattern { case Expr(1) % Expr(2)       => } }
      "+" in { Expr(1) + Expr(2) should matchPattern { case Expr(1) + Expr(2)       => } }
      "-" in { Expr(1) - Expr(2) should matchPattern { case Expr(1) - Expr(2)       => } }
      "<<" in { Expr(1) << Expr(2) should matchPattern { case Expr(1) << Expr(2)    => } }
      ">>" in { Expr(1) >> Expr(2) should matchPattern { case Expr(1) >> Expr(2)    => } }
      ">>>" in { Expr(1) >>> Expr(2) should matchPattern { case Expr(1) >>> Expr(2) => } }
      "&" in { Expr(1) & Expr(2) should matchPattern { case Expr(1) & Expr(2)       => } }
      "^" in { Expr(1) ^ Expr(2) should matchPattern { case Expr(1) ^ Expr(2)       => } }
      "|" in { Expr(1) | Expr(2) should matchPattern { case Expr(1) `|` Expr(2)     => } }
      "&&" in { Expr(1) && Expr(2) should matchPattern { case Expr(1) && Expr(2)    => } }
      "||" in { Expr(1) || Expr(2) should matchPattern { case Expr(1) || Expr(2)    => } }
    }
  }
}
