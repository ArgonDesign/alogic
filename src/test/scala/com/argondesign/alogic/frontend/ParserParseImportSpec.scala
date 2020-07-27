////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Tests for parsing Import
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import org.scalatest.freespec.AnyFreeSpec

final class ParserParseImportSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  "The parser should build correct ASTs for Import" - {
    "Import one" - {
      "without alias" - {
        "ident" in {
          "import a;".asTree[Import] shouldBe {
            ImportOne(0, ExprIdent(Ident("a", Nil)), None)
          }
        }

        "select" in {
          "import a.b;".asTree[Import] shouldBe {
            ImportOne(0, ExprDot(ExprIdent(Ident("a", Nil)), "b", Nil), None)
          }
        }

        "call" in {
          "import a.b(0);".asTree[Import] shouldBe {
            ImportOne(
              0,
              ExprCall(
                ExprDot(ExprIdent(Ident("a", Nil)), "b", Nil),
                ArgP(ExprNum(false, 0)) :: Nil
              ),
              None
            )
          }
        }
      }

      "with alias" - {
        "ident" in {
          "import a as x;".asTree[Import] shouldBe {
            ImportOne(0, ExprIdent(Ident("a", Nil)), Some(Ident("x", Nil)))
          }
        }

        "select" in {
          "import a.b as y;".asTree[Import] shouldBe {
            ImportOne(0, ExprDot(ExprIdent(Ident("a", Nil)), "b", Nil), Some(Ident("y", Nil)))
          }
        }

        "call" in {
          "import a.b(0) as z#[2];".asTree[Import] shouldBe {
            ImportOne(
              0,
              ExprCall(
                ExprDot(ExprIdent(Ident("a", Nil)), "b", Nil),
                ArgP(ExprNum(false, 0)) :: Nil
              ),
              Some(Ident("z", ExprNum(false, 2) :: Nil))
            )
          }
        }
      }

      "relative" - {
        "." in {
          "import .a;".asTree[Import] shouldBe {
            ImportOne(1, ExprIdent(Ident("a", Nil)), None)
          }
        }

        ".." in {
          "import ..a;".asTree[Import] shouldBe {
            ImportOne(2, ExprIdent(Ident("a", Nil)), None)
          }
        }

        "..." in {
          "import ...a;".asTree[Import] shouldBe {
            ImportOne(3, ExprIdent(Ident("a", Nil)), None)
          }
        }
      }
    }

  }
}
