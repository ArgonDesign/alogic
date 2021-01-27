////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Tests for parsing Using
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import org.scalatest.freespec.AnyFreeSpec

final class ParserParseUsingSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  "The parser should build correct ASTs for Using" - {
    "Using one" - {
      "without alias" - {
        "ident" in {
          "using a;".asTree[Using]() shouldBe {
            UsingOne(ExprIdent("a", Nil), None)
          }
        }

        "select" in {
          "using a.b;".asTree[Using]() shouldBe {
            UsingOne(ExprDot(ExprIdent("a", Nil), "b", Nil), None)
          }
        }

        "call" in {
          "using a.b(0);".asTree[Using]() shouldBe {
            UsingOne(
              ExprCall(
                ExprDot(ExprIdent("a", Nil), "b", Nil),
                ArgP(ExprNum(false, 0)) :: Nil
              ),
              None
            )
          }
        }
      }

      "with alias" - {
        "ident" in {
          "using a as x;".asTree[Using]() shouldBe {
            UsingOne(ExprIdent("a", Nil), Some(Ident("x", Nil)))
          }
        }

        "select" in {
          "using a.b as y;".asTree[Using]() shouldBe {
            UsingOne(ExprDot(ExprIdent("a", Nil), "b", Nil), Some(Ident("y", Nil)))
          }
        }

        "call" in {
          "using a.b(0) as z#[2];".asTree[Using]() shouldBe {
            UsingOne(
              ExprCall(
                ExprDot(ExprIdent("a", Nil), "b", Nil),
                ArgP(ExprNum(false, 0)) :: Nil
              ),
              Some(Ident("z", ExprNum(false, 2) :: Nil))
            )
          }
        }
      }
    }

    "Using all" - {
      "ident" in {
        "using a.*;".asTree[Using]() shouldBe {
          UsingAll(ExprIdent("a", Nil), false)
        }
      }

      "select" in {
        "using a.b.*;".asTree[Using]() shouldBe {
          UsingAll(ExprDot(ExprIdent("a", Nil), "b", Nil), false)
        }
      }

      "call" in {
        "using a.b(0).*;".asTree[Using]() shouldBe {
          UsingAll(
            ExprCall(ExprDot(ExprIdent("a", Nil), "b", Nil), ArgP(ExprNum(false, 0)) :: Nil),
            false
          )
        }
      }
    }
  }
}
