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

final class ParserParseFromSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  "The parser should build correct ASTs for From" - {
    "From one" - {
      "without alias" - {
        "ident" in {
          "from a import b;".asTree[From] shouldBe {
            FromOne(0, ExprIdent(Ident("a", Nil)), ExprIdent(Ident("b", Nil)), None)
          }
        }

        "select" in {
          "from a.b import c;".asTree[From] shouldBe {
            FromOne(
              0,
              ExprDot(ExprIdent(Ident("a", Nil)), "b", Nil),
              ExprIdent(Ident("c", Nil)),
              None
            )
          }
        }

        "call" in {
          "from a.b(0) import c;".asTree[From] shouldBe {
            FromOne(
              0,
              ExprCall(
                ExprDot(ExprIdent(Ident("a", Nil)), "b", Nil),
                ArgP(ExprNum(false, 0)) :: Nil
              ),
              ExprIdent(Ident("c", Nil)),
              None
            )
          }
        }
      }

      "with alias" - {
        "ident" in {
          "from a import b as x;".asTree[From] shouldBe {
            FromOne(
              0,
              ExprIdent(Ident("a", Nil)),
              ExprIdent(Ident("b", Nil)),
              Some(Ident("x", Nil))
            )
          }
        }

        "select" in {
          "from a.b import c as y;".asTree[From] shouldBe {
            FromOne(
              0,
              ExprDot(ExprIdent(Ident("a", Nil)), "b", Nil),
              ExprIdent(Ident("c", Nil)),
              Some(Ident("y", Nil))
            )
          }
        }

        "call" in {
          "from a.b(0) import c as z#[2];".asTree[From] shouldBe {
            FromOne(
              0,
              ExprCall(
                ExprDot(ExprIdent(Ident("a", Nil)), "b", Nil),
                ArgP(ExprNum(false, 0)) :: Nil
              ),
              ExprIdent(Ident("c", Nil)),
              Some(Ident("z", ExprNum(false, 2) :: Nil))
            )
          }
        }
      }

      "relative" - {
        "." in {
          "from .a import b;".asTree[From] shouldBe {
            FromOne(1, ExprIdent(Ident("a", Nil)), ExprIdent(Ident("b", Nil)), None)
          }
        }

        ".." in {
          "from ..a import b;".asTree[From] shouldBe {
            FromOne(2, ExprIdent(Ident("a", Nil)), ExprIdent(Ident("b", Nil)), None)
          }
        }

        "..." in {
          "from ...a import b;".asTree[From] shouldBe {
            FromOne(3, ExprIdent(Ident("a", Nil)), ExprIdent(Ident("b", Nil)), None)
          }
        }
      }
    }

    "Import all" - {
      "ident" in {
        "from a import *;".asTree[From] shouldBe {
          FromAll(0, ExprIdent(Ident("a", Nil)))
        }
      }

      "select" in {
        "from a.b import *;".asTree[From] shouldBe {
          FromAll(0, ExprDot(ExprIdent(Ident("a", Nil)), "b", Nil))
        }
      }

      "call" in {
        "from a.b(0) import *;".asTree[From] shouldBe {
          FromAll(
            0,
            ExprCall(ExprDot(ExprIdent(Ident("a", Nil)), "b", Nil), ArgP(ExprNum(false, 0)) :: Nil)
          )
        }
      }

      "relative" - {
        "." in {
          "from .a import *;".asTree[From] shouldBe {
            FromAll(1, ExprIdent(Ident("a", Nil)))
          }
        }

        ".." in {
          "from ..a import *;".asTree[From] shouldBe {
            FromAll(2, ExprIdent(Ident("a", Nil)))
          }
        }

        "..." in {
          "from ...a  import *;".asTree[From] shouldBe {
            FromAll(3, ExprIdent(Ident("a", Nil)))
          }
        }
      }
    }
  }
}
