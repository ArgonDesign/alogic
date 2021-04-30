////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Tests for parsing Ent
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FuncVariant
import com.argondesign.alogic.core.Types._
import org.scalatest.freespec.AnyFreeSpec

final class ParserParseEntSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  "The parser should build correct ASTs for Ent" - {
    "definition" - {
      "var" in {
        "bool x;".asTree[Ent]() shouldBe {
          EntSplice(DescVar(Ident("x", Nil), Nil, ExprType(TypeUInt(1)), None))
        }
      }

      "control function" in {
        """void f() {}""".asTree[Ent]() shouldBe {
          EntSplice(DescFunc(Ident("f", Nil), Nil, FuncVariant.Ctrl, ExprType(TypeVoid), Nil, Nil))
        }
      }
    }

    "import" in {
      """import "a" as a;""".asTree[Ent]() shouldBe {
        EntSplice(ImportOne("a", Ident("a", Nil)))
      }
    }

    "using" in {
      "using a;".asTree[Ent]() shouldBe {
        EntSplice(UsingOne(ExprIdent("a", Nil), None))
      }
    }

    "from" in {
      """from "a" import b;""".asTree[Ent]() shouldBe {
        EntSplice(FromOne("a", ExprIdent("b", Nil), None))
      }
    }

    "assertion" in {
      "static assert false;".asTree[Ent]() shouldBe {
        EntSplice(AssertionStatic(ExprInt(false, 1, 0), None))
      }
    }

    "connection" - {
      "single" in {
        "i.a -> j.b;".asTree[Ent]() shouldBe {
          EntConnect(
            ExprDot(ExprIdent("i", Nil), "a", Nil),
            List(ExprDot(ExprIdent("j", Nil), "b", Nil))
          )
        }
      }

      "multiple" in {
        "i.a -> j.b, k.c;".asTree[Ent]() shouldBe {
          EntConnect(
            ExprDot(ExprIdent("i", Nil), "a", Nil),
            List(
              ExprDot(ExprIdent("j", Nil), "b", Nil),
              ExprDot(ExprIdent("k", Nil), "c", Nil)
            )
          )
        }
      }

      "dict" in {
        "i.a#[0] -> j.b#[1, 2];".asTree[Ent]() shouldBe {
          EntConnect(
            ExprDot(ExprIdent("i", Nil), "a", Expr(0) :: Nil),
            List(ExprDot(ExprIdent("j", Nil), "b", Expr(1) :: Expr(2) :: Nil))
          )
        }
      }

      "wildcard inputs" in {
        "* -> i.*;".asTree[Ent]() shouldBe {
          EntConnectInputs(ExprIdent("i", Nil))
        }
      }
    }

    "fence block" in {
      "fence { a = 1; }".asTree[Ent]() shouldBe {
        EntCombProcess(List(StmtAssign(ExprIdent("a", Nil), Expr(1))))
      }
    }

    "verbatim block" - {
      "verilog" in {
        "verbatim verilog {\n    +-/* comment */ {{{}}}\n  }".asTree[Ent]() shouldBe {
          EntVerbatim("verilog", "\n    +-/* comment */ {{{}}}\n  ")
        }
      }

      "other" in {
        "verbatim other {\n    +-/* comment */ {{{}}}\n  }".asTree[Ent]() shouldBe {
          EntVerbatim("other", "\n    +-/* comment */ {{{}}}\n  ")
        }
      }
    }
  }
}
