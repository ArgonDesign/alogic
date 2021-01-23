////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Tests for parsing From
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
        """from "a" import b;""".asTree[From]() shouldBe {
          FromOne("a", ExprIdent(Ident("b", Nil)), None)
        }

        """from "a" import b.c;""".asTree[From]() shouldBe {
          FromOne("a", ExprDot(ExprIdent(Ident("b", Nil)), "c", Nil), None)
        }

        """from "a" import b.c(2);""".asTree[From]() shouldBe {
          FromOne(
            "a",
            ExprCall(ExprDot(ExprIdent(Ident("b", Nil)), "c", Nil), ArgP(Expr(2)) :: Nil),
            None
          )
        }
      }

      "with alias" - {
        """from "a" import b as x;""".asTree[From]() shouldBe {
          FromOne("a", ExprIdent(Ident("b", Nil)), Some(Ident("x", Nil)))
        }

        """from "a" import b.c as x;""".asTree[From]() shouldBe {
          FromOne("a", ExprDot(ExprIdent(Ident("b", Nil)), "c", Nil), Some(Ident("x", Nil)))
        }

        """from "a" import b.c(2) as x;""".asTree[From]() shouldBe {
          FromOne(
            "a",
            ExprCall(ExprDot(ExprIdent(Ident("b", Nil)), "c", Nil), ArgP(Expr(2)) :: Nil),
            Some(Ident("x", Nil))
          )
        }
      }
    }

    "Import all" - {
      """from "a" import *;""".asTree[From]() shouldBe {
        FromAll("a")
      }
    }
  }
}
