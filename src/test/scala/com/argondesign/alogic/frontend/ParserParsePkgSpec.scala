////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Tests for parsing Pkg
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FuncVariant
import com.argondesign.alogic.core.Types._
import org.scalatest.freespec.AnyFreeSpec

final class ParserParsePkgSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  "The parser should build correct ASTs for Pkg" - {
    "definition" - {
      "var" in {
        "bool x;".asTree[Pkg]() shouldBe {
          PkgSplice(DescVar(Ident("x", Nil), Nil, ExprType(TypeUInt(1)), None))
        }
      }

      "comb function" in {
        """void f() {}""".asTree[Pkg]() shouldBe {
          PkgSplice(DescFunc(Ident("f", Nil), Nil, FuncVariant.Comb, ExprType(TypeVoid), Nil, Nil))
        }
      }
    }

    "import" in {
      """import "a" as a;""".asTree[Pkg]() shouldBe {
        PkgSplice(ImportOne("a", Ident("a", Nil)))
      }
    }

    "using" in {
      "using a;".asTree[Pkg]() shouldBe {
        PkgSplice(UsingOne(ExprIdent("a", Nil), None))
      }
    }

    "from" in {
      """from "a" import b;""".asTree[Pkg]() shouldBe {
        PkgSplice(FromOne("a", ExprIdent("b", Nil), None))
      }
    }

    "assertion" in {
      "static assert false;".asTree[Pkg]() shouldBe {
        PkgSplice(AssertionStatic(ExprInt(false, 1, 0), None))
      }
    }

    "compile" - {
      "without alias" in {
        "compile 1;".asTree[Pkg]() shouldBe {
          PkgCompile(Expr(1), None)
        }
      }

      "with alias" in {
        "compile 1 as a;".asTree[Pkg]() shouldBe {
          PkgCompile(Expr(1), Some(Ident("a", Nil)))
        }
      }
    }
  }
}
