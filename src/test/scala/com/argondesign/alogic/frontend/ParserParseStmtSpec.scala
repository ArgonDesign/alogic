////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Tests for parsing Stmt
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.SourceContext
import com.argondesign.alogic.core.Types._
import org.scalatest.freespec.AnyFreeSpec

final class ParserParseStmtSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  "The parser should build correct ASTs for Stmt" - {
    "definition" - {
      "var" in {
        "bool x;".asTree[Stmt] shouldBe {
          StmtSplice(DescVar(Ident("x", Nil), Nil, ExprType(TypeUInt(1)), None))
        }
      }

      "val" in {
        "const bool x = true;".asTree[Stmt](SourceContext.FuncCtrl) shouldBe {
          StmtSplice(DescVal(Ident("x", Nil), Nil, ExprType(TypeUInt(1)), ExprInt(false, 1, 1)))
        }
      }
    }

    "import" in {
      """import "a" as a;""".asTree[Stmt] shouldBe {
        StmtSplice(ImportOne("a", Ident("a", Nil)))
      }
    }

    "using" in {
      "using a;".asTree[Stmt] shouldBe {
        StmtSplice(UsingOne(ExprIdent(Ident("a", Nil)), None))
      }
    }

    "from" in {
      """from "a" import b;""".asTree[Stmt] shouldBe {
        StmtSplice(FromOne("a", ExprIdent(Ident("b", Nil)), None))
      }
    }

    "assertion" in {
      "static assert false;".asTree[Stmt] shouldBe {
        StmtSplice(AssertionStatic(ExprInt(false, 1, 0), None))
      }
    }

    "blocks" - {
      "empty block" in {
        "{}".asTree[Stmt] shouldBe StmtBlock(Nil)
      }

      "single statement block" in {
        "{ 1; }".asTree[Stmt] shouldBe StmtBlock(List(StmtExpr(Expr(1))))
      }

      "multiple statement block" in {
        "{ 1; 2; 3; }".asTree[Stmt] shouldBe {
          StmtBlock(List(StmtExpr(Expr(1)), StmtExpr(Expr(2)), StmtExpr(Expr(3))))
        }
      }
    }

    "if" - {
      "without else, without brace" in {
        "if (1) a;"
          .asTree[Stmt] shouldBe StmtIf(Expr(1), List(StmtExpr(ExprIdent(Ident("a", Nil)))), Nil)
      }

      "with else, without brace" in {
        "if (1) fence; else return;"
          .asTree[Stmt](SourceContext.FuncCtrl) shouldBe StmtIf(
          Expr(1),
          List(StmtFence()),
          List(StmtBlock(List(StmtReturn(comb = false, None))))
        )
      }

      "without else, with brace" in {
        "if (1) {a;}"
          .asTree[Stmt] shouldBe StmtIf(Expr(1), List(StmtExpr(ExprIdent(Ident("a", Nil)))), Nil)
      }

      "with else, with brace" in {
        "if (1) {fence;} else {return;}".asTree[Stmt](SourceContext.FuncCtrl) shouldBe {
          StmtIf(
            Expr(1),
            List(StmtFence()),
            List(StmtBlock(List(StmtReturn(comb = false, None))))
          )
        }
      }

      "with explicitly empty else" in {
        "if (1) {} else {};"
          .asTree[Stmt](SourceContext.Entity) shouldBe StmtIf(
          Expr(1),
          Nil,
          List(StmtBlock(Nil))
        )
      }
    }

    "case" - {
      "without default" in {
        """case (1) {
          | 1: a;
          | 2: b;
          |}
          |""".stripMargin.asTree[Stmt] shouldBe {
          StmtCase(
            Expr(1),
            List(
              CaseRegular(List(Expr(1)), List(StmtExpr(ExprIdent(Ident("a", Nil))))),
              CaseRegular(List(Expr(2)), List(StmtExpr(ExprIdent(Ident("b", Nil)))))
            )
          )
        }
      }

      "with default" in {
        """case (1) {
          | default: c;
          |}
          |""".stripMargin.asTree[Stmt] shouldBe {
          StmtCase(
            Expr(1),
            List(
              CaseDefault(List(StmtExpr(ExprIdent(Ident("c", Nil)))))
            )
          )
        }
      }

      "with multiple labels" in {
        """case (1) {
          | 1: c;
          | 2, 3: d;
          |}
          |""".stripMargin.asTree[Stmt] shouldBe {
          StmtCase(
            Expr(1),
            List(
              CaseRegular(List(Expr(1)), List(StmtExpr(ExprIdent(Ident("c", Nil))))),
              CaseRegular(List(Expr(2), Expr(3)), List(StmtExpr(ExprIdent(Ident("d", Nil)))))
            )
          )
        }
      }

      "with multiple defaults" in {
        """case (1) {
          | default: c;
          | default: d;
          |}
          |""".stripMargin.asTree[Stmt] shouldBe {
          StmtCase(
            Expr(1),
            List(
              CaseDefault(List(StmtExpr(ExprIdent(Ident("c", Nil))))),
              CaseDefault(List(StmtExpr(ExprIdent(Ident("d", Nil)))))
            )
          )
        }
      }

      "ordering" in {
        """case (1) {
          | 1: a;
          | default: b;
          | 3: c;
          | default: d;
          |}
          |""".stripMargin.asTree[Stmt] shouldBe {
          StmtCase(
            Expr(1),
            List(
              CaseRegular(List(Expr(1)), List(StmtExpr(ExprIdent(Ident("a", Nil))))),
              CaseDefault(List(StmtExpr(ExprIdent(Ident("b", Nil))))),
              CaseRegular(List(Expr(3)), List(StmtExpr(ExprIdent(Ident("c", Nil))))),
              CaseDefault(List(StmtExpr(ExprIdent(Ident("d", Nil)))))
            )
          )
        }
      }

      "without braces" in {
        """case (1) {
          | 1: a;
          | default: c;
          |}
          |""".stripMargin.asTree[Stmt] shouldBe {
          StmtCase(
            Expr(1),
            List(
              CaseRegular(List(Expr(1)), List(StmtExpr(ExprIdent(Ident("a", Nil))))),
              CaseDefault(List(StmtExpr(ExprIdent(Ident("c", Nil)))))
            )
          )
        }
      }

      "with braces" in {
        """case (1) {
          | 1: {a;}
          | default: {c;}
          |}
          |""".stripMargin.asTree[Stmt] shouldBe {
          StmtCase(
            Expr(1),
            List(
              CaseRegular(List(Expr(1)), List(StmtExpr(ExprIdent(Ident("a", Nil))))),
              CaseDefault(List(StmtExpr(ExprIdent(Ident("c", Nil)))))
            )
          )
        }
      }

      "desc (gen)" in {
        """case (1) {
          | gen if (false) {}
          |}
          |""".stripMargin.asTree[Stmt] shouldBe {
          StmtCase(
            Expr(1),
            List(
              CaseSplice(
                DescGenIf(Ident("", Nil), Nil, GenCase(ExprInt(false, 1, 0), Nil) :: Nil, Nil)
              )
            )
          )
        }
      }
    }

    "loop" in {
      """loop {
        |  1;
        |}""".stripMargin.asTree[Stmt] shouldBe StmtLoop(List(StmtExpr(Expr(1))))
    }

    "while" in {
      """while (a) {
        |  fence;
        |}""".stripMargin
        .asTree[Stmt] shouldBe StmtWhile(ExprIdent(Ident("a", Nil)), List(StmtFence()))
    }

    "do" in {
      """do {
        | fence;
        |} while(b);""".stripMargin
        .asTree[Stmt] shouldBe StmtDo(ExprIdent(Ident("b", Nil)), List(StmtFence()))
    }

    "for" - {
      "empty" in {
        "for(;;){}".asTree[Stmt] shouldBe StmtFor(Nil, None, Nil, Nil)
      }

      "with single init assign" in {
        """for (a=2;a;a--) {
          |  2;
          |}""".stripMargin.asTree[Stmt] shouldBe {
          StmtFor(
            List(StmtAssign(ExprIdent(Ident("a", Nil)), Expr(2))),
            Some(ExprIdent(Ident("a", Nil))),
            List(StmtPost(ExprIdent(Ident("a", Nil)), "--")),
            List(StmtExpr(Expr(2)))
          )

        }
      }

      "with single init decl" in {
        """for (i8 a=2;a;a--) {
          |  2;
          |}""".stripMargin.asTree[Stmt] shouldBe {
          StmtFor(
            List(StmtSplice(DescVar(Ident("a", Nil), Nil, ExprType(TypeSInt(8)), Some(Expr(2))))),
            Some(ExprIdent(Ident("a", Nil))),
            List(StmtPost(ExprIdent(Ident("a", Nil)), "--")),
            List(StmtExpr(Expr(2)))
          )
        }
      }

      "with multiple init" in {
        "for (i8 a=2, b=1;;) {}".asTree[Stmt] shouldBe {
          StmtFor(
            List(
              StmtSplice(DescVar(Ident("a", Nil), Nil, ExprType(TypeSInt(8)), Some(Expr(2)))),
              StmtAssign(ExprIdent(Ident("b", Nil)), Expr(1))
            ),
            None,
            Nil,
            Nil
          )
        }
      }

      "with multiple step" in {
        "for (;;a++, b--) {}".asTree[Stmt] shouldBe {
          StmtFor(
            Nil,
            None,
            List(
              StmtPost(ExprIdent(Ident("a", Nil)), "++"),
              StmtPost(ExprIdent(Ident("b", Nil)), "--")
            ),
            Nil
          )
        }
      }

      "with assign step" in {
        "for (;;a = b) {}".asTree[Stmt] shouldBe {
          StmtFor(
            Nil,
            None,
            List(
              StmtAssign(ExprIdent(Ident("a", Nil)), ExprIdent(Ident("b", Nil)))
            ),
            Nil
          )
        }
      }

      "with update step" in {
        "for (;;a += b) {}".asTree[Stmt] shouldBe {
          StmtFor(
            Nil,
            None,
            List(
              StmtUpdate(ExprIdent(Ident("a", Nil)), "+", ExprIdent(Ident("b", Nil)))
            ),
            Nil
          )
        }
      }
    }

    "let" - {
      "single declaration" in {
        "let (i2 a=1) loop {}".asTree[Stmt] shouldBe {
          StmtLet(
            List(StmtSplice(DescVar(Ident("a", Nil), Nil, ExprType(TypeSInt(2)), Some(Expr(1))))),
            List(StmtLoop(Nil))
          )
        }
      }

      "multiple declarations" in {
        "let (i2 a=b, i2 c=a) loop {}".asTree[Stmt] shouldBe {
          StmtLet(
            List(
              StmtSplice(
                DescVar(
                  Ident("a", Nil),
                  Nil,
                  ExprType(TypeSInt(2)),
                  Some(ExprIdent(Ident("b", Nil)))
                )
              ),
              StmtSplice(
                DescVar(
                  Ident("c", Nil),
                  Nil,
                  ExprType(TypeSInt(2)),
                  Some(ExprIdent(Ident("a", Nil)))
                )
              )
            ),
            List(StmtLoop(Nil))
          )
        }
      }
    }

    "fence" in {
      "fence;".asTree[Stmt] shouldBe StmtFence()
    }

    "break" in {
      "break;".asTree[Stmt] shouldBe StmtBreak()
    }

    "continue" in {
      "continue;".asTree[Stmt] shouldBe StmtContinue()
    }

    "goto" - {
      "goto call" in {
        "goto a();".asTree[Stmt] shouldBe StmtGoto(ExprCall(ExprIdent(Ident("a", Nil)), Nil))
      }

      "goto symbol" in {
        "goto b;".asTree[Stmt] shouldBe StmtGoto(ExprIdent(Ident("b", Nil)))
      }

      "goto const" in {
        "goto 1;".asTree[Stmt] shouldBe StmtGoto(ExprNum(false, 1))
      }
    }

    "return" - {
      "entity" - {
        "void" in {
          "return;".asTree[Stmt](SourceContext.FuncCtrl) shouldBe StmtReturn(comb = false, None)
        }

        "value" in {
          "return 0;"
            .asTree[Stmt](SourceContext.FuncCtrl) shouldBe StmtReturn(comb = false, Some(Expr(0)))
        }
      }

      "record" - {
        "void" in {
          "return;".asTree[Stmt](SourceContext.FuncComb) shouldBe StmtReturn(comb = true, None)
        }

        "value" in {
          "return 0;"
            .asTree[Stmt](SourceContext.FuncComb) shouldBe StmtReturn(comb = true, Some(Expr(0)))
        }
      }
    }

    "assign" - {
      "a = 1;".asTree[Stmt] shouldBe StmtAssign(ExprIdent(Ident("a", Nil)), Expr(1))
    }

    "update" - {
      "+=" in {
        "b += 2;".asTree[Stmt] shouldBe StmtUpdate(ExprIdent(Ident("b", Nil)), "+", Expr(2))
      }

      "<<=" in {
        "c <<= 3;".asTree[Stmt] shouldBe StmtUpdate(ExprIdent(Ident("c", Nil)), "<<", Expr(3))
      }
    }

    "post" - {
      "++" in {
        "d++;".asTree[Stmt] shouldBe StmtPost(ExprIdent(Ident("d", Nil)), "++")
      }

      "--" in {
        "e--;".asTree[Stmt] shouldBe StmtPost(ExprIdent(Ident("e", Nil)), "--")
      }
    }

    "expr" - {
      "identifier" in {
        "a;".asTree[Stmt] shouldBe StmtExpr(ExprIdent(Ident("a", Nil)))
      }

      "call" in {
        "b();".asTree[Stmt] shouldBe StmtExpr(ExprCall(ExprIdent(Ident("b", Nil)), Nil))
      }
    }

    "wait" - {
      "with condition" in {
        "wait a;".asTree[Stmt] shouldBe {
          StmtWait(ExprIdent(Ident("a", Nil)))
        }
      }

      "without condition" in {
        "wait;".asTree[Stmt] shouldBe {
          StmtWait(ExprInt(false, 1, 0))
        }
      }
    }
  }
}
