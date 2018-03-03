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
// Namer tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.Expr._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.Symbols._

import org.scalatest.FreeSpec

final class DesugarSpec extends FreeSpec with AlogicTest {

  implicit val cc = new CompilerContext
  val namer = new Namer
  val desugar = new Desugar

  "Desugar should" - {
    "rewire postfix statements as assignments" - {
      for (op <- List("++", "--")) {
        op in {
          val tree = s"{ i2 a; a${op}; }".asTree[StmtBlock] rewrite namer rewrite desugar

          cc.messages shouldBe empty

          inside(tree) {
            case StmtBlock(List(StmtDecl(decl), stmt)) =>
              val Sym(dSym) = decl.ref
              inside(stmt) {
                case StmtAssign(lhs, rhs) =>
                  lhs shouldBe ExprRef(Sym(dSym))
                  inside(rhs) {
                    case ExprBinary(ExprRef(Sym(sym)), opStr, incr) =>
                      opStr shouldBe op.init
                      sym should be theSameInstanceAs dSym
                      inside(incr) {
                        case ExprAtCall("zx", List(width, value)) =>
                          width shouldBe ExprAtCall("bits", List(ExprRef(Sym(dSym))))
                          value shouldBe ExprNum(false, Some(1), 1)
                      }
                  }
              }
          }
        }
      }
    }

    "rewire update statements as assignments" - {
      for (op <- List("*", "/", "%", "+", "-", "<<", ">>", ">>>", "&", "|", "^", "~^")) {
        s"${op}=" in {
          val tree = s"{ i100 a; a ${op}= 2; }".asTree[StmtBlock] rewrite namer rewrite desugar

          cc.messages shouldBe empty

          inside(tree) {
            case StmtBlock(List(StmtDecl(decl), stmt)) =>
              val Sym(dSym) = decl.ref
              inside(stmt) {
                case StmtAssign(lhs, rhs) =>
                  lhs shouldBe ExprRef(Sym(dSym))
                  inside(rhs) {
                    case ExprBinary(ExprRef(Sym(sym)), `op`, Expr(2)) =>
                      sym should be theSameInstanceAs dSym
                  }
              }
          }
        }
      }
    }

    "lift 'let' initializers and drop 'let' statement" - {
      for {
        (name, loop, pattern) <- List[(String, String, PartialFunction[Any, Unit])](
          ("loop", "loop {}", { case _: StmtLoop => }),
          ("while", "while (b) {}", { case _: StmtWhile => }),
          ("do", "do {} while(b);", { case _: StmtDo => }),
          ("for", "for(;;) {}", { case _: StmtFor => })
        )
      } {
        name in {
          val tree = s"{ i2 b; let (i2 a = 0, b = a) ${loop} }".asTree[StmtBlock] rewrite namer rewrite desugar

          inside(tree) {
            case StmtBlock(List(StmtDecl(declB), declA, assignB, loop)) =>
              val Sym(dSymB) = declB.ref
              dSymB.denot.name.str shouldBe "b"
              inside(declA) {
                case StmtDecl(Decl(Sym(dSymA), TypeInt(true, Expr(2)), Some(Expr(0)))) =>
                  inside(assignB) {
                    case StmtAssign(ExprRef(Sym(symB)), ExprRef(Sym(symA))) =>
                      symB.denot.name.str shouldBe "b";
                      symA should be theSameInstanceAs dSymA
                  }
                  loop should matchPattern(pattern)
              }
          }
        }
      }
    }

    "stip redundant blocks around" - {
      for {
        (name, content, pattern) <- List[(String, String, PartialFunction[Any, Unit])](
          ("block", "{}", { case StmtBlock(Nil) => }),
          ("if", "if (1) {}", { case StmtIf(Expr(1), StmtBlock(Nil), None) => }),
          ("case", "case (1) {1:1;}", { case StmtCase(Expr(1), List(_), Nil) => }),
          ("loop", "loop {}", { case StmtLoop(Nil) => }),
          ("while", "while(1) {}", { case StmtWhile(Expr(1), Nil) => }),
          ("do", "do {} while(1);", { case StmtDo(Expr(1), Nil) => }),
          ("for", "for (;;) {}", { case StmtFor(Nil, None, Nil, Nil) => }),
          ("fence", "fence;", { case StmtFence() => }),
          ("break", "break;", { case StmtBreak() => }),
          ("goto", "goto a;", { case StmtGoto(Sym(ErrorSymbol)) => }),
          ("return", "return;", { case StmtReturn() => }),
          ("=", "1 = 1;", { case StmtAssign(Expr(1), Expr(1)) => }),
          ("expr", "1 + 2;", { case StmtExpr(Expr(1) + Expr(2)) => }),
          ("decl", "i3 a;", { case StmtDecl(_) => }),
          ("read", "read;", { case StmtRead() => }),
          ("write", "write;", { case StmtWrite() => })
        )
      } {
        name in {
          val tree = s"{ { { { ${content} } } } }".asTree[StmtBlock] rewrite namer rewrite desugar
          tree should matchPattern(pattern)
        }
      }
    }

    "strip blocks around default case body" in {
      val tree = "case (1) {  default: 1; }".asTree[Stmt] rewrite desugar
      inside(tree) {
        case StmtCase(Expr(1), Nil, default) =>
          default shouldBe List(StmtExpr(Expr(1)))
      }
    }

    "strip blocks around fence block body" in {
      val entity = "fsm a { fence { 2; } }".asTree[Entity]
      cc.addGlobalEntity(entity)
      val tree = entity rewrite namer rewrite desugar

      inside(tree) {
        case entity: Entity =>
          entity.fenceStmts shouldBe List(StmtExpr(Expr(2)))
      }
    }

  }
}
