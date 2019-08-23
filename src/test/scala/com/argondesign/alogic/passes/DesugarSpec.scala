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

package com.argondesign.alogic.passes
import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.typer.Typer
import org.scalatest.FreeSpec

final class DesugarSpec extends FreeSpec with AlogicTest {

  implicit val cc = new CompilerContext
  val namer = new Namer
  val typer = new Typer
  val desugar = new Desugar

  def xform(tree: Tree) = {
    tree match {
      case Root(_, entity: Entity) => cc.addGlobalEntity(entity)
      case entity: Entity          => cc.addGlobalEntity(entity)
      case _                       =>
    }
    tree rewrite namer rewrite typer rewrite desugar
  }

  "Desugar should" - {
    "rewire postfix statements as assignments" - {
      for (op <- List("++", "--")) {
        op in {
          val tree = xform(s"{ i2 a; a${op}; }".asTree[Stmt])

          cc.messages shouldBe empty

          inside(tree) {
            case StmtBlock(List(StmtDecl(Decl(dSym, _)), stmt)) =>
              inside(stmt) {
                case StmtAssign(lhs, rhs) =>
                  lhs shouldBe ExprSym(dSym)
                  inside(rhs) {
                    case ExprBinary(ExprSym(sym), opStr, ExprInt(false, 2, v)) if v == 1 =>
                      opStr shouldBe op.init
                      sym should be theSameInstanceAs dSym
                  }
              }
          }
        }
      }
    }

    "rewire update statements as assignments" - {
      for (op <- List("*", "/", "%", "+", "-", "<<", ">>", ">>>", "&", "|", "^")) {
        s"${op}=" in {
          val tree = xform(s"{ i100 a; a ${op}= 100'd2; }".asTree[Stmt])

          cc.messages shouldBe empty

          inside(tree) {
            case StmtBlock(List(StmtDecl(Decl(dSym, _)), stmt)) =>
              inside(stmt) {
                case StmtAssign(lhs, rhs) =>
                  lhs shouldBe ExprSym(dSym)
                  inside(rhs) {
                    case ExprBinary(ExprSym(sym), `op`, ExprInt(false, 100, v)) if v == 2 =>
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
          ("loop", "loop { fence; }", { case _: StmtLoop => }),
          ("while", "while (b) {}", { case _: StmtWhile  => }),
          ("do", "do {} while(b);", { case _: StmtDo     => }),
          ("for", "for(;;) {}", { case _: StmtFor        => })
        )
      } {
        name in {
          val tree = xform(s"{ i2 b; let (i2 a = 2'd0, b = a) ${loop} }".asTree[Stmt])

          inside(tree) {
            case StmtBlock(List(StmtDecl(declB: Decl), declA: StmtDecl, assignB, loop)) =>
              val dSymB = declB.symbol
              dSymB.name shouldBe "b"
              inside(declA) {
                case StmtDecl(Decl(dSymA, Some(ExprInt(false, 2, v)))) if v == 0 =>
                  dSymA.kind shouldBe TypeSInt(Expr(2))
                  inside(assignB) {
                    case StmtAssign(ExprSym(symB), ExprSym(symA)) =>
                      symB.name shouldBe "b";
                      symA should be theSameInstanceAs dSymA
                  }
                  loop should matchPattern(pattern)
              }
          }
        }
      }
    }
  }
}
