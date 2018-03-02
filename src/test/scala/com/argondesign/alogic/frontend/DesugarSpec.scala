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
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types._

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
        (name, loop, pattern) <- List(
          ("loop", "loop {}", { a: Any => a match { case StmtLoop(_) => () } }),
          ("while", "while (b) {}", { a: Any => a match { case StmtWhile(_, _) => () } }),
          ("do", "do {} while(b);", { a: Any => a match { case StmtDo(_, _) => () } }),
          ("for", "for(;;) {}", { a: Any => a match { case StmtFor(_, _, _, _) => () } })
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
                  loop should matchPattern(PartialFunction(pattern))
              }
          }
        }
      }

    }

  }
}
