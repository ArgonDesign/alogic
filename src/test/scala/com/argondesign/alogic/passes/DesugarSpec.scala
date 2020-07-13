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
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.Symbols.Symbol
import org.scalatest.freespec.AnyFreeSpec

final class DesugarSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  private def desugar(text: String): Thicket = Thicket {
    transformWithPass(Namer andThen Elaborate andThen TypeCheck andThen Desugar, text) map {
      _ flatMap {
        case (decl, defn) => List(decl, defn)
      }
    } getOrElse Nil
  }

  "Desugar should" - {
    "rewire postfix statements as assignments" - {
      for (op <- List("++", "--")) {
        op in {
          desugar {
            s"""
            |struct s {
            |  void function() {
            |    i2 a; a$op;
            |  }
            |}"""
          } getFirst {
            case DefnFunc(_, _, body) => body
          } tap {
            inside(_) {
              case List(StmtDecl(DeclVar(dSym, _)), _: StmtDefn, StmtAssign(lhs, rhs)) =>
                lhs shouldBe ExprSym(dSym)
                inside(rhs) {
                  case ExprBinary(ExprSym(sym), opStr, ExprInt(true, 2, v)) if v == 1 =>
                    opStr shouldBe op.init
                    sym should be theSameInstanceAs dSym
                }
            }
          }
          cc.messages shouldBe empty
        }
      }
    }

    "rewire update statements as assignments" - {
      for (op <- List("*", "/", "%", "+", "-", "<<", ">>", ">>>", "&", "|", "^")) {
        s"$op=" in {
          desugar {
            s"""
            |struct s {
            |  void function() {
            |    i100 a; a $op= 100'd2;
            |  }
            |}"""
          } getFirst {
            case DefnFunc(_, _, body) => body
          } tap {
            inside(_) {
              case List(StmtDecl(DeclVar(dSym, _)), _: StmtDefn, StmtAssign(lhs, rhs)) =>
                lhs shouldBe ExprSym(dSym)
                inside(rhs) {
                  case ExprBinary(ExprSym(sym), `op`, ExprInt(false, 100, v)) if v == 2 =>
                    sym should be theSameInstanceAs dSym
                }
            }
          }
          cc.messages shouldBe empty
        }
      }
    }

    "lift 'let' initializers and drop 'let' statement" - {
      for {
        (name, loop, pattern) <- List[(String, String, PartialFunction[Any, Unit])](
          ("loop", "loop { fence; }", { case _: StmtLoop => }),
          ("while", "while (b) {}", { case _: StmtWhile => }),
          ("do", "do {} while(b);", { case _: StmtDo => }),
          ("for", "for(;;) {}", { case _: StmtFor => })
        )
      } {
        name in {
          desugar {
            s"""
            |fsm e {
            |  void function() {
            |    i2 b;
            |    let (i2 a = 2'd0, b = a) $loop
            |  }
            |}"""
          } getFirst {
            case DefnFunc(_, _, body) => body
          } tap {
            inside(_) {
              case List(
                    StmtDecl(declB),
                    StmtDefn(defnB),
                    StmtDecl(declA),
                    StmtDefn(defnA),
                    assign,
                    loop
                  ) =>
                declB.symbol.name shouldBe "b"
                defnB.symbol.name shouldBe "b"
                declB.symbol should be theSameInstanceAs defnB.symbol
                declA.symbol.name shouldBe "a"
                defnA.symbol.name shouldBe "a"
                declA.symbol should be theSameInstanceAs defnA.symbol
                declA should matchPattern {
                  case DeclVar(_, ExprType(TypeSInt(w))) if w == 2 =>
                }
                defnA should matchPattern {
                  case DefnVar(_, Some(ExprInt(false, 2, v))) if v == 0 =>
                }
                inside(assign) {
                  case StmtAssign(ExprSym(symB), ExprSym(symA)) =>
                    symA should be theSameInstanceAs declA.symbol
                    symB should be theSameInstanceAs declB.symbol
                }
                loop should matchPattern(pattern)
            }
          }
          cc.messages shouldBe empty
        }
      }
    }

    "replace singleton instances with entity + instance" in {
      desugar {
        """
          |network a {
          |  new fsm b {}
          |}"""
      } getFirst {
        case Thicket(body) => body
      } tap {
        inside(_) {
          case List(decl_a: DeclEntity, defn_a: DefnEntity) =>
            inside(decl_a) {
              case DeclEntity(_, List(decl_b_e, decl_b_i)) =>
                inside(decl_b_e) {
                  case DeclEntity(b_e, _) =>
                    decl_b_i should matchPattern {
                      case DeclInstance(_, ExprSym(`b_e`)) =>
                    }
                }
            }
            inside(defn_a) {
              case DefnEntity(_, _, List(EntDefn(defn_b_e), EntDefn(defn_b_i))) =>
                defn_b_e should matchPattern { case DefnEntity(b_e, _, Nil) => }
                defn_b_i shouldBe a[DefnInstance]
            }
        }
      }
    }

    "rewrite -> connections as <- assignments" - {
      for {
        (connect, patterns) <- List[(String, List[PartialFunction[Any, Unit]])](
          // format: off
          ("i -> oa", List({ case EntAssign(ExprSym(Symbol("oa")), ExprSym(Symbol("i"))) => })),
          ("i -> ob", List({ case EntAssign(ExprSym(Symbol("ob")), ExprSym(Symbol("i"))) => })),
          ("i -> oa, ob", List({ case EntAssign(ExprSym(Symbol("oa")), ExprSym(Symbol("i"))) => },
                               { case EntAssign(ExprSym(Symbol("ob")), ExprSym(Symbol("i"))) => })),
          ("i -> oc, ob, oa", List({ case EntAssign(ExprSym(Symbol("oc")), ExprSym(Symbol("i"))) => },
                                   { case EntAssign(ExprSym(Symbol("ob")), ExprSym(Symbol("i"))) => },
                                   { case EntAssign(ExprSym(Symbol("oa")), ExprSym(Symbol("i"))) => })),
          // format: on
        )
      } {
        connect in {
          desugar {
            s"""network n {
               |  in bool i;
               |  out bool oa;
               |  out bool ob;
               |  out bool oc;
               |  $connect;
               |}"""
          } collect {
            case ent: EntAssign => ent
          } tap { ents =>
            ents should have length patterns.length
            ents zip patterns foreach {
              case (ent, pattern) => ent should matchPattern(pattern)
            }
          }
          cc.messages shouldBe empty
        }
      }
    }

    "make cardinal ports explicit" - {
      for {
        (connect, pattern) <- List[(String, PartialFunction[Any, Unit])](
          // format: off
          ("i -> inner.ii", { case EntAssign(ExprSel(ExprSym(Symbol("inner")), "ii", Nil), ExprSym(Symbol("i"))) => }),
          ("i -> inner.in", { case EntAssign(ExprSel(ExprSym(Symbol("inner")), "in", Nil), ExprSym(Symbol("i"))) => }),
          ("i -> inner",    { case EntAssign(ExprSel(ExprSym(Symbol("inner")), "in", Nil), ExprSym(Symbol("i"))) => } ),
          ("inner.oo  -> o", { case EntAssign(ExprSym(Symbol("o")), ExprSel(ExprSym(Symbol("inner")), "oo", Nil)) => }),
          ("inner.out -> o", { case EntAssign(ExprSym(Symbol("o")), ExprSel(ExprSym(Symbol("inner")), "out", Nil)) => }),
          ("inner     -> o", { case EntAssign(ExprSym(Symbol("o")), ExprSel(ExprSym(Symbol("inner")), "out", Nil)) => }),
          ("inner.oo -> inner.ii", { case EntAssign(ExprSel(ExprSym(Symbol("inner")), "ii", Nil), ExprSel(ExprSym(Symbol("inner")), "oo", Nil)) => }),
          ("inner.oo -> inner.in", { case EntAssign(ExprSel(ExprSym(Symbol("inner")), "in", Nil), ExprSel(ExprSym(Symbol("inner")), "oo", Nil)) => }),
          ("inner.oo -> inner",    { case EntAssign(ExprSel(ExprSym(Symbol("inner")), "in", Nil), ExprSel(ExprSym(Symbol("inner")), "oo", Nil)) => }),
          ("inner.out -> inner.ii", { case EntAssign(ExprSel(ExprSym(Symbol("inner")), "ii", Nil), ExprSel(ExprSym(Symbol("inner")), "out", Nil)) => }),
          ("inner.out -> inner.in", { case EntAssign(ExprSel(ExprSym(Symbol("inner")), "in", Nil), ExprSel(ExprSym(Symbol("inner")), "out", Nil)) => }),
          ("inner.out -> inner",    { case EntAssign(ExprSel(ExprSym(Symbol("inner")), "in", Nil), ExprSel(ExprSym(Symbol("inner")), "out", Nil)) => }),
          ("inner -> inner.ii", { case EntAssign(ExprSel(ExprSym(Symbol("inner")), "ii", Nil), ExprSel(ExprSym(Symbol("inner")), "out", Nil)) => }),
          ("inner -> inner.in", { case EntAssign(ExprSel(ExprSym(Symbol("inner")), "in", Nil), ExprSel(ExprSym(Symbol("inner")), "out", Nil)) => }),
          ("inner -> inner",    { case EntAssign(ExprSel(ExprSym(Symbol("inner")), "in", Nil), ExprSel(ExprSym(Symbol("inner")), "out", Nil)) => }),
          // format: on
        )
      } {
        connect in {
          desugar {
            s"""network n {
               |  in bool i;
               |  out bool o;
               |
               |  new fsm inner {
               |    in bool ii;
               |    in bool;
               |    out bool oo;
               |    out bool;
               |  }
               |
               |  $connect;
               |}"""
          } getFirst {
            case ent: EntAssign => ent
          } tap {
            _ should matchPattern(pattern)
          }
          cc.messages shouldBe empty
        }
      }

    }
  }
}
