////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Tests for elaboration related name handling
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.Messages.Warning
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types.TypeNum
import com.argondesign.alogic.core.Types.TypeUInt
import com.argondesign.alogic.core.enums.EntityVariant
import org.scalatest.freespec.AnyFreeSpec

final class ElaborationSpec extends AnyFreeSpec with AlogicTest {

  implicit private val cc: CompilerContext = new CompilerContext

  private val fe = new Frontend

  private def elaborate(text: String): Option[Desc] =
    fe.elaborate(Source("TyperCheckerExprSpec", text)) pipe {
      case Left(ms)      => ms foreach cc.addMessage; None
      case Right(result) => Some(result)
    }

  "Elaboration should " - {
    "issue error for redefinition of a name" in {
      elaborate {
        """void f() {
          |  u1 foo;
          |  u2 foo;
          |}""".stripMargin
      }

      cc.messages should have length 2
      cc.messages(0) should beThe[Error](
        "'foo' has multiple definitions"
      )
      cc.messages(1) should beThe[Error](
        "'foo' has multiple definitions"
      )
      cc.messages(0).loc.line shouldBe 2
      cc.messages(1).loc.line shouldBe 3
    }

    "issue error for redefinition of type" in {
      elaborate {
        """fsm f {
          |  u1 foo;
          |  u2 foo;
          |}""".stripMargin
      }

      cc.messages should have length 2
      cc.messages(0) should beThe[Error](
        "'foo' has multiple definitions"
      )
      cc.messages(1) should beThe[Error](
        "'foo' has multiple definitions"
      )
      cc.messages(0).loc.line shouldBe 2
      cc.messages(1).loc.line shouldBe 3
    }

    "issue warning for variable hiding" ignore {
      elaborate {
        """fsm f() {
          |  u1 foo;
          |  { u2 foo; }
          |}""".stripMargin
      }

      cc.messages should have length 1
      cc.messages(0) should beThe[Warning](
        "Definition of 'foo' hides previous definition at",
        ".*:2"
      )
      cc.messages(0).loc.line shouldBe 3
    }

    "not issue warning for variable hiding for later symbol" in {
      elaborate {
        """void f() {
          |  { u2 foo; }
          |  u1 foo;
          |}""".stripMargin
      }

      cc.messages shouldBe empty
    }

    "cope with using the same name in non-intersecting scopes" in {
      elaborate {
        """void f () {
          |  { u1 foo; }
          |  { u2 foo; }
          |}""".stripMargin
      }

      cc.messages shouldBe empty
    }

    "issue error for use before definition for symbols defined in statements" ignore {
      elaborate {
        """{
          |  u1 foo = bar;
          |  u1 bar;
          |}""".stripMargin
      }

      cc.messages.loneElement should beThe[Error]("'bar' used before it is defined")
      cc.messages.loneElement.loc.line shouldBe 2
    }

    "not issue error for use before definition for symbols not defined in statements" in {
      elaborate {
        """fsm a {
          |  void main() {
          |    foo();
          |  }
          |  void foo() {}
          |}""".stripMargin
      }
      cc.messages shouldBe empty
    }

    "issue error for undefined term names" in {
      elaborate {
        """void f() {
          |  u1 foo = bar;
          |}""".stripMargin
      }

      cc.messages should have length 1
      cc.messages(0) should beThe[Error]("'bar' is undefined")
      cc.messages(0).loc.line shouldBe 2
    }

    "issue error for undefined type names" in {
      elaborate {
        """void f() {
          |  foo_t foo;
          |}""".stripMargin
      }

      cc.messages should have length 1
      cc.messages(0) should beThe[Error]("'foo_t' is undefined")
      cc.messages(0).loc.line shouldBe 2
    }

    "insert names from 'for ()' loop initializers into the loop scope" ignore {
      elaborate {
        """void f() {
          |  for (bool b=true;;) {
          |    i2 b;
          |  }
          |}""".stripMargin
      }

      cc.messages should have length 1
      cc.messages(0) should beThe[Warning](
        "Definition of 'b' hides previous definition at",
        ".*:1"
      )
      cc.messages(0).loc.line shouldBe 2
    }

    "insert names from 'let ()' initializers into the following loop scope" ignore {
      elaborate {
        """
          |void f() {
          |  let (bool a=true) do {
          |    i2 a;
          |  } while (1);
          |}""".stripMargin
      }

      cc.messages should have length 1
      cc.messages(0) should beThe[Warning](
        "Definition of 'a' hides previous definition at",
        ".*:1"
      )
      cc.messages(0).loc.line shouldBe 2
    }

    "resolve term names to their correct definitions" in {
      elaborate {
        """void f () {
          |  bool a;
          |  {
          |    bool a;
          |    a = false;
          |  }
          |  a = true;
          |}""".stripMargin
      } pipe {
        _.value
      } getFirst {
        case DescFunc(_, _, _, _, _, body) => body
      } tap {
        inside(_) {
          case List(
                StmtSplice(DescVar(Sym(outer1), _, _, _)),
                block,
                StmtAssign(ExprSym(outer2), _)
              ) =>
            outer1.loc.line shouldBe 2
            outer1 should be theSameInstanceAs outer2
            inside(block) {
              case StmtBlock(
                    List(StmtSplice(DescVar(Sym(inner1), _, _, _)), StmtAssign(ExprSym(inner2), _))
                  ) =>
                inner1.loc.line shouldBe 4
                inner1 should be theSameInstanceAs inner2
                inner1 shouldNot be theSameInstanceAs outer1
            }
        }

//        cc.messages.loneElement should beThe[Warning](
//          "Definition of 'a' hides previous definition at",
//          ".*:2"
//        )
      }
    }

    "resolve term names to their correct definitions - builtin" in {
      elaborate {
        """|
           |void f() {
           |  @bits;
           |}""".stripMargin
      } pipe {
        _.value
      } getFirst {
        case ExprSym(symbol) => symbol
      } tap {
        _ shouldBe cc.builtins.get("@bits").asInstanceOf[SymbolTable.Local].symbol
      }
      cc.messages shouldBe empty
    }

    "resolve type names to their correct definitions - typedef" in {
      elaborate {
        """typedef bool foo_t;
          |
          |fsm a {
          |  fence {
          |    { bool foo_t = 0; }
          |    foo_t b;
          |  }
          |}""".stripMargin

      } pipe {
        _.value
      } tap {
        inside(_) {
          case DescPackage(
                _,
                _,
                List(PkgSplice(typedef: DescType), PkgSplice(entity: DescEntity))
              ) =>
            inside(typedef) {
              case DescType(Sym(defSym), _, _) =>
                defSym.loc.line shouldBe 1
                inside(entity) {
                  case DescEntity(_, _, EntityVariant.Fsm, List(EntCombProcess(List(_, stmt)))) =>
                    inside(stmt) {
                      case StmtSplice(DescVar(Sym(symbol), _, ExprSym(`defSym`), _)) =>
                        symbol.loc.line shouldBe 6
                    }
                }
            }
        }
      }

//      cc.messages.loneElement should beThe[Warning](
//        "Definition of 'foo_t' hides previous definition at",
//        ".*:1"
//      )
//      cc.messages.loneElement.loc.line shouldBe 5
    }

    "resolve type names to their correct definitions - struct" in {
      elaborate {
        """struct bar_t {
          |  bool a;
          |}
          |
          |fsm a {
          |  fence {
          |    { bool bar_t; }
          |    bar_t b;
          |  }
          |}""".stripMargin
      } pipe {
        _.value
      } tap {
        inside(_) {
          case DescPackage(
                _,
                _,
                List(PkgSplice(record: DescRecord), PkgSplice(entity: DescEntity))
              ) =>
            inside(record) {
              case DescRecord(Sym(cSymbol), _, _) =>
                cSymbol.loc.line shouldBe 1
                inside(entity) {
                  case DescEntity(_, _, EntityVariant.Fsm, List(EntCombProcess(List(_, stmt)))) =>
                    inside(stmt) {
                      case StmtSplice(DescVar(Sym(symbol), _, ExprSym(`cSymbol`), _)) =>
                        symbol.loc.line shouldBe 8
                    }
                }
            }
        }
      }

//      cc.messages.loneElement should beThe[Warning](
//        "Definition of 'bar_t' hides previous definition at",
//        ".*:1"
//      )
//      cc.messages.loneElement.loc.line shouldBe 7
    }

    "resolve function references to later definitions" in {
      elaborate {
        """fsm a {
          |  void main () { foo(); }
          |  void foo () {}
          |}""".stripMargin
      } pipe {
        _.value
      } tap {
        inside(_) {
          case DescPackage(_, _, List(PkgSplice(entity: DescEntity))) =>
            inside(entity) {
              case DescEntity(_, _, EntityVariant.Fsm, List(main, foo)) =>
                inside(main) {
                  case EntSplice(
                        DescFunc(_, _, _, _, _, List(StmtExpr(ExprCall(ExprSym(fooInMain), _))))
                      ) =>
                    inside(foo) {
                      case EntSplice(DescFunc(Sym(fooInDef), _, _, _, _, _)) =>
                        fooInMain should be theSameInstanceAs fooInDef
                    }
                }
            }
        }
      }

      cc.messages shouldBe empty
    }

    "resolve goto targets" in {
      elaborate {
        """fsm a {
          |  void main() { goto foo(); }
          |  void foo() {}
          |}""".stripMargin
      } pipe {
        _.value
      } tap {
        inside(_) {
          case DescPackage(_, _, List(PkgSplice(entity: DescEntity))) =>
            inside(entity) {
              case DescEntity(_, _, EntityVariant.Fsm, List(main, foo)) =>
                inside(main) {
                  case EntSplice(
                        DescFunc(Sym(_), _, _, _, _, List(StmtGoto(ExprCall(ExprSym(sym), Nil))))
                      ) =>
                    inside(foo) {
                      case EntSplice(DescFunc(Sym(fooSym), _, _, _, _, _)) =>
                        sym should be theSameInstanceAs fooSym
                    }
                }
            }
        }
      }

      cc.messages shouldBe empty
    }

    "resolve names inside type arguments" in {
      elaborate {
        """void f() {
          |  i8 a;
          |  i8 b;
          |  int(b)[a] c;
          |}""".stripMargin
      } pipe {
        _.value
      } getFirst {
        case DescFunc(_, _, _, _, _, body) => body
      } tap {
        inside(_) {
          case List(StmtSplice(descA: Desc), StmtSplice(descB: Desc), StmtSplice(descC: Desc)) =>
            val symA = descA.symbol
            val symB = descB.symbol
            inside(descC) {
              case DescVar(_, _, ExprIndex(et, sz), _) =>
                inside(sz) {
                  case ExprSym(sym) =>
                    sym should be theSameInstanceAs symA
                }
                inside(et) {
                  case ExprCall(ExprType(TypeNum(true)), List(ArgP(ExprSym(sym)))) =>
                    sym should be theSameInstanceAs symB
                }
            }
        }
      }

      cc.messages shouldBe empty
    }

    "resolve names inside array dimension" in {
      elaborate {
        """fsm f {
          |  i8 a;
          |  bool b[a];
          |}""".stripMargin
      } pipe {
        _.value
      } getFirst {
        case DescEntity(_, _, _, body) => body
      } tap {
        inside(_) {
          case List(EntSplice(descA: Desc), EntSplice(descB: Desc)) =>
            val symA = descA.symbol
            inside(descB) {
              case DescArray(_, _, ExprType(TypeUInt(w)), sz) if w == 1 =>
                inside(sz) {
                  case ExprSym(sym) =>
                    sym should be theSameInstanceAs symA
                }
            }
        }
      }

      cc.messages shouldBe empty
    }

    "resolve names inside type expressions" in {
      elaborate {
        """|void f() {
           |  uint($clog2(1368));
           |}""".stripMargin
      } pipe {
        _.value
      } getFirst {
        case StmtExpr(e) => e
      } tap {
        _ should matchPattern {
          case ExprCall(
                ExprType(TypeNum(false)),
                List(ArgP(ExprCall(ExprSym(Symbol("$clog2")), List(ArgP(_)))))
              ) =>
        }
      }
      cc.messages shouldBe empty
    }

    "use unique symbols in definitions" in {
      elaborate {
        """fsm a {
          |  i8 a;
          |}""".stripMargin
      } pipe {
        _.value
      } tap {
        inside(_) {
          case DescPackage(_, _, List(PkgSplice(DescEntity(Sym(outerA), _, _, List(desc))))) =>
            inside(desc) {
              case EntSplice(DescVar(Sym(innerA), _, _, _)) =>
                outerA shouldNot be theSameInstanceAs innerA
            }
        }
      }
      cc.messages shouldBe empty
    }

    "attach source attributes to symbols - function" in {
      elaborate {
        """|fsm foo {
           |  (* reclimit = 1 *)
           |  void a() {}
           |}""".stripMargin
      } pipe {
        _.value
      } getFirst {
        case Sym(symbol @ Symbol("a")) => symbol
      } tap {
        _.attr.recLimit.value shouldBe 1
      }
    }

    "attach source attributes to symbols - entity" in {
      elaborate {
        """|(* stacklimit = 2 *)
           |fsm foo {
           |}""".stripMargin
      } pipe {
        _.value
      } getFirst {
        case Desc(Sym(s @ Symbol("foo"))) => s
      } tap {
        _.attr.stackLimit.value shouldBe 2
      }
    }

    "attach source attributes to symbols - nested entity" in {
      elaborate {
        """|network foo {
           |  (* stacklimit = 3 *)
           |  fsm bar {
           |  }
           |}""".stripMargin
      } pipe {
        _.value
      } getFirst {
        case Desc(Sym(s @ Symbol("bar"))) => s
      } tap {
        _.attr.stackLimit.value shouldBe 3
      }
    }

    "attach source attributes to symbols - declaration" in {
      elaborate {
        """|fsm foo {
           |  (* liftsrams *)
           |  i8 a;
           |}""".stripMargin
      } pipe {
        _.value
      } getFirst {
        case DescVar(Sym(symbol), _, _, _) => symbol
      } tap {
        _.attr.liftSrams.value shouldBe true
      }
    }
  }
}
