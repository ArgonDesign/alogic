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
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.Names._

import org.scalatest.FlatSpec

import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.core.Warning
import com.argondesign.alogic.core.Symbols.ErrorSymbol

final class NamerSpec extends FlatSpec with AlogicTest {

  implicit val cc = new CompilerContext
  val namer = new Namer

  "The Namer" should "issue error for redefinition of variable" in {
    """|{
       |  u1 foo;
       |  u2 foo;
       |}""".asTree[StmtBlock] rewrite namer

    cc.messages.loneElement should beThe[Error](
      "Redefinition of name 'foo' with previous definition at",
      ".*:2"
    )
    cc.messages(0).loc.line shouldBe 3
  }

  it should "issue error for redefinition of type" in {
    val root = """|typedef u1 foo;
                  |typedef u2 foo;
                  |fsm a {}""".asTree[Root]
    cc.addGlobalEntity(root.entity)
    root rewrite namer

    cc.messages.loneElement should beThe[Error](
      "Redefinition of type 'foo' with previous definition at",
      ".*:1"
    )
    cc.messages(0).loc.line shouldBe 2
  }

  it should "issue warning for variable hiding" in {
    """|{
       |  u1 foo;
       |  { u2 foo; }
       |}""".asTree[StmtBlock] rewrite namer

    cc.messages.loneElement should beThe[Warning](
      "Definition of name 'foo' hides previous definition at",
      ".*:2"
    )
    cc.messages(0).loc.line shouldBe 3
  }

  it should "issue warning for variable hiding even for later symbol" in {
    val entity = """|fsm a {
                    |  void main() { bool foo; }
                    |  void foo() {}
                    |}""".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer

    cc.messages.loneElement should beThe[Warning](
      "Definition of name 'foo' hides previous definition at",
      ".*:3"
    )
    cc.messages(0).loc.line shouldBe 2
  }

  it should "cope with using the same name in non-intersecting scopes" in {
    """|{
       |  { u1 foo; }
       |  { u2 foo; }
       |}""".asTree[StmtBlock] rewrite namer

    cc.messages shouldBe empty
  }

  it should "cope with multi level typedefs" in {
    val root = """|typedef bool a;
                  |typedef a b;
                  |fsm c {}""".asTree[Root]
    cc.addGlobalEntity(root.entity)
    root rewrite namer

    cc.messages shouldBe empty
  }

  it should "issue error for undefined term names" in {
    """|{
       |  u1 foo = bar;
       |}""".asTree[StmtBlock] rewrite namer

    cc.messages.loneElement should beThe[Error]("Name 'bar' is not defined")
    cc.messages(0).loc.line shouldBe 2
  }

  it should "issue error for undefined type names" in {
    """|{
       |  foo_t foo;
       |}""".asTree[StmtBlock] rewrite namer

    cc.messages.loneElement should beThe[Error]("Type 'foo_t' is not defined")
    cc.messages(0).loc.line shouldBe 2
  }

  it should "insert names from 'for ()' loop initializers into the loop scope" in {
    """|for (bool b=true;;) {
       | i2 b;
       |}""".asTree[Stmt] rewrite namer

    cc.messages.loneElement should beThe[Error](
      "Redefinition of name 'b' with previous definition at",
      ".*:1"
    )
    cc.messages(0).loc.line shouldBe 2
  }

  it should "insert names from 'let ()' initializers into the following loop scope" in {
    """|let (bool a=true) do {
       | i2 a;
       |} while (1);""".asTree[Stmt] rewrite namer

    cc.messages.loneElement should beThe[Error](
      "Redefinition of name 'a' with previous definition at",
      ".*:1"
    )
    cc.messages(0).loc.line shouldBe 2
  }

  it should "counstruct struct types" in {
    val root = """|typedef bool e_t;
                  |struct a {
                  |  bool b;
                  |  i8 c;
                  |  e_t  d;
                  |};
                  |fsm b {}""".stripMargin.asTree[Root]
    cc.addGlobalEntity(root.entity)

    val tree = root rewrite namer

    inside(tree) {
      case Root(List(
        TypeDefinitionTypedef(Sym(eSym), _),
        TypeDefinitionStruct(Sym(aSym), names, kind)
        ), _) =>
        aSym.loc.line shouldBe 2
        aSym.denot.name shouldBe TypeName("a")
        aSym shouldBe 'typeSymbol

        aSym.denot.kind shouldBe TypeStruct(
          List("b", "c", "d"),
          List(TypeInt(false, Expr(1)), TypeInt(true, Expr(8)), TypeRef(Sym(eSym)))
        )
    }

    cc.messages shouldBe empty
  }

  it should "resolve term names to their correct definitions" in {
    val tree = """|{
                  |  bool a;
                  |  {
                  |    bool a;
                  |    a = false;
                  |  }
                  |  a = true;
                  |}""".asTree[StmtBlock] rewrite namer

    inside(tree) {
      case StmtBlock(List(StmtDecl(Decl(Sym(outer1), _, _)), block, StmtAssign(ExprRef(Sym(outer2)), _))) =>
        outer1.loc.line shouldBe 2
        outer1 should be theSameInstanceAs outer2
        outer1 shouldBe 'termSymbol
        inside(block) {
          case StmtBlock(List(StmtDecl(Decl(Sym(inner1), _, _)), StmtAssign(ExprRef(Sym(inner2)), _))) =>
            inner1.loc.line shouldBe 4
            inner1 should be theSameInstanceAs inner2
            inner1 shouldNot be theSameInstanceAs outer1
            inner1 shouldBe 'termSymbol
        }
    }

    cc.messages.loneElement should beThe[Warning](
      "Definition of name 'a' hides previous definition at",
      ".*:2"
    )
  }

  it should "resolve type names to their correct definitions - typedef" in {
    val root = """|typedef bool foo_t;
                  |
                  |fsm a {
                  |  fence {
                  |    bool foo_t = 0;
                  |    foo_t b;
                  |  }
                  |}""".stripMargin.asTree[Root]

    cc.addGlobalEntity(root.entity)

    val tree = root rewrite namer

    inside(tree) {
      case Root(List(typedef), entity) =>
        inside(typedef) {
          case TypeDefinitionTypedef(Sym(declSym), _) =>
            declSym.loc.line shouldBe 1
            declSym.denot.kind shouldBe TypeInt(false, Expr(1))
            inside(entity) {
              case Entity(_, _, _, _, _, _, List(StmtBlock(List(_, stmt))), _, _) =>
                inside(stmt) {
                  case StmtDecl(Decl(Sym(_), TypeRef(Sym(instSym)), _)) =>
                    instSym should be theSameInstanceAs declSym
                    instSym shouldBe 'typeSymbol
                }
            }
        }
    }

    cc.messages shouldBe empty
  }

  it should "resolve type names to their correct definitions - struct" in {
    val root = """|struct bar_t {
                  |  bool a;
                  |};
                  |
                  |fsm a {
                  |  fence {
                  |    bool bar_t;
                  |    bar_t b;
                  |  }
                  |}""".stripMargin.asTree[Root]

    cc.addGlobalEntity(root.entity)

    val tree = root rewrite namer

    inside(tree) {
      case Root(List(typedef), entity) =>
        inside(typedef) {
          case TypeDefinitionStruct(Sym(declSym), _, _) =>
            declSym.loc.line shouldBe 1
            inside(entity) {
              case Entity(_, _, _, _, _, _, List(StmtBlock(List(_, stmt))), _, _) =>
                inside(stmt) {
                  case StmtDecl(Decl(Sym(_), TypeRef(Sym(instSym)), _)) =>
                    instSym should be theSameInstanceAs declSym
                    instSym shouldBe 'typeSymbol
                }
            }
        }
    }

    cc.messages shouldBe empty
  }

  it should "resovle function references to later definitions" in {
    val entity = """|fsm a {
                    |  void main () { foo(); }
                    |  void foo () {}
                    |}""".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    inside(tree) {
      case entity: Entity =>
        inside(entity.functions) {
          case List(main, foo) =>
            inside(main) {
              case Function(_, List(StmtExpr(ExprCall(ExprRef(Sym(fooInMain)), _)))) =>
                inside(foo) {
                  case Function(Sym(fooInDef), _) =>
                    fooInMain should be theSameInstanceAs fooInDef
                }
            }
        }
    }

    cc.messages shouldBe empty
  }

  it should "resolve entity symbols in instantiations" in {
    val entityA = """fsm a {}""".asTree[Entity]
    val entityB = """|network b {
                     |  a = new a();
                     |}""".asTree[Entity]

    cc.addGlobalEntities(List(entityA, entityB))

    val treeA = entityA rewrite namer
    val treeB = entityB rewrite (new Namer)

    val aSym = treeA match {
      case Entity(Sym(symbol), _, _, _, _, _, _, _, _) => symbol
      case _ => fail
    }

    aSym shouldBe 'typeSymbol

    inside(treeB) {
      case Entity(_, _, instances, _, _, _, _, _, _) =>
        inside(instances.head) {
          case Instance(Sym(_), Sym(sym), Nil, Nil) =>
            sym should be theSameInstanceAs aSym
        }
    }

    cc.messages shouldBe empty
  }

  it should "resolve instantiations of nested entities" in {
    val entity = """|network a {
                    |  new fsm b { }
                    |}""".asTree[Entity]

    cc.addGlobalEntity(entity)

    val tree = entity rewrite namer

    inside(tree) {
      case Entity(_, _, List(bInstance), _, _, _, _, List(bEntity), _) =>
        inside(bInstance) {
          case Instance(Sym(iSym), Sym(eSym), _, _) =>
            iSym shouldBe 'termSymbol
            eSym shouldBe 'typeSymbol
            inside(bEntity) {
              case Entity(Sym(sym), _, _, _, _, _, _, _, _) =>
                eSym should be theSameInstanceAs sym
                iSym shouldNot be theSameInstanceAs sym
            }
        }
    }

    cc.messages shouldBe empty
  }

  it should "resolve goto targets" in {
    val entity = """|fsm a {
                    |  void main() { goto foo; }
                    |  void foo() {}
                    |}""".asTree[Entity]

    cc.addGlobalEntity(entity)

    val tree = entity rewrite namer

    inside(tree) {
      case Entity(_, _, _, _, List(main, foo), _, _, _, _) =>
        inside(main) {
          case Function(Sym(_), List(StmtGoto(Sym(sym)))) =>
            sym shouldBe 'termSymbol
            inside(foo) {
              case Function(Sym(fooSym), Nil) =>
                sym should be theSameInstanceAs fooSym
            }
        }
    }

    cc.messages shouldBe empty
  }

  it should "resolve @bits arguments to plain term names" in {
    val entity = """|fsm a {
                    |  void main() {
                    |    bool b;
                    |    @bits(b);
                    |  }
                    |}""".stripMargin.asTree[Entity]

    cc.addGlobalEntity(entity)

    val tree = entity rewrite namer

    inside(tree) {
      case Entity(_, _, _, _, List(main), _, _, _, _) =>
        inside(main) {
          case Function(Sym(_), List(StmtDecl(decl), StmtExpr(expr))) =>
            inside(decl) {
              case Decl(Sym(dSym), TypeInt(false, Expr(1)), None) =>
                inside(expr) {
                  case ExprAtCall("bits", List(ExprRef(Sym(rSym)))) =>
                    rSym should be theSameInstanceAs dSym
                    rSym shouldBe 'termSymbol
                    rSym.loc.line shouldBe 3
                }
            }
        }
    }

    cc.messages shouldBe empty
  }

  it should "resolve @bits arguments to term names with select" in {
    val entity = """|fsm a {
                    |  void main() {
                    |    bool b;
                    |    @bits(b.c.d);
                    |  }
                    |}""".stripMargin.asTree[Entity]

    cc.addGlobalEntity(entity)

    val tree = entity rewrite namer

    inside(tree) {
      case Entity(_, _, _, _, List(main), _, _, _, _) =>
        inside(main) {
          case Function(Sym(_), List(StmtDecl(decl), StmtExpr(expr))) =>
            inside(decl) {
              case Decl(Sym(dSym), TypeInt(false, Expr(1)), None) =>
                inside(expr) {
                  case ExprAtCall("bits", List(ExprSelect(ExprSelect(ExprRef(Sym(rSym)), "c"), "d"))) =>
                    rSym should be theSameInstanceAs dSym
                    rSym shouldBe 'termSymbol
                    rSym.loc.line shouldBe 3
                }
            }
        }
    }

    cc.messages shouldBe empty
  }

  it should "resolve @bits arguments to plain type names" in {
    val root = """|typedef bool a;
                  |fsm b {
                  |  void main() {
                  |    @bits(a);
                  |  }
                  |}""".stripMargin.asTree[Root]

    cc.addGlobalEntity(root.entity)

    val tree = root rewrite namer

    inside(tree) {
      case Root(List(typedef), entity) =>
        inside(typedef) {
          case TypeDefinitionTypedef(Sym(dSym), TypeInt(false, Expr(1))) =>
            inside(entity) {
              case Entity(_, _, _, _, List(main), _, _, _, _) =>
                inside(main) {
                  case Function(Sym(_), List(StmtExpr(expr))) =>
                    inside(expr) {
                      case ExprAtCall("bits", List(ExprRef(Sym(rSym)))) =>
                        rSym should be theSameInstanceAs dSym
                        rSym shouldBe 'typeSymbol
                        rSym.loc.line shouldBe 1
                    }
                }
            }
        }
    }

    cc.messages shouldBe empty
  }

  it should "resolve @bits arguments to type names with select" in {
    val root = """|typedef bool a;
                  |fsm b {
                  |  void main() {
                  |    @bits(a.c.d);
                  |  }
                  |}""".stripMargin.asTree[Root]

    cc.addGlobalEntity(root.entity)

    val tree = root rewrite namer

    inside(tree) {
      case Root(List(typedef), entity) =>
        inside(typedef) {
          case TypeDefinitionTypedef(Sym(dSym), TypeInt(false, Expr(1))) =>
            inside(entity) {
              case Entity(_, _, _, _, List(main), _, _, _, _) =>
                inside(main) {
                  case Function(Sym(_), List(StmtExpr(expr))) =>
                    inside(expr) {
                      case ExprAtCall("bits", List(ExprSelect(ExprSelect(ExprRef(Sym(rSym)), "c"), "d"))) =>
                        rSym should be theSameInstanceAs dSym
                        rSym shouldBe 'typeSymbol
                        rSym.loc.line shouldBe 1
                    }
                }
            }
        }
    }

    cc.messages shouldBe empty
  }

  it should "issue error if both type and term names are available" in {
    val root = """|typedef bool a;
                  |fsm b {
                  |  void main() {
                  |    u1 a;
                  |    @bits(a);
                  |  }
                  |}""".stripMargin.asTree[Root]

    cc.addGlobalEntity(root.entity)

    root rewrite namer

    cc.messages.loneElement should beThe[Error](
      "Name 'a' in this context can resolve to either of",
      "term 'a' defined at .*:4",
      "type 'a' defined at .*:1"
    )
  }

  it should "not resolve other identifiers in expressions to type names" in {
    val root = """|typedef bool a;
                  |fsm b {
                  |  void main() {
                  |    bool c = a;
                  |  }
                  |}""".stripMargin.asTree[Root]

    cc.addGlobalEntity(root.entity)

    val tree = root rewrite namer

    inside(tree) {
      case Root(_, entity: Entity) =>
        inside(entity.functions.head.body.head) {
          case StmtDecl(Decl(_, _, Some(ExprRef(Sym(sym))))) =>
            sym should be theSameInstanceAs ErrorSymbol
        }
    }

    cc.messages.loneElement should beThe[Error]("Name 'a' is not defined")
    cc.messages(0).loc.line shouldBe 4
  }

  it should "resovle names inside type arguments" in {
    val block = """|{
                   |  i8 a;
                   |  i8 b;
                   |  int(a, b) c;
                   |}""".asTree[StmtBlock]

    val tree = block rewrite namer

    inside(tree) {
      case StmtBlock(List(StmtDecl(declA), StmtDecl(declB), StmtDecl(declC))) =>
        val Sym(symA) = declA.ref
        val Sym(symB) = declB.ref
        inside(declC) {
          case Decl(_, TypeVector(elementType, size), _) =>
            inside(size) {
              case ExprRef(Sym(sym)) =>
                sym should be theSameInstanceAs symA
            }
            inside(elementType) {
              case TypeInt(true, ExprRef(Sym(sym))) =>
                sym should be theSameInstanceAs symB
            }
        }
    }

    cc.messages shouldBe empty
  }

  it should "resovle names inside array dimension" in {
    val block = """|{
                   |  i8 a;
                   |  i8 b;
                   |  bool c[a][b];
                   |}""".asTree[StmtBlock]

    val tree = block rewrite namer

    inside(tree) {
      case StmtBlock(List(StmtDecl(declA), StmtDecl(declB), StmtDecl(declC))) =>
        val Sym(symA) = declA.ref
        val Sym(symB) = declB.ref
        inside(declC) {
          case Decl(_, TypeArray(TypeArray(TypeInt(false, Expr(1)), size1), size2), _) =>
            inside(size1) {
              case ExprRef(Sym(sym)) =>
                sym should be theSameInstanceAs symB
            }
            inside(size2) {
              case ExprRef(Sym(sym)) =>
                sym should be theSameInstanceAs symA
            }
        }
    }

    cc.messages shouldBe empty
  }

  it should "attach correct types to symbol denotations - entity" in {
    val root = """fsm a {}""".asTree[Root]
    cc.addGlobalEntity(root.entity)
    val tree = root rewrite namer

    val symA = tree collectFirst { case Sym(symbol) if symbol.denot.name.str == "a" => symbol }
    symA.value.denot.kind shouldBe TypeEntity
  }

}
