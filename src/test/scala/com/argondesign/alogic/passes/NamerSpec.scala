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
import com.argondesign.alogic.ast.Trees.Expr._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes._
import com.argondesign.alogic.core.Names.TypeName
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Symbols.ErrorSymbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.core.Warning
import org.scalatest.FlatSpec

final class NamerSpec extends FlatSpec with AlogicTest {

  implicit val cc = new CompilerContext
  val namer = new Namer

  lazy val atBits = cc.getGlobalTermSymbolRef("@bits")

  "The Namer" should "issue error for redefinition of variable" in {
    """|{
       |  u1 foo;
       |  u2 foo;
       |}""".asTree[Stmt] rewrite namer

    cc.messages should have length 2
    cc.messages(0) should beThe[Error](
      "Redefinition of name 'foo' with previous definition at",
      ".*:2"
    )
    cc.messages(0).loc.line shouldBe 3
    cc.messages(1) should beThe[Warning]("Variable 'foo' is unused")
    cc.messages(1).loc.line shouldBe 3
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
       |}""".asTree[Stmt] rewrite namer

    cc.messages should have length 3
    cc.messages(0) should beThe[Warning](
      "Definition of name 'foo' hides previous definition at",
      ".*:2"
    )
    cc.messages(0).loc.line shouldBe 3
    cc.messages(1) should beThe[Warning]("Variable 'foo' is unused")
    cc.messages(1).loc.line shouldBe 2
    cc.messages(2) should beThe[Warning]("Variable 'foo' is unused")
    cc.messages(2).loc.line shouldBe 3
  }

  it should "issue warning for variable hiding even for later symbol" in {
    val entity = """|fsm a {
                    |  void main() { bool foo; }
                    |  void foo() {}
                    |}""".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer

    cc.messages should have length 3
    cc.messages(0) should beThe[Warning](
      "Definition of name 'foo' hides previous definition at",
      ".*:3"
    )
    cc.messages(0).loc.line shouldBe 2
    cc.messages(1) should beThe[Warning]("Variable 'foo' is unused")
    cc.messages(2) should beThe[Warning]("Function 'foo' is unused")
  }

  it should "cope with using the same name in non-intersecting scopes" in {
    """|{
       |  { u1 foo; }
       |  { u2 foo; }
       |}""".asTree[Stmt] rewrite namer

    cc.messages should have length 2
    cc.messages(0) should beThe[Warning]("Variable 'foo' is unused")
    cc.messages(1) should beThe[Warning]("Variable 'foo' is unused")
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
       |}""".asTree[Stmt] rewrite namer

    cc.messages should have length 2
    cc.messages(0) should beThe[Error]("Name 'bar' is not defined")
    cc.messages(0).loc.line shouldBe 2
    cc.messages(1) should beThe[Warning]("Variable 'foo' is unused")
  }

  it should "issue error for undefined type names" in {
    """|{
       |  foo_t foo;
       |}""".asTree[Stmt] rewrite namer

    cc.messages should have length 2
    cc.messages(0) should beThe[Error]("Type 'foo_t' is not defined")
    cc.messages(0).loc.line shouldBe 2
    cc.messages(1) should beThe[Warning]("Variable 'foo' is unused")
  }

  it should "insert names from 'for ()' loop initializers into the loop scope" in {
    """|for (bool b=true;;) {
       | i2 b;
       |}""".asTree[Stmt] rewrite namer

    cc.messages should have length 2
    cc.messages(0) should beThe[Error](
      "Redefinition of name 'b' with previous definition at",
      ".*:1"
    )
    cc.messages(0).loc.line shouldBe 2
    cc.messages(1) should beThe[Warning]("Variable 'b' is unused")
    cc.messages(1).loc.line shouldBe 2
  }

  it should "insert names from 'let ()' initializers into the following loop scope" in {
    """|let (bool a=true) do {
       | i2 a;
       |} while (1);""".asTree[Stmt] rewrite namer

    cc.messages should have length 2
    cc.messages(0) should beThe[Error](
      "Redefinition of name 'a' with previous definition at",
      ".*:1"
    )
    cc.messages(0).loc.line shouldBe 2
    cc.messages(1) should beThe[Warning]("Variable 'a' is unused")
    cc.messages(1).loc.line shouldBe 2
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
                ),
                _) =>
        aSym.loc.line shouldBe 2
        aSym.denot.name shouldBe TypeName("a")
        aSym shouldBe 'typeSymbol

        aSym.denot.kind shouldBe TypeStruct(
          "a",
          List("b", "c", "d"),
          List(TypeUInt(Expr(1)), TypeSInt(Expr(8)), TypeRef(Sym(eSym)))
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
                  |}""".asTree[Stmt] rewrite namer

    inside(tree) {
      case StmtBlock(
          List(StmtDecl(Decl(Sym(outer1), _, _)), block, StmtAssign(ExprRef(Sym(outer2)), _))) =>
        outer1.loc.line shouldBe 2
        outer1 should be theSameInstanceAs outer2
        outer1 shouldBe 'termSymbol
        inside(block) {
          case StmtBlock(
              List(StmtDecl(Decl(Sym(inner1), _, _)), StmtAssign(ExprRef(Sym(inner2)), _))) =>
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

  it should "resolve term names to their correct definitions - builtin" in {
    val tree = "@bits".asTree[Expr] rewrite namer

    tree shouldBe cc.getGlobalTermSymbolRef("@bits")
    cc.messages shouldBe empty
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
            declSym.denot.kind shouldBe TypeUInt(Expr(1))
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

    cc.messages should have length 2
    cc.messages(0) should beThe[Warning]("Variable 'foo_t' is unused")
    cc.messages(1) should beThe[Warning]("Variable 'b' is unused")
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

    cc.messages should have length 2
    cc.messages(0) should beThe[Warning]("Variable 'bar_t' is unused")
    cc.messages(1) should beThe[Warning]("Variable 'b' is unused")
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
      case _                                           => fail
    }

    aSym shouldBe 'typeSymbol

    inside(treeB) {
      case Entity(_, _, instances, _, _, _, _, _, _) =>
        inside(instances.head) {
          case Instance(Sym(_), Sym(sym), Nil, Nil) =>
            sym should be theSameInstanceAs aSym
        }
    }

    cc.messages.loneElement should beThe[Warning]("Instance 'a' is unused")
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

    cc.messages.loneElement should beThe[Warning]("Instance 'b' is unused")
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
          case Function(Sym(_), List(StmtGoto(ExprRef(Sym(sym))))) =>
            sym shouldBe 'termSymbol
            inside(foo) {
              case Function(Sym(fooSym), Nil) =>
                sym should be theSameInstanceAs fooSym
            }
        }
    }

    cc.messages shouldBe empty
  }

  it should "resolve @bits arguments to term names even if the argument is a valid type expression" in {
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
              case Decl(Sym(dSym), TypeUInt(Expr(1)), None) =>
                inside(expr) {
                  case ExprCall(`atBits`, List(ExprRef(Sym(rSym)))) =>
                    rSym should be theSameInstanceAs dSym
                    rSym shouldBe 'termSymbol
                    rSym.loc.line shouldBe 3
                }
            }
        }
    }

    cc.messages shouldBe empty
  }

  it should "resolve @bits arguments to type names if the argument is a valid type expression" in {
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
          case TypeDefinitionTypedef(Sym(dSym), TypeUInt(Expr(1))) =>
            inside(entity) {
              case Entity(_, _, _, _, List(main), _, _, _, _) =>
                inside(main) {
                  case Function(Sym(_), List(StmtExpr(expr))) =>
                    inside(expr) {
                      case ExprCall(`atBits`, List(ExprRef(Sym(rSym)))) =>
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

  it should "not resolve @bits arguments to type names if the argument is not a type expression" in {
    val root = """|typedef bool a;
                  |fsm b {
                  |  void main() {
                  |    i4 a;
                  |    @bits(a + 2);
                  |  }
                  |}""".stripMargin.asTree[Root]

    cc.addGlobalEntity(root.entity)

    val tree = root rewrite namer

    inside(tree) {
      case Root(List(typedef), entity) =>
        inside(typedef) {
          case TypeDefinitionTypedef(Sym(_), TypeUInt(Expr(1))) =>
            inside(entity) {
              case Entity(_, _, _, _, List(main), _, _, _, _) =>
                inside(main) {
                  case Function(Sym(_), List(StmtDecl(decl), StmtExpr(expr))) =>
                    val Sym(dSym) = decl.ref
                    inside(expr) {
                      case ExprCall(`atBits`, List(ExprRef(Sym(rSym)) + Expr(2))) =>
                        rSym should be theSameInstanceAs dSym
                        rSym shouldBe 'termSymbol
                        rSym.loc.line shouldBe 4
                    }
                }
            }
        }
    }

    cc.messages shouldBe empty
  }

  it should "issue error if @bits argument is ambiguous and can reslove to both type and term names" in {
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

    cc.messages should have length 2
    cc.messages(0) should beThe[Error]("Name 'a' is not defined")
    cc.messages(0).loc.line shouldBe 4
    cc.messages(1) should beThe[Warning]("Variable 'c' is unused")
  }

  it should "resovle names inside type arguments" in {
    val block = """|{
                   |  i8 a;
                   |  i8 b;
                   |  int(a, b) c;
                   |}""".asTree[Stmt]

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
              case TypeSInt(ExprRef(Sym(sym))) =>
                sym should be theSameInstanceAs symB
            }
        }
    }

    cc.messages.loneElement should beThe[Warning]("Variable 'c' is unused")
  }

  it should "resovle names inside array dimension" in {
    val block = """|{
                   |  i8 a;
                   |  i8 b;
                   |  bool c[a][b];
                   |}""".asTree[Stmt]

    val tree = block rewrite namer

    inside(tree) {
      case StmtBlock(List(StmtDecl(declA), StmtDecl(declB), StmtDecl(declC))) =>
        val Sym(symA) = declA.ref
        val Sym(symB) = declB.ref
        inside(declC) {
          case Decl(_, TypeArray(TypeArray(TypeUInt(Expr(1)), size1), size2), _) =>
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

    cc.messages.loneElement should beThe[Warning]("Array 'c' is unused")
  }

  it should "attach correct types to symbol denotations - entity" in {
    val root = """|fsm a {
                  |  in bool a;
                  |  out i3 b;
                  |  param u8 P = 2;
                  |}""".stripMargin.asTree[Root]
    cc.addGlobalEntity(root.entity)
    val tree = root rewrite namer

    val symA = tree collectFirst { case Sym(symbol) if symbol.denot.name.str == "a" => symbol }
    inside(symA.value.denot.kind) {
      case TypeEntity("a", List(symA, symB), List(symP)) =>
        symA.denot.kind shouldBe TypeIn(TypeUInt(Expr(1)), FlowControlTypeNone)
        symB.denot.kind shouldBe TypeOut(TypeSInt(Expr(3)), FlowControlTypeNone, StorageTypeDefault)
        symP.denot.kind shouldBe TypeParam(TypeUInt(Expr(8)))
    }
  }

  it should "attach correct types to symbol denotations - typedef" in {
    val root = "typedef bool a; fsm b {}".asTree[Root]
    cc.addGlobalEntity(root.entity)
    val tree = root rewrite namer

    val symA = tree collectFirst { case Sym(symbol) if symbol.denot.name.str == "a" => symbol }
    symA.value.denot.kind shouldBe TypeUInt(Expr(1))
  }

  it should "attach correct types to symbol denotations - struct" in {
    val root = "struct a { bool a; i2 b; }; fsm b {}".asTree[Root]
    cc.addGlobalEntity(root.entity)
    val tree = root rewrite namer

    val symA = tree collectFirst { case Sym(symbol) if symbol.denot.name.str == "a" => symbol }
    symA.value.denot.kind shouldBe TypeStruct(
      "a",
      List("a", "b"),
      List(TypeUInt(Expr(1)), TypeSInt(Expr(2)))
    )
  }

  it should "attach correct types to symbol denotations - function" in {
    val entity = "fsm foo { void a() {} }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst { case Sym(symbol) if symbol.denot.name.str == "a" => symbol }
    symA.value.denot.kind shouldBe TypeCtrlFunc(Nil, TypeVoid)
  }

  it should "attach correct types to symbol denotations - instance" in {
    val entityA = "fsm a {}".asTree[Entity]
    val entityB = "fsm b { c = new a(); }".asTree[Entity]
    cc.addGlobalEntities(List(entityA, entityB))
    entityA rewrite namer
    val tree = entityB rewrite namer

    val symA = tree collectFirst { case Sym(symbol) if symbol.denot.name.str == "a" => symbol }
    symA.value.denot.kind shouldBe TypeEntity("a", Nil, Nil)
    val symC = tree collectFirst { case Sym(symbol) if symbol.denot.name.str == "c" => symbol }
    symC.value.denot.kind shouldBe TypeRef(Sym(symA.value))
  }

  it should "attach correct types to symbol denotations - decl in" in {
    val entity = "fsm foo { in bool a; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst { case Sym(symbol) if symbol.denot.name.str == "a" => symbol }
    symA.value.denot.kind shouldBe TypeIn(TypeUInt(Expr(1)), FlowControlTypeNone)
  }

  it should "attach correct types to symbol denotations - decl out" in {
    val entity = "fsm foo { out bool a; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst { case Sym(symbol) if symbol.denot.name.str == "a" => symbol }
    symA.value.denot.kind shouldBe TypeOut(TypeUInt(Expr(1)),
                                           FlowControlTypeNone,
                                           StorageTypeDefault)
  }

  it should "attach correct types to symbol denotations - decl param" in {
    val entity = "fsm foo { param bool a = false; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst { case Sym(symbol) if symbol.denot.name.str == "a" => symbol }
    symA.value.denot.kind shouldBe TypeParam(TypeUInt(Expr(1)))
  }

  it should "attach correct types to symbol denotations - decl const" in {
    val entity = "fsm foo { const bool a = false; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst { case Sym(symbol) if symbol.denot.name.str == "a" => symbol }
    symA.value.denot.kind shouldBe TypeConst(TypeUInt(Expr(1)))
  }

  it should "attach correct types to symbol denotations - decl pipeline" in {
    val entity = "fsm foo { pipeline bool a; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst { case Sym(symbol) if symbol.denot.name.str == "a" => symbol }
    symA.value.denot.kind shouldBe TypePipeline(TypeUInt(Expr(1)))
  }

  it should "attach correct types to symbol denotations - decl array" in {
    val entity = "fsm foo { bool a[2][3]; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst { case Sym(symbol) if symbol.denot.name.str == "a" => symbol }
    symA.value.denot.kind shouldBe TypeArray(TypeArray(TypeUInt(Expr(1)), Expr(3)), Expr(2))
  }

  it should "attach correct types to symbol denotations - decl vec" in {
    val entity = "fsm foo { int(3, 2, 4) a; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst { case Sym(symbol) if symbol.denot.name.str == "a" => symbol }
    symA.value.denot.kind shouldBe TypeVector(TypeVector(TypeSInt(Expr(4)), Expr(2)), Expr(3))
  }

  it should "attach correct types to symbol denotations - decl scalar" in {
    val entity = "fsm foo { u2 a; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst { case Sym(symbol) if symbol.denot.name.str == "a" => symbol }
    symA.value.denot.kind shouldBe TypeUInt(Expr(2))
  }

  it should "attach correct types to symbol denotations - decl void" in {
    val entity = "fsm foo { void a; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst { case Sym(symbol) if symbol.denot.name.str == "a" => symbol }
    symA.value.denot.kind shouldBe TypeVoid
  }

  it should "attach correct types to symbol denotations - decl ref" in {
    val root = "typedef bool b; fsm foo { b a; }".asTree[Root]
    cc.addGlobalEntity(root.entity)
    val tree = root rewrite namer

    val symB = tree collectFirst { case Sym(symbol) if symbol.denot.name.str == "b" => symbol }
    symB.value shouldBe 'typeSymbol
    val symA = tree collectFirst { case Sym(symbol) if symbol.denot.name.str == "a" => symbol }
    symA.value.denot.kind shouldBe TypeRef(Sym(symB.value))
  }

  it should "issue warning for unused local variables" in {
    "{ i8 b; }".asTree[Stmt] rewrite namer
    cc.messages.loneElement should beThe[Warning]("Variable 'b' is unused")
  }

  it should "not issue warning for variable used in nested scope" in {
    "{ i8 b; { b; } }".asTree[Stmt] rewrite namer
    cc.messages shouldBe empty
  }

  it should "issue warning for unused entity variables" in {
    val entity = "fsm a { i8 b; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages.loneElement should beThe[Warning]("Variable 'b' is unused")
  }

  it should "issue warning for unused arrays" in {
    val entity = "fsm a { i8 b[2]; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages.loneElement should beThe[Warning]("Array 'b' is unused")
  }

  it should "issue warning for unused input ports" in {
    val entity = "fsm a { in i8 b; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages.loneElement should beThe[Warning]("Input port 'b' is unused")
  }

  it should "issue warning for unused output ports" in {
    val entity = "fsm a { out i8 b; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages.loneElement should beThe[Warning]("Output port 'b' is unused")
  }

  it should "issue warning for unused parameters" in {
    val entity = "fsm a { param i8 b = 8'd9; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages.loneElement should beThe[Warning]("Parameter 'b' is unused")
  }

  it should "issue warning for unused input ports - but not in verbatim entity" in {
    val entity = "verbatim entity a { in i8 b; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages shouldBe empty
  }

  it should "issue warning for unused output ports - but not in verbatim entity" in {
    val entity = "verbatim entity a { out i8 b; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages shouldBe empty
  }

  it should "issue warning for unused parameters - but not in verbatim entity" in {
    val entity = "verbatim entity a { param i8 b = 8'd9; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages shouldBe empty
  }

  it should "issue warning for unused constants" in {
    val entity = "fsm a { const i8 b = 8'd9; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages.loneElement should beThe[Warning]("Constant 'b' is unused")
  }

  it should "issue warning for unused pipeline variables" in {
    val entity = "fsm a { pipeline i8 b; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages.loneElement should beThe[Warning]("Pipeline variable 'b' is unused")
  }

  it should "issue warning for unused functions" in {
    val entity = "fsm a { void foo() {} }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages.loneElement should beThe[Warning]("Function 'foo' is unused")
  }

  it should "issue warning for unused functions - but not for main" in {
    val entity = "fsm a { void main() {} }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages shouldBe empty
  }

  it should "issue warning for unused entities" in {
    val entity = "network a { fsm bar {} }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages.loneElement should beThe[Warning]("Entity 'bar' is unused")
  }

  it should "issue warning for unused instances" in {
    val entity = "network a { new fsm bar {} }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages.loneElement should beThe[Warning]("Instance 'bar' is unused")
  }

  it should "not issue warning for unused type definitions" in {
    val root = "typedef bool b; network a { }".asTree[Root]
    cc.addGlobalEntity(root.entity)
    root rewrite namer
    cc.messages shouldBe empty
  }

  it should "issue unused warnings in source line order" in {
    """|{
       |  bool a;
       |  {
       |    bool b;
       |    {
       |      bool c;
       |    }
       |  }
       |}""".asTree[Stmt] rewrite namer

    cc.messages should have length 3
    cc.messages(0) should beThe[Warning]("Variable 'a' is unused")
    cc.messages(0).loc.line shouldBe 2
    cc.messages(1) should beThe[Warning]("Variable 'b' is unused")
    cc.messages(1).loc.line shouldBe 4
    cc.messages(2) should beThe[Warning]("Variable 'c' is unused")
    cc.messages(2).loc.line shouldBe 6
  }

  it should "attach source attributes to symbol denotations - function" in {
    val entity = "fsm foo { (* reclimit = 1 *) void a() {} }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst {
      case Sym(symbol) if symbol.denot.name.str == "a" => symbol
    }
    symA.value.denot.kind shouldBe TypeCtrlFunc(Nil, TypeVoid)
    symA.value.attr.recLimit.get.value shouldBe Expr(1)
  }

  it should "attach source attributes to symbol denotations - entity" in {
    val entity = "(* stacklimit = 2 *) fsm foo { }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst {
      case Sym(symbol) if symbol.denot.name.str == "foo" => symbol
    }
    symA.value.denot.kind shouldBe TypeEntity("foo", Nil, Nil)
    symA.value.attr.stackLimit.get.value shouldBe Expr(2)
  }

  it should "attach source attributes to symbol denotations - nested entity" in {
    val entity = "network foo { (* stacklimit = 3 *) fsm bar {} }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst {
      case Sym(symbol) if symbol.denot.name.str == "bar" => symbol
    }
    symA.value.denot.kind shouldBe TypeEntity("bar", Nil, Nil)
    symA.value.attr.stackLimit.get.value shouldBe Expr(3)
  }
}
