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
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.core.Symbols.TypeSymbol
import com.argondesign.alogic.core.Types.TypeArray
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.core.Warning
import org.scalatest.FlatSpec

final class NamerSpec extends FlatSpec with AlogicTest {

  implicit val cc = new CompilerContext
  val namer = new Namer

  lazy val atBits = ExprRef(cc.lookupGlobalTerm("@bits"))

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

  it should "construct struct types" in {
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
        aSym.uniqueName shouldBe TypeName("a")
        aSym.isTypeSymbol shouldBe true

        aSym.kind shouldBe TypeStruct(
          "a",
          List("b", "c", "d"),
          List(TypeUInt(Expr(1)), TypeSInt(Expr(8)), eSym.kind)
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
      case StmtBlock(List(StmtDecl(Decl(outer1, _)), block, StmtAssign(ExprRef(outer2), _))) =>
        outer1.loc.line shouldBe 2
        outer1 should be theSameInstanceAs outer2
        outer1.isTermSymbol shouldBe true
        inside(block) {
          case StmtBlock(List(StmtDecl(Decl(inner1, _)), StmtAssign(ExprRef(inner2), _))) =>
            inner1.loc.line shouldBe 4
            inner1 should be theSameInstanceAs inner2
            inner1 shouldNot be theSameInstanceAs outer1
            inner1.isTermSymbol shouldBe true
        }
    }

    cc.messages.loneElement should beThe[Warning](
      "Definition of name 'a' hides previous definition at",
      ".*:2"
    )
  }

  it should "resolve term names to their correct definitions - builtin" in {
    val tree = "@bits".asTree[Expr] rewrite namer

    tree shouldBe atBits
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
            declSym.kind shouldBe TypeUInt(Expr(1))
            inside(entity) {
              case Entity(_, List(EntCombProcess(List(StmtBlock(List(_, stmt)))))) =>
                inside(stmt) {
                  case StmtDecl(Decl(symbol, _)) =>
                    symbol.kind shouldBe declSym.kind
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
              case Entity(_, List(EntCombProcess(List(StmtBlock(List(_, stmt)))))) =>
                inside(stmt) {
                  case StmtDecl(Decl(symbol, _)) =>
                    symbol.kind shouldBe declSym.kind
                }
            }
        }
    }

    cc.messages should have length 2
    cc.messages(0) should beThe[Warning]("Variable 'bar_t' is unused")
    cc.messages(1) should beThe[Warning]("Variable 'b' is unused")
  }

  it should "resolve function references to later definitions" in {
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
              case EntFunction(_, List(StmtExpr(ExprCall(ExprRef(fooInMain), _)))) =>
                inside(foo) {
                  case EntFunction(Sym(fooInDef), _) =>
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
    val treeB = entityB rewrite new Namer

    val aSym = treeA match {
      case Entity(Sym(symbol), _) => symbol
      case _                      => fail
    }

    aSym.isTypeSymbol shouldBe true

    inside(treeB) {
      case Entity(_, instances) =>
        inside(instances.head) {
          case EntInstance(Sym(_), Sym(sym), Nil, Nil) =>
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
      case Entity(_, List(bEntity, bInstance)) =>
        inside(bInstance) {
          case EntInstance(Sym(iSym), Sym(eSym), _, _) =>
            iSym.isTermSymbol shouldBe true
            eSym.isTypeSymbol shouldBe true
            inside(bEntity) {
              case EntEntity(Entity(Sym(sym), _)) =>
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
      case Entity(_, List(main, foo)) =>
        inside(main) {
          case EntFunction(Sym(_), List(StmtGoto(ExprRef(sym)))) =>
            sym.isTermSymbol shouldBe true
            inside(foo) {
              case EntFunction(Sym(fooSym), Nil) =>
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
      case Entity(_, List(main)) =>
        inside(main) {
          case EntFunction(Sym(_), List(StmtDecl(decl), StmtExpr(expr))) =>
            inside(decl) {
              case Decl(dSym, None) =>
                dSym.kind shouldBe TypeUInt(Expr(1))
                inside(expr) {
                  case ExprCall(`atBits`, List(ExprRef(rSym))) =>
                    rSym should be theSameInstanceAs dSym
                    rSym.isTermSymbol shouldBe true
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
              case Entity(_, List(main)) =>
                inside(main) {
                  case EntFunction(Sym(_), List(StmtExpr(expr))) =>
                    inside(expr) {
                      case ExprCall(`atBits`, List(ExprRef(rSym))) =>
                        rSym should be theSameInstanceAs dSym
                        rSym.isTypeSymbol shouldBe true
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
              case Entity(_, List(main)) =>
                inside(main) {
                  case EntFunction(Sym(_), List(StmtDecl(decl: Decl), StmtExpr(expr))) =>
                    val dSym = decl.symbol
                    inside(expr) {
                      case ExprCall(`atBits`, List(ExprRef(rSym) + Expr(2))) =>
                        rSym should be theSameInstanceAs dSym
                        rSym.isTermSymbol shouldBe true
                        rSym.loc.line shouldBe 4
                    }
                }
            }
        }
    }

    cc.messages shouldBe empty
  }

  it should "issue error if @bits argument is ambiguous and can resolve to both type and term names" in {
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
        inside(entity.functions.head.stmts.head) {
          case StmtDecl(Decl(_, Some(ExprRef(sym)))) =>
            sym should be theSameInstanceAs ErrorSymbol
        }
    }

    cc.messages should have length 2
    cc.messages(0) should beThe[Error]("Name 'a' is not defined")
    cc.messages(0).loc.line shouldBe 4
    cc.messages(1) should beThe[Warning]("Variable 'c' is unused")
  }

  it should "resolve names inside type arguments" in {
    val block = """|{
                   |  i8 a;
                   |  i8 b;
                   |  int(b)[a] c;
                   |}""".asTree[Stmt]

    val tree = block rewrite namer

    inside(tree) {
      case StmtBlock(List(StmtDecl(declA: Decl), StmtDecl(declB: Decl), StmtDecl(declC: Decl))) =>
        val symA = declA.symbol
        val symB = declB.symbol
        inside(declC) {
          case Decl(symbol, _) =>
            symbol.kind match {
              case TypeVector(elementType, size) =>
                inside(size) {
                  case ExprRef(sym) =>
                    sym should be theSameInstanceAs symA
                }
                inside(elementType) {
                  case TypeSInt(ExprRef(sym)) =>
                    sym should be theSameInstanceAs symB
                }
              case _ => fail()
            }
        }
    }

    cc.messages.loneElement should beThe[Warning]("Variable 'c' is unused")
  }

  it should "resolve names inside array dimension" in {
    val block = """|{
                   |  i8 a;
                   |  (* unused *) bool b[a];
                   |}""".asTree[Stmt]

    val tree = block rewrite namer

    inside(tree) {
      case StmtBlock(List(StmtDecl(declA: Decl), StmtDecl(declB: Decl))) =>
        val symA = declA.symbol
        inside(declB) {
          case Decl(symbol, _) =>
            val TypeArray(TypeUInt(Expr(1)), size) = symbol.kind
            inside(size) {
              case ExprRef(sym) =>
                sym should be theSameInstanceAs symA
            }
        }
    }

    cc.messages shouldBe empty
  }

  it should "resolve names inside type expressions" in {
    val block = "uint($clog2(1368))".asTree[Expr]

    val tree = block rewrite namer

    tree should matchPattern {
      case ExprType(TypeUInt(ExprCall(_: ExprRef, List(_: ExprNum)))) =>
    }
    cc.messages shouldBe empty
  }

  it should "attach correct types to symbols - entity" in {
    val root = """|fsm a {
                  |  in bool a;
                  |  out i3 b;
                  |  param u8 P = 2;
                  |}""".stripMargin.asTree[Root]
    cc.addGlobalEntity(root.entity)
    val tree = root rewrite namer

    val symA = tree getFirst { case Entity(Sym(symbol), _) => symbol }
    inside(symA.kind) {
      case TypeEntity("a", List(symA, symB), List(symP)) =>
        symA.kind shouldBe TypeIn(TypeUInt(Expr(1)), FlowControlTypeNone)
        symB.kind shouldBe TypeOut(TypeSInt(Expr(3)), FlowControlTypeNone, StorageTypeDefault)
        symP.kind shouldBe TypeParam(TypeUInt(Expr(8)))
    }
  }

  it should "attach correct types to symbols - typedef" in {
    val root = "typedef bool a; fsm b {}".asTree[Root]
    cc.addGlobalEntity(root.entity)
    val tree = root rewrite namer

    val symA = tree collectFirst { case Sym(symbol) if symbol.name == "a" => symbol }
    symA.value.kind shouldBe TypeUInt(Expr(1))
  }

  it should "attach correct types to symbols - struct" in {
    val root = "struct a { bool a; i2 b; }; fsm b {}".asTree[Root]
    cc.addGlobalEntity(root.entity)
    val tree = root rewrite namer

    val symA = tree collectFirst { case Sym(symbol) if symbol.name == "a" => symbol }
    symA.value.kind shouldBe TypeStruct(
      "a",
      List("a", "b"),
      List(TypeUInt(Expr(1)), TypeSInt(Expr(2)))
    )
  }

  it should "attach correct types to symbols - function" in {
    val entity = "fsm foo { void a() {} }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst { case Sym(symbol) if symbol.name == "a" => symbol }
    symA.value.kind shouldBe TypeCtrlFunc(Nil, TypeVoid)
  }

  it should "attach correct types to symbols - instance" in {
    val entityA = "fsm a {}".asTree[Entity]
    val entityB = "fsm b { c = new a(); }".asTree[Entity]
    cc.addGlobalEntities(List(entityA, entityB))
    entityA rewrite namer
    val tree = entityB rewrite namer

    val symA = tree collectFirst {
      case Sym(symbol: TypeSymbol) if symbol.name == "a" => symbol
    }
    symA.value.kind shouldBe TypeEntity("a", Nil, Nil)
    val symC = tree collectFirst {
      case Sym(symbol: TermSymbol) if symbol.name == "c" => symbol
    }
    symC.value.kind shouldBe TypeInstance(symA.value)
  }

  it should "attach correct types to symbols - decl in" in {
    val entity = "fsm foo { in bool a; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst { case Decl(symbol, _) if symbol.name == "a" => symbol }
    symA.value.kind shouldBe TypeIn(TypeUInt(Expr(1)), FlowControlTypeNone)
  }

  it should "attach correct types to symbols - decl out" in {
    val entity = "fsm foo { out bool a; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst { case Decl(symbol, _) if symbol.name == "a" => symbol }
    symA.value.kind shouldBe TypeOut(TypeUInt(Expr(1)), FlowControlTypeNone, StorageTypeDefault)
  }

  it should "attach correct types to symbols - decl param" in {
    val entity = "fsm foo { param bool a = false; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst { case Decl(symbol, _) if symbol.name == "a" => symbol }
    symA.value.kind shouldBe TypeParam(TypeUInt(Expr(1)))
  }

  it should "attach correct types to symbols - decl const" in {
    val entity = "fsm foo { const bool a = false; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst { case Decl(symbol, _) if symbol.name == "a" => symbol }
    symA.value.kind shouldBe TypeConst(TypeUInt(Expr(1)))
  }

  it should "attach correct types to symbols - decl pipeline" in {
    val entity = "fsm foo { pipeline bool a; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst { case Decl(symbol, _) if symbol.name == "a" => symbol }
    symA.value.kind shouldBe TypePipeline(TypeUInt(Expr(1)))
  }

  it should "attach correct types to symbols - decl array" in {
    val entity = "fsm foo { bool a[2]; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst { case Decl(symbol, _) if symbol.name == "a" => symbol }
    symA.value.kind shouldBe TypeArray(TypeUInt(Expr(1)), Expr(2))
  }

  it should "attach correct types to symbols - decl vec" in {
    val entity = "fsm foo { i4[3][2] a; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst { case Decl(symbol, _) if symbol.name == "a" => symbol }
    symA.value.kind shouldBe TypeVector(TypeVector(TypeSInt(Expr(4)), Expr(2)), Expr(3))
  }

  it should "attach correct types to symbols - decl scalar" in {
    val entity = "fsm foo { u2 a; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst { case Decl(symbol, _) if symbol.name == "a" => symbol }
    symA.value.kind shouldBe TypeUInt(Expr(2))
  }

  it should "attach correct types to symbols - decl void" in {
    val entity = "fsm foo { void a; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst { case Decl(symbol, _) if symbol.name == "a" => symbol }
    symA.value.kind shouldBe TypeVoid
  }

  it should "attach correct types to symbols - decl ref" in {
    val root = "typedef bool b; fsm foo { b a; }".asTree[Root]
    cc.addGlobalEntity(root.entity)
    val tree = root rewrite namer

    val symB = tree collectFirst { case Sym(symbol) if symbol.name == "b" => symbol }
    symB.value.isTypeSymbol shouldBe true
    val symA = tree collectFirst { case Decl(symbol, _) if symbol.name == "a" => symbol }
    symA.value.kind shouldBe symB.value.kind
  }

  it should "issue warning for unused local variables" in {
    "{ i8 b; }".asTree[Stmt] rewrite namer
    cc.messages.loneElement should beThe[Warning]("Variable 'b' is unused")
  }

  it should "issue warning for unused local variables - but not with 'unused' attribute" in {
    "{ (* unused *) i8 b; }".asTree[Stmt] rewrite namer
    cc.messages shouldBe empty
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

  it should "issue warning for unused entity variables - but not with 'unused' attribute" in {
    val entity = "fsm a { (* unused *) i8 b; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages shouldBe empty
  }

  it should "issue warning for unused arrays" in {
    val entity = "fsm a { i8 b[2]; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages.loneElement should beThe[Warning]("Array 'b' is unused")
  }

  it should "issue warning for unused arrays - but not with 'unused' attribute" in {
    val entity = "fsm a { (* unused *) i8 b[2]; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages shouldBe empty
  }

  it should "issue warning for unused input ports" in {
    val entity = "fsm a { in i8 b; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages.loneElement should beThe[Warning]("Input port 'b' is unused")
  }

  it should "issue warning for unused input ports - but not with 'unused' attribute" in {
    val entity = "fsm a { (* unused *) in i8 b; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages shouldBe empty
  }

  it should "issue warning for unused input ports - but not in verbatim entity" in {
    val entity = "verbatim entity a { in i8 b; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages shouldBe empty
  }

  it should "issue warning for unused output ports" in {
    val entity = "fsm a { out i8 b; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages.loneElement should beThe[Warning]("Output port 'b' is unused")
  }

  it should "issue warning for unused output ports - but not with 'unused' attribute" in {
    val entity = "fsm a { (* unused *) out i8 b; }".asTree[Entity]
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

  it should "issue warning for unused parameters" in {
    val entity = "fsm a { param i8 b = 8'd9; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages.loneElement should beThe[Warning]("Parameter 'b' is unused")
  }

  it should "issue warning for unused parameters - but not with 'unused' attribute" in {
    val entity = "fsm a { (* unused *) param i8 b = 8'd9; }".asTree[Entity]
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

  it should "issue warning for unused constants - but not with 'unused' attribute" in {
    val entity = "fsm a { (* unused *) const i8 b = 8'd9; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages shouldBe empty
  }

  it should "issue warning for unused pipeline variables" in {
    val entity = "fsm a { pipeline i8 b; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages.loneElement should beThe[Warning]("Pipeline variable 'b' is unused")
  }

  it should "issue warning for unused pipeline variables - but not with 'unused' attribute" in {
    val entity = "fsm a { (* unused *) pipeline i8 b; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages shouldBe empty
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

  it should "issue warning for unused functions - but not with 'unused' attribute" in {
    val entity = "fsm a { (* unused *) void foo() {} }".asTree[Entity]
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

  it should "issue warning for unused entities - but not with 'unused' attribute" in {
    val entity = "network a { (* unused *) fsm bar {} }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages shouldBe empty
  }

  it should "issue warning for unused instances" in {
    val entity = "network a { new fsm bar {} }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages.loneElement should beThe[Warning]("Instance 'bar' is unused")
  }

  it should "issue warning for unused instances - but not with 'unused' attribute" in {
    val entity = "network a { (* unused *) new fsm bar {} }".asTree[Entity]
    cc.addGlobalEntity(entity)
    entity rewrite namer
    cc.messages shouldBe empty
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

  it should "attach source attributes to symbols - function" in {
    val entity = "fsm foo { (* reclimit = 1 *) void a() {} }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst {
      case Sym(symbol) if symbol.name == "a" => symbol
    }
    symA.value.kind shouldBe TypeCtrlFunc(Nil, TypeVoid)
    symA.value.attr.recLimit.get.value shouldBe Expr(1)
  }

  it should "attach source attributes to symbols - entity" in {
    val entity = "(* stacklimit = 2 *) fsm foo { }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst { case Entity(Sym(symbol), _) => symbol }
    symA.value.kind shouldBe TypeEntity("foo", Nil, Nil)
    symA.value.attr.stackLimit.get.value shouldBe Expr(2)
  }

  it should "attach source attributes to symbols - nested entity" in {
    val entity = "network foo { (* stacklimit = 3 *) fsm bar {} }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symA = tree collectFirst {
      case Entity(Sym(symbol), _) if symbol.name == "bar" => symbol
    }
    symA.value.kind shouldBe TypeEntity("bar", Nil, Nil)
    symA.value.attr.stackLimit.get.value shouldBe Expr(3)
  }

  it should "attach source attributes to symbols - instance" in {
    val entity = "fsm foo { (* unused *) new (* stacklimit = 3 *) fsm bar {} }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val (iSymbol, eSymbol) = tree getFirst {
      case EntInstance(Sym(iSymbol: TermSymbol), Sym(eSymbol: TypeSymbol), _, _) =>
        (iSymbol, eSymbol)
    }
    iSymbol.kind shouldBe TypeInstance(eSymbol)
    iSymbol.attr.unused.get.value shouldBe true
    eSymbol.kind shouldBe TypeEntity("bar", Nil, Nil)
    eSymbol.attr.stackLimit.get.value shouldBe Expr(3)
  }

  it should "attach source attributes to symbols - declaration" in {
    val entity = "fsm foo { (* unused *) i8 a; }".asTree[Entity]
    cc.addGlobalEntity(entity)
    val tree = entity rewrite namer

    val symbol = tree getFirst { case Decl(symbol: TermSymbol, _) => symbol }
    symbol.kind shouldBe TypeSInt(Expr(8))
    symbol.attr.unused.get.value shouldBe true
  }

  it should "issue error for a vector of structs definition" in {
    val root = "struct s { bool a; }; fsm foo { (* unused *) s[2] b; }".asTree[Root]
    cc.addGlobalEntity(root.entity)
    root rewrite namer

    cc.messages.loneElement should beThe[Error]("Vector element cannot have a struct type")
  }

  it should "issue error for a memory of structs definition" in {
    val root = "struct s { bool a; }; fsm foo { (* unused *) s b[2]; }".asTree[Root]
    cc.addGlobalEntity(root.entity)
    root rewrite namer

    cc.messages.loneElement should beThe[Error]("Memory element cannot have a struct type")
  }
}
