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
import org.scalatest.FreeSpec

final class NamerSpec extends FreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext
  lazy val atBits = ExprSym(cc.lookupGlobalTerm("@bits"))
  val namer = new Namer

  def xform(tree: Tree): Tree = {
    tree match {
      case Root(_, entity) => cc.addGlobalEntity(entity)
      case entity: Entity  => cc.addGlobalEntity(entity)
      case _               =>
    }
    tree rewrite namer
  }

  "The Namer should " - {
    "issue error for redefinition of variable" in {
      """|{
         |  u1 foo;
         |  u2 foo;
         |}""".stripMargin.asTree[Stmt] rewrite namer

      cc.messages should have length 1
      cc.messages(0) should beThe[Error](
        "Redefinition of name 'foo' with previous definition at",
        ".*:2"
      )
      cc.messages(0).loc.line shouldBe 3
    }

    "issue error for redefinition of type" in {
      val root = """|typedef u1 foo;
                    |typedef u2 foo;
                    |fsm a {}""".stripMargin.asTree[Root]
      cc.addGlobalEntity(root.entity)
      root rewrite namer

      cc.messages.loneElement should beThe[Error](
        "Redefinition of type 'foo' with previous definition at",
        ".*:1"
      )
      cc.messages(0).loc.line shouldBe 2
    }

    "issue warning for variable hiding" in {
      """|{
         |  u1 foo;
         |  { u2 foo; }
         |}""".stripMargin.asTree[Stmt] rewrite namer

      cc.messages should have length 1
      cc.messages(0) should beThe[Warning](
        "Definition of name 'foo' hides previous definition at",
        ".*:2"
      )
      cc.messages(0).loc.line shouldBe 3
    }

    "issue warning for variable hiding even for later symbol" in {
      val entity = """|fsm a {
                      |  void main() { bool foo; }
                      |  void foo() {}
                      |}""".stripMargin.asTree[Entity]
      cc.addGlobalEntity(entity)
      entity rewrite namer

      cc.messages should have length 1
      cc.messages(0) should beThe[Warning](
        "Definition of name 'foo' hides previous definition at",
        ".*:3"
      )
      cc.messages(0).loc.line shouldBe 2
    }

    "cope with using the same name in non-intersecting scopes" in {
      """|{
         |  { u1 foo; }
         |  { u2 foo; }
         |}""".stripMargin.asTree[Stmt] rewrite namer

      cc.messages shouldBe empty
    }

    "cope with multi level typedefs" in {
      val root = """|typedef bool a;
                    |typedef a b;
                    |fsm c {}""".stripMargin.asTree[Root]
      cc.addGlobalEntity(root.entity)
      root rewrite namer

      cc.messages shouldBe empty
    }

    "issue error for undefined term names" in {
      """|{
         |  u1 foo = bar;
         |}""".stripMargin.asTree[Stmt] rewrite namer

      cc.messages should have length 1
      cc.messages(0) should beThe[Error]("Name 'bar' is not defined")
      cc.messages(0).loc.line shouldBe 2
    }

    "issue error for undefined type names" in {
      """|{
         |  foo_t foo;
         |}""".stripMargin.asTree[Stmt] rewrite namer

      cc.messages should have length 1
      cc.messages(0) should beThe[Error]("Type 'foo_t' is not defined")
      cc.messages(0).loc.line shouldBe 2
    }

    "insert names from 'for ()' loop initializers into the loop scope" in {
      """|for (bool b=true;;) {
         | i2 b;
         |}""".stripMargin.asTree[Stmt] rewrite namer

      cc.messages should have length 1
      cc.messages(0) should beThe[Error](
        "Redefinition of name 'b' with previous definition at",
        ".*:1"
      )
      cc.messages(0).loc.line shouldBe 2
    }

    "insert names from 'let ()' initializers into the following loop scope" in {
      """|let (bool a=true) do {
         | i2 a;
         |} while (1);""".stripMargin.asTree[Stmt] rewrite namer

      cc.messages should have length 1
      cc.messages(0) should beThe[Error](
        "Redefinition of name 'a' with previous definition at",
        ".*:1"
      )
      cc.messages(0).loc.line shouldBe 2
    }

    "construct struct types" in {
      val root = """|typedef bool e_t;
                    |struct a {
                    |  bool b;
                    |  i8 c;
                    |  e_t  d;
                    |}
                    |fsm b {}""".stripMargin.asTree[Root]
      cc.addGlobalEntity(root.entity)

      val tree = root rewrite namer

      inside(tree) {
        case Root(List(
                    Defn(eSym),
                    Defn(aSym)
                  ),
                  _) =>
          aSym.loc.line shouldBe 2
          aSym.uniqueName shouldBe TypeName("a")
          aSym.isTypeSymbol shouldBe true

          aSym.kind shouldBe TypeStruct(
            "a",
            List("b", "c", "d"),
            List(TypeUInt(Expr(1)), TypeSInt(Expr(8)), TypeRef(Sym(eSym, Nil)))
          )
      }

      cc.messages shouldBe empty
    }

    "resolve term names to their correct definitions" in {
      val tree = """|{
                    |  bool a;
                    |  {
                    |    bool a;
                    |    a = false;
                    |  }
                    |  a = true;
                    |}""".stripMargin.asTree[Stmt] rewrite namer

      inside(tree) {
        case StmtBlock(List(StmtDecl(Decl(outer1, _)), block, StmtAssign(ExprSym(outer2), _))) =>
          outer1.loc.line shouldBe 2
          outer1 should be theSameInstanceAs outer2
          outer1.isTermSymbol shouldBe true
          inside(block) {
            case StmtBlock(List(StmtDecl(Decl(inner1, _)), StmtAssign(ExprSym(inner2), _))) =>
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

    "resolve term names to their correct definitions - builtin" in {
      val tree = "@bits".asTree[Expr] rewrite namer

      tree shouldBe atBits
      cc.messages shouldBe empty
    }

    "resolve type names to their correct definitions - typedef" in {
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
            case Defn(defSym) =>
              defSym.loc.line shouldBe 1
              defSym.kind shouldBe TypeUInt(Expr(1))
              inside(entity) {
                case Entity(_, List(EntCombProcess(List(StmtBlock(List(_, stmt)))))) =>
                  inside(stmt) {
                    case StmtDecl(Decl(symbol, _)) =>
                      symbol.kind shouldBe TypeRef(Sym(defSym, Nil))
                  }
              }
          }
      }

      cc.messages shouldBe empty
    }

    "resolve type names to their correct definitions - struct" in {
      val root = """|struct bar_t {
                    |  bool a;
                    |}
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
            case Defn(defSym) =>
              defSym.loc.line shouldBe 1
              inside(entity) {
                case Entity(_, List(EntCombProcess(List(StmtBlock(List(_, stmt)))))) =>
                  inside(stmt) {
                    case StmtDecl(Decl(symbol, _)) =>
                      symbol.kind shouldBe TypeRef(Sym(defSym, Nil))
                  }
              }
          }
      }

      cc.messages shouldBe empty
    }

    "resolve type names to their correct definitions - dict a" in {
      val root = """|network a {
                    |  gen for (uint N < 10) {
                    |    in bool i#[N];
                    |    out bool o#[N];
                    |    i#[N] -> o#[N];
                    |  }
                    |}""".stripMargin.asTree[Root]

      cc.addGlobalEntity(root.entity)

      val tree = root rewrite namer

      inside(tree) {
        case Root(Nil, entity) =>
          inside(entity) {
            case Entity(_, List(EntGen(GenRange(Decl(nSym, _), _, _, body)))) =>
              inside(body) {
                case GenDecl(DeclRef(Sym(dISym, ExprSym(nASym) :: Nil), _, _)) ::
                      GenDecl(DeclRef(Sym(dOSym, ExprSym(nBSym) :: Nil), _, _)) ::
                      EntConnect(
                      ExprRef(Sym(cISym, ExprSym(nCSym) :: Nil)),
                      ExprRef(Sym(cOSym, ExprSym(nDSym) :: Nil)) :: Nil
                    ) :: Nil =>
                  dISym shouldBe theSameInstanceAs(cISym)
                  dOSym shouldBe theSameInstanceAs(cOSym)
                  nSym shouldBe theSameInstanceAs(nASym)
                  nSym shouldBe theSameInstanceAs(nBSym)
                  nSym shouldBe theSameInstanceAs(nCSym)
                  nSym shouldBe theSameInstanceAs(nDSym)
              }
          }
      }
    }

    "resolve type names to their correct definitions - dict b" in {
      val root = """|network a {
                    |  gen for (uint N < 2) {
                    |    in bool i#[N];
                    |    out bool o#[N];
                    |  }
                    |  i#[0] -> o#[0];
                    |  i#[1] -> o#[1];
                    |}""".stripMargin.asTree[Root]

      cc.addGlobalEntity(root.entity)

      val tree = root rewrite namer

      inside(tree) {
        case Root(Nil, Entity(_, List(EntGen(gen), conn0, conn1))) =>
          inside(gen) {
            case GenRange(_, _, _, body) =>
              inside(body) {
                case GenDecl(DeclRef(Sym(iSym, _), _, _)) ::
                      GenDecl(DeclRef(Sym(oSym, _), _, _)) :: Nil =>
                  inside(conn0) {
                    case EntConnect(
                        ExprRef(Sym(lSym, Expr(0) :: Nil)),
                        ExprRef(Sym(rSym, Expr(0) :: Nil)) :: Nil
                        ) =>
                      iSym shouldBe theSameInstanceAs(lSym.kind.asChoice.symbols.head)
                      oSym shouldBe theSameInstanceAs(rSym.kind.asChoice.symbols.head)
                  }
                  inside(conn1) {
                    case EntConnect(
                        ExprRef(Sym(lSym, Expr(1) :: Nil)),
                        ExprRef(Sym(rSym, Expr(1) :: Nil)) :: Nil
                        ) =>
                      iSym shouldBe theSameInstanceAs(lSym.kind.asChoice.symbols.head)
                      oSym shouldBe theSameInstanceAs(rSym.kind.asChoice.symbols.head)
                  }
              }

          }
      }

      cc.messages shouldBe empty
    }

    "resolve function references to later definitions" in {
      val entity = """|fsm a {
                      |  void main () { foo(); }
                      |  void foo () {}
                      |}""".stripMargin.asTree[Entity]
      cc.addGlobalEntity(entity)
      val tree = entity rewrite namer

      inside(tree) {
        case entity: Entity =>
          inside(entity.functions) {
            case List(main, foo) =>
              inside(main) {
                case EntFunction(_, List(StmtExpr(ExprCall(ExprSym(fooInMain), _)))) =>
                  inside(foo) {
                    case EntFunction(Sym(fooInDef, Nil), _) =>
                      fooInMain should be theSameInstanceAs fooInDef
                  }
              }
          }
      }

      cc.messages shouldBe empty
    }

    "resolve entity symbols in instantiations" in {
      val entityA = """fsm a {}""".stripMargin.asTree[Entity]
      val entityB = """|network b {
                       |  a = new a();
                       |}""".stripMargin.asTree[Entity]

      cc.addGlobalEntities(List(entityA, entityB))

      val treeA = entityA rewrite namer
      val treeB = entityB rewrite new Namer

      val aSym = treeA match {
        case Entity(Sym(symbol, Nil), _) => symbol
        case _                           => fail
      }

      aSym.isTypeSymbol shouldBe true

      inside(treeB) {
        case Entity(_, instances) =>
          inside(instances.head) {
            case EntInstance(Sym(_, Nil), Sym(sym, Nil), Nil, Nil) =>
              sym should be theSameInstanceAs aSym
          }
      }
    }

    "resolve instantiations of nested entities" in {
      val entity = """|network a {
                      |  new fsm b { }
                      |}""".stripMargin.asTree[Entity]

      cc.addGlobalEntity(entity)

      val tree = entity rewrite namer

      inside(tree) {
        case Entity(_, List(bEntity, bInstance)) =>
          inside(bInstance) {
            case EntInstance(Sym(iSym, Nil), Sym(eSym, Nil), _, _) =>
              iSym.isTermSymbol shouldBe true
              eSym.isTypeSymbol shouldBe true
              inside(bEntity) {
                case EntEntity(Entity(Sym(sym, Nil), _)) =>
                  eSym should be theSameInstanceAs sym
                  iSym shouldNot be theSameInstanceAs sym
              }
          }
      }
    }

    "resolve goto targets" in {
      val entity = """|fsm a {
                      |  void main() { goto foo; }
                      |  void foo() {}
                      |}""".stripMargin.asTree[Entity]

      cc.addGlobalEntity(entity)

      val tree = entity rewrite namer

      inside(tree) {
        case Entity(_, List(main, foo)) =>
          inside(main) {
            case EntFunction(Sym(_, Nil), List(StmtGoto(ExprSym(sym)))) =>
              sym.isTermSymbol shouldBe true
              inside(foo) {
                case EntFunction(Sym(fooSym, Nil), Nil) =>
                  sym should be theSameInstanceAs fooSym
              }
          }
      }

      cc.messages shouldBe empty
    }

    "resolve @bits arguments to term names even if the argument is a valid type expression" in {
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
            case EntFunction(Sym(_, Nil), List(StmtDecl(decl), StmtExpr(expr))) =>
              inside(decl) {
                case Decl(dSym, None) =>
                  dSym.kind shouldBe TypeUInt(Expr(1))
                  inside(expr) {
                    case ExprCall(`atBits`, List(ExprSym(rSym))) =>
                      rSym should be theSameInstanceAs dSym
                      rSym.isTermSymbol shouldBe true
                      rSym.loc.line shouldBe 3
                  }
              }
          }
      }

      cc.messages shouldBe empty
    }

    "resolve @bits arguments to type names if the argument is a valid type expression" in {
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
            case Defn(dSym) =>
              dSym.kind shouldBe TypeUInt(Expr(1))
              inside(entity) {
                case Entity(_, List(main)) =>
                  inside(main) {
                    case EntFunction(Sym(_, Nil), List(StmtExpr(expr))) =>
                      inside(expr) {
                        case ExprCall(`atBits`, List(ExprSym(rSym))) =>
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

    "not resolve @bits arguments to type names if the argument is not a type expression" in {
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
            case Defn(defSym) =>
              defSym.kind shouldBe TypeUInt(Expr(1))
              inside(entity) {
                case Entity(_, List(main)) =>
                  inside(main) {
                    case EntFunction(Sym(_, Nil), List(StmtDecl(decl: Decl), StmtExpr(expr))) =>
                      val dSym = decl.symbol
                      inside(expr) {
                        case ExprCall(`atBits`, List(ExprSym(rSym) + Expr(2))) =>
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

    "issue error if @bits argument is ambiguous and can resolve to both type and term names" in {
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

    "not resolve other identifiers in expressions to type names" in {
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
            case StmtDecl(Decl(_, Some(ExprSym(sym)))) =>
              sym should be theSameInstanceAs ErrorSymbol
          }
      }

      cc.messages should have length 1
      cc.messages(0) should beThe[Error]("Name 'a' is not defined")
      cc.messages(0).loc.line shouldBe 4
    }

    "resolve names inside type arguments" in {
      val block = """|{
                     |  i8 a;
                     |  i8 b;
                     |  int(b)[a] c;
                     |}""".stripMargin.asTree[Stmt]

      val tree = block rewrite namer

      inside(tree) {
        case StmtBlock(List(StmtDecl(declA: Decl), StmtDecl(declB: Decl), StmtDecl(declC: Decl))) =>
          val symA = declA.symbol
          val symB = declB.symbol
          inside(declC) {
            case Decl(symbol, _) =>
              symbol.kind match {
                case TypeVector(elementType, sz) =>
                  inside(sz) {
                    case ExprSym(sym) =>
                      sym should be theSameInstanceAs symA
                  }
                  inside(elementType) {
                    case TypeSInt(ExprSym(sym)) =>
                      sym should be theSameInstanceAs symB
                  }
                case _ => fail()
              }
          }
      }

      cc.messages shouldBe empty
    }

    "resolve names inside array dimension" in {
      val block = """|{
                     |  i8 a;
                     |  bool b[a];
                     |}""".stripMargin.asTree[Stmt]

      val tree = block rewrite namer

      inside(tree) {
        case StmtBlock(List(StmtDecl(declA: Decl), StmtDecl(declB: Decl))) =>
          val symA = declA.symbol
          inside(declB) {
            case Decl(symbol, _) =>
              val TypeArray(TypeUInt(Expr(1)), size) = symbol.kind
              inside(size) {
                case ExprSym(sym) =>
                  sym should be theSameInstanceAs symA
              }
          }
      }

      cc.messages shouldBe empty
    }

    "resolve names inside type expressions" in {
      val block = "uint($clog2(1368))".asTree[Expr]

      val tree = block rewrite namer

      tree should matchPattern {
        case ExprType(TypeUInt(ExprCall(_: ExprSym, List(_: ExprNum)))) =>
      }
      cc.messages shouldBe empty
    }

    "attach correct types to symbols - entity" in {
      val root = """|fsm a {
                    |  in bool a;
                    |  out i3 b;
                    |  param u8 P = 2;
                    |}""".stripMargin.asTree[Root]
      cc.addGlobalEntity(root.entity)
      val tree = root rewrite namer

      val symA = tree getFirst { case Entity(Sym(symbol, Nil), _) => symbol }
      inside(symA.kind) {
        case TypeEntity("a", List(sA, sB), List(sP)) =>
          sA.kind shouldBe TypeIn(TypeUInt(Expr(1)), FlowControlTypeNone)
          sB.kind shouldBe TypeOut(TypeSInt(Expr(3)), FlowControlTypeNone, StorageTypeDefault)
          sP.kind shouldBe TypeParam(TypeUInt(Expr(8)))
      }
    }

    "attach correct types to symbols - typedef" in {
      val root = "typedef bool a; fsm b {}".asTree[Root]
      cc.addGlobalEntity(root.entity)
      val tree = root rewrite namer

      val symA = tree collectFirst { case Defn(symbol) if symbol.name == "a" => symbol }
      symA.value.kind shouldBe TypeUInt(Expr(1))
    }

    "attach correct types to symbols - struct" in {
      val root = "struct a { bool a; i2 b; } fsm b {}".asTree[Root]
      cc.addGlobalEntity(root.entity)
      val tree = root rewrite namer

      val symA = tree collectFirst { case Defn(symbol) if symbol.name == "a" => symbol }
      symA.value.kind shouldBe TypeStruct(
        "a",
        List("a", "b"),
        List(TypeUInt(Expr(1)), TypeSInt(Expr(2)))
      )
    }

    "attach correct types to symbols - function" in {
      val entity = "fsm foo { void a() {} }".asTree[Entity]
      cc.addGlobalEntity(entity)
      val tree = entity rewrite namer

      val symA = tree collectFirst { case Sym(symbol, Nil) if symbol.name == "a" => symbol }
      symA.value.kind shouldBe TypeCtrlFunc(Nil, TypeVoid)
    }

    "attach correct types to symbols - instance" in {
      val entityA = "fsm a {}".asTree[Entity]
      val entityB = "fsm b { c = new a(); }".asTree[Entity]
      cc.addGlobalEntities(List(entityA, entityB))
      entityA rewrite namer
      val tree = entityB rewrite namer

      val symA = tree collectFirst {
        case Sym(symbol: TypeSymbol, Nil) if symbol.name == "a" => symbol
      }
      symA.value.kind shouldBe TypeEntity("a", Nil, Nil)
      val symC = tree collectFirst {
        case Sym(symbol: TermSymbol, Nil) if symbol.name == "c" => symbol
      }
      symC.value.kind shouldBe TypeInstance(symA.value)
    }

    "attach correct types to symbols - decl in" in {
      val entity = "fsm foo { in bool a; }".asTree[Entity]
      cc.addGlobalEntity(entity)
      val tree = entity rewrite namer

      val symA = tree collectFirst { case Decl(symbol, _) if symbol.name == "a" => symbol }
      symA.value.kind shouldBe TypeIn(TypeUInt(Expr(1)), FlowControlTypeNone)
    }

    "attach correct types to symbols - decl out" in {
      val entity = "fsm foo { out bool a; }".asTree[Entity]
      cc.addGlobalEntity(entity)
      val tree = entity rewrite namer

      val symA = tree collectFirst { case Decl(symbol, _) if symbol.name == "a" => symbol }
      symA.value.kind shouldBe TypeOut(TypeUInt(Expr(1)), FlowControlTypeNone, StorageTypeDefault)
    }

    "attach correct types to symbols - decl param" in {
      val entity = "fsm foo { param bool a = false; }".asTree[Entity]
      cc.addGlobalEntity(entity)
      val tree = entity rewrite namer

      val symA = tree collectFirst { case Decl(symbol, _) if symbol.name == "a" => symbol }
      symA.value.kind shouldBe TypeParam(TypeUInt(Expr(1)))
    }

    "attach correct types to symbols - decl const" in {
      val entity = "fsm foo { const bool a = false; }".asTree[Entity]
      cc.addGlobalEntity(entity)
      val tree = entity rewrite namer

      val symA = tree collectFirst { case Decl(symbol, _) if symbol.name == "a" => symbol }
      symA.value.kind shouldBe TypeConst(TypeUInt(Expr(1)))
    }

    "attach correct types to symbols - decl pipeline" in {
      val entity = "fsm foo { pipeline bool a; }".asTree[Entity]
      cc.addGlobalEntity(entity)
      val tree = entity rewrite namer

      val symA = tree collectFirst { case Decl(symbol, _) if symbol.name == "a" => symbol }
      symA.value.kind shouldBe TypePipeline(TypeUInt(Expr(1)))
    }

    "attach correct types to symbols - decl array" in {
      val entity = "fsm foo { bool a[2]; }".asTree[Entity]
      cc.addGlobalEntity(entity)
      val tree = entity rewrite namer

      val symA = tree collectFirst { case Decl(symbol, _) if symbol.name == "a" => symbol }
      symA.value.kind shouldBe TypeArray(TypeUInt(Expr(1)), Expr(2))
    }

    "attach correct types to symbols - decl vec" in {
      val entity = "fsm foo { i4[3][2] a; }".asTree[Entity]
      cc.addGlobalEntity(entity)
      val tree = entity rewrite namer

      val symA = tree collectFirst { case Decl(symbol, _) if symbol.name == "a" => symbol }
      symA.value.kind shouldBe TypeVector(TypeVector(TypeSInt(Expr(4)), Expr(2)), Expr(3))
    }

    "attach correct types to symbols - decl scalar" in {
      val entity = "fsm foo { u2 a; }".asTree[Entity]
      cc.addGlobalEntity(entity)
      val tree = entity rewrite namer

      val symA = tree collectFirst { case Decl(symbol, _) if symbol.name == "a" => symbol }
      symA.value.kind shouldBe TypeUInt(Expr(2))
    }

    "attach correct types to symbols - decl void" in {
      val entity = "fsm foo { void a; }".asTree[Entity]
      cc.addGlobalEntity(entity)
      val tree = entity rewrite namer

      val symA = tree collectFirst { case Decl(symbol, _) if symbol.name == "a" => symbol }
      symA.value.kind shouldBe TypeVoid
    }

    "attach correct types to symbols - decl ref" in {
      val root = "typedef bool b; fsm foo { b a; }".asTree[Root]
      cc.addGlobalEntity(root.entity)
      val tree = root rewrite namer

      val symB = tree collectFirst { case Defn(symbol) if symbol.name == "b" => symbol }
      symB.value.isTypeSymbol shouldBe true
      val symA = tree collectFirst { case Decl(symbol, _) if symbol.name == "a" => symbol }
      symA.value.kind shouldBe TypeRef(Sym(symB.value, Nil))
    }

    "attach source attributes to symbols - function" in {
      val entity = "fsm foo { (* reclimit = 1 *) void a() {} }".asTree[Entity]
      cc.addGlobalEntity(entity)
      val tree = entity rewrite namer

      val symA = tree collectFirst {
        case Sym(symbol, Nil) if symbol.name == "a" => symbol
      }
      symA.value.kind shouldBe TypeCtrlFunc(Nil, TypeVoid)
      symA.value.attr.recLimit.get.value shouldBe Expr(1)
    }

    "attach source attributes to symbols - entity" in {
      val entity = "(* stacklimit = 2 *) fsm foo { }".asTree[Entity]
      cc.addGlobalEntity(entity)
      val tree = entity rewrite namer

      val symA = tree collectFirst { case Entity(Sym(symbol, Nil), _) => symbol }
      symA.value.kind shouldBe TypeEntity("foo", Nil, Nil)
      symA.value.attr.stackLimit.get.value shouldBe Expr(2)
    }

    "attach source attributes to symbols - nested entity" in {
      val entity = "network foo { (* stacklimit = 3 *) fsm bar {} }".asTree[Entity]
      cc.addGlobalEntity(entity)
      val tree = entity rewrite namer

      val symA = tree collectFirst {
        case Entity(Sym(symbol, Nil), _) if symbol.name == "bar" => symbol
      }
      symA.value.kind shouldBe TypeEntity("bar", Nil, Nil)
      symA.value.attr.stackLimit.get.value shouldBe Expr(3)
    }

    "attach source attributes to symbols - instance" in {
      val entity = "fsm foo { (* unused *) new (* stacklimit = 3 *) fsm bar {} }".asTree[Entity]
      cc.addGlobalEntity(entity)
      val tree = entity rewrite namer

      val (iSymbol, eSymbol) = tree getFirst {
        case EntInstance(Sym(iSymbol: TermSymbol, Nil), Sym(eSymbol: TypeSymbol, Nil), _, _) =>
          (iSymbol, eSymbol)
      }
      iSymbol.kind shouldBe TypeInstance(eSymbol)
      iSymbol.attr.unused.get.value shouldBe true
      eSymbol.kind shouldBe TypeEntity("bar", Nil, Nil)
      eSymbol.attr.stackLimit.get.value shouldBe Expr(3)
    }

    "attach source attributes to symbols - declaration" in {
      val entity = "fsm foo { (* unused *) i8 a; }".asTree[Entity]
      cc.addGlobalEntity(entity)
      val tree = entity rewrite namer

      val symbol = tree getFirst { case Decl(symbol: TermSymbol, _) => symbol }
      symbol.kind shouldBe TypeSInt(Expr(8))
      symbol.attr.unused.get.value shouldBe true
    }

    "check dictionary identifier declarations only appear inside 'gen' loops" - {
      "entity" - {
        for {
          (variant, text, hint) <- List(
            ("network", "bool a#[0];", "Declaration"),
            ("network", "typedef bool a#[0];", "Definition"),
            ("network", "fsm a#[0] {}", "Definition"),
            ("network", "a#[0] = new e();", "Declaration"),
            ("fsm ", "void a#[0]() {}", "Definition")
          )
        } {
          val error = s"$hint with dictionary identifier must appear directly in 'gen' loop scope."
          text - {
            "entity" in {
              xform(s"$variant e {$text}".asTree[Root])
              cc.messages.loneElement should beThe[Error](error)
            }

            "gen if" in {
              xform(s"$variant e { gen if (true) {$text} }".asTree[Root])
              cc.messages.loneElement should beThe[Error](error)
            }

            "gen else" in {
              xform(s"$variant e { gen if (false) {} else {$text} }".asTree[Root])
              cc.messages.loneElement should beThe[Error](error)
            }

            "gen for" in {
              xform(s"$variant e { gen for (uint N = 0; N < 0 ; N++) {$text} }".asTree[Root])
              cc.messages shouldBe empty
            }

            "gen range" in {
              xform(s"$variant e { gen for (uint N < 0) {$text} }".asTree[Root])
              cc.messages shouldBe empty
            }
          }
        }
      }

      "stmt" - {
        val text = "bool a#[0];"
        val error =
          s"Declaration with dictionary identifier must appear directly in 'gen' loop scope."

        "gen if" in {
          s"{gen if (true) {$text}}".asTree[Stmt] rewrite namer
          cc.messages.loneElement should beThe[Error](error)
        }

        "gen else" in {
          s"{gen if (true) {} else {$text}}".asTree[Stmt] rewrite namer
          cc.messages.loneElement should beThe[Error](error)
        }

        "gen for" in {
          s"{gen for (uint N = 0; N < 0 ; N++) {$text}}".asTree[Stmt] rewrite namer
          cc.messages shouldBe empty
        }

        "gen range" in {
          s"{gen for (uint N < 0) {$text}}".asTree[Stmt] rewrite namer
          cc.messages shouldBe empty
        }

        "block gen if" in {
          s"{gen if (true) {{$text}}}".asTree[Stmt] rewrite namer
          cc.messages.loneElement should beThe[Error](error)
        }

        "block gen else" in {
          s"{gen if (true) {} else {{$text}}}".asTree[Stmt] rewrite namer
          cc.messages.loneElement should beThe[Error](error)
        }

        "block in gen for" in {
          s"{gen for (uint N = 0; N < 0 ; N++) {{$text}}}".asTree[Stmt] rewrite namer
          cc.messages.loneElement should beThe[Error](error)
        }

        "block in gen range" in {
          s"{gen for (uint N < 0) {{$text}}}".asTree[Stmt] rewrite namer
          cc.messages.loneElement should beThe[Error](error)
        }

        "block" in {
          s"{$text}".asTree[Stmt] rewrite namer
          cc.messages.loneElement should beThe[Error](error)
        }

        "if" in {
          s"if (true) $text".asTree[Stmt] rewrite namer
          cc.messages.loneElement should beThe[Error](error)
        }

        "else" in {
          s"if (true) {} else $text".asTree[Stmt] rewrite namer
          cc.messages.loneElement should beThe[Error](error)
        }

        "case clause - regular" in {
          s"case (1) { 0: $text }".asTree[Stmt] rewrite namer
          cc.messages.loneElement should beThe[Error](error)
        }

        "case clause - default" in {
          s"case (1) { default: $text }".asTree[Stmt] rewrite namer
          cc.messages.loneElement should beThe[Error](error)
        }
      }
    }
  }
}
