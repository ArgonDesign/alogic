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
// Parser tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees.Expr.ImplicitConversions._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeReady
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeValid
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.core.Message
import com.argondesign.alogic.core.Warning
import org.scalatest.FreeSpec
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

final class ParserSpec extends FreeSpec with AlogicTest {

  implicit val cc = new CompilerContext

  def beSyntaxError: Matcher[Message] = beSyntaxError("")

  def beSyntaxError(text: String): Matcher[Message] = Matcher { (msg: Message) =>
    val matchesMessage = if (text.isEmpty) {
      msg.msg(0) startsWith "Syntax error: "
    } else {
      msg.msg(0) == s"Syntax error: ${text}"
    }
    MatchResult(
      msg.isInstanceOf[Error] && matchesMessage,
      s"'${msg}' was not a Syntax error message matching 'Syntax error: ${text}'",
      s"'${msg}' was the correct Syntex error message"
    )
  }

  "The parser" - {

    /////////////////////////////////////////////////////////////////////////////
    // Valid input
    /////////////////////////////////////////////////////////////////////////////

    "should accept valid input" - {

      "file without type definitions" in {
        """|fsm foo {
           |
           |}""".stripMargin.asTree[Root] should matchPattern {
          case Root(Nil, _: Entity) =>
        }
        cc.messages shouldBe empty
      }

    }

    /////////////////////////////////////////////////////////////////////////////
    // Invalid input
    /////////////////////////////////////////////////////////////////////////////

    "should not accept input with invalid syntax" - {

      "syntax error" in {
        a[AsTreeSyntaxErrorException] shouldBe thrownBy {
          "$!%".asTree[Root]
        }
        cc.messages.loneElement should beSyntaxError
      }

      "file without entity defintition" in {
        a[AsTreeSyntaxErrorException] should be thrownBy {
          "typedef i8 a;".asTree[Root]
        }
        cc.messages.loneElement should beSyntaxError
      }

      "empty let statement" in {
        a[AsTreeSyntaxErrorException] should be thrownBy {
          "let () for(a=1;a;a--) { a+=b; }".asTree[Stmt]
        }
        cc.messages should not be empty
        cc.messages(0) should beSyntaxError("empty 'let ()' statement")
      }

      "empty do loop" in {
        a[AsTreeSyntaxErrorException] should be thrownBy {
          "do { a=1; } while();".asTree[Stmt]
        }
        cc.messages.loneElement should beSyntaxError("empty 'while ()' condition")
      }

      "empty while loop" in {
        a[AsTreeSyntaxErrorException] should be thrownBy {
          "while ()".asTree[Stmt]
        }
        cc.messages should not be empty
        cc.messages(0) should beSyntaxError("empty 'while ()' condition")
      }

      "missing parameter list after instantiation" in {
        a[AsTreeSyntaxErrorException] should be thrownBy {
          "network a { b = new c; }".asTree[Entity]
        }
        cc.messages should not be empty
        cc.messages(0) should beSyntaxError(
          "missing parameter list '()' after entity name in instantiation 'new c'")
      }

      "empty concatenation" in {
        a[AsTreeSyntaxErrorException] should be thrownBy {
          "{}".asTree[Expr]
        }
        cc.messages should not be empty
      }

      // TODO: Mandatory blocks
    }

    "should signal error for malformed input" - {}

    /////////////////////////////////////////////////////////////////////////////
    // AST representations
    /////////////////////////////////////////////////////////////////////////////

    "should build correct ASTs for" - {

      "type definitions" - {

        "typedef" in {
          "typedef u8 foo;".asTree[TypeDefinition] shouldBe {
            TypeDefinitionTypedef(Ident("foo"), TypeUInt(Expr(8)))
          }
        }

        "struct" in {
          "struct bar { u8 foo; i2 baz; }".asTree[TypeDefinition] shouldBe {
            TypeDefinitionStruct(
              Ident("bar"),
              List("foo", "baz"),
              List(TypeUInt(Expr(8)), TypeSInt(Expr(2)))
            )
          }
        }

      }

      "declarations" - {
        "scalar" - {
          "without initializer" in {
            "bool a".asTree[DeclIdent] shouldBe DeclIdent(Ident("a"), TypeUInt(Expr(1)), None)
          }

          "with initializer" in {
            "bool b = true".asTree[DeclIdent] shouldBe {
              DeclIdent(Ident("b"), TypeUInt(Expr(1)), Some(ExprInt(false, 1, 1)))
            }
          }

          "with attribute" in {
            inside("(* foo *) bool b".asTree[DeclIdent]) {
              case DeclIdent(ident @ Ident("b"), TypeUInt(Expr(1)), None) =>
                ident.hasAttr shouldBe true
                ident.attr shouldBe Map("foo" -> Expr(1))
            }
          }
        }

        "array" - {
          "1D" in {
            "i8 c[2]".asTree[DeclIdent] shouldBe {
              DeclIdent(Ident("c"), TypeArray(TypeSInt(Expr(8)), Expr(2)), None)
            }
          }

          "2D" ignore {
            "i8 d[2][3]".asTree[DeclIdent] shouldBe {
              DeclIdent(Ident("d"), TypeArray(TypeArray(TypeSInt(Expr(8)), Expr(3)), Expr(2)), None)
            }
          }

          "2D array of 2D vector" ignore {
            "i2[8] e[5][4]".asTree[DeclIdent] shouldBe {
              DeclIdent(
                Ident("e"),
                TypeArray(TypeArray(TypeVector(TypeSInt(Expr(2)), Expr(8)), Expr(4)), Expr(5)),
                None
              )
            }
          }

          "1D with attribute" in {
            inside("(* foo *) i8 c[2]".asTree[DeclIdent]) {
              case DeclIdent(ident @ Ident("c"), TypeArray(TypeSInt(Expr(8)), Expr(2)), None) =>
                ident.hasAttr shouldBe true
                ident.attr shouldBe Map("foo" -> Expr(1))
            }
          }
        }

        "output" - {
          "no flow control" - {
            "default" in {
              "out i2 a".asTree[DeclIdent] shouldBe {
                DeclIdent(Ident("a"),
                          TypeOut(TypeSInt(Expr(2)), FlowControlTypeNone, StorageTypeDefault),
                          None)
              }
            }

            "wire" in {
              "out wire u2 a".asTree[DeclIdent] shouldBe {
                DeclIdent(Ident("a"),
                          TypeOut(TypeUInt(Expr(2)), FlowControlTypeNone, StorageTypeWire),
                          None)
              }
            }
          }

          "valid flow control" - {
            "default" in {
              "out sync i2 a".asTree[DeclIdent] shouldBe {
                DeclIdent(Ident("a"),
                          TypeOut(TypeSInt(Expr(2)), FlowControlTypeValid, StorageTypeDefault),
                          None)
              }
            }

            "wire" in {
              "out sync wire i2 a".asTree[DeclIdent] shouldBe {
                DeclIdent(Ident("a"),
                          TypeOut(TypeSInt(Expr(2)), FlowControlTypeValid, StorageTypeWire),
                          None)
              }
            }
          }

          "valid/ready flow control" - {
            "default" in {
              "out sync ready i2 a".asTree[DeclIdent] shouldBe {
                DeclIdent(
                  Ident("a"),
                  TypeOut(TypeSInt(Expr(2)), FlowControlTypeReady, StorageTypeDefault),
                  None
                )
              }
            }

            "fslice" in {
              "out sync ready fslice i2 a".asTree[DeclIdent] shouldBe {
                DeclIdent(
                  Ident("a"),
                  TypeOut(TypeSInt(Expr(2)),
                          FlowControlTypeReady,
                          StorageTypeSlices(List(StorageSliceFwd))),
                  None
                )
              }
            }

            "bslice" in {
              "out sync ready bslice i2 a".asTree[DeclIdent] shouldBe {
                DeclIdent(
                  Ident("a"),
                  TypeOut(TypeSInt(Expr(2)),
                          FlowControlTypeReady,
                          StorageTypeSlices(List(StorageSliceBwd))),
                  None
                )
              }
            }

            "bubble" in {
              "out sync ready bubble i2 a".asTree[DeclIdent] shouldBe {
                DeclIdent(
                  Ident("a"),
                  TypeOut(TypeSInt(Expr(2)),
                          FlowControlTypeReady,
                          StorageTypeSlices(List(StorageSliceBub))),
                  None
                )
              }
            }

            "bslice bubble fslice" in {
              "out sync ready bslice bubble fslice i2 a".asTree[DeclIdent] shouldBe {
                DeclIdent(
                  Ident("a"),
                  TypeOut(
                    TypeSInt(Expr(2)),
                    FlowControlTypeReady,
                    StorageTypeSlices(List(StorageSliceBwd, StorageSliceBub, StorageSliceFwd))
                  ),
                  None
                )
              }
            }

            "with attribute" in {
              inside("(* foo *) out i2 a".asTree[DeclIdent]) {
                case DeclIdent(ident @ Ident("a"),
                               TypeOut(TypeSInt(Expr(2)), FlowControlTypeNone, StorageTypeDefault),
                               None) =>
                  ident.hasAttr shouldBe true
                  ident.attr shouldBe Map("foo" -> Expr(1))
              }
            }
          }
        }

        "inputs" - {
          "no flow control" in {
            "in i2 a".asTree[DeclIdent] shouldBe {
              DeclIdent(Ident("a"), TypeIn(TypeSInt(Expr(2)), FlowControlTypeNone), None)
            }
          }

          "valid flow control" in {
            "in sync i2 a".asTree[DeclIdent] shouldBe {
              DeclIdent(Ident("a"), TypeIn(TypeSInt(Expr(2)), FlowControlTypeValid), None)
            }
          }

          "valid/ready flow control" in {
            "in sync ready i2 a".asTree[DeclIdent] shouldBe {
              DeclIdent(Ident("a"), TypeIn(TypeSInt(Expr(2)), FlowControlTypeReady), None)
            }
          }

          "with attribute" in {
            inside("(* foo *) in i2 a".asTree[DeclIdent]) {
              case DeclIdent(ident @ Ident("a"),
                             TypeIn(TypeSInt(Expr(2)), FlowControlTypeNone),
                             None) =>
                ident.hasAttr shouldBe true
                ident.attr shouldBe Map("foo" -> Expr(1))
            }
          }
        }

        "parameter" in {
          "param i2 a = 2".asTree[DeclIdent] shouldBe {
            DeclIdent(Ident("a"), TypeParam(TypeSInt(Expr(2))), Some(Expr(2)))
          }
        }

        "parameter with attribute" in {
          inside("(* foo *) param i2 a = 2".asTree[DeclIdent]) {
            case DeclIdent(ident @ Ident("a"), TypeParam(TypeSInt(Expr(2))), Some(Expr(2))) =>
              ident.hasAttr shouldBe true
              ident.attr shouldBe Map("foo" -> Expr(1))
          }
        }

        "constant" in {
          "const i2 a = 2".asTree[DeclIdent] shouldBe {
            DeclIdent(Ident("a"), TypeConst(TypeSInt(Expr(2))), Some(Expr(2)))
          }
        }

        "constant with attribute" in {
          inside("(* foo *) const i2 a = 2".asTree[DeclIdent]) {
            case DeclIdent(ident @ Ident("a"), TypeConst(TypeSInt(Expr(2))), Some(Expr(2))) =>
              ident.hasAttr shouldBe true
              ident.attr shouldBe Map("foo" -> Expr(1))
          }
        }

        "pipeline variable" in {
          "pipeline u8 a".asTree[DeclIdent] shouldBe {
            DeclIdent(Ident("a"), TypePipeline(TypeUInt(Expr(8))), None)
          }
        }

        "pipeline variable with attribute" in {
          inside("(* foo *) pipeline u8 a".asTree[DeclIdent]) {
            case DeclIdent(ident @ Ident("a"), TypePipeline(TypeUInt(Expr(8))), None) =>
              ident.hasAttr shouldBe true
              ident.attr shouldBe Map("foo" -> Expr(1))
          }
        }

        "sram" in {
          "sram u8 a[10]".asTree[DeclIdent] shouldBe {
            DeclIdent(Ident("a"), TypeSram(TypeUInt(Expr(8)), Expr(10), StorageTypeReg), None)
          }
        }

        "sram with attribute" in {
          inside("(* foo *) sram u8 a[10]".asTree[DeclIdent]) {
            case DeclIdent(ident @ Ident("a"),
                           TypeSram(TypeUInt(Expr(8)), Expr(10), StorageTypeReg),
                           None) =>
              ident.hasAttr shouldBe true
              ident.attr shouldBe Map("foo" -> Expr(1))
          }
        }

        "sram wire" in {
          "sram wire u8 a[10]".asTree[DeclIdent] shouldBe {
            DeclIdent(Ident("a"), TypeSram(TypeUInt(Expr(8)), Expr(10), StorageTypeWire), None)
          }
        }

        "sram wire with attribute" in {
          inside("(* foo *) sram wire u8 a[10]".asTree[DeclIdent]) {
            case DeclIdent(ident @ Ident("a"),
                           TypeSram(TypeUInt(Expr(8)), Expr(10), StorageTypeWire),
                           None) =>
              ident.hasAttr shouldBe true
              ident.attr shouldBe Map("foo" -> Expr(1))
          }
        }

      }

      "types" - {
        "bool" in {
          "bool".asTree[Expr] shouldBe ExprType(TypeUInt(Expr(1)))
        }

        "bool is same as u1" in {
          "bool".asTree[Expr] shouldBe "u1".asTree[Expr]
        }

        "fixed unsigned ints" in {
          forAll(List("u1", "u2", "u3", "u44", "u128")) { str =>
            str.asTree[Expr] shouldBe ExprType(TypeUInt(Expr(str.tail.toInt)))
          }
        }

        "fixed signed ints" in {
          forAll(List("i1", "i2", "i3", "i44", "i128")) { str =>
            str.asTree[Expr] shouldBe ExprType(TypeSInt(Expr(str.tail.toInt)))
          }
        }

        "parametrized integers" - {
          "unsigned" in {
            "uint(N)".asTree[Expr] shouldBe ExprType(TypeUInt(ExprIdent("N")))
          }

          "signed" in {
            "int(N)".asTree[Expr] shouldBe ExprType(TypeSInt(ExprIdent("N")))
          }
        }

        "vectors" - {
          "1D u2" in {
            "u2[8]".asTree[Expr] shouldBe ExprType(TypeVector(TypeUInt(Expr(2)), Expr(8)))
          }

          "2D u2" in {
            "u2[4][8]".asTree[Expr] shouldBe {
              ExprType(TypeVector(TypeVector(TypeUInt(Expr(2)), Expr(8)), Expr(4)))
            }
          }

          "1D i2" in {
            "i2[8]".asTree[Expr] shouldBe ExprType(TypeVector(TypeSInt(Expr(2)), Expr(8)))
          }

          "2D i2" in {
            "i2[4][8]".asTree[Expr] shouldBe {
              ExprType(TypeVector(TypeVector(TypeSInt(Expr(2)), Expr(8)), Expr(4)))
            }
          }

          "1D uint(3)" in {
            "uint(3)[8]".asTree[Expr] shouldBe ExprType(TypeVector(TypeUInt(Expr(3)), Expr(8)))
          }

          "2D uint(3)" in {
            "uint(3)[4][8]".asTree[Expr] shouldBe {
              ExprType(TypeVector(TypeVector(TypeUInt(Expr(3)), Expr(8)), Expr(4)))
            }
          }

          "1D int(3)" in {
            "int(3)[8]".asTree[Expr] shouldBe ExprType(TypeVector(TypeSInt(Expr(3)), Expr(8)))
          }

          "2D int(3)" in {
            "int(3)[4][8]".asTree[Expr] shouldBe {
              ExprType(TypeVector(TypeVector(TypeSInt(Expr(3)), Expr(8)), Expr(4)))
            }
          }

          "1D bool" in {
            "bool[8]".asTree[Expr] shouldBe ExprType(TypeVector(TypeUInt(Expr(1)), Expr(8)))
          }

          "2D bool" in {
            "bool[4][8]".asTree[Expr] shouldBe {
              ExprType(TypeVector(TypeVector(TypeUInt(Expr(1)), Expr(8)), Expr(4)))
            }
          }
        }

        "void" in {
          "void".asTree[Expr] shouldBe ExprType(TypeVoid)
        }

        "unsized int" in {
          "int".asTree[Expr] shouldBe ExprType(TypeNum(true))
        }

        "unsized uint" in {
          "uint".asTree[Expr] shouldBe ExprType(TypeNum(false))
        }
      }

      "entity contents" - {
        "empty" in {
          "fsm a {}".asTree[Entity] shouldBe {
            Entity(Ident("a"), Nil)
          }
        }

        "declaration" in {
          """|network b {
             |  in bool p_in;
             |}""".stripMargin.asTree[Entity] shouldBe {
            Entity(
              Ident("b"),
              List(
                EntDecl(
                  DeclIdent(Ident("p_in"), TypeIn(TypeUInt(Expr(1)), FlowControlTypeNone), None))
              )
            )
          }
        }

        "instance without parameters" in {
          """|network c {
             |  i = new j();
             |}""".stripMargin.asTree[Entity] shouldBe {
            Entity(
              Ident("c"),
              List(EntInstance(Ident("i"), Ident("j"), Nil, Nil))
            )
          }
        }

        "instance with parameters" in {
          """|network d {
             |  i = new j(A=2, B=3);
             |}""".stripMargin.asTree[Entity] shouldBe {
            Entity(
              Ident("d"),
              List(EntInstance(Ident("i"), Ident("j"), List("A", "B"), List(Expr(2), Expr(3))))
            )
          }
        }

        "instance without attribue" in {
          val tree = """|network d2 {
                        |  (* foo *)
                        |  i = new j();
                        |}""".stripMargin.asTree[Entity]
          tree shouldBe {
            Entity(
              Ident("d2"),
              List(EntInstance(Ident("i"), Ident("j"), Nil, Nil))
            )
          }
          val ident = (tree collectFirst { case EntInstance(i: Ident, _, _, _) => i }).value
          ident.hasAttr shouldBe true
          ident.attr shouldBe Map("foo" -> Expr(1))
        }

        "single connection" in {
          """|network e {
                       |  i.a -> j.b;
                       |}""".stripMargin.asTree[Entity] shouldBe {
            Entity(
              Ident("e"),
              List(
                EntConnect(ExprSelect(ExprIdent("i"), "a"), List(ExprSelect(ExprIdent("j"), "b"))))
            )
          }
        }

        "multiple connections" in {
          """|network f {
                       |  i.a -> j.b, k.c;
                       |}""".stripMargin.asTree[Entity] shouldBe {
            Entity(
              Ident("f"),
              List(
                EntConnect(
                  ExprSelect(ExprIdent("i"), "a"),
                  List(ExprSelect(ExprIdent("j"), "b"), ExprSelect(ExprIdent("k"), "c"))
                ))
            )
          }
        }

        "fence block" in {
          """|fsm g {
             |  fence {
             |    a = 1;
             |  }
             |}""".stripMargin.asTree[Entity] shouldBe {
            Entity(
              Ident("g"),
              List(EntCombProcess(List(StmtBlock(List(StmtAssign(ExprIdent("a"), Expr(1)))))))
            )
          }
        }

        "function" in {
          """|fsm g {
             |  void main() {}
             |}""".stripMargin.asTree[Entity] shouldBe {
            Entity(
              Ident("g"),
              List(EntFunction(Ident("main"), Nil))
            )
          }
        }

        "function with attributes" in {
          val tree = """|fsm g {
                        |  (* foo, bar=2, baz = 1 + 2 *)
                        |  void main() {}
                        |}""".stripMargin.asTree[Entity]
          tree shouldBe {
            Entity(
              Ident("g"),
              List(EntFunction(Ident("main"), Nil))
            )
          }
          val ident = (tree collectFirst { case EntFunction(i: Ident, _) => i }).value
          ident.hasAttr shouldBe true
          ident.attr shouldBe {
            Map(
              "foo" -> Expr(1),
              "bar" -> Expr(2),
              "baz" -> ExprBinary(Expr(1), "+", Expr(2))
            )
          }
        }

        "nested fsm without auto instantiation" in {
          """|network  h {
             |  fsm i {}
             |}""".stripMargin.asTree[Entity] shouldBe {
            Entity(
              Ident("h"),
              List(EntEntity(Entity(Ident("i"), Nil)))
            )
          }
        }

        "nested fsm with attribute" in {
          val tree = """|network  h2 {
                        |  (* foo *) fsm i {}
                        |}""".stripMargin.asTree[Entity]
          tree shouldBe {
            Entity(
              Ident("h2"),
              List(EntEntity(Entity(Ident("i"), Nil)))
            )
          }
          val ident = (tree collectFirst { case ident @ Ident("i") => ident }).value
          ident.hasAttr shouldBe true
          ident.attr shouldBe Map("foo" -> Expr(1), "//variant" -> ExprStr("fsm"))
        }

        "nested fsm with auto instantiation" in {
          """|network  h3 {
             |  new fsm i {}
             |}""".stripMargin.asTree[Entity] shouldBe {
            Entity(
              Ident("h3"),
              List(
                EntEntity(Entity(Ident("i"), Nil)),
                EntInstance(Ident("i"), Ident("i"), Nil, Nil)
              )
            )
          }
        }
        "nested fsm with auto instantiation and attributes on instance" in {
          val tree = """|network  h4 {
                        |  (* foo *) new fsm i {}
                        |}""".stripMargin.asTree[Entity]
          tree shouldBe {
            Entity(
              Ident("h4"),
              List(
                EntEntity(Entity(Ident("i"), Nil)),
                EntInstance(Ident("i"), Ident("i"), Nil, Nil)
              )
            )
          }

          val iIdent = tree getFirst {
            case EntInstance(ident: Ident, _, _, _) => ident
          }
          iIdent.hasAttr shouldBe true
          iIdent.attr shouldBe Map("foo" -> Expr(1))

          val eIdent = tree getFirst {
            case Entity(ident @ Ident("i"), _) => ident
          }
          eIdent.hasAttr shouldBe true
          eIdent.attr shouldBe Map("//variant" -> ExprStr("fsm"))
        }

        "nested fsm with auto instantiation and attributes on entity" in {
          val tree = """|network  h4 {
                        |  new (* bar *) fsm i {}
                        |}""".stripMargin.asTree[Entity]
          tree shouldBe {
            Entity(
              Ident("h4"),
              List(
                EntEntity(Entity(Ident("i"), Nil)),
                EntInstance(Ident("i"), Ident("i"), Nil, Nil)
              )
            )
          }

          val iIdent = tree getFirst {
            case EntInstance(ident: Ident, _, _, _) => ident
          }
          iIdent.hasAttr shouldBe false

          val eIdent = tree getFirst {
            case Entity(ident @ Ident("i"), _) => ident
          }
          eIdent.hasAttr shouldBe true
          eIdent.attr shouldBe Map("bar" -> Expr(1), "//variant" -> ExprStr("fsm"))
        }

        "nested fsm with auto instantiation and attributes on both" in {
          val tree = """|network  h4 {
                        |  (* foo *) new (* bar *) fsm i {}
                        |}""".stripMargin.asTree[Entity]
          tree shouldBe {
            Entity(
              Ident("h4"),
              List(
                EntEntity(Entity(Ident("i"), Nil)),
                EntInstance(Ident("i"), Ident("i"), Nil, Nil)
              )
            )
          }

          val iIdent = tree getFirst {
            case EntInstance(ident: Ident, _, _, _) => ident
          }
          iIdent.hasAttr shouldBe true
          iIdent.attr shouldBe Map("foo" -> Expr(1))

          val eIdent = tree getFirst {
            case Entity(ident @ Ident("i"), _) => ident
          }
          eIdent.hasAttr shouldBe true
          eIdent.attr shouldBe Map("bar" -> Expr(1), "//variant" -> ExprStr("fsm"))
        }

        "verbatim verilog" in {
          """|fsm i {
                       |  verbatim verilog {
                       |    +-/* comment */ {{{}}}
                       |  }
                       |}""".stripMargin.asTree[Entity] shouldBe {
            Entity(
              Ident("i"),
              List(EntVerbatim("verilog", "\n    +-/* comment */ {{{}}}\n  "))
            )
          }
        }

        "verbatim other" in {
          """|fsm j {
             |  verbatim other {
             |    +-/* comment */ {{{}}}
             |  }
             |}""".stripMargin.asTree[Entity] shouldBe {
            Entity(
              Ident("j"),
              List(EntVerbatim("other", "\n    +-/* comment */ {{{}}}\n  "))
            )
          }
        }

        "multiple verbatim" in {
          """|fsm k {
                       |  verbatim verilog {
                       |    first
                       |  }
                       |
                       |  verbatim verilog {
                       |second
                       |  }
                       |}""".stripMargin.asTree[Entity] shouldBe {
            Entity(
              Ident("k"),
              List(
                EntVerbatim("verilog", "\n    first\n  "),
                EntVerbatim("verilog", "\nsecond\n  ")
              )
            )
          }
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

      "statements" - {
        "block as statement" in {
          "{}".asTree[Stmt] shouldBe StmtBlock(Nil)
        }

        "branching" - {
          "if without else, without brace" in {
            "if (1) a;".asTree[Stmt] shouldBe StmtIf(Expr(1), List(StmtExpr(ExprIdent("a"))), Nil)
          }

          "if with else, without brace" in {
            "if (1) fence; else return;".asTree[Stmt] shouldBe StmtIf(Expr(1),
                                                                      List(StmtFence()),
                                                                      List(StmtReturn()))
          }

          "if without else, with brace" in {
            "if (1) {a;}".asTree[Stmt] shouldBe StmtIf(Expr(1), List(StmtExpr(ExprIdent("a"))), Nil)
          }

          "if with else, with brace" in {
            "if (1) {fence;} else {return;}".asTree[Stmt] shouldBe {
              StmtIf(
                Expr(1),
                List(StmtFence()),
                List(StmtReturn())
              )
            }
          }

          "case without default" in {
            """|case (1) {
               | 1: a;
               | 2: b;
               |}
               |""".stripMargin.asTree[Stmt] shouldBe {
              StmtCase(
                Expr(1),
                List(
                  CaseRegular(List(Expr(1)), List(StmtExpr(ExprIdent("a")))),
                  CaseRegular(List(Expr(2)), List(StmtExpr(ExprIdent("b"))))
                )
              )
            }
          }

          "case with default" in {
            """|case (1) {
               | default: c;
               |}
               |""".stripMargin.asTree[Stmt] shouldBe {
              StmtCase(
                Expr(1),
                List(
                  CaseDefault(List(StmtExpr(ExprIdent("c"))))
                )
              )
            }
          }

          "case with multiple labels" in {
            """|case (1) {
               | 1: c;
               | 2, 3: d;
               |}
               |""".stripMargin.asTree[Stmt] shouldBe {
              StmtCase(
                Expr(1),
                List(
                  CaseRegular(List(Expr(1)), List(StmtExpr(ExprIdent("c")))),
                  CaseRegular(List(Expr(2), Expr(3)), List(StmtExpr(ExprIdent("d"))))
                )
              )
            }
          }

          "case with multiple defaults" in {
            """|case (1) {
               | default: c;
               | default: d;
               |}
               |""".stripMargin.asTree[Stmt] shouldBe {
              StmtCase(
                Expr(1),
                List(
                  CaseDefault(List(StmtExpr(ExprIdent("c")))),
                  CaseDefault(List(StmtExpr(ExprIdent("d"))))
                )
              )
            }
          }

          "case ordering" in {
            """|case (1) {
               | 1: a;
               | default: b;
               | 3: c;
               | default: d;
               |}
               |""".stripMargin.asTree[Stmt] shouldBe {
              StmtCase(
                Expr(1),
                List(
                  CaseRegular(List(Expr(1)), List(StmtExpr(ExprIdent("a")))),
                  CaseDefault(List(StmtExpr(ExprIdent("b")))),
                  CaseRegular(List(Expr(3)), List(StmtExpr(ExprIdent("c")))),
                  CaseDefault(List(StmtExpr(ExprIdent("d"))))
                )
              )
            }
          }

          "case without braces" in {
            """|case (1) {
               | 1: a;
               | default: c;
               |}
               |""".stripMargin.asTree[Stmt] shouldBe {
              StmtCase(
                Expr(1),
                List(
                  CaseRegular(List(Expr(1)), List(StmtExpr(ExprIdent("a")))),
                  CaseDefault(List(StmtExpr(ExprIdent("c"))))
                )
              )
            }
          }

          "case with braces" in {
            """|case (1) {
               | 1: {a;}
               | default: {c;}
               |}
               |""".stripMargin.asTree[Stmt] shouldBe {
              StmtCase(
                Expr(1),
                List(
                  CaseRegular(List(Expr(1)), List(StmtExpr(ExprIdent("a")))),
                  CaseDefault(List(StmtExpr(ExprIdent("c"))))
                )
              )
            }
          }
        }

        "loops" - {
          "loop" in {
            """|loop {
               |  1;
               |}""".stripMargin.asTree[Stmt] shouldBe StmtLoop(List(StmtExpr(Expr(1))))
          }

          "while" in {
            """|while (a) {
               |  fence;
               |}""".stripMargin.asTree[Stmt] shouldBe StmtWhile(ExprIdent("a"), List(StmtFence()))
          }

          "do" in {
            """|do {
               | fence;
               |} while(b);""".stripMargin.asTree[Stmt] shouldBe StmtDo(ExprIdent("b"), List(StmtFence()))
          }

          "for" - {
            "empty" in {
              "for(;;){}".asTree[Stmt] shouldBe StmtFor(Nil, None, Nil, Nil)
            }

            "with single init assign" in {
              """|for (a=2;a;a--) {
                 |  2;
                 |}""".stripMargin.asTree[Stmt] shouldBe {
                StmtFor(
                  List(StmtAssign(ExprIdent("a"), Expr(2))),
                  Some(ExprIdent("a")),
                  List(StmtPost(ExprIdent("a"), "--")),
                  List(StmtExpr(Expr(2)))
                )

              }
            }

            "with single init decl" in {
              """|for (i8 a=2;a;a--) {
                 |  2;
                 |}""".stripMargin.asTree[Stmt] shouldBe {
                StmtFor(
                  List(StmtDecl(DeclIdent(Ident("a"), TypeSInt(Expr(8)), Some(Expr(2))))),
                  Some(ExprIdent("a")),
                  List(StmtPost(ExprIdent("a"), "--")),
                  List(StmtExpr(Expr(2)))
                )
              }
            }

            "with multiple init" in {
              """|for (i8 a=2, b=1;;) {
                 |}""".stripMargin.asTree[Stmt] shouldBe {
                StmtFor(
                  List(
                    StmtDecl(DeclIdent(Ident("a"), TypeSInt(Expr(8)), Some(Expr(2)))),
                    StmtAssign(ExprIdent("b"), Expr(1))
                  ),
                  None,
                  Nil,
                  Nil
                )
              }
            }

            "with multiple step" in {
              """|for (;;a++, b--) {
                 |}""".stripMargin.asTree[Stmt] shouldBe {
                StmtFor(
                  Nil,
                  None,
                  List(
                    StmtPost(ExprIdent("a"), "++"),
                    StmtPost(ExprIdent("b"), "--")
                  ),
                  Nil
                )
              }
            }
          }
        }

        "let" - {
          "single assignment" in {
            "let (a=2) loop {}".asTree[Stmt] shouldBe {
              StmtLet(List(StmtAssign(ExprIdent("a"), Expr(2))), List(StmtLoop(Nil)))
            }
          }

          "single declaration" in {
            "let (i2 a=1) loop {}".asTree[Stmt] shouldBe {
              StmtLet(List(StmtDecl(DeclIdent(Ident("a"), TypeSInt(Expr(2)), Some(Expr(1))))),
                      List(StmtLoop(Nil)))
            }
          }

          "multiple initializers" in {
            "let (i2 a=b, c=a) loop {}".asTree[Stmt] shouldBe {
              StmtLet(
                List(
                  StmtDecl(DeclIdent(Ident("a"), TypeSInt(Expr(2)), Some(ExprIdent("b")))),
                  StmtAssign(ExprIdent("c"), ExprIdent("a"))
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

        "return" in {
          "return;".asTree[Stmt] shouldBe StmtReturn()
        }

        "goto" in {
          "goto foo;".asTree[Stmt] shouldBe StmtGoto(ExprIdent("foo"))
        }

        // TODO: assignments
        "assignments" - {
          "simple" in {
            "a = 1;".asTree[Stmt] shouldBe StmtAssign(ExprIdent("a"), Expr(1))
          }

          "update +=" in {
            "b += 2;".asTree[Stmt] shouldBe StmtUpdate(ExprIdent("b"), "+", Expr(2))
          }

          "update <<=" in {
            "c <<= 3;".asTree[Stmt] shouldBe StmtUpdate(ExprIdent("c"), "<<", Expr(3))
          }

          "postfix ++" in {
            "d++;".asTree[Stmt] shouldBe StmtPost(ExprIdent("d"), "++")
          }

          "postfix --" in {
            "e--;".asTree[Stmt] shouldBe StmtPost(ExprIdent("e"), "--")
          }
        }

        "expressions in statement position" - {
          "identifier" in {
            "a;".asTree[Stmt] shouldBe StmtExpr(ExprIdent("a"))
          }

          "call" in {
            "b();".asTree[Stmt] shouldBe StmtExpr(ExprCall(ExprIdent("b"), Nil))
          }
        }

        "declaration statements" - {
          "scalar without initializer" in {
            "u2 a;".asTree[Stmt] shouldBe StmtDecl(DeclIdent(Ident("a"), TypeUInt(Expr(2)), None))
          }

          "scalar with initializer" in {
            "i2 b = 3;".asTree[Stmt] shouldBe {
              StmtDecl(DeclIdent(Ident("b"), TypeSInt(Expr(2)), Some(Expr(3))))
            }
          }
        }

        "read statement" in {
          "read;".asTree[Stmt] shouldBe StmtRead()
        }

        "write statement" in {
          "write;".asTree[Stmt] shouldBe StmtWrite()
        }

      }

      "expressions" - {

        "literals" - {
          "string" in {
            "\"foo\"".asTree[Expr] shouldBe ExprStr("foo")
          }

          "true" in {
            "true".asTree[Expr] shouldBe ExprInt(false, 1, 1)
          }

          "false" in {
            "false".asTree[Expr] shouldBe ExprInt(false, 1, 0)
          }

          "unsized integers" - {
            for {
              (literal, result, msg) <- List(
                ("      17 ", ExprNum(false, 17), ""),
                ("     +17 ", ExprNum(false, 17), ""),
                ("     -17 ", ExprError(), "Negative unsigned literal"),
                ("      17u", ExprNum(false, 17), ""),
                ("     +17u", ExprNum(false, 17), ""),
                ("     -17u", ExprError(), "Negative unsigned literal"),
                ("      17s", ExprNum(true, 17), ""),
                ("     +17s", ExprNum(true, 17), ""),
                ("     -17s", ExprNum(true, -17), ""),
                (" 0b10001 ", ExprNum(false, 17), ""),
                ("+0b10001 ", ExprNum(false, 17), ""),
                ("-0b10001 ", ExprError(), "Negative unsigned literal"),
                (" 0b10001u", ExprNum(false, 17), ""),
                ("+0b10001u", ExprNum(false, 17), ""),
                ("-0b10001u", ExprError(), "Negative unsigned literal"),
                (" 0b10001s", ExprNum(true, 17), ""),
                ("+0b10001s", ExprNum(true, 17), ""),
                ("-0b10001s", ExprNum(true, -17), ""),
                ("    0o21 ", ExprNum(false, 17), ""),
                ("   +0o21 ", ExprNum(false, 17), ""),
                ("   -0o21 ", ExprError(), "Negative unsigned literal"),
                ("    0o21u", ExprNum(false, 17), ""),
                ("   +0o21u", ExprNum(false, 17), ""),
                ("   -0o21u", ExprError(), "Negative unsigned literal"),
                ("    0o21s", ExprNum(true, 17), ""),
                ("   +0o21s", ExprNum(true, 17), ""),
                ("   -0o21s", ExprNum(true, -17), ""),
                ("    0d17 ", ExprNum(false, 17), ""),
                ("   +0d17 ", ExprNum(false, 17), ""),
                ("   -0d17 ", ExprError(), "Negative unsigned literal"),
                ("    0d17u", ExprNum(false, 17), ""),
                ("   +0d17u", ExprNum(false, 17), ""),
                ("   -0d17u", ExprError(), "Negative unsigned literal"),
                ("    0d17s", ExprNum(true, 17), ""),
                ("   +0d17s", ExprNum(true, 17), ""),
                ("   -0d17s", ExprNum(true, -17), ""),
                ("   0d017 ", ExprNum(false, 17), ""),
                ("  +0d017 ", ExprNum(false, 17), ""),
                ("  -0d017 ", ExprError(), "Negative unsigned literal"),
                ("   0d017u", ExprNum(false, 17), ""),
                ("  +0d017u", ExprNum(false, 17), ""),
                ("  -0d017u", ExprError(), "Negative unsigned literal"),
                ("   0d017s", ExprNum(true, 17), ""),
                ("  +0d017s", ExprNum(true, 17), ""),
                ("  -0d017s", ExprNum(true, -17), ""),
                ("    0x11 ", ExprNum(false, 17), ""),
                ("   +0x11 ", ExprNum(false, 17), ""),
                ("   -0x11 ", ExprError(), "Negative unsigned literal"),
                ("    0x11u", ExprNum(false, 17), ""),
                ("   +0x11u", ExprNum(false, 17), ""),
                ("   -0x11u", ExprError(), "Negative unsigned literal"),
                ("    0x11s", ExprNum(true, 17), ""),
                ("   +0x11s", ExprNum(true, 17), ""),
                ("   -0x11s", ExprNum(true, -17), ""),
                ("       0 ", ExprNum(false, 0), ""),
                ("      +0 ", ExprNum(false, 0), ""),
                ("      -0 ", ExprNum(false, 0), ""),
                ("       0u", ExprNum(false, 0), ""),
                ("      +0u", ExprNum(false, 0), ""),
                ("      -0u", ExprNum(false, 0), ""),
                ("       0s", ExprNum(true, 0), ""),
                ("      +0s", ExprNum(true, 0), ""),
                ("      -0s", ExprNum(true, 0), ""),
                // Whitespace after sign
                ("     + 17 ", ExprNum(false, 17), ""),
                ("     - 17 ", ExprError(), "Negative unsigned literal"),
                ("     + 17u", ExprNum(false, 17), ""),
                ("     - 17u", ExprError(), "Negative unsigned literal"),
                ("     + 17s", ExprNum(true, 17), ""),
                ("     - 17s", ExprNum(true, -17), ""),
                ("+ 0b10001 ", ExprNum(false, 17), ""),
                ("- 0b10001 ", ExprError(), "Negative unsigned literal"),
                ("+ 0b10001u", ExprNum(false, 17), ""),
                ("- 0b10001u", ExprError(), "Negative unsigned literal"),
                ("+ 0b10001s", ExprNum(true, 17), ""),
                ("- 0b10001s", ExprNum(true, -17), ""),
                ("   + 0x11 ", ExprNum(false, 17), ""),
                ("   - 0x11 ", ExprError(), "Negative unsigned literal"),
                ("   + 0x11u", ExprNum(false, 17), ""),
                ("   - 0x11u", ExprError(), "Negative unsigned literal"),
                ("   + 0x11s", ExprNum(true, 17), ""),
                ("   - 0x11s", ExprNum(true, -17), ""),
                ("      + 0 ", ExprNum(false, 0), ""),
                ("      - 0 ", ExprNum(false, 0), ""),
                ("      + 0u", ExprNum(false, 0), ""),
                ("      - 0u", ExprNum(false, 0), ""),
                ("      + 0s", ExprNum(true, 0), ""),
                ("      - 0s", ExprNum(true, 0), ""),
                // Underscores
                ("      1_7 ", ExprNum(false, 17), ""),
                ("     +1_7 ", ExprNum(false, 17), ""),
                ("     -1_7 ", ExprError(), "Negative unsigned literal"),
                ("      1_7u", ExprNum(false, 17), ""),
                ("     +1_7u", ExprNum(false, 17), ""),
                ("     -1_7u", ExprError(), "Negative unsigned literal"),
                ("      1_7s", ExprNum(true, 17), ""),
                ("     +1_7s", ExprNum(true, 17), ""),
                ("     -1_7s", ExprNum(true, -17), ""),
                (" 0b1_0_0_0_1 ", ExprNum(false, 17), ""),
                ("+0b1_0_0_0_1 ", ExprNum(false, 17), ""),
                ("-0b1_0_0_0_1 ", ExprError(), "Negative unsigned literal"),
                (" 0b1_0_0_0_1u", ExprNum(false, 17), ""),
                ("+0b1_0_0_0_1u", ExprNum(false, 17), ""),
                ("-0b1_0_0_0_1u", ExprError(), "Negative unsigned literal"),
                (" 0b1_0_0_0_1s", ExprNum(true, 17), ""),
                ("+0b1_0_0_0_1s", ExprNum(true, 17), ""),
                ("-0b1_0_0_0_1s", ExprNum(true, -17), ""),
                ("    0x1_1 ", ExprNum(false, 17), ""),
                ("   +0x1_1 ", ExprNum(false, 17), ""),
                ("   -0x1_1 ", ExprError(), "Negative unsigned literal"),
                ("    0x1_1u", ExprNum(false, 17), ""),
                ("   +0x1_1u", ExprNum(false, 17), ""),
                ("   -0x1_1u", ExprError(), "Negative unsigned literal"),
                ("    0x1_1s", ExprNum(true, 17), ""),
                ("   +0x1_1s", ExprNum(true, 17), ""),
                ("   -0x1_1s", ExprNum(true, -17), ""),
                // Malformed cases
                ("     0b2 ", ExprError(), "Invalid digit for base 2 value"),
                ("     0o8 ", ExprError(), "Invalid digit for base 8 value"),
                ("     0da ", ExprError(), "Invalid digit for base 10 value"),
                ("     017 ",
                 ExprError(),
                 "Invalid literal '017',\nuse prefix '0o' for octal or '0d' for decimal with leading zeros")
              )
            } {
              literal in {
                literal.asTree[Expr] shouldBe result
                if (msg.nonEmpty) {
                  cc.messages.loneElement should beThe[Error]((msg split '\n').toSeq: _*)
                } else {
                  cc.messages shouldBe empty
                }
              }
            }
          }

          "sized integers" - {
            for {
              (literal, result, msg) <- List(
                // format: off
                (" 4'd3     ", ExprInt(false, 4, 3), ""),
                (" 4'sd3    ", ExprInt(true, 4, 3), ""),
                ("+4'd3     ", ExprInt(false, 4, 3), ""),
                ("+4'sd3    ", ExprInt(true, 4, 3), ""),
                ("-4'd3     ", ExprError(), "Negative unsigned literal"),
                ("-4'sd3    ", ExprInt(true, 4, -3), ""),
                (" 4'd0     ", ExprInt(false, 4, 0), ""),
                (" 4'sd0    ", ExprInt(true, 4, 0), ""),
                ("+4'd0     ", ExprInt(false, 4, 0), ""),
                ("+4'sd0    ", ExprInt(true, 4, 0), ""),
                ("-4'd0     ", ExprInt(false, 4, 0), ""),
                ("-4'sd0    ", ExprInt(true, 4, 0), ""),
                (" 4'd1     ", ExprInt(false, 4, 1), ""),
                (" 4'sd1    ", ExprInt(true, 4, 1), ""),
                ("+4'd1     ", ExprInt(false, 4, 1), ""),
                ("+4'sd1    ", ExprInt(true, 4, 1), ""),
                ("-4'd1     ", ExprError(), "Negative unsigned literal"),
                ("-4'sd1    ", ExprInt(true, 4, -1), ""),
                (" 4'd15    ", ExprInt(false, 4, 15), ""),
                (" 4'sd15   ", ExprInt(true, 4, -1), "Apparently positive literal stands for negative value -1"),
                ("+4'd15    ", ExprInt(false, 4, 15), ""),
                ("+4'sd15   ", ExprInt(true, 4, -1), "Apparently positive literal stands for negative value -1"),
                ("-4'd15    ", ExprError(), "Negative unsigned literal"),
                ("-4'sd15   ", ExprInt(true, 4, 1), "Apparently negative literal stands for positive value 1"),
                (" 4'd7     ", ExprInt(false, 4, 7), ""),
                (" 4'sd7    ", ExprInt(true, 4, 7), ""),
                ("+4'd7     ", ExprInt(false, 4, 7), ""),
                ("+4'sd7    ", ExprInt(true, 4, 7), ""),
                ("-4'd7     ", ExprError(), "Negative unsigned literal"),
                ("-4'sd7    ", ExprInt(true, 4, -7), ""),
                (" 4'd8     ", ExprInt(false, 4, 8), ""),
                (" 4'sd8    ", ExprInt(true, 4, -8), "Apparently positive literal stands for negative value -8"),
                ("+4'd8     ", ExprInt(false, 4, 8), ""),
                ("+4'sd8    ", ExprInt(true, 4, -8), "Apparently positive literal stands for negative value -8"),
                ("-4'd8     ", ExprError(), "Negative unsigned literal"),
                ("-4'sd8    ", ExprInt(true, 4, -8), ""),
                (" 4'd16    ", ExprError(), "Value specifier for 4 bit literal requires 5 bits"),
                (" 4'sd16   ", ExprError(), "Value specifier for 4 bit literal requires 5 bits"),
                ("+4'd16    ", ExprError(), "Value specifier for 4 bit literal requires 5 bits"),
                ("+4'sd16   ", ExprError(), "Value specifier for 4 bit literal requires 5 bits"),
                ("-4'd16    ", ExprError(), "Value specifier for 4 bit literal requires 5 bits"),
                ("-4'sd16   ", ExprError(), "Value specifier for 4 bit literal requires 5 bits"),
                (" 4'b11    ", ExprInt(false, 4, 3), ""),
                (" 4'sb11   ", ExprInt(true, 4, 3), ""),
                ("+4'b11    ", ExprInt(false, 4, 3), ""),
                ("+4'sb11   ", ExprInt(true, 4, 3), ""),
                ("-4'b11    ", ExprError(), "Negative unsigned literal"),
                ("-4'sb11   ", ExprInt(true, 4, -3), ""),
                (" 4'b0     ", ExprInt(false, 4, 0), ""),
                (" 4'sb0    ", ExprInt(true, 4, 0), ""),
                ("+4'b0     ", ExprInt(false, 4, 0), ""),
                ("+4'sb0    ", ExprInt(true, 4, 0), ""),
                ("-4'b0     ", ExprInt(false, 4, 0), ""),
                ("-4'sb0    ", ExprInt(true, 4, 0), ""),
                (" 4'b1     ", ExprInt(false, 4, 1), ""),
                (" 4'sb1    ", ExprInt(true, 4, 1), ""),
                ("+4'b1     ", ExprInt(false, 4, 1), ""),
                ("+4'sb1    ", ExprInt(true, 4, 1), ""),
                ("-4'b1     ", ExprError(), "Negative unsigned literal"),
                ("-4'sb1    ", ExprInt(true, 4, -1), ""),
                (" 4'b1111  ", ExprInt(false, 4, 15), ""),
                (" 4'sb1111 ", ExprInt(true, 4, -1), "Apparently positive literal stands for negative value -1"),
                ("+4'b1111  ", ExprInt(false, 4, 15), ""),
                ("+4'sb1111 ", ExprInt(true, 4, -1), "Apparently positive literal stands for negative value -1"),
                ("-4'b1111  ", ExprError(), "Negative unsigned literal"),
                ("-4'sb1111 ", ExprInt(true, 4, 1), "Apparently negative literal stands for positive value 1"),
                (" 4'b111   ", ExprInt(false, 4, 7), ""),
                (" 4'sb111  ", ExprInt(true, 4, 7), ""),
                ("+4'b111   ", ExprInt(false, 4, 7), ""),
                ("+4'sb111  ", ExprInt(true, 4, 7), ""),
                ("-4'b111   ", ExprError(), "Negative unsigned literal"),
                ("-4'sb111  ", ExprInt(true, 4, -7), ""),
                (" 4'b1000  ", ExprInt(false, 4, 8), ""),
                (" 4'sb1000 ", ExprInt(true, 4, -8), "Apparently positive literal stands for negative value -8"),
                ("+4'b1000  ", ExprInt(false, 4, 8), ""),
                ("+4'sb1000 ", ExprInt(true, 4, -8), "Apparently positive literal stands for negative value -8"),
                ("-4'b1000  ", ExprError(), "Negative unsigned literal"),
                ("-4'sb1000 ", ExprInt(true, 4, -8), ""),
                (" 4'b10000 ", ExprError(), "Value specifier for 4 bit literal requires 5 bits"),
                (" 4'sb10000", ExprError(), "Value specifier for 4 bit literal requires 5 bits"),
                ("+4'b10000 ", ExprError(), "Value specifier for 4 bit literal requires 5 bits"),
                ("+4'sb10000", ExprError(), "Value specifier for 4 bit literal requires 5 bits"),
                ("-4'b10000 ", ExprError(), "Value specifier for 4 bit literal requires 5 bits"),
                ("-4'sb10000", ExprError(), "Value specifier for 4 bit literal requires 5 bits"),
                (" 4'h3     ", ExprInt(false, 4, 3), ""),
                (" 4'sh3    ", ExprInt(true, 4, 3), ""),
                ("+4'h3     ", ExprInt(false, 4, 3), ""),
                ("+4'sh3    ", ExprInt(true, 4, 3), ""),
                ("-4'h3     ", ExprError(), "Negative unsigned literal"),
                ("-4'sh3    ", ExprInt(true, 4, -3), ""),
                (" 4'h0     ", ExprInt(false, 4, 0), ""),
                (" 4'sh0    ", ExprInt(true, 4, 0), ""),
                ("+4'h0     ", ExprInt(false, 4, 0), ""),
                ("+4'sh0    ", ExprInt(true, 4, 0), ""),
                ("-4'h0     ", ExprInt(false, 4, 0), ""),
                ("-4'sh0    ", ExprInt(true, 4, 0), ""),
                (" 4'h1     ", ExprInt(false, 4, 1), ""),
                (" 4'sh1    ", ExprInt(true, 4, 1), ""),
                ("+4'h1     ", ExprInt(false, 4, 1), ""),
                ("+4'sh1    ", ExprInt(true, 4, 1), ""),
                ("-4'h1     ", ExprError(), "Negative unsigned literal"),
                ("-4'sh1    ", ExprInt(true, 4, -1), ""),
                (" 4'hf     ", ExprInt(false, 4, 15), ""),
                (" 4'shf    ", ExprInt(true, 4, -1), "Apparently positive literal stands for negative value -1"),
                ("+4'hf     ", ExprInt(false, 4, 15), ""),
                ("+4'shf    ", ExprInt(true, 4, -1), "Apparently positive literal stands for negative value -1"),
                ("-4'hf     ", ExprError(), "Negative unsigned literal"),
                ("-4'shf    ", ExprInt(true, 4, 1), "Apparently negative literal stands for positive value 1"),
                (" 4'h7     ", ExprInt(false, 4, 7), ""),
                (" 4'sh7    ", ExprInt(true, 4, 7), ""),
                ("+4'h7     ", ExprInt(false, 4, 7), ""),
                ("+4'sh7    ", ExprInt(true, 4, 7), ""),
                ("-4'h7     ", ExprError(), "Negative unsigned literal"),
                ("-4'sh7    ", ExprInt(true, 4, -7), ""),
                (" 4'h8     ", ExprInt(false, 4, 8), ""),
                (" 4'sh8    ", ExprInt(true, 4, -8), "Apparently positive literal stands for negative value -8"),
                ("+4'h8     ", ExprInt(false, 4, 8), ""),
                ("+4'sh8    ", ExprInt(true, 4, -8), "Apparently positive literal stands for negative value -8"),
                ("-4'h8     ", ExprError(), "Negative unsigned literal"),
                ("-4'sh8    ", ExprInt(true, 4, -8), ""),
                (" 4'h10    ", ExprError(), "Value specifier for 4 bit literal requires 5 bits"),
                (" 4'sh10   ", ExprError(), "Value specifier for 4 bit literal requires 5 bits"),
                ("+4'sh10   ", ExprError(), "Value specifier for 4 bit literal requires 5 bits"),
                ("+4'h10    ", ExprError(), "Value specifier for 4 bit literal requires 5 bits"),
                ("-4'h10    ", ExprError(), "Value specifier for 4 bit literal requires 5 bits"),
                ("-4'sh10   ", ExprError(), "Value specifier for 4 bit literal requires 5 bits"),
                // Malformed cases
                (" 4'b2     ", ExprError(), "Invalid digit for base 2 value"),
                (" 4'da     ", ExprError(), "Invalid digit for base 10 value"),
                (" 0'b0     ", ExprError(), "0 width integer literal"),
                (" 0'd0     ", ExprError(), "0 width integer literal"),
                (" 0'h0     ", ExprError(), "0 width integer literal")
                // format: on
              )
            } {
              literal in {
                literal.asTree[Expr] shouldBe result
                if (msg.nonEmpty) {
                  result match {
                    case _: ExprError => cc.messages.loneElement should beThe[Error](msg)
                    case _            => cc.messages.loneElement should beThe[Warning](msg)
                  }
                } else {
                  cc.messages shouldBe empty
                }
              }

            }
          }

        }

        "simple" - {

          "bracket" in {
            "(((1)))".asTree[Expr] shouldBe Expr(1)
          }

          "call with no arguments" in {
            "a()".asTree[Expr] shouldBe ExprCall(ExprIdent("a"), Nil)
          }

          "call with 1 argument" in {
            "b(2)".asTree[Expr] shouldBe ExprCall(ExprIdent("b"), List(Expr(2)))
          }

          "call with 2 arguments" in {
            "c(d, e)".asTree[Expr] shouldBe {
              ExprCall(ExprIdent("c"), List(ExprIdent("d"), ExprIdent("e")))
            }
          }

          for (op <- List("+", "-", "~", "!", "&", "|", "^", "'")) {
            s"unary ${op}" in {
              s"${op}(2)".asTree[Expr] shouldBe ExprUnary(op, Expr(2))
            }
          }

          for (op <- List("*",
                          "/",
                          "%",
                          "+",
                          "-",
                          "<<",
                          ">>",
                          ">>>",
                          "<<<",
                          ">",
                          ">=",
                          "<",
                          "<=",
                          "==",
                          "!=",
                          "&",
                          "^",
                          "|",
                          "&&",
                          "||")) {
            s"binary $op" in {
              s"4 ${op} 3".asTree[Expr] shouldBe ExprBinary(Expr(4), op, Expr(3))
            }
          }

          "ternary" in {
            "1 ? 2 : 3".asTree[Expr] shouldBe ExprTernary(Expr(1), Expr(2), Expr(3))
          }

          "repetition" in {
            "{N{a}}".asTree[Expr] shouldBe ExprRep(ExprIdent("N"), ExprIdent("a"))
          }

          "concatenation" in {
            "{0, 1, 2}".asTree[Expr] shouldBe ExprCat(List(Expr(0), Expr(1), Expr(2)))
          }

          "multiple concatenation " in {
            "{N{a, b}}".asTree[Expr] shouldBe {
              ExprRep(ExprIdent("N"), ExprCat(List(ExprIdent("a"), ExprIdent("b"))))
            }
          }

          "index 1x" in {
            "a[0]".asTree[Expr] shouldBe ExprIndex(ExprIdent("a"), Expr(0))
          }

          "index 2x" in {
            "a[0][2]".asTree[Expr] shouldBe {
              ExprIndex(ExprIndex(ExprIdent("a"), Expr(0)), Expr(2))
            }
          }

          "slice 1x" in {
            "b[1:0]".asTree[Expr] shouldBe ExprSlice(ExprIdent("b"), Expr(1), ":", Expr(0))
          }

          "slice 2x" in {
            "b[2+:0][1-:1]".asTree[Expr] should matchPattern {
              case ExprSlice(ExprSlice(ExprIdent("b"), Expr(2), "+:", Expr(0)),
                             Expr(1),
                             "-:",
                             Expr(1)) =>
            }
          }

          "select 1x" in {
            "a.b".asTree[Expr] shouldBe ExprSelect(ExprIdent("a"), "b")
          }

          "select 2x" in {
            "a.b.c".asTree[Expr] shouldBe ExprSelect(ExprSelect(ExprIdent("a"), "b"), "c")
          }

          "@id" in {
            "@zx".asTree[Expr] shouldBe ExprIdent("@zx")
          }

          "$id" in {
            "$clog2".asTree[Expr] shouldBe ExprIdent("$clog2")
          }

          "@ call" in {
            "@zx(0, a)".asTree[Expr] shouldBe {
              ExprCall(ExprIdent("@zx"), List(Expr(0), ExprIdent("a")))
            }
          }

          "$ call" in {
            "$clog2(a)".asTree[Expr] shouldBe {
              ExprCall(ExprIdent("$clog2"), List(ExprIdent("a")))
            }
          }

          "identifier" in {
            "foo".asTree[Expr] shouldBe ExprIdent("foo")
          }

          "type" in {
            "i8".asTree[Expr] shouldBe ExprType(TypeSInt(Expr(8)))
          }
        }

        "honouring precedence" - {
          "1 + 2 * 3" in {
            "1 + 2 * 3".asTree[Expr] shouldBe {
              Expr(1) + ExprBinary(Expr(2), "*", Expr(3))
            }
          }
          "1 + 2 + 3" in {
            "1 + 2 + 3".asTree[Expr] shouldBe {
              ExprBinary(Expr(1), "+", Expr(2)) + Expr(3)
            }
          }

          "a.b && a.c" in {
            "a.b && a.c".asTree[Expr] shouldBe {
              ExprSelect(ExprIdent("a"), "b") && ExprSelect(ExprIdent("a"), "c")
            }
          }

          "a.b && a.c == 1" in {
            "a.b && a.c == 1".asTree[Expr] shouldBe {
              ExprSelect(ExprIdent("a"), "b") &&
              ExprBinary(ExprSelect(ExprIdent("a"), "c"), "==", Expr(1))
            }
          }

          "a.b && a[0]" in {
            "a.b && a[0]".asTree[Expr] shouldBe {
              ExprSelect(ExprIdent("a"), "b") && ExprIndex(ExprIdent("a"), 0)
            }
          }

          "a.b && a[1:0]" in {
            "a.b && a[1:0]".asTree[Expr] shouldBe {
              ExprSelect(ExprIdent("a"), "b") && ExprSlice(ExprIdent("a"), 1, ":", 0)
            }
          }

          "a.b[1]" in {
            "a.b[1]".asTree[Expr] shouldBe {
              ExprIndex(ExprSelect(ExprIdent("a"), "b"), 1)
            }
          }
          // TODO: complete all precedence checks
        }

        "honouring associativity" - {
          for {
            (expr, equiv) <- List(
              ("a()()", "(a())()"),
              ("a[0][0]", "(a[0])[0]"),
              ("a[1:0][1:0]", "(a[1:0])[1:0]"),
              ("a.b.c", "(a.b).c"),
              ("+ + (a)", "+ (+ (a))"),
              ("- - (a)", "- (- (a))"),
              ("~ ~ (a)", "~ (~ (a))"),
              ("! ! (a)", "! (! (a))"),
              ("& & (a)", "& (& (a))"),
              ("| | (a)", "| (| (a))"),
              ("^ ^ (a)", "^ (^ (a))"),
              ("a * b * c", "(a * b) * c"),
              ("a / b / c", "(a / b) / c"),
              ("a % b % c", "(a % b) % c"),
              ("a + b + c", "(a + b) + c"),
              ("a - b - c", "(a - b) - c"),
              ("a << b << c", "(a << b) << c"),
              ("a >> b >> c", "(a >> b) >> c"),
              ("a <<< b <<< c", "(a <<< b) <<< c"),
              ("a >>> b >>> c", "(a >>> b) >>> c"),
              ("a > b > c", "(a > b) > c"),
              ("a >= b >= c", "(a >= b) >= c"),
              ("a < b < c", "(a < b) < c"),
              ("a <= b <= c", "(a <= b) <= c"),
              ("a == b == c", "(a == b) == c"),
              ("a != b != c", "(a != b) != c"),
              ("a & b & c", "(a & b) & c"),
              ("a ^ b ^ c", "(a ^ b) ^ c"),
              ("a | b | c", "(a | b) | c"),
              ("a && b && c", "(a && b) && c"),
              ("a || b || c", "(a || b) || c"),
              ("a ? b : c ? d : e", "a ? b : (c ? d : e)")
            )
          } {
            expr in { expr.asTree[Expr] shouldBe equiv.asTree[Expr] }
          }
        }
      }

      "gen" - {
        "if" - {
          "with out else" in {
            "gen if (i < 0) { fence; }".asTree[Gen] shouldBe {
              GenIf(
                ExprIdent("i") < ExprNum(false, 0),
                List(StmtFence()),
                Nil
              )
            }
          }

          "with else" in {
            "gen if (i < 0) { fence; } else { fence; break; }".asTree[Gen] shouldBe {
              GenIf(
                ExprIdent("i") < ExprNum(false, 0),
                List(StmtFence()),
                List(StmtFence(), StmtBreak())
              )
            }
          }

          "with 1 else if" in {
            """|gen if (i) {
               |  fence;
               |} else if (j) {
               |  return;
               |} else {
               |  break;
               |}""".stripMargin.asTree[Gen] shouldBe {
              GenIf(
                ExprIdent("i"),
                List(StmtFence()),
                List(
                  GenIf(
                    ExprIdent("j"),
                    List(StmtReturn()),
                    List(StmtBreak())
                  ))
              )
            }
          }

          "with 2 else if" in {
            """|gen if (i) {
               |  fence;
               |} else if (j) {
               |  return;
               |} else if (k) {
               |  f();
               |} else {
               |  break;
               |}""".stripMargin.asTree[Gen] shouldBe {
              GenIf(
                ExprIdent("i"),
                List(StmtFence()),
                List(
                  GenIf(
                    ExprIdent("j"),
                    List(StmtReturn()),
                    List(
                      GenIf(
                        ExprIdent("k"),
                        List(StmtExpr(ExprCall(ExprIdent("f"), Nil))),
                        List(StmtBreak())
                      ))
                  ))
              )
            }
          }
        }

        "for" - {
          "empty" in {
            "gen for(;;){}".asTree[Gen] shouldBe GenFor(Nil, None, Nil, Nil)
          }

          "with single init assign" in {
            """|gen for (a=2;a;a--) {
               |  2;
               |}""".stripMargin.asTree[Gen] shouldBe {
              GenFor(
                List(StmtAssign(ExprIdent("a"), Expr(2))),
                Some(ExprIdent("a")),
                List(StmtPost(ExprIdent("a"), "--")),
                List(StmtExpr(Expr(2)))
              )
            }
          }

          "with single init decl" in {
            """|gen for (i8 a=2;a;a--) {
               |  2;
               |}""".stripMargin.asTree[Gen] shouldBe {
              GenFor(
                List(StmtDecl(DeclIdent(Ident("a"), TypeGen(TypeSInt(Expr(8))), Some(Expr(2))))),
                Some(ExprIdent("a")),
                List(StmtPost(ExprIdent("a"), "--")),
                List(StmtExpr(Expr(2)))
              )
            }
          }

          "with multiple init" in {
            """|gen for (i8 a=2, b=1;;) {
               |}""".stripMargin.asTree[Gen] shouldBe {
              GenFor(
                List(
                  StmtDecl(DeclIdent(Ident("a"), TypeGen(TypeSInt(Expr(8))), Some(Expr(2)))),
                  StmtAssign(ExprIdent("b"), Expr(1))
                ),
                None,
                Nil,
                Nil
              )
            }
          }

          "with multiple step" in {
            """|gen for (;;a++, b--) {
               |}""".stripMargin.asTree[Gen] shouldBe {
              GenFor(
                Nil,
                None,
                List(
                  StmtPost(ExprIdent("a"), "++"),
                  StmtPost(ExprIdent("b"), "--")
                ),
                Nil
              )
            }
          }
        }

        "range" - {
          "with <" in {
            "gen for (u8 i < 10) { fence; }".asTree[Gen] shouldBe {
              GenRange(
                DeclIdent(Ident("i"), TypeGen(TypeUInt(8)), None),
                "<",
                ExprNum(false, 10),
                List(StmtFence())
              )
            }
          }

          "with <=" in {
            "gen for (i8 j <= 20) { break; }".asTree[Gen] shouldBe {
              GenRange(
                DeclIdent(Ident("j"), TypeGen(TypeSInt(8)), None),
                "<=",
                ExprNum(false, 20),
                List(StmtBreak())
              )
            }
          }
        }
      }
    }

    /////////////////////////////////////////////////////////////////////////////
    // Locations
    /////////////////////////////////////////////////////////////////////////////

    "should assign correct locations to tree nodes" in {

      val tree = """|fsm foo {
                    |  void main() {
                    |    bar i;
                    |    loop { }
                    |  }
                    |}""".stripMargin.asTree[Entity]

      inside(tree) {
        case entity: Entity =>
          entity.loc.line shouldBe 1
          inside(entity.body.loneElement) {
            case function: EntFunction =>
              function.loc.line shouldBe 2
              inside(function.stmts(0)) {
                case stmtDecl: StmtDecl =>
                  stmtDecl.loc.line shouldBe 3
                  inside(stmtDecl.decl) {
                    case decl: DeclIdent =>
                      decl.loc.line shouldBe 3
                      decl.ident.loc.line shouldBe 3
                      inside(decl.kind) {
                        case TypeIdent(ident: Ident) =>
                          ident.loc.line shouldBe 3
                      }
                  }
              }
              inside(function.stmts(1)) {
                case stmtLoop: StmtLoop =>
                  stmtLoop.loc.line shouldBe 4
                  stmtLoop.body shouldBe empty
              }
          }
      }

      cc.messages shouldBe empty
    }

  }

}
