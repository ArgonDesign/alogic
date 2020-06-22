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
import com.argondesign.alogic.core.FuncVariant
import com.argondesign.alogic.core.Message
import com.argondesign.alogic.core.SourceAttribute
import com.argondesign.alogic.core.SourceContext
import com.argondesign.alogic.core.Warning
import com.argondesign.alogic.core.enums.EntityVariant
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

final class ParserSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  def beSyntaxError: Matcher[Message] = beSyntaxError("")

  def beSyntaxError(text: String): Matcher[Message] = Matcher { msg: Message =>
    val matchesMessage = if (text.isEmpty) {
      msg.msg(0) startsWith "Syntax error: "
    } else {
      msg.msg(0) == s"Syntax error: $text"
    }
    MatchResult(
      msg.isInstanceOf[Error] && matchesMessage,
      s"'$msg' was not a Syntax error message matching 'Syntax error: $text'",
      s"'$msg' was the correct Syntex error message"
    )
  }

  "The parser" - {

    /////////////////////////////////////////////////////////////////////////////
    // Valid input
    /////////////////////////////////////////////////////////////////////////////

    "should accept valid input" - {

      "file without type definitions" in {
        """fsm foo {
          |
          |}""".stripMargin.asTree[Root] should matchPattern {
          case Root(List(RizDesc(_: DescEntity))) =>
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
          "fsm".asTree[Root]
        }
        cc.messages.loneElement should beSyntaxError
      }

      "file without entity defintition" ignore {
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

      "missing parameter list after instantiation" ignore {
        a[AsTreeSyntaxErrorException] should be thrownBy {
          "network a { b = new c; }".asTree[Desc]
        }
        cc.messages should not be empty
        cc.messages(0) should beSyntaxError(
          "missing parameter list '()' after entity name in instantiation 'new c'"
        )
      }

      "empty concatenation" in {
        a[AsTreeSyntaxErrorException] should be thrownBy {
          "{}".asTree[Expr]
        }
        cc.messages should not be empty
      }

      "missing brace" in {
        a[AsTreeSyntaxErrorException] should be thrownBy {
          "a = {".asTree[Stmt]
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

      "declarations" - {
        "typedef" in {
          "typedef u8 foo;".asTree[Desc] shouldBe {
            DescType(Ident("foo", Nil), ExprType(TypeUInt(8)))
          }
        }

        "variable" - {
          "without initializer" in {
            "bool a;".asTree[Desc] shouldBe DescVar(Ident("a", Nil), ExprType(TypeUInt(1)), None)
          }

          "with initializer" in {
            "bool b = true;".asTree[Desc] shouldBe {
              DescVar(Ident("b", Nil), ExprType(TypeUInt(1)), Some(ExprInt(false, 1, 1)))
            }
          }

          "with attribute" in {
            inside("(* foo *) bool b;".asTree[Desc]) {
              case DescVar(ident @ Ident("b", Nil), ExprType(TypeUInt(w)), None) if w == 1 =>
                ident.attr shouldBe Map("foo" -> SourceAttribute.Flag())
            }
          }
        }

        "array" - {
          "1D" in {
            "i8 c[2];".asTree[Desc] shouldBe {
              DescArray(Ident("c", Nil), ExprType(TypeSInt(8)), Expr(2))
            }
          }

          "1D with attribute" in {
            inside("(* foo *) i8 c[2];".asTree[Desc]) {
              case DescArray(ident @ Ident("c", Nil), ExprType(TypeSInt(w1)), Expr(w2))
                  if w1 == 8 && w2 == 2 =>
                ident.attr shouldBe Map("foo" -> SourceAttribute.Flag())
            }
          }
        }

        "output" - {
          "no flow control" - {
            "default" in {
              "out i2 a;".asTree[Desc] shouldBe {
                DescOut(
                  Ident("a", Nil),
                  ExprType(TypeSInt(2)),
                  FlowControlTypeNone,
                  StorageTypeDefault,
                  None
                )
              }
            }

            "wire" in {
              "out wire u2 a;".asTree[Desc] shouldBe {
                DescOut(
                  Ident("a", Nil),
                  ExprType(TypeUInt(2)),
                  FlowControlTypeNone,
                  StorageTypeWire,
                  None
                )
              }
            }
          }

          "valid flow control" - {
            "default" in {
              "out sync i2 a;".asTree[Desc] shouldBe {
                DescOut(
                  Ident("a", Nil),
                  ExprType(TypeSInt(2)),
                  FlowControlTypeValid,
                  StorageTypeDefault,
                  None
                )
              }
            }

            "wire" in {
              "out sync wire i2 a;".asTree[Desc] shouldBe {
                DescOut(
                  Ident("a", Nil),
                  ExprType(TypeSInt(2)),
                  FlowControlTypeValid,
                  StorageTypeWire,
                  None
                )
              }
            }
          }

          "valid/ready flow control" - {
            "default" in {
              "out sync ready i2 a;".asTree[Desc] shouldBe {
                DescOut(
                  Ident("a", Nil),
                  ExprType(TypeSInt(2)),
                  FlowControlTypeReady,
                  StorageTypeDefault,
                  None
                )
              }
            }

            "fslice" in {
              "out sync ready fslice i2 a;".asTree[Desc] shouldBe {
                DescOut(
                  Ident("a", Nil),
                  ExprType(TypeSInt(2)),
                  FlowControlTypeReady,
                  StorageTypeSlices(List(StorageSliceFwd)),
                  None
                )
              }
            }

            "bslice" in {
              "out sync ready bslice i2 a;".asTree[Desc] shouldBe {
                DescOut(
                  Ident("a", Nil),
                  ExprType(TypeSInt(2)),
                  FlowControlTypeReady,
                  StorageTypeSlices(List(StorageSliceBwd)),
                  None
                )
              }
            }

            "bubble" in {
              "out sync ready bubble i2 a;".asTree[Desc] shouldBe {
                DescOut(
                  Ident("a", Nil),
                  ExprType(TypeSInt(2)),
                  FlowControlTypeReady,
                  StorageTypeSlices(List(StorageSliceBub)),
                  None
                )
              }
            }

            "bslice bubble fslice" in {
              "out sync ready bslice bubble fslice i2 a;".asTree[Desc] shouldBe {
                DescOut(
                  Ident("a", Nil),
                  ExprType(TypeSInt(2)),
                  FlowControlTypeReady,
                  StorageTypeSlices(List(StorageSliceBwd, StorageSliceBub, StorageSliceFwd)),
                  None
                )
              }
            }

            "with attribute" in {
              inside("(* foo *) out i2 a;".asTree[Desc]) {
                case DescOut(
                      ident @ Ident("a", Nil),
                      ExprType(TypeSInt(w)),
                      FlowControlTypeNone,
                      StorageTypeDefault,
                      None
                    ) if w == 2 =>
                  ident.attr shouldBe Map("foo" -> SourceAttribute.Flag())
              }
            }
          }
        }

        "inputs" - {
          "no flow control" in {
            "in i2 a;".asTree[Desc] shouldBe {
              DescIn(Ident("a", Nil), ExprType(TypeSInt(2)), FlowControlTypeNone)
            }
          }

          "valid flow control" in {
            "in sync i2 a;".asTree[Desc] shouldBe {
              DescIn(Ident("a", Nil), ExprType(TypeSInt(2)), FlowControlTypeValid)
            }
          }

          "valid/ready flow control" in {
            "in sync ready i2 a;".asTree[Desc] shouldBe {
              DescIn(Ident("a", Nil), ExprType(TypeSInt(2)), FlowControlTypeReady)
            }
          }

          "with attribute" in {
            inside("(* foo *) in i2 a;".asTree[Desc]) {
              case DescIn(ident @ Ident("a", Nil), ExprType(TypeSInt(w)), FlowControlTypeNone)
                  if w == 2 =>
                ident.attr shouldBe Map("foo" -> SourceAttribute.Flag())
            }
          }
        }

        "parameter" - {
          "without initializer" in {
            "param bool a;".asTree[Desc] shouldBe {
              DescParam(Ident("a", Nil), ExprType(TypeUInt(1)), None)
            }
          }

          "with initializer" in {
            "param i2 a = 2;".asTree[Desc] shouldBe {
              DescParam(Ident("a", Nil), ExprType(TypeSInt(2)), Some(Expr(2)))
            }
          }

          "with attribute" in {
            inside("(* foo *) param i2 a = 2;".asTree[Desc]) {
              case DescParam(ident @ Ident("a", Nil), ExprType(TypeSInt(w)), Some(Expr(2)))
                  if w == 2 =>
                ident.attr shouldBe Map("foo" -> SourceAttribute.Flag())
            }
          }

          "type without initialzier" in {
            "param type T;".asTree[Desc] shouldBe {
              DescParamType(Ident("T", Nil), None)
            }
          }

          "type with initialzier" in {
            "param type T = uint;".asTree[Desc] shouldBe {
              DescParamType(Ident("T", Nil), Some(ExprType(TypeNum(false))))
            }
          }
        }

        "constant" in {
          "const i2 a = 2;".asTree[Desc] shouldBe {
            DescConst(Ident("a", Nil), ExprType(TypeSInt(2)), Expr(2))
          }
        }

        "constant with attribute" in {
          inside("(* foo *) const i2 a = 2;".asTree[Desc]) {
            case DescConst(ident @ Ident("a", Nil), ExprType(TypeSInt(w1)), Expr(2)) if w1 == 2 =>
              ident.attr shouldBe Map("foo" -> SourceAttribute.Flag())
          }
        }

        "pipeline variable" in {
          "pipeline u8 a;".asTree[Desc] shouldBe {
            DescPipeline(Ident("a", Nil), ExprType(TypeUInt(8)))
          }
        }

        "pipeline variable with attribute" in {
          inside("(* foo *) pipeline u8 a;".asTree[Desc]) {
            case DescPipeline(ident @ Ident("a", Nil), ExprType(TypeUInt(w))) if w == 8 =>
              ident.attr shouldBe Map("foo" -> SourceAttribute.Flag())
          }
        }

        "sram" in {
          "sram u8 a[10];".asTree[Desc] shouldBe {
            DescSram(Ident("a", Nil), ExprType(TypeUInt(8)), Expr(10), StorageTypeReg)
          }
        }

        "sram with attribute" in {
          inside("(* foo *) sram u8 a[10];".asTree[Desc]) {
            case DescSram(ident @ Ident("a", Nil), ExprType(TypeUInt(w)), Expr(10), StorageTypeReg)
                if w == 8 =>
              ident.attr shouldBe Map("foo" -> SourceAttribute.Flag())
          }
        }

        "sram wire" in {
          "sram wire u8 a[10];".asTree[Desc] shouldBe {
            DescSram(Ident("a", Nil), ExprType(TypeUInt(8)), Expr(10), StorageTypeWire)
          }
        }

        "sram wire with attribute" in {
          inside("(* foo *) sram wire u8 a[10];".asTree[Desc]) {
            case DescSram(ident @ Ident("a", Nil), ExprType(TypeUInt(w)), Expr(10), StorageTypeWire)
                if w == 8 =>
              ident.attr shouldBe Map("foo" -> SourceAttribute.Flag())
          }
        }

        "fsm" in {
          "fsm a {}".asTree[Desc] shouldBe {
            DescEntity(Ident("a", Nil), EntityVariant.Fsm, Nil)
          }
        }

        "network" in {
          "network a {}".asTree[Desc] shouldBe {
            DescEntity(Ident("a", Nil), EntityVariant.Net, Nil)
          }
        }

        "verbatim entity" in {
          "verbatim entity a {}".asTree[Desc] shouldBe {
            DescEntity(Ident("a", Nil), EntityVariant.Ver, Nil)
          }
        }

        "struct" in {
          "struct a {}".asTree[Desc] shouldBe {
            DescRecord(Ident("a", Nil), Nil)
          }
        }

        "instance" in {
          "i = new j();".asTree[Desc] shouldBe {
            DescInstance(Ident("i", Nil), ExprCall(ExprRef(Ident("j", Nil)), Nil))
          }
        }

        "instance with parameters" in {
          "i = new j(A=2, B=3);".stripMargin.asTree[Desc] shouldBe {
            DescInstance(
              Ident("i", Nil),
              ExprCall(ExprRef(Ident("j", Nil)), List(ArgN("A", Expr(2)), ArgN("B", Expr(3))))
            )
          }
        }

        "instance with attribute" in {
          val tree = "(* foo *) i = new j();".asTree[Desc]
          inside(tree) {
            case DescInstance(ident @ Ident("i", Nil), ExprCall(ExprRef(Ident("j", Nil)), Nil)) =>
              ident.attr shouldBe Map("foo" -> SourceAttribute.Flag())
          }
        }

        "isolated function" in {
          "void main() {}".asTree[Desc] shouldBe {
            DescFunc(Ident("main", Nil), FuncVariant.None, ExprType(TypeVoid), Nil, Nil)
          }
        }

        "function in entity" in {
          "void main() {}".asTree[Ent] shouldBe {
            EntDesc(DescFunc(Ident("main", Nil), FuncVariant.Ctrl, ExprType(TypeVoid), Nil, Nil))
          }
        }

        "function in record" in {
          "void main() {}".asTree[Rec] shouldBe {
            RecDesc(DescFunc(Ident("main", Nil), FuncVariant.Method, ExprType(TypeVoid), Nil, Nil))
          }
        }

        "function with attributes" in {
          val tree = "(* foo, bar=2, baz = 1 + 2 *) void main() {}".asTree[Desc]
          inside(tree) {
            case DescFunc(
                  ident @ Ident("main", Nil),
                  FuncVariant.None,
                  ExprType(TypeVoid),
                  Nil,
                  Nil
                ) =>
              ident.attr shouldBe {
                Map(
                  "foo" -> SourceAttribute.Flag(),
                  "bar" -> SourceAttribute.Expr(Expr(2)),
                  "baz" -> SourceAttribute.Expr(ExprBinary(Expr(1), "+", Expr(2)))
                )
              }
          }
        }

        "singleton entity" in {
          "new fsm i {}".asTree[Desc] shouldBe {
            DescSingleton(Ident("i", Nil), EntityVariant.Fsm, Nil)
          }
        }

        "singleton entity with attribute" in {
          inside("(* foo *) new fsm i {}".asTree[Desc]) {
            case DescSingleton(ident @ Ident("i", Nil), EntityVariant.Fsm, Nil) =>
              ident.attr shouldBe Map("foo" -> SourceAttribute.Flag())
          }
        }

        "foreign function" - {
          "a" in {
            "import u8 f();".asTree[Desc] shouldBe {
              DescFunc(Ident("f", Nil), FuncVariant.Xeno, ExprType(TypeUInt(8)), Nil, Nil)
            }
          }

          "b" in {
            "import void f(u2 i);".asTree[Desc] shouldBe {
              DescFunc(
                Ident("f", Nil),
                FuncVariant.Xeno,
                ExprType(TypeVoid),
                DescVar(Ident("i", Nil), ExprType(TypeUInt(2)), None) :: Nil,
                Nil
              )
            }
          }

          "c" in {
            "import void f(u2 i, i3 j);".asTree[Desc] shouldBe {
              DescFunc(
                Ident("f", Nil),
                FuncVariant.Xeno,
                ExprType(TypeVoid),
                DescVar(Ident("i", Nil), ExprType(TypeUInt(2)), None) ::
                  DescVar(Ident("j", Nil), ExprType(TypeSInt(3)), None) ::
                  Nil,
                Nil
              )
            }
          }
        }

        "function with parameters" - {
          "a" in {
            "void f(u2 i) {}".asTree[Desc] shouldBe {
              DescFunc(
                Ident("f", Nil),
                FuncVariant.None,
                ExprType(TypeVoid),
                DescVar(Ident("i", Nil), ExprType(TypeUInt(2)), None) :: Nil,
                Nil
              )
            }
          }

          "b" in {
            "void f(u2 i, i3 j) {}".asTree[Desc] shouldBe {
              DescFunc(
                Ident("f", Nil),
                FuncVariant.None,
                ExprType(TypeVoid),
                DescVar(Ident("i", Nil), ExprType(TypeUInt(2)), None) ::
                  DescVar(Ident("j", Nil), ExprType(TypeSInt(3)), None) ::
                  Nil,
                Nil
              )
            }
          }
        }

        "function with non-void return type" - {
          "a" in {
            "u10 f() {}".asTree[Desc] shouldBe {
              DescFunc(
                Ident("f", Nil),
                FuncVariant.None,
                ExprType(TypeUInt(10)),
                Nil,
                Nil
              )
            }
          }

          "b" in {
            "r f() {}".asTree[Desc] shouldBe {
              DescFunc(
                Ident("f", Nil),
                FuncVariant.None,
                ExprRef(Ident("r", Nil)),
                Nil,
                Nil
              )
            }
          }
        }

        "static function" in {
          "static void f() {}".asTree[Desc] shouldBe {
            DescFunc(
              Ident("f", Nil),
              FuncVariant.Static,
              ExprType(TypeVoid),
              Nil,
              Nil
            )
          }
        }
      }

      "attributes" - {
        "flag" in {
          inside("(* flag *) void a;".asTree[Desc]) {
            case DescVar(ident @ Ident("a", Nil), ExprType(TypeVoid), None) =>
              ident.attr shouldBe Map("flag" -> SourceAttribute.Flag())
          }
        }

        "expr" in {
          inside("(* expr = 2 *) void a;".asTree[Desc]) {
            case DescVar(ident @ Ident("a", Nil), ExprType(TypeVoid), None) =>
              ident.attr shouldBe Map("expr" -> SourceAttribute.Expr(Expr(2)))
          }
        }

        "slices" in {
          inside("(* expr = bubble fslice *) void a;".asTree[Desc]) {
            case DescVar(ident @ Ident("a", Nil), ExprType(TypeVoid), None) =>
              ident.attr shouldBe Map(
                "expr" -> SourceAttribute.Slices(List(StorageSliceBub, StorageSliceFwd))
              )
          }
        }
      }

      "types" - {
        "bool" in {
          "bool".asTree[Expr] shouldBe ExprType(TypeUInt(1))
        }

        "bool is same as u1" in {
          "bool".asTree[Expr] shouldBe "u1".asTree[Expr]
        }

        "fixed unsigned ints" in {
          forAll(List("u1", "u2", "u3", "u44", "u128")) { str =>
            str.asTree[Expr] shouldBe ExprType(TypeUInt(str.tail.toInt))
          }
        }

        "fixed signed ints" in {
          forAll(List("i1", "i2", "i3", "i44", "i128")) { str =>
            str.asTree[Expr] shouldBe ExprType(TypeSInt(str.tail.toInt))
          }
        }

        "parametrized integers" - {
          "unsigned" in {
            "uint(N)".asTree[Expr] shouldBe ExprCall(
              ExprType(TypeNum(false)),
              List(ArgP(ExprRef(Ident("N", Nil))))
            )
          }

          "signed" in {
            "int(N)".asTree[Expr] shouldBe ExprCall(
              ExprType(TypeNum(true)),
              List(ArgP(ExprRef(Ident("N", Nil))))
            )
          }
        }

        "vectors" - {
          "1D u2" in {
            "u2[8]".asTree[Expr] shouldBe ExprIndex(ExprType(TypeUInt(2)), Expr(8))
          }

          "2D u2" in {
            "u2[4][8]".asTree[Expr] shouldBe {
              ExprIndex(ExprIndex(ExprType(TypeUInt(2)), Expr(4)), Expr(8))
            }
          }

          "1D i2" in {
            "i2[8]".asTree[Expr] shouldBe ExprIndex(ExprType(TypeSInt(2)), Expr(8))
          }

          "2D i2" in {
            "i2[4][8]".asTree[Expr] shouldBe {
              ExprIndex(ExprIndex(ExprType(TypeSInt(2)), Expr(4)), Expr(8))
            }
          }

          "1D uint(3)" in {
            "uint(3)[8]".asTree[Expr] shouldBe ExprIndex(
              ExprCall(ExprType(TypeNum(false)), ArgP(Expr(3)) :: Nil),
              Expr(8)
            )
          }

          "2D uint(3)" in {
            "uint(3)[4][8]".asTree[Expr] shouldBe {
              ExprIndex(
                ExprIndex(ExprCall(ExprType(TypeNum(false)), ArgP(Expr(3)) :: Nil), Expr(4)),
                Expr(8)
              )
            }
          }

          "1D int(3)" in {
            "int(3)[8]".asTree[Expr] shouldBe ExprIndex(
              ExprCall(ExprType(TypeNum(true)), ArgP(Expr(3)) :: Nil),
              Expr(8)
            )
          }

          "2D int(3)" in {
            "int(3)[4][8]".asTree[Expr] shouldBe {
              ExprIndex(
                ExprIndex(ExprCall(ExprType(TypeNum(true)), ArgP(Expr(3)) :: Nil), Expr(4)),
                Expr(8)
              )
            }
          }

          "1D bool" in {
            "bool[8]".asTree[Expr] shouldBe ExprIndex(ExprType(TypeUInt(1)), Expr(8))
          }

          "2D bool" in {
            "bool[4][8]".asTree[Expr] shouldBe {
              ExprIndex(ExprIndex(ExprType(TypeUInt(1)), Expr(4)), Expr(8))
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

        "call" - {
          "foo(1)" in {
            "foo(1) x;".asTree[Desc] should matchPattern {
              case DescVar(_, ExprCall(ExprRef(Ident("foo", Nil)), ArgP(Expr(1)) :: Nil), None) =>
            }
          }

          "foo(2'd0, 3'd1)" in {
            "foo(2'd0, 3'd1) x;".asTree[Desc] should matchPattern {
              case DescVar(
                    _,
                    ExprCall(
                      ExprRef(Ident("foo", Nil)),
                      ArgP(ExprInt(false, 2, aa)) :: ArgP(ExprInt(false, 3, bb)) :: Nil
                    ),
                    None
                  ) if aa == 0 && bb == 1 =>
            }
          }
        }
      }

      "entity contents" - {
        "declaration" in {
          "bool x;".stripMargin.asTree[Ent] shouldBe {
            EntDesc(DescVar(Ident("x", Nil), ExprType(TypeUInt(1)), None))
          }
        }

        "single connection" in {
          "i.a -> j.b;".asTree[Ent] shouldBe {
            EntConnect(
              ExprSelect(ExprRef(Ident("i", Nil)), "a", Nil),
              List(ExprSelect(ExprRef(Ident("j", Nil)), "b", Nil))
            )
          }
        }

        "multiple connections" in {
          "i.a -> j.b, k.c;".asTree[Ent] shouldBe {
            EntConnect(
              ExprSelect(ExprRef(Ident("i", Nil)), "a", Nil),
              List(
                ExprSelect(ExprRef(Ident("j", Nil)), "b", Nil),
                ExprSelect(ExprRef(Ident("k", Nil)), "c", Nil)
              )
            )
          }
        }

        "dict connection" in {
          "i.a#[0] -> j.b#[1, 2];".asTree[Ent] shouldBe {
            EntConnect(
              ExprSelect(ExprRef(Ident("i", Nil)), "a", Expr(0) :: Nil),
              List(ExprSelect(ExprRef(Ident("j", Nil)), "b", Expr(1) :: Expr(2) :: Nil))
            )
          }
        }

        "fence block" in {
          "fence { a = 1; }".asTree[Ent] shouldBe {
            EntCombProcess(List(StmtAssign(ExprRef(Ident("a", Nil)), Expr(1))))
          }
        }

        "static assert with no message" in {
          "static assert false;".asTree[Ent] shouldBe {
            EntAssertion(AssertionStatic(ExprInt(false, 1, 0), None))
          }
        }

        "static assert with message" in {
          """static assert false, "msg";""".asTree[Ent] shouldBe {
            EntAssertion(AssertionStatic(ExprInt(false, 1, 0), Some("msg")))
          }
        }

        "verbatim verilog" in {
          "verbatim verilog {\n    +-/* comment */ {{{}}}\n  }".asTree[Ent] shouldBe {
            EntVerbatim("verilog", "\n    +-/* comment */ {{{}}}\n  ")
          }
        }

        "verbatim other" in {
          "verbatim other {\n    +-/* comment */ {{{}}}\n  }".asTree[Ent] shouldBe {
            EntVerbatim("other", "\n    +-/* comment */ {{{}}}\n  ")
          }
        }
      }

      "record contents" - {
        "declaration" in {
          "bool x;".asTree[Rec] shouldBe {
            RecDesc(DescVar(Ident("x", Nil), ExprType(TypeUInt(1)), None))
          }
        }

        "static assert with no message" in {
          "static assert false;".asTree[Rec] shouldBe {
            RecAssertion(AssertionStatic(ExprInt(false, 1, 0), None))
          }
        }

        "static assert with message" in {
          """static assert false, "msg";""".asTree[Rec] shouldBe {
            RecAssertion(AssertionStatic(ExprInt(false, 1, 0), Some("msg")))
          }
        }

        "static method" in {
          """static void f() {}""".asTree[Rec] shouldBe {
            RecDesc(DescFunc(Ident("f", Nil), FuncVariant.Static, ExprType(TypeVoid), Nil, Nil))
          }
        }

        "method" in {
          """void f() {}""".asTree[Rec] shouldBe {
            RecDesc(DescFunc(Ident("f", Nil), FuncVariant.Method, ExprType(TypeVoid), Nil, Nil))
          }
        }
      }

      "statements" - {
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

        "branching" - {
          "if without else, without brace" in {
            "if (1) a;"
              .asTree[Stmt] shouldBe StmtIf(Expr(1), List(StmtExpr(ExprRef(Ident("a", Nil)))), Nil)
          }

          "if with else, without brace" in {
            "if (1) fence; else return;"
              .asTree[Stmt](SourceContext.Entity) shouldBe StmtIf(
              Expr(1),
              List(StmtFence()),
              List(StmtReturn(comb = false, None))
            )
          }

          "if without else, with brace" in {
            "if (1) {a;}"
              .asTree[Stmt] shouldBe StmtIf(Expr(1), List(StmtExpr(ExprRef(Ident("a", Nil)))), Nil)
          }

          "if with else, with brace" in {
            "if (1) {fence;} else {return;}".asTree[Stmt](SourceContext.Entity) shouldBe {
              StmtIf(
                Expr(1),
                List(StmtFence()),
                List(StmtReturn(comb = false, None))
              )
            }
          }

          "case without default" in {
            """case (1) {
              | 1: a;
              | 2: b;
              |}
              |""".stripMargin.asTree[Stmt] shouldBe {
              StmtCase(
                Expr(1),
                List(
                  CaseRegular(List(Expr(1)), List(StmtExpr(ExprRef(Ident("a", Nil))))),
                  CaseRegular(List(Expr(2)), List(StmtExpr(ExprRef(Ident("b", Nil)))))
                )
              )
            }
          }

          "case with default" in {
            """case (1) {
              | default: c;
              |}
              |""".stripMargin.asTree[Stmt] shouldBe {
              StmtCase(
                Expr(1),
                List(
                  CaseDefault(List(StmtExpr(ExprRef(Ident("c", Nil)))))
                )
              )
            }
          }

          "case with multiple labels" in {
            """case (1) {
              | 1: c;
              | 2, 3: d;
              |}
              |""".stripMargin.asTree[Stmt] shouldBe {
              StmtCase(
                Expr(1),
                List(
                  CaseRegular(List(Expr(1)), List(StmtExpr(ExprRef(Ident("c", Nil))))),
                  CaseRegular(List(Expr(2), Expr(3)), List(StmtExpr(ExprRef(Ident("d", Nil)))))
                )
              )
            }
          }

          "case with multiple defaults" in {
            """case (1) {
              | default: c;
              | default: d;
              |}
              |""".stripMargin.asTree[Stmt] shouldBe {
              StmtCase(
                Expr(1),
                List(
                  CaseDefault(List(StmtExpr(ExprRef(Ident("c", Nil))))),
                  CaseDefault(List(StmtExpr(ExprRef(Ident("d", Nil)))))
                )
              )
            }
          }

          "case ordering" in {
            """case (1) {
              | 1: a;
              | default: b;
              | 3: c;
              | default: d;
              |}
              |""".stripMargin.asTree[Stmt] shouldBe {
              StmtCase(
                Expr(1),
                List(
                  CaseRegular(List(Expr(1)), List(StmtExpr(ExprRef(Ident("a", Nil))))),
                  CaseDefault(List(StmtExpr(ExprRef(Ident("b", Nil))))),
                  CaseRegular(List(Expr(3)), List(StmtExpr(ExprRef(Ident("c", Nil))))),
                  CaseDefault(List(StmtExpr(ExprRef(Ident("d", Nil)))))
                )
              )
            }
          }

          "case without braces" in {
            """case (1) {
              | 1: a;
              | default: c;
              |}
              |""".stripMargin.asTree[Stmt] shouldBe {
              StmtCase(
                Expr(1),
                List(
                  CaseRegular(List(Expr(1)), List(StmtExpr(ExprRef(Ident("a", Nil))))),
                  CaseDefault(List(StmtExpr(ExprRef(Ident("c", Nil)))))
                )
              )
            }
          }

          "case with braces" in {
            """case (1) {
              | 1: {a;}
              | default: {c;}
              |}
              |""".stripMargin.asTree[Stmt] shouldBe {
              StmtCase(
                Expr(1),
                List(
                  CaseRegular(List(Expr(1)), List(StmtExpr(ExprRef(Ident("a", Nil))))),
                  CaseDefault(List(StmtExpr(ExprRef(Ident("c", Nil)))))
                )
              )
            }
          }
        }

        "loops" - {
          "loop" in {
            """loop {
              |  1;
              |}""".stripMargin.asTree[Stmt] shouldBe StmtLoop(List(StmtExpr(Expr(1))))
          }

          "while" in {
            """while (a) {
              |  fence;
              |}""".stripMargin
              .asTree[Stmt] shouldBe StmtWhile(ExprRef(Ident("a", Nil)), List(StmtFence()))
          }

          "do" in {
            """do {
              | fence;
              |} while(b);""".stripMargin
              .asTree[Stmt] shouldBe StmtDo(ExprRef(Ident("b", Nil)), List(StmtFence()))
          }

          "for" - {
            "empty" in {
              "for(;;){}".asTree[Stmt] shouldBe StmtFor(Nil, None, Nil, Nil)
            }

            "with single init assign" in {
              """for (a=2;a;a--) {
                |  2;
                |}""".stripMargin.asTree[Stmt] shouldBe {
                StmtFor(
                  List(StmtAssign(ExprRef(Ident("a", Nil)), Expr(2))),
                  Some(ExprRef(Ident("a", Nil))),
                  List(StmtPost(ExprRef(Ident("a", Nil)), "--")),
                  List(StmtExpr(Expr(2)))
                )

              }
            }

            "with single init decl" in {
              """for (i8 a=2;a;a--) {
                |  2;
                |}""".stripMargin.asTree[Stmt] shouldBe {
                StmtFor(
                  List(StmtDesc(DescVar(Ident("a", Nil), ExprType(TypeSInt(8)), Some(Expr(2))))),
                  Some(ExprRef(Ident("a", Nil))),
                  List(StmtPost(ExprRef(Ident("a", Nil)), "--")),
                  List(StmtExpr(Expr(2)))
                )
              }
            }

            "with multiple init" in {
              """for (i8 a=2, b=1;;) {
                |}""".stripMargin.asTree[Stmt] shouldBe {
                StmtFor(
                  List(
                    StmtDesc(DescVar(Ident("a", Nil), ExprType(TypeSInt(8)), Some(Expr(2)))),
                    StmtAssign(ExprRef(Ident("b", Nil)), Expr(1))
                  ),
                  None,
                  Nil,
                  Nil
                )
              }
            }

            "with multiple step" in {
              """for (;;a++, b--) {
                |}""".stripMargin.asTree[Stmt] shouldBe {
                StmtFor(
                  Nil,
                  None,
                  List(
                    StmtPost(ExprRef(Ident("a", Nil)), "++"),
                    StmtPost(ExprRef(Ident("b", Nil)), "--")
                  ),
                  Nil
                )
              }
            }
          }
        }

        "let" - {
          "single declaration" in {
            "let (i2 a=1) loop {}".asTree[Stmt] shouldBe {
              StmtLet(
                List(StmtDesc(DescVar(Ident("a", Nil), ExprType(TypeSInt(2)), Some(Expr(1))))),
                List(StmtLoop(Nil))
              )
            }
          }

          "multiple declarations" in {
            "let (i2 a=b, i2 c=a) loop {}".asTree[Stmt] shouldBe {
              StmtLet(
                List(
                  StmtDesc(
                    DescVar(Ident("a", Nil), ExprType(TypeSInt(2)), Some(ExprRef(Ident("b", Nil))))
                  ),
                  StmtDesc(
                    DescVar(Ident("c", Nil), ExprType(TypeSInt(2)), Some(ExprRef(Ident("a", Nil))))
                  )
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

        "return" - {
          "entity" - {
            "void" in {
              "return;".asTree[Stmt](SourceContext.Entity) shouldBe StmtReturn(comb = false, None)
            }

            "value" in {
              "return 0;"
                .asTree[Stmt](SourceContext.Entity) shouldBe StmtReturn(comb = false, Some(Expr(0)))
            }
          }

          "record" - {
            "void" in {
              "return;".asTree[Stmt](SourceContext.Record) shouldBe StmtReturn(comb = true, None)
            }

            "value" in {
              "return 0;"
                .asTree[Stmt](SourceContext.Record) shouldBe StmtReturn(comb = true, Some(Expr(0)))
            }
          }
        }

        "goto" - {
          "goto call" in {
            "goto a();".asTree[Stmt] shouldBe StmtGoto(ExprCall(ExprRef(Ident("a", Nil)), Nil))
          }

          "goto symbol" in {
            "goto b;".asTree[Stmt] shouldBe StmtGoto(ExprRef(Ident("b", Nil)))
          }

          "goto const" in {
            "goto 1;".asTree[Stmt] shouldBe StmtGoto(ExprNum(false, 1))
          }
        }

        // TODO: assignments
        "assignments" - {
          "simple" in {
            "a = 1;".asTree[Stmt] shouldBe StmtAssign(ExprRef(Ident("a", Nil)), Expr(1))
          }

          "update +=" in {
            "b += 2;".asTree[Stmt] shouldBe StmtUpdate(ExprRef(Ident("b", Nil)), "+", Expr(2))
          }

          "update <<=" in {
            "c <<= 3;".asTree[Stmt] shouldBe StmtUpdate(ExprRef(Ident("c", Nil)), "<<", Expr(3))
          }

          "postfix ++" in {
            "d++;".asTree[Stmt] shouldBe StmtPost(ExprRef(Ident("d", Nil)), "++")
          }

          "postfix --" in {
            "e--;".asTree[Stmt] shouldBe StmtPost(ExprRef(Ident("e", Nil)), "--")
          }
        }

        "expressions in statement position" - {
          "identifier" in {
            "a;".asTree[Stmt] shouldBe StmtExpr(ExprRef(Ident("a", Nil)))
          }

          "call" in {
            "b();".asTree[Stmt] shouldBe StmtExpr(ExprCall(ExprRef(Ident("b", Nil)), Nil))
          }
        }

        "declaration statements" - {
          "scalar without initializer" in {
            "u2 a;".asTree[Stmt] shouldBe StmtDesc(
              DescVar(Ident("a", Nil), ExprType(TypeUInt(2)), None)
            )
          }

          "scalar with initializer" in {
            "i2 b = 3;".asTree[Stmt] shouldBe {
              StmtDesc(DescVar(Ident("b", Nil), ExprType(TypeSInt(2)), Some(Expr(3))))
            }
          }

          "constant" in {
            "const u6 x = 7;".asTree[Stmt] shouldBe StmtDesc(
              DescVal(Ident("x", Nil), ExprType(TypeUInt(6)), Expr(7))
            )
          }
        }

        "read statement" in {
          "read;".asTree[Stmt] shouldBe StmtRead()
        }

        "write statement" in {
          "write;".asTree[Stmt] shouldBe StmtWrite()
        }

        "assert statement with no message" in {
          "assert false;".asTree[Stmt] shouldBe {
            StmtAssertion(AssertionAssert(ExprInt(false, 1, 0), None))
          }
        }

        "assert statement with message" in {
          """assert false, "msg";""".asTree[Stmt] shouldBe {
            StmtAssertion(AssertionAssert(ExprInt(false, 1, 0), Some("msg")))
          }
        }

        "static assert statement with no message" in {
          "static assert false;".asTree[Stmt] shouldBe {
            StmtAssertion(AssertionStatic(ExprInt(false, 1, 0), None))
          }
        }

        "static assert statement with message" in {
          """static assert false, "msg";""".asTree[Stmt] shouldBe {
            StmtAssertion(AssertionStatic(ExprInt(false, 1, 0), Some("msg")))
          }
        }

        "wait statement" - {
          "with condition" in {
            "wait a;".asTree[Stmt] shouldBe {
              StmtWait(ExprRef(Ident("a", Nil)))
            }
          }

          "without condition" in {
            "wait;".asTree[Stmt] shouldBe {
              StmtWait(ExprInt(false, 1, 0))
            }
          }
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
                (
                  "     017 ",
                  ExprError(),
                  "Invalid literal '017',\nuse prefix '0o' for octal or '0d' for decimal with leading zeros"
                )
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

          "call" - {
            "with no arguments" in {
              "a()".asTree[Expr] shouldBe ExprCall(ExprRef(Ident("a", Nil)), Nil)
            }

            "with 1 positional argument" in {
              "b(2)".asTree[Expr] shouldBe ExprCall(ExprRef(Ident("b", Nil)), List(ArgP(Expr(2))))
            }

            "with 2 positional arguments" in {
              "c(d, e)".asTree[Expr] shouldBe {
                ExprCall(
                  ExprRef(Ident("c", Nil)),
                  List(ArgP(ExprRef(Ident("d", Nil))), ArgP(ExprRef(Ident("e", Nil))))
                )
              }
            }

            "with 1 named argument" in {
              "b(x = 2)"
                .asTree[Expr] shouldBe ExprCall(ExprRef(Ident("b", Nil)), List(ArgN("x", Expr(2))))
            }

            "with 2 named arguments" in {
              "c(x=d, y=e)".asTree[Expr] shouldBe {
                ExprCall(
                  ExprRef(Ident("c", Nil)),
                  List(ArgN("x", ExprRef(Ident("d", Nil))), ArgN("y", ExprRef(Ident("e", Nil))))
                )
              }
            }

            "with 1 positional and 1 named argument" in {
              "c(d, y=e)".asTree[Expr] shouldBe {
                ExprCall(
                  ExprRef(Ident("c", Nil)),
                  List(ArgP(ExprRef(Ident("d", Nil))), ArgN("y", ExprRef(Ident("e", Nil))))
                )
              }
            }

            "with 1 dict argument" in {
              "c(x#[0]=d)".asTree[Expr] shouldBe {
                ExprCall(
                  ExprRef(Ident("c", Nil)),
                  List(
                    ArgD("x", Expr(0) :: Nil, ExprRef(Ident("d", Nil)))
                  )
                )
              }
            }

            "with 2 dict arguments" in {
              "c(x#[0]=d, x#[1]=e)".asTree[Expr] shouldBe {
                ExprCall(
                  ExprRef(Ident("c", Nil)),
                  List(
                    ArgD("x", Expr(0) :: Nil, ExprRef(Ident("d", Nil))),
                    ArgD("x", Expr(1) :: Nil, ExprRef(Ident("e", Nil)))
                  )
                )
              }
            }
          }

          for (op <- List("+", "-", "~", "!", "&", "|", "^", "'")) {
            s"unary $op" in {
              s"$op(2)".asTree[Expr] shouldBe ExprUnary(op, Expr(2))
            }
          }

          for (
            op <- List(
              "'",
              "*",
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
              "||"
            )
          ) {
            s"binary $op" in {
              s"4 $op 3".asTree[Expr] shouldBe ExprBinary(Expr(4), op, Expr(3))
            }
          }

          "ternary" in {
            "1 ? 2 : 3".asTree[Expr] shouldBe ExprTernary(Expr(1), Expr(2), Expr(3))
          }

          "repetition" in {
            "{N{a}}"
              .asTree[Expr] shouldBe ExprRep(ExprRef(Ident("N", Nil)), ExprRef(Ident("a", Nil)))
          }

          "concatenation" in {
            "{0, 1, 2}".asTree[Expr] shouldBe ExprCat(List(Expr(0), Expr(1), Expr(2)))
          }

          "multiple concatenation " in {
            "{N{a, b}}".asTree[Expr] shouldBe {
              ExprRep(
                ExprRef(Ident("N", Nil)),
                ExprCat(List(ExprRef(Ident("a", Nil)), ExprRef(Ident("b", Nil))))
              )
            }
          }

          "index 1x" in {
            "a[0]".asTree[Expr] shouldBe ExprIndex(ExprRef(Ident("a", Nil)), Expr(0))
          }

          "index 2x" in {
            "a[0][2]".asTree[Expr] shouldBe {
              ExprIndex(ExprIndex(ExprRef(Ident("a", Nil)), Expr(0)), Expr(2))
            }
          }

          "slice 1x" in {
            "b[1:0]"
              .asTree[Expr] shouldBe ExprSlice(ExprRef(Ident("b", Nil)), Expr(1), ":", Expr(0))
          }

          "slice 2x" in {
            "b[2+:0][1-:1]".asTree[Expr] should matchPattern {
              case ExprSlice(
                    ExprSlice(ExprRef(Ident("b", Nil)), Expr(2), "+:", Expr(0)),
                    Expr(1),
                    "-:",
                    Expr(1)
                  ) =>
            }
          }

          "select 1x" in {
            "a.b".asTree[Expr] shouldBe ExprSelect(ExprRef(Ident("a", Nil)), "b", Nil)
          }

          "select 2x" in {
            "a.b.c".asTree[Expr] shouldBe ExprSelect(
              ExprSelect(ExprRef(Ident("a", Nil)), "b", Nil),
              "c",
              Nil
            )
          }

          "@id" in {
            "@zx".asTree[Expr] shouldBe ExprRef(Ident("@zx", Nil))
          }

          "$id" in {
            "$clog2".asTree[Expr] shouldBe ExprRef(Ident("$clog2", Nil))
          }

          "@ call" in {
            "@zx(0, a)".asTree[Expr] shouldBe {
              ExprCall(
                ExprRef(Ident("@zx", Nil)),
                List(ArgP(Expr(0)), ArgP(ExprRef(Ident("a", Nil))))
              )
            }
          }

          "$ call" in {
            "$clog2(a)".asTree[Expr] shouldBe {
              ExprCall(ExprRef(Ident("$clog2", Nil)), List(ArgP(ExprRef(Ident("a", Nil)))))
            }
          }

          "identifier" in {
            "foo".asTree[Expr] shouldBe ExprRef(Ident("foo", Nil))
          }

          "type" in {
            "i8".asTree[Expr] shouldBe ExprType(TypeSInt(8))
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
              ExprSelect(ExprRef(Ident("a", Nil)), "b", Nil) && ExprSelect(
                ExprRef(Ident("a", Nil)),
                "c",
                Nil
              )
            }
          }

          "a.b && a.c == 1" in {
            "a.b && a.c == 1".asTree[Expr] shouldBe {
              ExprSelect(ExprRef(Ident("a", Nil)), "b", Nil) &&
              ExprBinary(ExprSelect(ExprRef(Ident("a", Nil)), "c", Nil), "==", Expr(1))
            }
          }

          "a.b && a[0]" in {
            "a.b && a[0]".asTree[Expr] shouldBe {
              ExprSelect(ExprRef(Ident("a", Nil)), "b", Nil) && ExprIndex(
                ExprRef(Ident("a", Nil)),
                0
              )
            }
          }

          "a.b && a[1:0]" in {
            "a.b && a[1:0]".asTree[Expr] shouldBe {
              ExprSelect(ExprRef(Ident("a", Nil)), "b", Nil) && ExprSlice(
                ExprRef(Ident("a", Nil)),
                1,
                ":",
                0
              )
            }
          }

          "a.b[1]" in {
            "a.b[1]".asTree[Expr] shouldBe {
              ExprIndex(ExprSelect(ExprRef(Ident("a", Nil)), "b", Nil), 1)
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
                ExprRef(Ident("i", Nil)) < ExprNum(false, 0),
                List(StmtFence()),
                Nil
              )
            }
          }

          "with else" in {
            "gen if (i < 0) { fence; } else { fence; break; }".asTree[Gen] shouldBe {
              GenIf(
                ExprRef(Ident("i", Nil)) < ExprNum(false, 0),
                List(StmtFence()),
                List(StmtFence(), StmtBreak())
              )
            }
          }

          "with 1 else if" in {
            """gen if (i) {
              |  fence;
              |} else if (j) {
              |  return;
              |} else {
              |  break;
              |}""".stripMargin.asTree[Gen](SourceContext.Entity) shouldBe {
              GenIf(
                ExprRef(Ident("i", Nil)),
                List(StmtFence()),
                List(
                  GenIf(
                    ExprRef(Ident("j", Nil)),
                    List(StmtReturn(comb = false, None)),
                    List(StmtBreak())
                  )
                )
              )
            }
          }

          "with 2 else if" in {
            """gen if (i) {
              |  fence;
              |} else if (j) {
              |  return;
              |} else if (k) {
              |  f();
              |} else {
              |  break;
              |}""".stripMargin.asTree[Gen](SourceContext.Entity) shouldBe {
              GenIf(
                ExprRef(Ident("i", Nil)),
                List(StmtFence()),
                List(
                  GenIf(
                    ExprRef(Ident("j", Nil)),
                    List(StmtReturn(comb = false, None)),
                    List(
                      GenIf(
                        ExprRef(Ident("k", Nil)),
                        List(StmtExpr(ExprCall(ExprRef(Ident("f", Nil)), Nil))),
                        List(StmtBreak())
                      )
                    )
                  )
                )
              )
            }
          }
        }

        "for" - {
          "empty decl" in {
            a[AsTreeSyntaxErrorException] shouldBe thrownBy {
              "gen for(;1;1++){}".asTree[Gen]
            }
          }

          "empty cond" in {
            a[AsTreeSyntaxErrorException] shouldBe thrownBy {
              "gen for(uint a = 0;;1++){}".asTree[Gen]
            }
          }

          "empty step" in {
            a[AsTreeSyntaxErrorException] shouldBe thrownBy {
              "gen for(uint a = 0;1;){}".asTree[Gen]
            }
          }

          "with single init decl" in {
            """gen for (i8 a=2;a;a--) {
              |  2;
              |}""".stripMargin.asTree[Gen] shouldBe {
              GenFor(
                List(StmtDesc(DescGen(Ident("a", Nil), ExprType(TypeSInt(8)), Expr(2)))),
                ExprRef(Ident("a", Nil)),
                List(StmtPost(ExprRef(Ident("a", Nil)), "--")),
                List(StmtExpr(Expr(2)))
              )
            }
          }

          "with multiple init decl" in {
            """gen for (i8 a=2, u8 b=1;1;1++) {
              |}""".stripMargin.asTree[Gen] shouldBe {
              GenFor(
                List(
                  StmtDesc(DescGen(Ident("a", Nil), ExprType(TypeSInt(8)), Expr(2))),
                  StmtDesc(DescGen(Ident("b", Nil), ExprType(TypeUInt(8)), Expr(1)))
                ),
                Expr(1),
                List(StmtPost(Expr(1), "++")),
                Nil
              )
            }
          }

          "with multiple step" in {
            """gen for (uint a = 0;a;a++, b--) {
              |}""".stripMargin.asTree[Gen] shouldBe {
              GenFor(
                List(
                  StmtDesc(DescGen(Ident("a", Nil), ExprType(TypeNum(false)), Expr(0)))
                ),
                ExprRef(Ident("a", Nil)),
                List(
                  StmtPost(ExprRef(Ident("a", Nil)), "++"),
                  StmtPost(ExprRef(Ident("b", Nil)), "--")
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
                List(StmtDesc(DescGen(Ident("i", Nil), ExprType(TypeUInt(8)), Expr(0)))),
                "<",
                ExprNum(false, 10),
                List(StmtFence())
              )
            }
          }

          "with <=" in {
            "gen for (i8 j <= 20) { break; }".asTree[Gen] shouldBe {
              GenRange(
                List(StmtDesc(DescGen(Ident("j", Nil), ExprType(TypeSInt(8)), Expr(0)))),
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

    "should assign correct locations to tree nodes" - {

      "simple" in {
        val tree = """fsm foo {
                     |  void main() {
                     |    bar i;
                     |    loop { }
                     |  }
                     |}""".stripMargin.asTree[Desc]

        inside(tree) {
          case entity @ DescEntity(_, EntityVariant.Fsm, eBody) =>
            entity.loc.line shouldBe 1
            entity.loc.file shouldBe "<asTree>"
            inside(eBody.loneElement) {
              case function @ EntDesc(
                    DescFunc(_, FuncVariant.Ctrl, ExprType(TypeVoid), Nil, fBody)
                  ) =>
                function.loc.line shouldBe 2
                function.loc.file shouldBe "<asTree>"
                inside(fBody(0)) {
                  case stmtDesc: StmtDesc =>
                    stmtDesc.loc.line shouldBe 3
                    stmtDesc.loc.file shouldBe "<asTree>"
                    inside(stmtDesc.desc) {
                      case desc @ DescVar(ident: Ident, spec, None) =>
                        desc.loc.line shouldBe 3
                        desc.loc.file shouldBe "<asTree>"
                        ident.loc.line shouldBe 3
                        ident.loc.file shouldBe "<asTree>"
                        inside(spec) {
                          case ExprRef(ident @ Ident("bar", Nil)) =>
                            ident.loc.line shouldBe 3
                            ident.loc.file shouldBe "<asTree>"
                        }
                    }
                }
                inside(fBody(1)) {
                  case stmtLoop: StmtLoop =>
                    stmtLoop.loc.line shouldBe 4
                    stmtLoop.loc.file shouldBe "<asTree>"
                    stmtLoop.body shouldBe empty
                }
            }
        }

        cc.messages shouldBe empty
      }

      "with line directives" in {
        val tree = """fsm foo {
                     |#line 20 "foo.bar"
                     |  void main() {
                     |#line 100
                     |    bar
                     |    i;
                     |#line 2 "another"
                     |    loop { }
                     |  }
                     |}""".stripMargin.asTree[Desc]

        inside(tree) {
          case entity @ DescEntity(_, EntityVariant.Fsm, eBody) =>
            entity.loc.line shouldBe 1
            entity.loc.file shouldBe "<asTree>"
            inside(eBody.loneElement) {
              case function @ EntDesc(
                    DescFunc(_, FuncVariant.Ctrl, ExprType(TypeVoid), Nil, fBody)
                  ) =>
                function.loc.line shouldBe 20
                function.loc.file shouldBe "foo.bar"
                inside(fBody(0)) {
                  case stmtDesc: StmtDesc =>
                    stmtDesc.loc.line shouldBe 100
                    stmtDesc.loc.file shouldBe "foo.bar"
                    inside(stmtDesc.desc) {
                      case desc @ DescVar(ident: Ident, spec, None) =>
                        desc.loc.line shouldBe 100
                        desc.loc.file shouldBe "foo.bar"
                        ident.loc.line shouldBe 101
                        ident.loc.file shouldBe "foo.bar"
                        inside(spec) {
                          case ExprRef(ident @ Ident("bar", Nil)) =>
                            ident.loc.line shouldBe 100
                            ident.loc.file shouldBe "foo.bar"
                        }
                    }
                }
                inside(fBody(1)) {
                  case stmtLoop: StmtLoop =>
                    stmtLoop.loc.line shouldBe 2
                    stmtLoop.loc.file shouldBe "another"
                    stmtLoop.body shouldBe empty
                }
            }
        }

        cc.messages shouldBe empty
      }

    }

  }

}
