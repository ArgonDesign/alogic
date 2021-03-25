////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Tests for parsing Desc
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeReady
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeValid
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FuncVariant
import com.argondesign.alogic.core.SourceContext
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.enums.EntityVariant
import org.scalatest.freespec.AnyFreeSpec

final class ParserParseDescSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  "The parser should build correct ASTs for Desc" - {
    "declarations" - {
      "var" - {
        "without initializer" in {
          "bool a;"
            .asTree[Desc]() shouldBe DescVar(Ident("a", Nil), Nil, ExprType(TypeUInt(1)), None)
        }

        "with initializer" in {
          "bool b = true;".asTree[Desc]() shouldBe {
            DescVar(Ident("b", Nil), Nil, ExprType(TypeUInt(1)), Some(ExprInt(false, 1, 1)))
          }
        }

        "with attribute" in {
          "(* foo *) bool b;".asTree[Desc]() shouldBe {
            DescVar(
              Ident("b", Nil),
              List(AttrBool("foo")),
              ExprType(TypeUInt(1)),
              None
            )
          }
        }
      }

      // val in Stmt context only

      "static" - {
        "without initializer" in {
          "static bool a;".asTree[Desc]() shouldBe {
            DescStatic(Ident("a", Nil), Nil, ExprType(TypeUInt(1)), None)
          }
        }

        "with initializer" in {
          "static bool b = true;".asTree[Desc]() shouldBe {
            DescStatic(Ident("b", Nil), Nil, ExprType(TypeUInt(1)), Some(ExprInt(false, 1, 1)))
          }
        }

        "with attribute" in {
          "(* foo *) static bool b;".asTree[Desc]() shouldBe {
            DescStatic(
              Ident("b", Nil),
              List(AttrBool("foo")),
              ExprType(TypeUInt(1)),
              None
            )
          }
        }
      }

      "in" - {
        "no flow control" in {
          "in i2 a;".asTree[Desc]() shouldBe {
            DescIn(Ident("a", Nil), Nil, ExprType(TypeSInt(2)), FlowControlTypeNone)
          }
        }

        "valid flow control" in {
          "in sync i2 a;".asTree[Desc]() shouldBe {
            DescIn(Ident("a", Nil), Nil, ExprType(TypeSInt(2)), FlowControlTypeValid)
          }
        }

        "valid/ready flow control" in {
          "in sync ready i2 a;".asTree[Desc]() shouldBe {
            DescIn(Ident("a", Nil), Nil, ExprType(TypeSInt(2)), FlowControlTypeReady)
          }
        }

        "with attribute" in {
          "(* foo *) in i2 a;".asTree[Desc]() shouldBe {
            DescIn(
              Ident("a", Nil),
              List(AttrBool("foo")),
              ExprType(TypeSInt(2)),
              FlowControlTypeNone
            )
          }
        }

        "unnamed" in {
          "in bool;".asTree[Desc]() shouldBe {
            DescIn(Ident("in", Nil), Nil, ExprType(TypeUInt(1)), FlowControlTypeNone)
          }
        }
      }

      "out" - {
        "no flow control" - {
          "default" in {
            "out i2 a;".asTree[Desc]() shouldBe {
              DescOut(
                Ident("a", Nil),
                Nil,
                ExprType(TypeSInt(2)),
                FlowControlTypeNone,
                StorageTypeDefault,
                None
              )
            }
          }

          "wire" in {
            "out wire u2 a;".asTree[Desc]() shouldBe {
              DescOut(
                Ident("a", Nil),
                Nil,
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
            "out sync i2 a;".asTree[Desc]() shouldBe {
              DescOut(
                Ident("a", Nil),
                Nil,
                ExprType(TypeSInt(2)),
                FlowControlTypeValid,
                StorageTypeDefault,
                None
              )
            }
          }

          "wire" in {
            "out sync wire i2 a;".asTree[Desc]() shouldBe {
              DescOut(
                Ident("a", Nil),
                Nil,
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
            "out sync ready i2 a;".asTree[Desc]() shouldBe {
              DescOut(
                Ident("a", Nil),
                Nil,
                ExprType(TypeSInt(2)),
                FlowControlTypeReady,
                StorageTypeDefault,
                None
              )
            }
          }

          "fslice" in {
            "out sync ready fslice i2 a;".asTree[Desc]() shouldBe {
              DescOut(
                Ident("a", Nil),
                Nil,
                ExprType(TypeSInt(2)),
                FlowControlTypeReady,
                StorageTypeSlices(List(StorageSliceFwd)),
                None
              )
            }
          }

          "bslice" in {
            "out sync ready bslice i2 a;".asTree[Desc]() shouldBe {
              DescOut(
                Ident("a", Nil),
                Nil,
                ExprType(TypeSInt(2)),
                FlowControlTypeReady,
                StorageTypeSlices(List(StorageSliceBwd)),
                None
              )
            }
          }

          "bubble" in {
            "out sync ready bubble i2 a;".asTree[Desc]() shouldBe {
              DescOut(
                Ident("a", Nil),
                Nil,
                ExprType(TypeSInt(2)),
                FlowControlTypeReady,
                StorageTypeSlices(List(StorageSliceBub)),
                None
              )
            }
          }

          "bslice bubble fslice" in {
            "out sync ready bslice bubble fslice i2 a;".asTree[Desc]() shouldBe {
              DescOut(
                Ident("a", Nil),
                Nil,
                ExprType(TypeSInt(2)),
                FlowControlTypeReady,
                StorageTypeSlices(List(StorageSliceBwd, StorageSliceBub, StorageSliceFwd)),
                None
              )
            }
          }

          "with attribute" in {
            "(* foo *) out i2 a;".asTree[Desc]() shouldBe {
              DescOut(
                Ident("a", Nil),
                List(AttrBool("foo")),
                ExprType(TypeSInt(2)),
                FlowControlTypeNone,
                StorageTypeDefault,
                None
              )
            }
          }
        }

        "unnamed" in {
          "out bool;".asTree[Desc]() shouldBe {
            DescOut(
              Ident("out", Nil),
              Nil,
              ExprType(TypeUInt(1)),
              FlowControlTypeNone,
              StorageTypeDefault,
              None
            )
          }
        }

        "unnamed with initializer" in {
          "out bool = true;".asTree[Desc]() shouldBe {
            DescOut(
              Ident("out", Nil),
              Nil,
              ExprType(TypeUInt(1)),
              FlowControlTypeNone,
              StorageTypeDefault,
              Some(ExprInt(false, 1, 1))
            )
          }
        }
      }

      "pipe var" - {
        "plain" in {
          "pipeline u8 a;".asTree[Desc]() shouldBe {
            DescPipeVar(Ident("a", Nil), Nil, ExprType(TypeUInt(8)))
          }
        }

        "with attribute" in {
          "(* foo *) pipeline u8 a;".asTree[Desc]() shouldBe {
            DescPipeVar(Ident("a", Nil), List(AttrBool("foo")), ExprType(TypeUInt(8)))
          }
        }
      }

      "pipe in" in {
        "in pipeline a;".asTree[Desc]() shouldBe {
          DescPipeIn(Ident("a", Nil), Nil, Nil, FlowControlTypeNone)
        }
      }

      "pipe out" - {
        "without initalizer" in {
          "out pipeline a;".asTree[Desc]() shouldBe {
            DescPipeOut(Ident("a", Nil), Nil, Nil, FlowControlTypeNone, StorageTypeDefault)
          }
        }

        "with initializer" in {
          an[AsTreeSyntaxErrorException] shouldBe thrownBy {
            "out pipeline a = 0;".asTree[Desc]()
          }
          cc.messages.loneElement should beThe[Error](
            "Pipeline output port cannot have an initializer"
          )
        }
      }

      "param" - {
        "without initializer" in {
          "param bool a;".asTree[Desc]() shouldBe {
            DescParam(Ident("a", Nil), Nil, ExprType(TypeUInt(1)), None, false)
          }
        }

        "with initializer" in {
          "param i2 a = 2;".asTree[Desc]() shouldBe {
            DescParam(Ident("a", Nil), Nil, ExprType(TypeSInt(2)), Some(Expr(2)), false)
          }
        }

        "with attribute" in {
          "(* foo *) param i2 a = 2;".asTree[Desc]() shouldBe {
            DescParam(
              Ident("a", Nil),
              List(AttrBool("foo")),
              ExprType(TypeSInt(2)),
              Some(Expr(2)),
              false
            )
          }
        }
      }

      "param type" - {
        "without initialzier" in {
          "param type T;".asTree[Desc]() shouldBe {
            DescParamType(Ident("T", Nil), Nil, None, false)
          }
        }

        "with initialzier" in {
          "param type T = uint;".asTree[Desc]() shouldBe {
            DescParamType(Ident("T", Nil), Nil, Some(ExprType(TypeNum(false))), false)
          }
        }
      }

      "const" - {
        "plain" in {
          "const i2 a = 2;".asTree[Desc](SourceContext.Entity) shouldBe {
            DescConst(Ident("a", Nil), Nil, ExprType(TypeSInt(2)), Expr(2))
          }
        }

        "with attribute" in {
          "(* foo *) const i2 a = 2;".asTree[Desc](SourceContext.Entity) shouldBe {
            DescConst(Ident("a", Nil), List(AttrBool("foo")), ExprType(TypeSInt(2)), Expr(2))
          }
        }
      }

      "array" - {
        "1D" in {
          "i8 c[2];".asTree[Desc]() shouldBe {
            DescArray(Ident("c", Nil), Nil, ExprType(TypeSInt(8)), Expr(2))
          }
        }

        "1D with attribute" in {
          "(* foo *) i8 c[2];".asTree[Desc]() shouldBe {
            DescArray(Ident("c", Nil), List(AttrBool("foo")), ExprType(TypeSInt(8)), Expr(2))
          }
        }
      }

      "sram" - {
        "plain" in {
          "sram u8 a[10];".asTree[Desc]() shouldBe {
            DescSram(Ident("a", Nil), Nil, ExprType(TypeUInt(8)), Expr(10), StorageTypeReg)
          }
        }

        "with attribute" in {
          "(* foo *) sram u8 a[10];".asTree[Desc]() shouldBe {
            DescSram(
              Ident("a", Nil),
              List(AttrBool("foo")),
              ExprType(TypeUInt(8)),
              Expr(10),
              StorageTypeReg
            )
          }
        }

        "wire" in {
          "sram wire u8 a[10];".asTree[Desc]() shouldBe {
            DescSram(Ident("a", Nil), Nil, ExprType(TypeUInt(8)), Expr(10), StorageTypeWire)
          }
        }

        "wire with attribute" in {
          "(* foo *) sram wire u8 a[10];".asTree[Desc]() shouldBe {
            DescSram(
              Ident("a", Nil),
              List(AttrBool("foo")),
              ExprType(TypeUInt(8)),
              Expr(10),
              StorageTypeWire
            )
          }
        }
      }

      "type" in {
        "typedef u8 foo;".asTree[Desc]() shouldBe {
          DescType(Ident("foo", Nil), Nil, ExprType(TypeUInt(8)))
        }
      }

      "entity" - {
        "fsm" in {
          "fsm a {}".asTree[Desc]() shouldBe {
            DescEntity(Ident("a", Nil), Nil, EntityVariant.Fsm, Nil)
          }
        }

        "network" in {
          "network a {}".asTree[Desc]() shouldBe {
            DescEntity(Ident("a", Nil), Nil, EntityVariant.Net, Nil)
          }
        }

        "verbatim entity" in {
          "verbatim entity a {}".asTree[Desc]() shouldBe {
            DescEntity(Ident("a", Nil), Nil, EntityVariant.Ver, Nil)
          }
        }
      }

      "record" in {
        "struct a {}".asTree[Desc]() shouldBe {
          DescRecord(Ident("a", Nil), Nil, Nil)
        }
      }

      "instance" - {
        "plain" in {
          "i = new j;".asTree[Desc]() shouldBe {
            DescInstance(Ident("i", Nil), Nil, ExprIdent("j", Nil))
          }
        }

        "with parameters" in {
          "i = new j(A=2, B=3);".stripMargin.asTree[Desc]() shouldBe {
            DescInstance(
              Ident("i", Nil),
              Nil,
              ExprCall(ExprIdent("j", Nil), List(ArgN("A", Expr(2)), ArgN("B", Expr(3))))
            )
          }
        }

        "with attribute" in {
          "(* foo *) i = new j;".asTree[Desc]() shouldBe {
            DescInstance(Ident("i", Nil), List(AttrBool("foo")), ExprIdent("j", Nil))
          }
        }
      }

      "singleton" - {
        "fsm" in {
          "new fsm i {}".asTree[Desc]() shouldBe {
            DescSingleton(Ident("i", Nil), Nil, EntityVariant.Fsm, Nil)
          }
        }

        "with attribute" in {
          "(* foo *) new fsm i {}".asTree[Desc]() shouldBe {
            DescSingleton(Ident("i", Nil), List(AttrBool("foo")), EntityVariant.Fsm, Nil)
          }
        }
      }

      "function" - {
        "plain" in {
          "void main() {}".asTree[Desc](SourceContext.Entity) shouldBe {
            DescFunc(Ident("main", Nil), Nil, FuncVariant.Ctrl, ExprType(TypeVoid), Nil, Nil)
          }
        }

        "with attributes" in {
          "(* foo, bar=2, baz = 1 + 2 *) void main() {}".asTree[Desc](
            SourceContext.Entity
          ) shouldBe {
            DescFunc(
              Ident("main", Nil),
              List(AttrBool("foo"), AttrExpr("bar", Expr(2)), AttrExpr("baz", Expr(1) + Expr(2))),
              FuncVariant.Ctrl,
              ExprType(TypeVoid),
              Nil,
              Nil
            )
          }
        }

        "foreign" - {
          "a" in {
            "import u8 f();".asTree[Desc]() shouldBe {
              DescFunc(Ident("f", Nil), Nil, FuncVariant.Xeno, ExprType(TypeUInt(8)), Nil, Nil)
            }
          }

          "b" in {
            "import void f(u2 i);".asTree[Desc]() shouldBe {
              DescFunc(
                Ident("f", Nil),
                Nil,
                FuncVariant.Xeno,
                ExprType(TypeVoid),
                DescVar(Ident("i", Nil), Nil, ExprType(TypeUInt(2)), None) :: Nil,
                Nil
              )
            }
          }

          "c" in {
            "import void f(u2 i, i3 j);".asTree[Desc]() shouldBe {
              DescFunc(
                Ident("f", Nil),
                Nil,
                FuncVariant.Xeno,
                ExprType(TypeVoid),
                DescVar(Ident("i", Nil), Nil, ExprType(TypeUInt(2)), None) ::
                  DescVar(Ident("j", Nil), Nil, ExprType(TypeSInt(3)), None) ::
                  Nil,
                Nil
              )
            }
          }
        }

        "with parameters" - {
          "a" in {
            "void f(u2 i) {}".asTree[Desc](SourceContext.Record) shouldBe {
              DescFunc(
                Ident("f", Nil),
                Nil,
                FuncVariant.Method,
                ExprType(TypeVoid),
                DescVar(Ident("i", Nil), Nil, ExprType(TypeUInt(2)), None) :: Nil,
                Nil
              )
            }
          }

          "b" in {
            "void f(u2 i, i3 j) {}".asTree[Desc](SourceContext.Record) shouldBe {
              DescFunc(
                Ident("f", Nil),
                Nil,
                FuncVariant.Method,
                ExprType(TypeVoid),
                DescVar(Ident("i", Nil), Nil, ExprType(TypeUInt(2)), None) ::
                  DescVar(Ident("j", Nil), Nil, ExprType(TypeSInt(3)), None) ::
                  Nil,
                Nil
              )
            }
          }
        }

        "with non-void return type" - {
          "a" in {
            "u10 f() {}".asTree[Desc](SourceContext.Package) shouldBe {
              DescFunc(
                Ident("f", Nil),
                Nil,
                FuncVariant.Comb,
                ExprType(TypeUInt(10)),
                Nil,
                Nil
              )
            }
          }

          "b" in {
            "r f() {}".asTree[Desc](SourceContext.Package) shouldBe {
              DescFunc(
                Ident("f", Nil),
                Nil,
                FuncVariant.Comb,
                ExprIdent("r", Nil),
                Nil,
                Nil
              )
            }
          }
        }

        "static" in {
          "static void f() {}".asTree[Desc](SourceContext.Record) shouldBe {
            DescFunc(
              Ident("f", Nil),
              Nil,
              FuncVariant.Static,
              ExprType(TypeVoid),
              Nil,
              Nil
            )
          }
        }
      }

      "package" in {
        "bool a; u2 b;".asTree[DescPackage]() shouldBe DescPackage(
          Ident("<asTree>", Nil),
          Nil,
          List(
            PkgSplice(DescVar(Ident("a", Nil), Nil, ExprType(TypeUInt(1)), None)),
            PkgSplice(DescVar(Ident("b", Nil), Nil, ExprType(TypeUInt(2)), None))
          )
        )
      }

      "gen if" - {
        "unnamed without else" in {
          "gen if (i < 0) { fence; }".asTree[Desc]() shouldBe {
            DescGenIf(
              Ident("", Nil),
              Nil,
              List(GenCase(ExprIdent("i", Nil) < ExprNum(false, 0), List(StmtFence()))),
              Nil
            )
          }
        }

        "unnamed with else" in {
          "gen if (i < 0) { fence; } else { fence; break; }".asTree[Desc]() shouldBe {
            DescGenIf(
              Ident("", Nil),
              Nil,
              List(GenCase(ExprIdent("i", Nil) < ExprNum(false, 0), List(StmtFence()))),
              List(StmtFence(), StmtBreak())
            )
          }
        }

        "unnamed with one else if" in {
          """gen if (i) {
            |  fence;
            |} else if (j) {
            |  continue;
            |} else {
            |  break;
            |}""".stripMargin.asTree[Desc](SourceContext.Entity) shouldBe {
            DescGenIf(
              Ident("", Nil),
              Nil,
              List(
                GenCase(ExprIdent("i", Nil), List(StmtFence())),
                GenCase(ExprIdent("j", Nil), List(StmtContinue()))
              ),
              List(StmtBreak())
            )
          }
        }

        "unnamed with two else if" in {
          """gen if (i) {
            |  fence;
            |} else if (j) {
            |  continue;
            |} else if (k) {
            |  f();
            |} else {
            |  break;
            |}""".stripMargin.asTree[Desc](SourceContext.Entity) shouldBe {
            DescGenIf(
              Ident("", Nil),
              Nil,
              List(
                GenCase(ExprIdent("i", Nil), List(StmtFence())),
                GenCase(ExprIdent("j", Nil), List(StmtContinue())),
                GenCase(
                  ExprIdent("k", Nil),
                  List(StmtExpr(ExprCall(ExprIdent("f", Nil), Nil)))
                )
              ),
              List(StmtBreak())
            )
          }
        }

        "named" in {
          "gen if (i < 0) : foo { fence; }".asTree[Desc]() shouldBe {
            DescGenIf(
              Ident("foo", Nil),
              Nil,
              List(GenCase(ExprIdent("i", Nil) < ExprNum(false, 0), List(StmtFence()))),
              Nil
            )
          }
        }

        "unnamed empty if" in {
          "gen if (i < 0) {}".asTree[Desc]() shouldBe {
            DescGenIf(
              Ident("", Nil),
              Nil,
              List(GenCase(ExprIdent("i", Nil) < ExprNum(false, 0), Nil)),
              Nil
            )
          }
        }

        "unnamed empty if empty else if" in {
          "gen if (i < 0) {} else if (i < 1) {}".asTree[Desc]() shouldBe {
            DescGenIf(
              Ident("", Nil),
              Nil,
              List(
                GenCase(ExprIdent("i", Nil) < ExprNum(false, 0), Nil),
                GenCase(ExprIdent("i", Nil) < ExprNum(false, 1), Nil)
              ),
              Nil
            )
          }
        }

        "unnamed explicitly empty else" in {
          "gen if (i < 0) {} else {}".asTree[Desc]() shouldBe {
            DescGenIf(
              Ident("", Nil),
              Nil,
              List(GenCase(ExprIdent("i", Nil) < ExprNum(false, 0), Nil)),
              Nil
            )
          }
        }
      }

      "gen for" - {
        "unnamed with single init definition" in {
          """gen for (i8 a=2;a;a--) {
            |  2;
            |}""".stripMargin.asTree[Desc]() shouldBe {
            DescGenFor(
              Ident("", Nil),
              Nil,
              List(DescGenVar(Ident("a", Nil), Nil, ExprType(TypeSInt(8)), Expr(2))),
              ExprIdent("a", Nil),
              List(StmtPost(ExprIdent("a", Nil), "--")),
              List(StmtExpr(Expr(2)))
            )
          }
        }

        "unnamed with multiple init definitions" in {
          """gen for (i8 a=2, u8 b=1 ; 1 ; 1++) {
            |}""".stripMargin.asTree[Desc]() shouldBe {
            DescGenFor(
              Ident("", Nil),
              Nil,
              List(
                DescGenVar(Ident("a", Nil), Nil, ExprType(TypeSInt(8)), Expr(2)),
                DescGenVar(Ident("b", Nil), Nil, ExprType(TypeUInt(8)), Expr(1))
              ),
              Expr(1),
              List(StmtPost(Expr(1), "++")),
              Nil
            )
          }
        }

        "unnamed with multiple step" in {
          """gen for (uint a = 0 ; a ; a++, b--) {
            |}""".stripMargin.asTree[Desc]() shouldBe {
            DescGenFor(
              Ident("", Nil),
              Nil,
              List(
                DescGenVar(Ident("a", Nil), Nil, ExprType(TypeNum(false)), Expr(0))
              ),
              ExprIdent("a", Nil),
              List(
                StmtPost(ExprIdent("a", Nil), "++"),
                StmtPost(ExprIdent("b", Nil), "--")
              ),
              Nil
            )
          }
        }

        "named" in {
          "gen for (i8 a=2;a;a--) : foo {}".asTree[Desc]() shouldBe {
            DescGenFor(
              Ident("foo", Nil),
              Nil,
              List(DescGenVar(Ident("a", Nil), Nil, ExprType(TypeSInt(8)), Expr(2))),
              ExprIdent("a", Nil),
              List(StmtPost(ExprIdent("a", Nil), "--")),
              Nil
            )
          }
        }
      }

      "gen range" - {
        "unnamed with <" in {
          "gen for (u8 i < 10) { fence; }".asTree[Desc]() shouldBe {
            DescGenRange(
              Ident("", Nil),
              Nil,
              DescGenVar(Ident("i", Nil), Nil, ExprType(TypeUInt(8)), Expr(0)),
              "<",
              ExprNum(false, 10),
              List(StmtFence())
            )
          }
        }

        "unnamed with <=" in {
          "gen for (i8 j <= 20) { break; }".asTree[Desc]() shouldBe {
            DescGenRange(
              Ident("", Nil),
              Nil,
              DescGenVar(Ident("j", Nil), Nil, ExprType(TypeSInt(8)), Expr(0)),
              "<=",
              ExprNum(false, 20),
              List(StmtBreak())
            )
          }
        }

        "named" in {
          "gen for (u8 i < 10) : foo {}".asTree[Desc]() shouldBe {
            DescGenRange(
              Ident("foo", Nil),
              Nil,
              DescGenVar(Ident("i", Nil), Nil, ExprType(TypeUInt(8)), Expr(0)),
              "<",
              ExprNum(false, 10),
              Nil
            )
          }
        }
      }

      "attributes" - {
        "flag" in {
          "(* flag *) void a;".asTree[Desc]() shouldBe {
            DescVar(Ident("a", Nil), List(AttrBool("flag")), ExprType(TypeVoid), None)
          }
        }

        "expr" in {
          "(* expr = 2 *) void a;".asTree[Desc]() shouldBe {
            DescVar(Ident("a", Nil), List(AttrExpr("expr", Expr(2))), ExprType(TypeVoid), None)
          }
        }
      }
    }
  }

}
