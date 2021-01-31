////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Other type checking tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Source
import org.scalatest.freespec.AnyFreeSpec

final class TypeCheckerOtherSpec extends AnyFreeSpec with AlogicTest {

  implicit private val cc: CompilerContext = new CompilerContext

  private val fe = new Frontend

  private def elaborate(text: String): Option[Desc] =
    fe.elaborate(Source("TyperCheckerExprSpec", text)) pipe {
      case Left(ms)      => ms foreach cc.addMessage; None
      case Right(result) => Some(result)
    }

  private def typeCheck(tree: Tree): Unit =
    fe.typeCheck(tree) match {
      case Complete(_) =>
      case Unknown(_)  => fail()
      case Failure(ms) => ms foreach cc.addMessage
    }

  private def typeCheck(text: String): Tree =
    elaborate(text) tap { _ =>
      cc.messages foreach println
      cc.messages shouldBe empty
    } pipe { _.value } tap { tree =>
      typeCheck(tree)
    }

  "The Typer should type check other things" - {
    "control function bodies" - {
      for {
        (func, err) <- List(
          // format: off
          ("void f() {}", "Body of control function must end in a control statement" :: Nil),
          ("void f() { fence; }", Nil),
          ("void f() { return; }", Nil),
          ("void f() { $display(); }", "Body of control function must end in a control statement" :: Nil),
          ("void f() { fence; $display(); }", "Body of control function must end in a control statement" :: Nil)
          // format: on
        )
      } {
        func in {
          typeCheck {
            s"""
               |fsm a {
               |  $func
               |}
               |""".stripMargin
          }
          checkSingleError(err)
        }
      }
    }

    "combinational function bodies" - {
      for {
        (func, err) <- List(
          // format: off
          ("void f() {}", Nil),
          ("void f() { fence; }", "Body of combinational function must contain only combinational statements" :: Nil),
          ("void f() { return; }", Nil),
          ("void f() { $display(); }", Nil),
          ("void f() { fence; $display(); }", "Body of combinational function must contain only combinational statements" :: Nil)
          // format: on
        )
      } {
        func in {
          typeCheck {
            s"""
               |struct a {
               |  $func
               |}
               |""".stripMargin
          }
          checkSingleError(err)
        }
      }
    }

    "fence blocks bodies" - {
      for {
        (fenceBlock, err) <- List(
          ("fence { $display(); }", Nil),
          (
            "fence { $display(); fence; }",
            "'fence' block must contain only combinational statements" :: Nil
          )
        )
      } {
        fenceBlock in {
          typeCheck {
            s"""
               |fsm a {
               |  $fenceBlock
               |}
               |""".stripMargin
          }
          checkSingleError(err)
        }
      }
    }

    "declaration initializers" - {
      for {
        (decl, err) <- List(
          ("i8 a = 2", Nil),
          ("i8 a = 8'd2", Nil),
          (
            "i8 a = bool",
            "Initializer expression is of non-packed type, a 8 bit value is expected" :: Nil
          ),
          ("i8 a = 9'd2", "Initializer expression yields 9 bits, a 8 bit value is expected" :: Nil),
          ("i8 a = 7'd2", "Initializer expression yields 7 bits, a 8 bit value is expected" :: Nil),
          ("const uint a = 7'd2", "Unsized integer declaration has packed initializer" :: Nil)
        )
      } {
        decl in {
          elaborate {
            s"""
               |fsm a {
               |  $decl;
               |}
               |""".stripMargin
          } foreach typeCheck
          checkSingleError(err)
        }
      }
    }

    "declaration types" - {
      for {
        (entity, decl, err) <- List(
          ("fsm", "i8 a", Nil),
          ("fsm", "x a", "Type specifier does not name a type" :: Nil),
          ("fsm", "in i8 a", Nil),
          ("fsm", "in x a", "Type specifier does not name a type" :: Nil),
          ("fsm", "out i8 a", Nil),
          ("fsm", "out x a", "Type specifier does not name a type" :: Nil),
          ("fsm", "param i8 a = 0", Nil),
          ("fsm", "param x a = 0", "Type specifier does not name a type" :: Nil),
          ("fsm", "const i8 a = 0", Nil),
          ("fsm", "const x a = 0", "Type specifier does not name a type" :: Nil),
          ("network", "pipeline i8 a", Nil),
          ("network", "pipeline x a", "Type specifier does not name a type" :: Nil),
          ("fsm", "sram i8 a[1]", Nil),
          ("fsm", "sram x a[1]", "Type specifier does not name a type" :: Nil),
          ("fsm", "i8 a[1]", Nil),
          ("fsm", "x a[1]", "Type specifier does not name a type" :: Nil),
          ("fsm", "i8[1] a", Nil),
          ("fsm", "x[1] a", "Type specifier does not name a type" :: Nil)
        )
      } {
        decl in {
          elaborate {
            s"""network n {
               |  $entity f {
               |    in u2 x;
               |    $decl;
               |  }
               |  // Need to instantiate in order to type check
               |  ${if (decl startsWith "param") "i = new f();" else ""}
               |}""".stripMargin
          } foreach typeCheck
          checkSingleError(err)
        }
      }
    }
  }
}
