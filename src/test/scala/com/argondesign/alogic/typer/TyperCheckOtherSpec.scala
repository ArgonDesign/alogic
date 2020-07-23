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
// Statement type checking tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.typer

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.passes.Elaborate
import com.argondesign.alogic.passes.Namer
import com.argondesign.alogic.passes.TypeCheck
import org.scalatest.freespec.AnyFreeSpec

final class TyperCheckOtherSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  private def typeCheck(text: String): Thicket = Thicket {
    transformWithPass(Namer andThen Elaborate andThen TypeCheck, text) map {
      _.toList flatMap { case (decl, defn) => List(decl, defn) }
    } getOrElse Nil
  }

  "The Typer should type check statements" - {
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
          ("i8 a = bool", "Initializer expression is of non-packed type" :: Nil),
          ("i8 a = 9'd2", "Initializer expression yields 9 bits, 8 bits are expected" :: Nil),
          ("i8 a = 7'd2", "Initializer expression yields 7 bits, 8 bits are expected" :: Nil),
          ("const uint a = 7'd2", "Unsized integer declaration has packed initializer" :: Nil)
        )
      } {
        decl in {
          typeCheck {
            s"""
               |fsm a {
               |  $decl;
               |}
               |""".stripMargin
          }
          checkSingleError(err)
        }
      }
    }

    "declaration types" - {
      for {
        (decl, err) <- List(
          ("i8 a", Nil),
          ("x a", "Expression does not name a type" :: Nil),
          ("in i8 a", Nil),
          ("in x a", "Expression does not name a type" :: Nil),
          ("out i8 a", Nil),
          ("out x a", "Expression does not name a type" :: Nil),
          ("param i8 a = 0", Nil),
          ("param x a = 0", "Expression does not name a type" :: Nil),
          ("const i8 a = 0", Nil),
          ("const x a = 0", "Expression does not name a type" :: Nil),
          ("pipeline i8 a", Nil),
          ("pipeline x a", "Expression does not name a type" :: Nil),
          ("sram i8 a[1]", Nil),
          ("sram x a[1]", "Expression does not name a type" :: Nil),
          ("i8 a[1]", Nil),
          ("x a[1]", "Expression does not name a type" :: Nil),
          ("i8[1] a", Nil),
          ("x[1] a", "Expression does not name a type" :: Nil)
        )
      } {
        decl in {
          typeCheck {
            s"""
            |fsm f {
            |  u2 x;
            |  $decl;
            |}"""
          }
          checkSingleError(err)
        }
      }
    }
  }
}
