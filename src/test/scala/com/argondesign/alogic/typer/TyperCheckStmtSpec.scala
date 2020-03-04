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
import org.scalatest.FreeSpec

final class TyperCheckStmtSpec extends FreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  private def typeCheck(text: String): Thicket = Thicket {
    transformWithPass(Namer andThen Elaborate andThen TypeCheck, text) map {
      _ flatMap {
        case (decl, defn) => List(decl, defn)
      }
    } getOrElse Nil
  }

  "The Typer should type check statements" - {
    "blocks" - {
      for {
        (stmt, err) <- List(
          ("{ $display(); }", Nil),
          ("{ fence; }", Nil),
          ("{ $display(); fence; }", Nil),
          ("{ $display(); fence; $display(); fence; }", Nil),
          ("{ fence; $display();}",
           "Block must contain only combinational statements, or end with a control statement" :: Nil)
        )
      } {
        stmt in {
          typeCheck {
            s"""
               |fsm a {
               |  void main() {
               |    $stmt
               |    fence;
               |  }
               |}
               |""".stripMargin
          }
          checkSingleError(err)
        }
      }
    }

    "fundamental flow control statements" - {
      for {
        (stmt, err) <- List(
          ("if (1) $display();", Nil),
          ("if (1) $display(); else $display();", Nil),
          ("if (1) fence;", Nil),
          ("if (1) fence; else fence;", Nil),
          ("if (1) fence; else $display();",
           "Either both or neither branches of if-else must be control statements" :: Nil),
          ("if (1) $display(); else fence;",
           "Either both or neither branches of if-else must be control statements" :: Nil),
          ("if (void) $display();",
           "Condition of 'if' statement is of neither numeric nor packed type" :: Nil),
          ("case(1) { 1: $display(); }", Nil),
          ("case(1) { 1: fence; }", Nil),
          ("case(1) { default: $display(); }", Nil),
          ("case(1) { default: fence; }", Nil),
          ("case(1) { 1: $display(); 2: $display(); }", Nil),
          ("case(1) { 1: fence; 2: fence; }", Nil),
          ("case(1) { 1: $display(); default: $display(); }", Nil),
          ("case(1) { 1: fence; default: fence; }", Nil),
          ("case(1) { 1: $display(); 2: fence; }",
           "Either all or no cases of a case statement must be control statements" :: Nil),
          ("case(1) { 1: fence; 2: $display(); }",
           "Either all or no cases of a case statement must be control statements" :: Nil),
          ("case(1) { 1: $display(); default: fence; }",
           "Either all or no cases of a case statement must be control statements" :: Nil),
          ("case(1) { 1: fence; default: $display(); }",
           "Either all or no cases of a case statement must be control statements" :: Nil),
          ("case(void) { 1: $display(); }",
           "'case' expression is of neither numeric nor packed type" :: Nil),
          ("loop { fence; }", Nil),
          ("loop { }", "Body of 'loop' must be a control statement" :: Nil),
          ("loop { $display(); }", "Body of 'loop' must end in a control statement" :: Nil),
          ("loop { fence; $display(); }", "Body of 'loop' must end in a control statement" :: Nil)
        )
      } {
        stmt in {
          typeCheck {
            s"""
               |fsm a {
               |  void main() {
               |    $stmt
               |    fence;
               |  }
               |}
               |""".stripMargin
          }
          checkSingleError(err)
        }
      }
    }

    "function bodies" - {
      for {
        (func, err) <- List(
          ("void main () { fence; }", Nil),
          ("void main () { $display(); }",
           "Body of function must end in a control statement" :: Nil),
          ("void main () { fence; $display(); }",
           "Body of function must end in a control statement" :: Nil)
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

    "fence blocks bodies" - {
      for {
        (fenceBlock, err) <- List(
          ("fence { $display(); }", Nil),
          ("fence { $display(); fence; }",
           "'fence' block must contain only combinational statements" :: Nil)
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

    "assignments" - {
      for {
        (assignment, err) <- List(
          ("a = 2", Nil),
          ("a = 8'd2", Nil),
          ("a = bool", "Right hand side of assignment is of non-packed type" :: Nil),
          ("bool = 8'd2", "Left hand side of assignment is of non-packed type" :: Nil),
          ("a += 8'd2", Nil),
          ("a += bool", "Right hand side of assignment is of non-packed type" :: Nil),
          ("bool += 8'd2", "Left hand side of assignment is of non-packed type" :: Nil),
          ("a = 9'd2", "Right hand side of assignment yields 9 bits, 8 bits are expected" :: Nil),
          ("a = 7'd2", "Right hand side of assignment yields 7 bits, 8 bits are expected" :: Nil)
        )
      } {
        assignment in {
          typeCheck {
            s"""
            |fsm x {
            |  void main() {
            |    (* unused *) i8 a;
            |    ${assignment};
            |    fence;
            |  }
            |}"""
          }
          checkSingleError(err)
        }
      }
    }

    "postfix increment/decrement" - {
      for (op <- List("++", "--")) {
        for {
          (assignment, err) <- List(
            (s"a${op}", Nil),
            (s"bool${op}", s"Target of postfix '${op}' is of non-packed type" :: Nil)
          )
        } {
          assignment in {
            typeCheck {
              s"""
              |fsm x {
              |  void main() {
              |    (* unused *) i8 a;
              |    ${assignment};
              |    fence;
              |  }
              |}"""
            }
            checkSingleError(err)
          }
        }
      }
    }

    "assignments to illegal lhs" - {
      val iPortErr = "Input port cannot be modified" :: Nil
      val oPortErr = "Output port with flow control can only be modified using .write()" :: Nil
      val constErr = "Constant cannot be modified" :: Nil
      val memoryErr = "Memory can only be modified using .write()" :: Nil

      for (op <- List("=", "+=")) {
        for {
          (assignment, err) <- List(
            (s"a ${op} 8'd0", iPortErr),
            (s"b ${op} 8'd0", Nil),
            (s"c ${op} 8'd0", iPortErr),
            (s"d ${op} 8'd0", oPortErr),
            (s"e ${op} 8'd0", iPortErr),
            (s"f ${op} 8'd0", oPortErr),
            (s"h ${op} 8'd0", constErr),
            (s"a[0] ${op} 1'b0", iPortErr),
            (s"b[0] ${op} 1'b0", Nil),
            (s"c[0] ${op} 1'b0", iPortErr),
            (s"d[0] ${op} 1'b0", oPortErr),
            (s"e[0] ${op} 1'b0", iPortErr),
            (s"f[0] ${op} 1'b0", oPortErr),
            (s"h[0] ${op} 1'b0", constErr),
            (s"i[0] ${op} 4'b0", memoryErr),
            (s"a[1:0] ${op} 2'b0", iPortErr),
            (s"b[1:0] ${op} 2'b0", Nil),
            (s"c[1:0] ${op} 2'b0", iPortErr),
            (s"d[1:0] ${op} 2'b0", oPortErr),
            (s"e[1:0] ${op} 2'b0", iPortErr),
            (s"f[1:0] ${op} 2'b0", oPortErr),
            (s"h[1:0] ${op} 2'b0", constErr),
            (s"i[0][1:0] ${op} 2'b0", memoryErr),
            (s"{b[1], a[0]} ${op} 2'b0", iPortErr),
            (s"{b[1], b[0]} ${op} 2'b0", Nil),
            (s"{b[1], c[0]} ${op} 2'b0", iPortErr),
            (s"{b[1], d[0]} ${op} 2'b0", oPortErr),
            (s"{b[1], e[0]} ${op} 2'b0", iPortErr),
            (s"{b[1], f[0]} ${op} 2'b0", oPortErr),
            (s"{b[1], h[0]} ${op} 2'b0", constErr),
            (s"{b[1], i[0]} ${op} 9'b0", memoryErr)
          )
        } {
          assignment in {
            typeCheck {
              s"""
              |fsm x {
              |  in i8 a;
              |  out i8 b;
              |  in sync i8 c;
              |  out sync i8 d;
              |  in sync ready i8 e;
              |  out sync ready i8 f;
              |  const i8 h = 8'd2;
              |  u8 i[4];
              |
              |  void main() {
              |    ${assignment};
              |    fence;
              |  }
              |}"""
            }
            checkSingleError(err)
          }
        }
      }
    }

    "postfix increment/decrement of illegal target" - {
      val iPortErr = "Input port cannot be modified" :: Nil
      val oPortErr = "Output port with flow control can only be modified using .write()" :: Nil
      val constErr = "Constant cannot be modified" :: Nil
      val memoryErr = "Memory can only be modified using .write()" :: Nil

      for (op <- List("++", "--")) {
        for {
          (assignment, err) <- List(
            (s"a${op}", iPortErr),
            (s"b${op}", Nil),
            (s"c${op}", iPortErr),
            (s"d${op}", oPortErr),
            (s"e${op}", iPortErr),
            (s"f${op}", oPortErr),
            (s"h${op}", constErr),
            (s"a[0]${op}", iPortErr),
            (s"b[0]${op}", Nil),
            (s"c[0]${op}", iPortErr),
            (s"d[0]${op}", oPortErr),
            (s"e[0]${op}", iPortErr),
            (s"f[0]${op}", oPortErr),
            (s"h[0]${op}", constErr),
            (s"i[0]${op}", memoryErr),
            (s"a[1:0]${op}", iPortErr),
            (s"b[1:0]${op}", Nil),
            (s"c[1:0]${op}", iPortErr),
            (s"d[1:0]${op}", oPortErr),
            (s"e[1:0]${op}", iPortErr),
            (s"f[1:0]${op}", oPortErr),
            (s"h[1:0]${op}", constErr),
            (s"i[0][1:0]${op}", memoryErr),
            (s"{b[1], a[0]}${op}", iPortErr),
            (s"{b[1], b[0]}${op}", Nil),
            (s"{b[1], c[0]}${op}", iPortErr),
            (s"{b[1], d[0]}${op}", oPortErr),
            (s"{b[1], e[0]}${op}", iPortErr),
            (s"{b[1], f[0]}${op}", oPortErr),
            (s"{b[1], h[0]}${op}", constErr),
            (s"{b[1], i[0]}${op}", memoryErr)
          )
        } {
          assignment in {
            typeCheck {
              s"""
              |fsm x {
              |  in i8 a;
              |  out i8 b;
              |  in sync i8 c;
              |  out sync i8 d;
              |  in sync ready i8 e;
              |  out sync ready i8 f;
              |  const i8 h = 8'd2;
              |  u8 i[4];
              |
              |  void main() {
              |    ${assignment};
              |    fence;
              |  }
              |}"""
            }
            checkSingleError(err)
          }
        }
      }
    }

    "error for pure expressions in statement position" - {
      val error = "A pure expression in statement position does nothing" :: Nil
      for {
        (text, err) <- List(
          ("1 * 2", error),
          ("-(1)", error),
          ("@randbit()", error),
          ("1 + @randbit()", error),
          ("$display(\"\")", Nil)
        )
      } {
        text in {
          typeCheck {
            s"""
               |void function() {
               | ${text};
               |}
               |""".stripMargin
          }
          checkSingleError(err)
        }
      }
    }
  }
}
