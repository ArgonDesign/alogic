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

import java.util.regex.Pattern

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.passes.Namer
import org.scalatest.FreeSpec

final class TyperCheckStmtSpec extends FreeSpec with AlogicTest {

  implicit val cc = new CompilerContext
  cc.postSpecialization = true

  val namer = new Namer
  val typer = new Typer

  def xform(tree: Tree) = {
    tree match {
      case Root(_, entity: EntityIdent) => cc.addGlobalEntity(entity)
      case entity: EntityIdent          => cc.addGlobalEntity(entity)
      case _                            =>
    }
    val node = tree rewrite namer match {
      case Root(_, entity) => entity
      case other           => other
    }
    node rewrite typer
  }

  def checkError(tree: Tree, err: String) = {
    val errors = cc.messages.filter { _.isInstanceOf[Error] }
    if (err.isEmpty) {
      errors shouldBe empty
    } else {
      errors.loneElement should beThe[Error](Pattern.quote(err))
    }
  }

  "The Typer should type check statements" - {
    "blocks" - {
      for {
        (stmt, msg) <- List(
          ("{ $display(); }", ""),
          ("{ fence; }", ""),
          ("{ $display(); fence; }", ""),
          ("{ $display(); fence; $display(); fence; }", ""),
          ("{ fence; $display();}",
           "Block must contain only combinatorial statements, or end with a control statement")
        )
      } {
        stmt in {
          val result = xform(stmt.asTree[Stmt])
          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](msg)
            result shouldBe StmtError()
          }
        }
      }
    }

    "fundamental flow control statements" - {
      for {
        (stmt, msg) <- List(
          ("if (1) $display();", ""),
          ("if (1) $display(); else $display();", ""),
          ("if (1) fence;", ""),
          ("if (1) fence; else fence;", ""),
          ("if (1) fence; else $display();",
           "Either both or neither branches of if-else must be control statements"),
          ("if (1) $display(); else fence;",
           "Either both or neither branches of if-else must be control statements"),
          ("case(1) { 1: $display(); }", ""),
          ("case(1) { 1: fence; }", ""),
          ("case(1) { default: $display(); }", ""),
          ("case(1) { default: fence; }", ""),
          ("case(1) { 1: $display(); 2: $display(); }", ""),
          ("case(1) { 1: fence; 2: fence; }", ""),
          ("case(1) { 1: $display(); default: $display(); }", ""),
          ("case(1) { 1: fence; default: fence; }", ""),
          ("case(1) { 1: $display(); 2: fence; }",
           "Either all or no cases of a case statement must be control statements"),
          ("case(1) { 1: fence; 2: $display(); }",
           "Either all or no cases of a case statement must be control statements"),
          ("case(1) { 1: $display(); default: fence; }",
           "Either all or no cases of a case statement must be control statements"),
          ("case(1) { 1: fence; default: $display(); }",
           "Either all or no cases of a case statement must be control statements"),
          ("loop { fence; }", ""),
          ("loop { $display(); }", "Body of 'loop' must be a control statement"),
          ("loop { fence; $display(); }", "Body of 'loop' must end in a control statement")
        )
      } {
        stmt in {
          val result = xform(stmt.asTree[Stmt])
          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](msg)
            result shouldBe StmtError()
          }
        }
      }
    }

    "function bodies" - {
      for {
        (func, msg) <- List(
          ("void main () { fence; }", ""),
          ("void main () { $display(); }", "Body of function must end in a control statement"),
          ("void main () { fence; $display(); }",
           "Body of function must end in a control statement")
        )
      } {
        func in {
          val tree = s"""|fsm a {
                         | ${func}
                         |}""".stripMargin.asTree[Entity]
          xform(tree)
          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](msg)
          }
        }
      }
    }

    "fence blocks bodies" - {
      for {
        (fb, msg) <- List(
          ("fence { $display(); }", ""),
          ("fence { $display(); fence; }",
           "'fence' block must contain only combinatorial statements")
        )
      } {
        fb in {
          val tree = s"""|fsm a {
                         | ${fb}
                         |}""".stripMargin.asTree[Entity]
          xform(tree)
          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](msg)
          }
        }
      }
    }

    "declaration initializers" - {
      for {
        (decl, msg) <- List(
          ("i8 a = 2", ""),
          ("i8 a = 8'd2", ""),
          ("i8 a = bool", "Initializer expression is of non-packed type"),
          ("i8 a = 9'd2", "Initializer expression yields 9 bits, 8 bits are expected"),
          ("i8 a = 7'd2", "Initializer expression yields 7 bits, 8 bits are expected")
        )
      } {
        decl in {
          val tree = s"{ (* unused *) ${decl}; }".asTree[Stmt]
          xform(tree)
          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](msg)
          }
        }
      }
    }

    "assignments" - {
      for {
        (assignment, msg) <- List(
          ("a = 2", ""),
          ("a = 8'd2", ""),
          ("a = bool", "Right hand side of assignment is of non-packed type"),
          ("bool = 8'd2", "Left hand side of assignment is of non-packed type"),
          ("a += 8'd2", ""),
          ("a += bool", "Right hand side of assignment is of non-packed type"),
          ("bool += 8'd2", "Left hand side of assignment is of non-packed type"),
          ("a = 9'd2", "Right hand side of assignment yields 9 bits, 8 bits are expected"),
          ("a = 7'd2", "Right hand side of assignment yields 7 bits, 8 bits are expected")
        )
      } {
        assignment in {
          val tree = s"""|fsm x {
                         |  void main() {
                         |    (* unused *) i8 a;
                         |    ${assignment};
                         |    fence;
                         |  }
                         |}""".stripMargin.asTree[Entity]
          xform(tree)
          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](msg)
          }
        }
      }
    }

    "postfix increment/decrement" - {
      for (op <- List("++", "--")) {
        for {
          (assignment, msg) <- List(
            (s"a${op}", ""),
            (s"bool${op}", s"Target of postfix '${op}' is of non-packed type")
          )
        } {
          assignment in {
            val tree = s"""|fsm x {
                           |  void main() {
                           |    (* unused *) i8 a;
                           |    ${assignment};
                           |    fence;
                           |  }
                           |}""".stripMargin.asTree[Entity]
            xform(tree)
            if (msg.isEmpty) {
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](Pattern.quote(msg))
            }
          }
        }
      }
    }

    "assignments to illegal lhs" - {
      val iPortMsg = "Input port cannot be modified"
      val oPortMsg =
        Pattern.quote("Output port with flow control can only be modified using .write()")
      val paramMsg = "Parameter cannot be modified"
      val constMsg = "Constant cannot be modified"
      val memoryMsg = Pattern.quote("Memory can only be modified using .write()")

      for (op <- List("=", "+=")) {
        for {
          (assignment, msg) <- List(
            (s"a ${op} 8'd0", iPortMsg),
            (s"b ${op} 8'd0", ""),
            (s"c ${op} 8'd0", iPortMsg),
            (s"d ${op} 8'd0", oPortMsg),
            (s"e ${op} 8'd0", iPortMsg),
            (s"f ${op} 8'd0", oPortMsg),
            (s"g ${op} 8'd0", paramMsg),
            (s"h ${op} 8'd0", constMsg),
            (s"a[0] ${op} 1'b0", iPortMsg),
            (s"b[0] ${op} 1'b0", ""),
            (s"c[0] ${op} 1'b0", iPortMsg),
            (s"d[0] ${op} 1'b0", oPortMsg),
            (s"e[0] ${op} 1'b0", iPortMsg),
            (s"f[0] ${op} 1'b0", oPortMsg),
            (s"g[0] ${op} 1'b0", paramMsg),
            (s"h[0] ${op} 1'b0", constMsg),
            (s"i[0] ${op} 4'b0", memoryMsg),
            (s"a[1:0] ${op} 2'b0", iPortMsg),
            (s"b[1:0] ${op} 2'b0", ""),
            (s"c[1:0] ${op} 2'b0", iPortMsg),
            (s"d[1:0] ${op} 2'b0", oPortMsg),
            (s"e[1:0] ${op} 2'b0", iPortMsg),
            (s"f[1:0] ${op} 2'b0", oPortMsg),
            (s"g[1:0] ${op} 2'b0", paramMsg),
            (s"h[1:0] ${op} 2'b0", constMsg),
            (s"i[0][1:0] ${op} 2'b0", memoryMsg),
            (s"{b[1], a[0]} ${op} 2'b0", iPortMsg),
            (s"{b[1], b[0]} ${op} 2'b0", ""),
            (s"{b[1], c[0]} ${op} 2'b0", iPortMsg),
            (s"{b[1], d[0]} ${op} 2'b0", oPortMsg),
            (s"{b[1], e[0]} ${op} 2'b0", iPortMsg),
            (s"{b[1], f[0]} ${op} 2'b0", oPortMsg),
            (s"{b[1], g[0]} ${op} 2'b0", paramMsg),
            (s"{b[1], h[0]} ${op} 2'b0", constMsg),
            (s"{b[1], i[0]} ${op} 9'b0", memoryMsg)
          )
        } {
          assignment in {
            val tree = s"""|fsm x {
                           |  (* unused *) in i8 a;
                           |  (* unused *) out i8 b;
                           |  (* unused *) in sync i8 c;
                           |  (* unused *) out sync i8 d;
                           |  (* unused *) in sync ready i8 e;
                           |  (* unused *) out sync ready i8 f;
                           |  (* unused *) param i8 g = 8'd2;
                           |  (* unused *) const i8 h = 8'd2;
                           |  (* unused *) u8 i[4];
                           |
                           |  void main() {
                           |    ${assignment};
                           |    fence;
                           |  }
                           |}""".stripMargin.asTree[Entity]
            xform(tree)
            if (msg.isEmpty) {
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }

        }
      }
    }

    "postfix increment/decrement of illegal target" - {
      val iPortMsg = "Input port cannot be modified"
      val oPortMsg =
        Pattern.quote("Output port with flow control can only be modified using .write()")
      val paramMsg = "Parameter cannot be modified"
      val constMsg = "Constant cannot be modified"
      val memoryMsg = Pattern.quote("Memory can only be modified using .write()")

      for (op <- List("++", "--")) {
        for {
          (assignment, msg) <- List(
            (s"a${op}", iPortMsg),
            (s"b${op}", ""),
            (s"c${op}", iPortMsg),
            (s"d${op}", oPortMsg),
            (s"e${op}", iPortMsg),
            (s"f${op}", oPortMsg),
            (s"g${op}", paramMsg),
            (s"h${op}", constMsg),
            (s"a[0]${op}", iPortMsg),
            (s"b[0]${op}", ""),
            (s"c[0]${op}", iPortMsg),
            (s"d[0]${op}", oPortMsg),
            (s"e[0]${op}", iPortMsg),
            (s"f[0]${op}", oPortMsg),
            (s"g[0]${op}", paramMsg),
            (s"h[0]${op}", constMsg),
            (s"i[0]${op}", memoryMsg),
            (s"a[1:0]${op}", iPortMsg),
            (s"b[1:0]${op}", ""),
            (s"c[1:0]${op}", iPortMsg),
            (s"d[1:0]${op}", oPortMsg),
            (s"e[1:0]${op}", iPortMsg),
            (s"f[1:0]${op}", oPortMsg),
            (s"g[1:0]${op}", paramMsg),
            (s"h[1:0]${op}", constMsg),
            (s"i[0][1:0]${op}", memoryMsg),
            (s"{b[1], a[0]}${op}", iPortMsg),
            (s"{b[1], b[0]}${op}", ""),
            (s"{b[1], c[0]}${op}", iPortMsg),
            (s"{b[1], d[0]}${op}", oPortMsg),
            (s"{b[1], e[0]}${op}", iPortMsg),
            (s"{b[1], f[0]}${op}", oPortMsg),
            (s"{b[1], g[0]}${op}", paramMsg),
            (s"{b[1], h[0]}${op}", constMsg),
            (s"{b[1], i[0]}${op}", memoryMsg)
          )
        } {
          assignment in {
            val tree = s"""|fsm x {
                           |  (* unused *) in i8 a;
                           |  (* unused *) out i8 b;
                           |  (* unused *) in sync i8 c;
                           |  (* unused *) out sync i8 d;
                           |  (* unused *) in sync ready i8 e;
                           |  (* unused *) out sync ready i8 f;
                           |  (* unused *) param i8 g = 8'd2;
                           |  (* unused *) const i8 h = 8'd2;
                           |  (* unused *) u8 i[4];
                           |
                           |  void main() {
                           |    ${assignment};
                           |    fence;
                           |  }
                           |}""".stripMargin.asTree[Entity]
            xform(tree)
            if (msg.isEmpty) {
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }
        }
      }
    }

    "error for signals with non-positive width" - {
      for {
        (decl, width) <- List(
          ("uint(-'sd1) a", -1),
          ("uint(-'sd1) a = 0", -1),
          ("uint(0) a", 0),
          ("uint(0) a = 0", 0)
        )
      } {
        decl in {
          val tree = s"""|fsm tmp {
                         |  (* unused *) ${decl};
                         |}""".stripMargin.asTree[Entity]
          xform(tree)
          cc.messages.loneElement should beThe[Error](s"Signal 'a' has declared width ${width}")
        }
      }
    }

    "error for pure expressions in statement position" - {
      for {
        (text, warn) <- List(
          ("1 * 2", true),
          ("-(1)", true),
          ("@randbit()", false),
          ("1 + @randbit()", false)
        )
      } {
        text in {
          xform(s"${text};".asTree[Stmt])
          if (warn) {
            cc.messages.loneElement should beThe[Error](
              "A pure expression in statement position does nothing"
            )
          } else {
            cc.messages shouldBe empty
          }
        }
      }
    }
  }
}
