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

package com.argondesign.alogic.frontend

import scala.collection.immutable.ListMap

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeValid
import com.argondesign.alogic.core.StorageTypes.StorageTypeReg
import com.argondesign.alogic.core.Types._

import org.scalatest.FreeSpec

final class CheckerSpec extends FreeSpec with AlogicTest {

  implicit val cc = new CompilerContext
  val checker = new Checker

  "The Checker should" - {
    "check usage of read/write statements" - {
      "accepting them in nested entities" - {
        for (word <- List("read", "write")) {
          word in {
            val tree = s"""|network a {
                           |  new fsm b {
                           |    void main() {
                           |      ${word};
                           |    }
                           |  }
                           |}""".asTree[Entity]

            tree rewrite checker shouldBe tree

            cc.messages shouldBe empty
          }
        }
      }

      "rejecting them in root entities" - {
        for (word <- List("read", "write")) {
          word in {
            val tree = s"""|fsm b {
                           |  void main() {
                           |    ${word};
                           |  }
                           |}""".asTree[Entity]

            val node = tree rewrite checker

            inside(node) {
              case Entity(_, _, _, _, List(main), _, _, _, _) =>
                inside(main) {
                  case Function(_, List(stmt)) =>
                    stmt shouldBe StmtError()
                }
            }

            cc.messages.loneElement should beThe[Error](
              s"${word.capitalize} statements are only allowed inside nested entities"
            )
          }
        }
      }
    }

    "ensure declaration statements" - {
      "do not declare" - {
        "input ports" in {
          val tree = "in bool a;".asTree[Stmt]

          tree rewrite checker shouldBe StmtError()

          cc.messages.loneElement should beThe[Error](
            "Only variables can be declared in declaration statements",
            "Input ports must be declared in the entity"
          )
        }

        "output ports" in {
          val tree = "out bool a;".asTree[Stmt]

          tree rewrite checker shouldBe StmtError()

          cc.messages.loneElement should beThe[Error](
            "Only variables can be declared in declaration statements",
            "Output ports must be declared in the entity"
          )
        }

        "parameters" in {
          val tree = "param bool a = false;".asTree[Stmt]

          tree rewrite checker shouldBe StmtError()

          cc.messages.loneElement should beThe[Error](
            "Only variables can be declared in declaration statements",
            "Parameters must be declared in the entity"
          )
        }

        "constants" in {
          val tree = "const bool a = false;".asTree[Stmt]

          tree rewrite checker shouldBe StmtError()

          cc.messages.loneElement should beThe[Error](
            "Only variables can be declared in declaration statements",
            "Constants must be declared in the entity"
          )
        }

        "arrays" in {
          val tree = "bool a[2];".asTree[Stmt]

          tree rewrite checker shouldBe StmtError()

          cc.messages.loneElement should beThe[Error](
            "Only variables can be declared in declaration statements",
            "Arrays must be declared in the entity"
          )
        }

        "pipeline variables" in {
          val tree = "pipeline bool a;".asTree[Stmt]

          tree rewrite checker shouldBe StmtError()

          cc.messages.loneElement should beThe[Error](
            "Only variables can be declared in declaration statements",
            "Pipeline variables must be declared in the outer entity"
          )
        }
      }

      "can declare" - {
        "scalars" in {
          val tree = "bool a;".asTree[Stmt]

          tree rewrite checker should not be StmtError()

          cc.messages shouldBe empty
        }

        "vectors" in {
          val tree = "int(2, 3) a;".asTree[Stmt]

          tree rewrite checker should not be StmtError()

          cc.messages shouldBe empty
        }
      }
    }

    "reject multiple fence blocks" in {
      val tree = """|fsm foo {
                    |  fence {}
                    |  fence {}
                    |}""".asTree[Entity]

      tree rewrite checker should matchPattern {
        case Entity(_, Nil, Nil, Nil, Nil, Nil, Nil, Nil, _) =>
      }

      cc.messages should have length 2
      cc.messages(0) should beThe[Error]("Multiple fence blocks specified in entity 'foo'")
      cc.messages(0).loc.line shouldBe 2
      cc.messages(1) should beThe[Error]("Multiple fence blocks specified in entity 'foo'")
      cc.messages(1).loc.line shouldBe 3
    }

    "reject output slices on ports" - {
      "with no flow control" in {
        val entity = """|network a {
                        |  out bubble i2 a;
                        |}""".asTree[Entity]

        val tree = entity rewrite checker

        inside(tree) {
          case Entity(_, List(decl), Nil, Nil, Nil, Nil, Nil, Nil, _) =>
            inside(decl) {
              case Decl(_, TypeOut(_, fc, st), _) =>
                fc shouldBe FlowControlTypeNone
                st shouldBe StorageTypeReg
            }
        }

        cc.messages.loneElement should {
          beThe[Error]("Output port 'a' without flow control specifier cannot use output slices")
        }
      }

      "with valid flow control" in {
        val entity = """|network a {
                        |  out sync bubble i2 a;
                        |}""".asTree[Entity]

        val tree = entity rewrite checker

        inside(tree) {
          case Entity(_, List(decl), Nil, Nil, Nil, Nil, Nil, Nil, _) =>
            inside(decl) {
              case Decl(_, TypeOut(_, fc, st), _) =>
                fc shouldBe FlowControlTypeValid
                st shouldBe StorageTypeReg
            }
        }

        cc.messages.loneElement should {
          beThe[Error]("Output port 'a' with 'sync' flow control specifier cannot use output slices")
        }
      }
    }

    "reject case statements with multiple defaults" in {
      val tree = """|case(1) {
                    | default: a;
                    | default: b;
                    |}""".asTree[Stmt]

      tree rewrite checker should matchPattern {
        case StmtCase(_, _, Nil) =>
      }

      cc.messages should have length 2
      cc.messages(0) should beThe[Error]("Multiple 'default' clauses specified in case statement")
      cc.messages(0).loc.line shouldBe 2
      cc.messages(1) should beThe[Error]("Multiple 'default' clauses specified in case statement")
      cc.messages(1).loc.line shouldBe 3
    }

    "reject disallowed entity contents" - {
      "instantiations in" - {
        for (variant <- List("fsm", "verilog")) {
          variant in {
            val tree = s"""|${variant} a {
                           |  c = new d();
                           |}""".asTree[Entity]

            tree rewrite checker should matchPattern {
              case Entity(_, Nil, Nil, Nil, Nil, Nil, Nil, Nil, _) =>
            }

            cc.messages.loneElement should beThe[Error](s"'${variant}' entity cannot contain instantiations")
            cc.messages(0).loc.line shouldBe 2
          }
        }
      }

      "connections in" - {
        for (variant <- List("fsm", "verilog")) {
          variant in {
            val tree = s"""|${variant} a {
                           |  a -> b;
                           |}""".asTree[Entity]

            tree rewrite checker should matchPattern {
              case Entity(_, Nil, Nil, Nil, Nil, Nil, Nil, Nil, _) =>
            }

            cc.messages.loneElement should beThe[Error](s"'${variant}' entity cannot contain connections")
            cc.messages(0).loc.line shouldBe 2
          }
        }
      }

      "nested entities without autoinst in" - {
        for (variant <- List("fsm", "verilog")) {
          variant in {
            val tree = s"""|${variant} a {
                           |  fsm d {}
                           |}""".asTree[Entity]

            tree rewrite checker should matchPattern {
              case Entity(_, Nil, Nil, Nil, Nil, Nil, Nil, Nil, _) =>
            }

            cc.messages.loneElement should beThe[Error](s"'${variant}' entity cannot contain nested entities")
            cc.messages(0).loc.line shouldBe 2
          }
        }
      }

      "nested entities with autoinst in" - {
        for (variant <- List("fsm", "verilog")) {
          variant in {
            val tree = s"""|${variant} a {
                           |  new fsm d {}
                           |}""".asTree[Entity]

            tree rewrite checker should matchPattern {
              case Entity(_, Nil, Nil, Nil, Nil, Nil, Nil, Nil, _) =>
            }

            cc.messages should have length 2
            cc.messages(0) should beThe[Error](s"'${variant}' entity cannot contain instantiations")
            cc.messages(0).loc.line shouldBe 2
            cc.messages(1) should beThe[Error](s"'${variant}' entity cannot contain nested entities")
            cc.messages(1).loc.line shouldBe 2
          }
        }
      }

      "functions in" - {
        for (variant <- List("network", "verilog")) {
          variant in {
            val tree = s"""|${variant} a {
                           |  void main() {}
                           |}""".asTree[Entity]

            tree rewrite checker should matchPattern {
              case Entity(_, Nil, Nil, Nil, Nil, Nil, Nil, Nil, _) =>
            }

            cc.messages.loneElement should beThe[Error](s"'${variant}' entity cannot contain function definitions")
            cc.messages(0).loc.line shouldBe 2
          }
        }
      }

      s"fence blocks in" - {
        for (variant <- List("network", "verilog")) {
          variant in {
            val tree = s"""|${variant} a {
                           |  fence {}
                           |}""".asTree[Entity]

            tree rewrite checker should matchPattern {
              case Entity(_, Nil, Nil, Nil, Nil, Nil, Nil, Nil, _) =>
            }

            cc.messages.loneElement should beThe[Error](s"'${variant}' entity cannot contain fence blocks")
            cc.messages(0).loc.line shouldBe 2
          }
        }
      }
    }

    "check lvalue expressions and" - {
      "reject" - {
        val badLvals = ListMap(
          "call" -> "a()",
          "unary - " -> "-a",
          "binary +" -> "a+b",
          "ternary" -> "a ? b : c",
          "rep" -> "{2{a}}",
          "@ call" -> "@bits(a)",
          "$ call" -> "$display(a)",
          "integer literal" -> "1",
          "string literal" -> """ "hello" """
        )

        for {
          (name, assign, op) <- List(
            ("assignment", "= 1", "="),
            ("update", "+= 1", "\\+="),
            ("postfix", "++", "\\+\\+")
          )
        } {
          name - {
            "simple invalid lvalues" - {
              for ((name, lval) <- badLvals) {
                name in {
                  s"${lval} ${assign};".asTree[Stmt] rewrite checker shouldBe a[StmtError]
                  cc.messages.loneElement should beThe[Error](s"Invalid expression on left hand side of '${op}'")
                }
              }
            }
            "invalid lvalues inside otherwise valid lvalue expressions" - {
              for ((name, lval) <- badLvals) {
                name in {
                  s"{x, ${lval}} ${assign};".asTree[Stmt] rewrite checker shouldBe a[StmtError]
                  cc.messages.loneElement should beThe[Error](s"Invalid expression on left hand side of '${op}'")
                }
              }
            }
          }
        }
      }

      "accept" - {
        val goodLvals = ListMap(
          "identifier " -> "a",
          "index" -> "a[2]",
          "slice" -> "a[2:1]",
          "select" -> "a.b",
          "cat" -> "{b, a}"
        )

        for {
          (name, assign) <- List(
            ("assignment", "= 1"),
            ("update", "+= 1"),
            ("postfix", "++")
          )
        } {
          name - {
            "simple valid lvalues" - {
              for ((name, lval) <- goodLvals) {
                name in {
                  val stmt = s"${lval} ${assign};".asTree[Stmt]
                  stmt rewrite checker should be theSameInstanceAs stmt
                  cc.messages shouldBe empty
                }
              }
            }
            "nested valid lvalues" - {
              for ((name, lval) <- goodLvals) {
                name in {
                  val stmt = s"{x, ${lval}} ${assign};".asTree[Stmt]
                  stmt rewrite checker should be theSameInstanceAs stmt
                  cc.messages shouldBe empty
                }
              }
            }
          }
        }
      }
    }

    "check connect expressions and" - {
      "reject invalid port references on" - {
        def check(test: String => Unit) = {
          "call" in test("a()")
          "unary - " in test("-a")
          "binary +" in test("a+b")
          "ternary" in test("a ? b : c")
          "rep" in test("{2{a}}")
          "@ call" in test("@bits(a)")
          "$ call" in test("$display(a)")
          "integer literal" in test("1")
          "string literal" in test(""" "hello" """)
          "index" in test("a[2]")
          "slice" in test("a[2:1]")
          "cat" in test("{b, a}")
          "multi-select" in test("a.b.c")
        }
        "left hand side of ->" - {
          check { ref =>
            val tree = s"${ref} -> b".asTree[Connect] rewrite checker
            tree should matchPattern {
              case Connect(ExprError(), List(_)) =>
            }
            cc.messages.loneElement should beThe[Error](
              s"Invalid port reference on left hand side of '->'",
              "Only identifiers, optionally followed by a single field selector are allowed"
            )
          }
        }
        "right hand side of -> in first position" - {
          check { ref =>
            val tree = s"a -> ${ref}".asTree[Connect] rewrite checker
            tree shouldBe Connect(ExprRef(Ident("a")), Nil)
            cc.messages.loneElement should beThe[Error](
              s"Invalid port reference on right hand side of '->'",
              "Only identifiers, optionally followed by a single field selector are allowed"
            )
          }
        }
        "right hand side of -> in second position" - {
          check { ref =>
            val tree = s"a -> b, ${ref}".asTree[Connect] rewrite checker
            tree shouldBe Connect(ExprRef(Ident("a")), List(ExprRef(Ident("b"))))
            cc.messages.loneElement should beThe[Error](
              s"Invalid port reference on right hand side of '->'",
              "Only identifiers, optionally followed by a single field selector are allowed"
            )
          }
        }
      }

      "accept valid port referencess on " - {
        def check(test: String => Unit) = {
          "identifier " in test("a")
          "single-select" in test("a.b")
        }
        "left hand side of ->" - {
          check { ref =>
            val connect = s"${ref} -> b".asTree[Connect]
            connect rewrite checker should be theSameInstanceAs connect
            cc.messages shouldBe empty
          }
        }
        "right hand side of -> in first position" - {
          check { ref =>
            val connect = s"a -> ${ref}".asTree[Connect]
            connect rewrite checker should be theSameInstanceAs connect
            cc.messages shouldBe empty
          }
        }
        "right hand side of -> in second position" - {
          check { ref =>
            val connect = s"a -> b, ${ref}".asTree[Connect]
            connect rewrite checker should be theSameInstanceAs connect
            cc.messages shouldBe empty
          }
        }
      }
    }
  }

}
