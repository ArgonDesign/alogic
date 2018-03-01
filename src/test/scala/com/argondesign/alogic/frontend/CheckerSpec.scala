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

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error

import org.scalatest.FreeSpec
import com.argondesign.alogic.core.FlowControlTypes._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.StorageTypes._

class CheckerSpec extends FreeSpec with AlogicTest {

  implicit val cc = new CompilerContext
  val checker = new Checker

  "The Checker" - {
    "should check @bits usage" - {
      "accepting well formed arguments" - {
        "simple identifiers" in {
          val expr = "@bits(a)".asTree[Expr]

          expr rewrite checker shouldBe expr

          cc.messages shouldBe empty
        }

        "single selects" in {
          val expr = "@bits(a.b)".asTree[Expr]

          expr rewrite checker shouldBe expr

          cc.messages shouldBe empty
        }

        "multiple selects" in {
          val expr = "@bits(a.b.c)".asTree[Expr]

          expr rewrite checker shouldBe expr

          cc.messages shouldBe empty
        }
      }

      "issuing error for other expressions" - {
        "binary" in {
          val expr = "@bits(a + b)".asTree[Expr]

          expr rewrite checker shouldBe ExprError()

          cc.messages.loneElement should beThe[Error](
            "Invalid expression passed to '@bits'",
            "Only identifiers, optionally followed by field lookups are allowed"
          )
        }
      }
    }

    "should check usage of read/write statements" - {
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

    "should ensure declaration statements" - {
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

    "should reject multiple fence blocks" in {
      val tree = """|fsm foo {
                    |  fence {}
                    |  fence {}
                    |}""".asTree[Entity]

      tree rewrite checker should matchPattern {
        case Entity(_, Nil, Nil, Nil, Nil, Nil, Nil, Nil, _) =>
      }

      cc.messages should have length 2
      cc.messages(0) should beThe[Error]("More than 1 fence blocks specified in entity 'foo'")
      cc.messages(0).loc.line shouldBe 2
      cc.messages(1) should beThe[Error]("More than 1 fence blocks specified in entity 'foo'")
      cc.messages(1).loc.line shouldBe 3
    }

    "should reject output slices on ports" - {
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

    "should reject case statements with multiple defaults" in {
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
  }

}
