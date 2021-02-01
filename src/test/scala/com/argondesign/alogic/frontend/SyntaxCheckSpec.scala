////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Test for SyntaxCheck
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.Messages.Warning
import com.argondesign.alogic.core.SourceContext
import org.scalatest.freespec.AnyFreeSpec

import scala.collection.immutable.ListMap

final class SyntaxCheckSpec extends AnyFreeSpec with AlogicTest {

  implicit private val cc: CompilerContext = new CompilerContext

  private def check(tree: Tree): Unit = {
    tree rewrite new SyntaxCheck should be theSameInstanceAs tree
  }

  "SyntaxCheck should" - {
    "check usage of pipeline port definitions" - {
      "accepting them in nested entities" - {
        for (desc <- List("in pipeline", "out pipeline")) {
          desc in {
            check {
              s"""network a {
                 |  new fsm b {
                 |    $desc;
                 |    void main() {
                 |      fence;
                 |    }
                 |  }
                 |}""".stripMargin.asTree[Desc]()
            }

            cc.messages shouldBe empty
          }
        }
      }

      "rejecting them in root entities" - {
        for (desc <- List("in pipeline", "out pipeline")) {
          desc in {
            check {
              s"""fsm b {
                 |  $desc;
                 |  void main() {
                 |    fence;
                 |  }
                 |}""".stripMargin.asTree[Desc]()
            }

            cc.messages.loneElement should beThe[Error](
              s"Pipeline ports are only allowed inside nested entities"
            )
          }
        }
      }
    }

    "ensure definition statements" - {
      "do not declare" - {
        "input ports" in {
          check {
            "in bool a;".asTree[Stmt]()
          }

          cc.messages.loneElement should beThe[Error](
            "Only variables can be defined in statement position"
          )
        }

        "output ports" in {
          check {
            "out bool a;".asTree[Stmt]()
          }

          cc.messages.loneElement should beThe[Error](
            "Only variables can be defined in statement position"
          )
        }

        "parameters" in {
          check {
            "param bool a = false;".asTree[Stmt]()
          }

          cc.messages.loneElement should beThe[Error](
            "Only variables can be defined in statement position"
          )
        }

        "constants" in {
          check {
            "bool a[2];".asTree[Stmt](SourceContext.FuncCtrl)
          }

          cc.messages.loneElement should beThe[Error](
            "Only variables can be defined in statement position"
          )
        }

        "arrays" in {
          check {
            "bool a[2];".asTree[Stmt]()
          }

          cc.messages.loneElement should beThe[Error](
            "Only variables can be defined in statement position"
          )
        }

        "pipeline variables" in {
          check {
            "pipeline bool a;".asTree[Stmt]()
          }

          cc.messages.loneElement should beThe[Error](
            "Only variables can be defined in statement position"
          )
        }

        "srams" in {
          check {
            "sram bool a[1];".asTree[Stmt]()
          }

          cc.messages.loneElement should beThe[Error](
            "Only variables can be defined in statement position"
          )
        }
      }

      "can declare" - {
        "variables" in {
          check {
            "bool a;".asTree[Stmt]()
          }

          cc.messages shouldBe empty
        }

        "const" in {
          check {
            "const bool a = true;".asTree[Stmt](SourceContext.FuncCtrl)
          }

          cc.messages shouldBe empty
        }

        "static" in {
          check {
            "static bool a;".asTree[Stmt]()
          }

          cc.messages shouldBe empty
        }
      }
    }

    "reject multiple fence blocks" in {
      check {
        """fsm foo {
          |  fence {}
          |  fence {}
          |}""".stripMargin.asTree[Desc]()
      }

      cc.messages should have length 2
      cc.messages(0) should beThe[Error]("Multiple fence blocks specified in entity")
      cc.messages(0).loc.line shouldBe 2
      cc.messages(1) should beThe[Error]("Multiple fence blocks specified in entity")
      cc.messages(1).loc.line shouldBe 3
    }

    "reject output slices on ports" - {
      "with no flow control" in {
        check {
          """network a {
            |  out bubble i2 a;
            |}""".stripMargin.asTree[Desc]()
        }

        cc.messages.loneElement should {
          beThe[Error]("Output port without flow control cannot use output slices")
        }
      }

      "with valid flow control" in {
        check {

          """network a {
            |  out sync bubble i2 a;
            |}""".stripMargin.asTree[Desc]()
        }

        cc.messages.loneElement should {
          beThe[Error]("Output port with 'sync' flow control cannot use output slices")
        }
      }
    }

    "reject case statements with multiple defaults" in {
      check {
        """case(1) {
          | default: a;
          | default: b;
          |}""".stripMargin.asTree[Stmt]()
      }

      cc.messages should have length 2
      cc.messages(0) should beThe[Error]("Multiple 'default' clauses specified in case statement")
      cc.messages(0).loc.line shouldBe 2
      cc.messages(1) should beThe[Error]("Multiple 'default' clauses specified in case statement")
      cc.messages(1).loc.line shouldBe 3
    }

    "reject case statements with default clasue not in last position" in {
      check {
        """case(1) {
          | default: a;
          | 1: b;
          |}""".stripMargin.asTree[Stmt]()
      }

      cc.messages.loneElement should beThe[Error](
        "'default' clause must come last in a case statement"
      )
      cc.messages.loneElement.loc.line shouldBe 2
    }

    "reject disallowed package contents" - {
      List(
        ("u8 a;", "Variable"),
        ("static u8 a;", "Variable"),
        ("pipeline u8 a;", "Variable"),
        ("in u8 a;", "Port"),
        ("out u8 a;", "Port"),
        ("i8 a[2];", "Distributed memory"),
        ("sram i8 a[2];", "SRAM"),
        ("c = new d();", "Instance"),
        ("new fsm f {}", "Singleton entity")
      ) foreach {
        case (text, hint) =>
          text in {
            check {
              text.stripMargin.asTree[DescPackage]()
            }
            cc.messages.loneElement should beThe[Error](
              s"$hint definition cannot appear in file scope"
            )
          }
      }

      "non-static assertion in" in {
        check {
          "assert false;".stripMargin.asTree[DescPackage]()
        }
        cc.messages.loneElement should beThe[Error](
          s"Only static assertions are allowed in file scope"
        )
      }
    }

    "reject disallowed entity contents" - {

      "instantiations in" - {
        for (variant <- List("fsm", "verbatim entity")) {
          variant in {
            check {
              s"""$variant a {
                 |  c = new d();
                 |}""".stripMargin.asTree[Desc]()
            }

            cc.messages.loneElement should beThe[Error](s"'$variant' cannot contain instantiations")
            cc.messages(0).loc.line shouldBe 2
          }
        }
      }

      "connections in" - {
        for (variant <- List("fsm", "verbatim entity")) {
          variant in {
            check {
              s"""$variant a {
                 |  a -> b;
                 |}""".stripMargin.asTree[Desc]()
            }

            cc.messages.loneElement should beThe[Error](s"'$variant' cannot contain connections")
            cc.messages(0).loc.line shouldBe 2
          }
        }
      }

      "nested entitiy definition" - {
        for (variant <- List("fsm", "verbatim entity")) {
          variant in {
            check {
              s"""$variant a {
                 |  fsm d {}
                 |}""".stripMargin.asTree[Desc]()
            }

            cc.messages.loneElement should beThe[Error](
              s"'$variant' cannot contain nested entities"
            )
            cc.messages(0).loc.line shouldBe 2
          }
        }
      }

      "nested singleton definition" - {
        for (variant <- List("fsm", "verbatim entity")) {
          variant in {
            check {
              s"""$variant a {
                 |  new fsm d {}
                 |}""".stripMargin.asTree[Desc]()
            }

            cc.messages.loneElement should beThe[Error](
              s"'$variant' cannot contain singleton entities"
            )
            cc.messages.loneElement.loc.line shouldBe 2
          }
        }
      }

      "functions in" - {
        for (variant <- List("network", "verbatim entity")) {
          variant in {
            check {
              s"""$variant a {
                 |  void main() {}
                 |}""".stripMargin.asTree[Desc]()
            }

            cc.messages.loneElement should beThe[Error](
              s"'$variant' cannot contain function definitions"
            )
            cc.messages(0).loc.line shouldBe 2
          }
        }
      }

      s"fence blocks in" - {
        for (variant <- List("network", "verbatim entity")) {
          variant in {
            check {
              s"""$variant a {
                 |  fence {}
                 |}""".stripMargin.asTree[Desc]()
            }

            cc.messages.loneElement should beThe[Error](s"'$variant' cannot contain fence blocks")
            cc.messages(0).loc.line shouldBe 2
          }
        }
      }

      s"definitions in fsm" - {
        for {
          (decl, msg) <- List(
            ("pipeline i8 a", "pipeline variable")
          )
        } {
          decl in {
            check {
              s"""fsm a {
                 |  $decl;
                 |}""".stripMargin.asTree[Desc]()
            }

            cc.messages.loneElement should beThe[Error](s"'fsm' cannot contain $msg definitions")
            cc.messages(0).loc.line shouldBe 2
          }
        }
      }

      s"definitions in networks" - {
        for {
          (decl, msg) <- List(
            ("i8 a", "variable"),
            ("i8 a[2]", "distributed memory"),
            ("sram i8 a[2]", "SRAM"),
            ("sram wire i8 a[2]", "SRAM"),
            ("sram s_t a[2]", "SRAM"),
            ("sram wire s_t a[2]", "SRAM")
          )
        } {
          decl in {
            check {
              s"""struct s_t {
                 |  bool s;
                 |}
                 |
                 |network a {
                 |  $decl;
                 |}""".stripMargin.asTree[DescPackage]()
            }

            cc.messages.loneElement should beThe[Error](
              s"'network' cannot contain $msg definitions"
            )
            cc.messages(0).loc.line shouldBe 6
          }

        }
      }

      s"definitions in verbatim" - {
        for {
          (decl, msg) <- List(
            ("i8 a", "variable"),
            ("i8 a[2]", "distributed memory"),
            ("pipeline i8 a", "pipeline variable"),
            ("sram s_t a[2]", "registered SRAM"),
            ("sram i8 a[2]", "registered SRAM")
          )
        } {
          decl in {
            check {
              s"""struct s_t {
                 |  bool s;
                 |}
                 |
                 |verbatim entity a {
                 |  $decl;
                 |}""".stripMargin.asTree[DescPackage]()
            }

            cc.messages.loneElement should beThe[Error](
              s"'verbatim entity' cannot contain $msg definitions"
            )
            cc.messages(0).loc.line shouldBe 6
          }
        }
      }

      "non-static assertion in" - {
        for (variant <- List("network", "fsm", "verbatim entity")) {
          variant in {
            check {
              s"""$variant a {
                 |  assert 0;
                 |}""".stripMargin.asTree[Desc]()
            }

            cc.messages.loneElement should beThe[Error](
              s"'$variant' cannot contain non-static assertions"
            )
            cc.messages(0).loc.line shouldBe 2
          }
        }
      }
    }

    "reject disallowed singleton contents" - {

      "parameters in" - {
        for (variant <- List("fsm", "network", "verbatim entity")) {
          variant in {
            check {
              s"""network outer {
                 |  new $variant inner {
                 |    param u8 P = 0;
                 |  }
                 |}""".stripMargin.asTree[Desc]()
            }

            cc.messages.loneElement should beThe[Error](
              s"Singleton entity cannot have parameters. Use a 'const' definition instead."
            )
            cc.messages.loneElement.loc.line shouldBe 3
          }
        }
      }

      "type parameters in" - {
        for (variant <- List("fsm", "network", "verbatim entity")) {
          variant in {
            check {
              s"""network outer {
                 |  new $variant inner {
                 |    param type P = bool;
                 |  }
                 |}""".stripMargin.asTree[Desc]()
            }

            cc.messages.loneElement should beThe[Error](
              s"Singleton entity cannot have type parameters. Use a 'typedef' instead."
            )
            cc.messages.loneElement.loc.line shouldBe 3
          }
        }
      }
    }

    "reject disallowed record contents" - {
      "definitions with disallowed type" - {
        for {
          (hint, decl) <- List(
            ("port", "in bool a;"),
            ("port", "out bool a;"),
            ("pipeline variable", "pipeline bool a;"),
            ("distributed memory", "bool a[10];"),
            ("SRAM", "sram bool a[10];"),
            ("entity", "fsm a {}"),
            ("entity", "network a {}"),
            ("entity", "verbatim entity a {}"),
            ("entity", "new fsm a {}"),
            ("entity", "new network a {}"),
            ("entity", "new verbatim entity a {}"),
            ("instance", "foo = new x;")
          )
        } {
          decl in {
            check {
              s"""struct a {
                 |  $decl

                 |}""".stripMargin.asTree[Desc]()
            }

            cc.messages.loneElement should beThe[Error] {
              s"'struct' cannot contain $hint definitions"
            }
            cc.messages(0).loc.line shouldBe 2
          }
        }
      }
    }

    "check lvalue expressions and" - {
      "reject" - {
        val badLvals = ListMap(
          "call" -> "a()",
          "unary ' " -> "'a",
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
                  check {
                    s"$lval $assign;".asTree[Stmt]()
                  }

                  cc.messages.loneElement should beThe[Error](
                    s"Invalid expression on left hand side of '$op'"
                  )
                }
              }
            }
            "invalid lvalues inside concatenation lvalue" - {
              for ((name, lval) <- badLvals) {
                name in {
                  check {
                    s"{x, $lval} $assign;".asTree[Stmt]()
                  }

                  cc.messages.loneElement should beThe[Error](
                    s"Invalid expression on left hand side of '$op'"
                  )
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
                  check {
                    s"$lval $assign;".asTree[Stmt]()
                  }

                  cc.messages shouldBe empty
                }
              }
            }
            "nested valid lvalues" - {
              for ((name, lval) <- goodLvals) {
                name in {
                  check {
                    s"{x, $lval} $assign;".asTree[Stmt]()
                  }

                  cc.messages shouldBe empty
                }
              }
            }
          }
        }
      }
    }

    "warn for non-verbatim entities with only verbatim contents" - {
      for (entity <- List("fsm", "network")) {
        entity in {
          check {
            s"""$entity a {
               | in bool b;
               | out bool c;
               | param i8 e = 2;
               | const i8 f = 2;
               |
               | verbatim verilog {}
               |}""".stripMargin.asTree[Desc]()
          }

          cc.messages.loneElement should beThe[Warning](
            s"Entity contains only verbatim blocks, use a 'verbatim entity' instead"
          )
        }
      }

    }

    "Check initializer expressions" - {
      "are not present in definitions where disallowed" - {
        for {
          (entity, decl, msg) <- List(
            // format: off
            ("fsm", "out wire i8 a = 0", "Output port with 'wire' storage specifier"),
            ("fsm", "out sync i8 a = 0", "Output port with 'sync' flow control"),
            ("fsm", "out sync ready i8 a = 0", "Output port with 'sync ready' flow control")
            // format: on
          )
        } {
          decl in {
            check {
              s"""struct s {
                 |  i8 b;
                 |}
                 |
                 |$entity x {
                 |  $decl;
                 |}""".stripMargin.asTree[DescPackage]()
            }

            if (msg.nonEmpty) {
              cc.messages.loneElement should beThe[Error](s"$msg cannot have an initializer")
            } else {
              cc.messages shouldBe empty
            }
          }
        }
      }
    }

    "warn for concatenations" - {
      "containing only a single expression" in {
        check {
          "{1'b1}".asTree[Expr]()
        }

        cc.messages.loneElement should beThe[Warning](
          s"Single expression concatenation"
        )
      }

      "but not for 2 or more expressions" in {
        check {
          "{1'b1, 1'b1}".asTree[Expr]()
        }

        cc.messages shouldBe empty
      }
    }

    "check usage of break/continue statements" - {
      "accepting them in looping statements" - {
        for (word <- List("break", "continue")) {
          word in {
            check {
              s"""fsm b {
                 |  void main() {
                 |    loop {
                 |      $word;
                 |    }
                 |  }
                 |}""".stripMargin.asTree[Desc]()
            }

            cc.messages shouldBe empty
          }
        }
      }

      "rejecting them outside loops" - {
        for (word <- List("break", "continue")) {
          word in {
            check {
              s"""fsm b {
                 |  void main() {
                 |    loop {}
                 |    $word;
                 |  }
                 |}""".stripMargin.asTree[Desc]()
            }

            cc.messages.loneElement should beThe[Error](
              s"${word.capitalize} statements are only allowed inside looping statements"
            )
          }
        }
      }
    }

    "error for verbatim entity output ports with storage specifiers" - {
      for {
        (decl, ok) <- List(
          ("out bool a", true),
          ("out wire bool a", false),
          ("out sync bool a", true),
          ("out sync wire bool a", false),
          ("out sync ready bool a", true),
          ("out sync ready bubble bool a", false),
          ("out sync ready fslice bool a", false),
          ("out sync ready bslice bool a", false),
          ("out sync ready wire bool a", false)
        )
      } {
        decl in {
          check {
            s"""verbatim entity a {
               |  $decl;
               |}""".stripMargin.asTree[Desc]()
          }

          if (ok) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error] {
              "'verbatim entity' output ports cannot use a storage specifier"
            }
          }
        }

      }
    }

    "check goto is to a call expression" - {
      for {
        (tgt, ok) <- List(
          ("a()", true),
          ("a.b()", true),
          ("a", false),
          ("1", false),
          ("bool", false)
        )
      } {
        tgt in {
          check {
            s"""fsm f {
               |  void main() {
               |    goto $tgt;
               |  }
               |}""".stripMargin.asTree[DescPackage]()
          }

          if (ok) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](
              s"Target of 'goto' statement must be a function call expression"
            )
          }
        }
      }
    }
  }
}
