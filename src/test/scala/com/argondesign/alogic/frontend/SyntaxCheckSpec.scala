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

import java.util.regex.Pattern
import scala.collection.immutable.ListMap

final class SyntaxCheckSpec extends AnyFreeSpec with AlogicTest {

  implicit private val cc: CompilerContext = new CompilerContext

  "SyntaxCheck should" - {
    "check usage of pipeline port definitions" - {
      "accepting them in nested entities" - {
        for (desc <- List("in pipeline", "out pipeline")) {
          desc in {
            SyntaxCheck {
              s"""network a {
                 |  new fsm b {
                 |    $desc;
                 |    void main() {
                 |      fence;
                 |    }
                 |  }
                 |}""".stripMargin.asTree[Desc]()
            } shouldBe empty
          }
        }
      }

      "rejecting them in root entities" - {
        for (desc <- List("in pipeline", "out pipeline")) {
          desc in {
            SyntaxCheck {
              s"""fsm b {
                 |  $desc;
                 |  void main() {
                 |    fence;
                 |  }
                 |}""".stripMargin.asTree[Desc]()
            }.loneElement should beThe[Error](
              s"Pipeline ports are only allowed inside nested entities"
            )
          }
        }
      }
    }

    "ensure definition statements" - {
      "do not declare" - {
        "input ports" in {
          SyntaxCheck {
            "in bool a;".asTree[Stmt]()
          }.loneElement should beThe[Error](
            "Only variables can be defined in statement position"
          )
        }

        "output ports" in {
          SyntaxCheck {
            "out bool a;".asTree[Stmt]()
          }.loneElement should beThe[Error](
            "Only variables can be defined in statement position"
          )
        }

        "parameters" in {
          SyntaxCheck {
            "param bool a = false;".asTree[Stmt]()
          }.loneElement should beThe[Error](
            "Only variables can be defined in statement position"
          )
        }

        "constants" in {
          SyntaxCheck {
            "bool a[2];".asTree[Stmt](SourceContext.FuncCtrl)
          }.loneElement should beThe[Error](
            "Only variables can be defined in statement position"
          )
        }

        "arrays" in {
          SyntaxCheck {
            "bool a[2];".asTree[Stmt]()
          }.loneElement should beThe[Error](
            "Only variables can be defined in statement position"
          )
        }

        "pipeline variables" in {
          SyntaxCheck {
            "pipeline bool a;".asTree[Stmt]()
          }.loneElement should beThe[Error](
            "Only variables can be defined in statement position"
          )
        }

        "srams" in {
          SyntaxCheck {
            "sram bool a[1];".asTree[Stmt]()
          }.loneElement should beThe[Error](
            "Only variables can be defined in statement position"
          )
        }
      }

      "can declare" - {
        "variables" in {
          SyntaxCheck {
            "bool a;".asTree[Stmt]()
          } shouldBe empty
        }

        "const" in {
          SyntaxCheck {
            "const bool a = true;".asTree[Stmt](SourceContext.FuncCtrl)
          } shouldBe empty
        }

        "static" in {
          SyntaxCheck {
            "static bool a;".asTree[Stmt]()
          } shouldBe empty
        }
      }
    }

    "reject multiple fence blocks" in {
      val messages = SyntaxCheck {
        """fsm foo {
          |  fence {}
          |  fence {}
          |}""".stripMargin.asTree[Desc]()
      }

      messages should have length 2
      messages(0) should beThe[Error]("Multiple fence blocks specified in entity")
      messages(0).loc.line shouldBe 2
      messages(1) should beThe[Error]("Multiple fence blocks specified in entity")
      messages(1).loc.line shouldBe 3
    }

    "reject output slices on ports" - {
      "with no flow control" in {
        SyntaxCheck {
          """network a {
            |  out bubble i2 a;
            |}""".stripMargin.asTree[Desc]()
        }.loneElement should {
          beThe[Error]("Output port without flow control cannot use output slices")
        }
      }

      "with valid flow control" in {
        SyntaxCheck {
          """network a {
            |  out sync bubble i2 a;
            |}""".stripMargin.asTree[Desc]()
        }.loneElement should {
          beThe[Error]("Output port with 'sync' flow control cannot use output slices")
        }
      }
    }

    "reject case statements with multiple defaults" in {
      val messages = SyntaxCheck {
        """case(1) {
          | default: a;
          | default: b;
          |}""".stripMargin.asTree[Stmt]()
      }

      messages should have length 2
      messages(0) should beThe[Error]("Multiple 'default' clauses specified in case statement")
      messages(0).loc.line shouldBe 2
      messages(1) should beThe[Error]("Multiple 'default' clauses specified in case statement")
      messages(1).loc.line shouldBe 3
    }

    "reject case statements with default clasue not in last position" in {
      SyntaxCheck {
        """case(1) {
          | default: a;
          | 1: b;
          |}""".stripMargin.asTree[Stmt]()
      }.loneElement should beThe[Error](
        "'default' clause must come last in a case statement"
      )
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
            SyntaxCheck {
              text.stripMargin.asTree[DescPackage]()
            }.loneElement should beThe[Error](
              s"$hint definition cannot appear in file scope"
            )
          }
      }

      "non-static assertion in" in {
        SyntaxCheck {
          "assert false;".stripMargin.asTree[DescPackage]()
        }.loneElement should beThe[Error](
          s"Only static assertions are allowed in file scope"
        )
      }
    }

    "reject disallowed entity contents" - {

      "instantiations in" - {
        for (variant <- List("fsm", "verbatim entity")) {
          variant in {
            SyntaxCheck {
              s"""$variant a {
                 |  c = new d();
                 |}""".stripMargin.asTree[Desc]()
            }.loneElement should beThe[Error](s"'$variant' cannot contain instantiations")
          }
        }
      }

      "connections in" - {
        for (variant <- List("fsm", "verbatim entity")) {
          variant in {
            SyntaxCheck {
              s"""$variant a {
                 |  a -> b;
                 |}""".stripMargin.asTree[Desc]()
            }.loneElement should beThe[Error](s"'$variant' cannot contain connections")
          }
        }
      }

      "nested entitiy definition" - {
        for (variant <- List("fsm", "verbatim entity")) {
          variant in {
            SyntaxCheck {
              s"""$variant a {
                 |  fsm d {}
                 |}""".stripMargin.asTree[Desc]()
            }.loneElement should beThe[Error](
              s"'$variant' cannot contain nested entities"
            )
          }
        }
      }

      "nested singleton definition" - {
        for (variant <- List("fsm", "verbatim entity")) {
          variant in {
            SyntaxCheck {
              s"""$variant a {
                 |  new fsm d {}
                 |}""".stripMargin.asTree[Desc]()
            }.loneElement should beThe[Error](
              s"'$variant' cannot contain singleton entities"
            )
          }
        }
      }

      "functions in" - {
        for (variant <- List("network", "verbatim entity")) {
          variant in {
            SyntaxCheck {
              s"""$variant a {
                 |  void main() {}
                 |}""".stripMargin.asTree[Desc]()
            }.loneElement should beThe[Error](
              s"'$variant' cannot contain function definitions"
            )
          }
        }
      }

      s"fence blocks in" - {
        for (variant <- List("network", "verbatim entity")) {
          variant in {
            SyntaxCheck {
              s"""$variant a {
                 |  fence {}
                 |}""".stripMargin.asTree[Desc]()
            }.loneElement should beThe[Error](s"'$variant' cannot contain fence blocks")
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
            SyntaxCheck {
              s"""fsm a {
                 |  $decl;
                 |}""".stripMargin.asTree[Desc]()
            }.loneElement should beThe[Error](s"'fsm' cannot contain $msg definitions")
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
            SyntaxCheck {
              s"""struct s_t {
                 |  bool s;
                 |}
                 |
                 |network a {
                 |  $decl;
                 |}""".stripMargin.asTree[DescPackage]()
            }.loneElement should beThe[Error](
              s"'network' cannot contain $msg definitions"
            )
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
            SyntaxCheck {
              s"""struct s_t {
                 |  bool s;
                 |}
                 |
                 |verbatim entity a {
                 |  $decl;
                 |}""".stripMargin.asTree[DescPackage]()
            }.loneElement should beThe[Error](
              s"'verbatim entity' cannot contain $msg definitions"
            )
          }
        }
      }

      "non-static assertion in" - {
        for (variant <- List("network", "fsm", "verbatim entity")) {
          variant in {
            SyntaxCheck {
              s"""$variant a {
                 |  assert 0;
                 |}""".stripMargin.asTree[Desc]()
            }.loneElement should beThe[Error](
              s"'$variant' cannot contain non-static assertions"
            )
          }
        }
      }
    }

    "reject disallowed singleton contents" - {

      "parameters in" - {
        for (variant <- List("fsm", "network", "verbatim entity")) {
          variant in {
            SyntaxCheck {
              s"""network outer {
                 |  new $variant inner {
                 |    param u8 P = 0;
                 |  }
                 |}""".stripMargin.asTree[Desc]()
            }.loneElement should beThe[Error](
              s"Singleton entity cannot have parameters. Use a 'const' definition instead."
            )
          }
        }
      }

      "type parameters in" - {
        for (variant <- List("fsm", "network", "verbatim entity")) {
          variant in {
            SyntaxCheck {
              s"""network outer {
                 |  new $variant inner {
                 |    param type P = bool;
                 |  }
                 |}""".stripMargin.asTree[Desc]()
            }.loneElement should beThe[Error](
              s"Singleton entity cannot have type parameters. Use a 'typedef' instead."
            )
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
            SyntaxCheck {
              s"""struct a {
                 |  $decl

                 |}""".stripMargin.asTree[Desc]()
            }.loneElement should beThe[Error] {
              s"'struct' cannot contain $hint definitions"
            }
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
          "built-in call" -> "@bits(a)",
          "integer literal" -> "1",
          "string literal" -> """ "hello" """
        )

        for {
          (name, assign, msg) <- List(
            ("assignment", "%s = 1;", "Left hand side of '=' is not a valid assignment target"),
            ("update", "%s += 1;", "Left hand side of '+=' is not a valid assignment target"),
            ("postfix", "%s++;", "Operand of '++' is not a valid assignment target"),
            ("connect", "1 -> %s;", "Right hand side of '->' is not a valid assignment target")
          )
        } {
          def parse(string: String): Tree =
            if (string.contains("->")) string.asTree[Ent]() else string.asTree[Stmt]()

          name - {
            "simple invalid lvalues" - {
              for ((name, lval) <- badLvals) {
                name in {
                  SyntaxCheck {
                    parse(assign.format(lval))
                  }.loneElement should beThe[Error](Pattern.quote(msg))
                }
              }
            }

            "invalid lvalues inside concatenation lvalue" - {
              for ((name, lval) <- badLvals) {
                name in {
                  SyntaxCheck {
                    parse(assign.format(s"{x, $lval}"))
                  }.loneElement should beThe[Error](Pattern.quote(msg))
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
                  SyntaxCheck {
                    s"$lval $assign;".asTree[Stmt]()
                  } shouldBe empty
                }
              }
            }
            "nested valid lvalues" - {
              for ((name, lval) <- goodLvals) {
                name in {
                  SyntaxCheck {
                    s"{x, $lval} $assign;".asTree[Stmt]()
                  } shouldBe empty
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
          SyntaxCheck {
            s"""$entity a {
               | in bool b;
               | out bool c;
               | param i8 e = 2;
               | const i8 f = 2;
               |
               | verbatim verilog {}
               |}""".stripMargin.asTree[Desc]()
          }.loneElement should beThe[Warning](
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
            val messages = SyntaxCheck {
              s"""struct s {
                 |  i8 b;
                 |}
                 |
                 |$entity x {
                 |  $decl;
                 |}""".stripMargin.asTree[DescPackage]()
            }

            if (msg.nonEmpty) {
              messages.loneElement should beThe[Error](s"$msg cannot have an initializer")
            } else {
              messages shouldBe empty
            }
          }
        }
      }
    }

    "warn for concatenations" - {
      "containing only a single expression" in {
        SyntaxCheck {
          "{1'b1}".asTree[Expr]()
        }.loneElement should beThe[Warning](
          s"Single expression concatenation"
        )
      }

      "but not for 2 or more expressions" in {
        SyntaxCheck {
          "{1'b1, 1'b1}".asTree[Expr]()
        } shouldBe empty
      }
    }

    "check usage of break/continue statements" - {
      "accepting them in looping statements" - {
        for (word <- List("break", "continue")) {
          word in {
            SyntaxCheck {
              s"""fsm b {
                 |  void main() {
                 |    loop {
                 |      $word;
                 |    }
                 |  }
                 |}""".stripMargin.asTree[Desc]()
            } shouldBe empty
          }
        }
      }

      "rejecting them outside loops" - {
        for (word <- List("break", "continue")) {
          word in {
            SyntaxCheck {
              s"""fsm b {
                 |  void main() {
                 |    loop {}
                 |    $word;
                 |  }
                 |}""".stripMargin.asTree[Desc]()
            }.loneElement should beThe[Error](
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
          val messages = SyntaxCheck {
            s"""verbatim entity a {
               |  $decl;
               |}""".stripMargin.asTree[Desc]()
          }

          if (ok) {
            messages shouldBe empty
          } else {
            messages.loneElement should beThe[Error] {
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
          val messages = SyntaxCheck {
            s"""fsm f {
               |  void main() {
               |    goto $tgt;
               |  }
               |}""".stripMargin.asTree[DescPackage]()
          }

          if (ok) {
            messages shouldBe empty
          } else {
            messages.loneElement should beThe[Error](
              s"Target of 'goto' statement must be a function call expression"
            )
          }
        }
      }
    }
  }
}
