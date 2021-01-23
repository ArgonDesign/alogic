////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-202- Argon Design Ltd. All rights reserved.
//
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
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeValid
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.Messages.Warning
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.SourceContext
import com.argondesign.alogic.core.StorageTypes.StorageTypeReg
import org.scalatest.freespec.AnyFreeSpec

import scala.collection.immutable.ListMap

final class SyntaxCheckSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext
  val checker = new SyntaxCheck

  "SyntaxCheck should" - {
    "check usage of pipeline port declarations" - {
      "accepting them in nested entities" - {
        for (desc <- List("in pipeline", "out pipeline")) {
          desc in {
            val tree = s"""network a {
                          |  new fsm b {
                          |    $desc;
                          |    void main() {
                          |      fence;
                          |    }
                          |  }
                          |}""".stripMargin.asTree[Desc]()

            tree rewrite checker shouldBe tree

            cc.messages shouldBe empty
          }
        }
      }

      "rejecting them in root entities" - {
        for (desc <- List("in pipeline", "out pipeline")) {
          desc in {
            val tree = s"""fsm b {
                          |  $desc;
                          |  void main() {
                          |    fence;
                          |  }
                          |}""".stripMargin.asTree[Desc]()

            tree rewrite checker

            cc.messages.loneElement should beThe[Error](
              s"Pipeline ports are only allowed inside nested entities"
            )
          }
        }
      }
    }

    "ensure declaration statements" - {
      "do not declare" - {
        "input ports" in {
          val tree = "in bool a;".asTree[Stmt]()

          tree rewrite checker shouldBe StmtError()

          cc.messages.loneElement should beThe[Error](
            "Only variables can be declared in declaration statements"
          )
        }

        "output ports" in {
          val tree = "out bool a;".asTree[Stmt]()

          tree rewrite checker shouldBe StmtError()

          cc.messages.loneElement should beThe[Error](
            "Only variables can be declared in declaration statements"
          )
        }

        "parameters" in {
          val tree = "param bool a = false;".asTree[Stmt]()

          tree rewrite checker shouldBe StmtError()

          cc.messages.loneElement should beThe[Error](
            "Only variables can be declared in declaration statements"
          )
        }

        "constants" in {
          val tree = "bool a[2];".asTree[Stmt](SourceContext.FuncCtrl)

          tree rewrite checker shouldBe StmtError()

          cc.messages.loneElement should beThe[Error](
            "Only variables can be declared in declaration statements"
          )
        }

        "arrays" in {
          val tree = "bool a[2];".asTree[Stmt]()

          tree rewrite checker shouldBe StmtError()

          cc.messages.loneElement should beThe[Error](
            "Only variables can be declared in declaration statements"
          )
        }

        "pipeline variables" in {
          val tree = "pipeline bool a;".asTree[Stmt]()

          tree rewrite checker shouldBe StmtError()

          cc.messages.loneElement should beThe[Error](
            "Only variables can be declared in declaration statements"
          )
        }

        "srams" in {
          val tree = "sram bool a[1];".asTree[Stmt]()

          tree rewrite checker shouldBe StmtError()

          cc.messages.loneElement should beThe[Error](
            "Only variables can be declared in declaration statements"
          )
        }
      }

      "can declare" - {
        "variables" in {
          val tree = "bool a;".asTree[Stmt]()
          tree rewrite checker should not be StmtError()
          cc.messages shouldBe empty
        }

        "const" in {
          val tree = "const bool a = true;".asTree[Stmt](SourceContext.FuncCtrl)
          tree rewrite checker should not be StmtError()
          cc.messages shouldBe empty
        }

        "static" in {
          val tree = "static bool a;".asTree[Stmt]()
          tree rewrite checker should not be StmtError()
          cc.messages shouldBe empty
        }
      }
    }

    "reject multiple fence blocks" in {
      val tree = """fsm foo {
                   |  fence {}
                   |  fence {}
                   |}""".stripMargin.asTree[Desc]()

      tree rewrite checker

      cc.messages should have length 2
      cc.messages(0) should beThe[Error]("Multiple fence blocks specified in entity")
      cc.messages(0).loc.line shouldBe 2
      cc.messages(1) should beThe[Error]("Multiple fence blocks specified in entity")
      cc.messages(1).loc.line shouldBe 3
    }

    "reject output slices on ports" - {
      "with no flow control" in {
        val entity = """network a {
                       |  out bubble i2 a;
                       |}""".stripMargin.asTree[Desc]()

        val tree = entity rewrite checker

        inside(tree) {
          case DescEntity(_, _, _, List(EntSplice(desc))) =>
            inside(desc) {
              case DescOut(_, _, _, fc, st, _) =>
                fc shouldBe FlowControlTypeNone
                st shouldBe StorageTypeReg
            }
        }

        cc.messages.loneElement should {
          beThe[Error]("Output port without flow control cannot use output slices")
        }
      }

      "with valid flow control" in {
        val entity = """network a {
                       |  out sync bubble i2 a;
                       |}""".stripMargin.asTree[Desc]()

        val tree = entity rewrite checker

        inside(tree) {
          case DescEntity(_, _, _, List(EntSplice(desc))) =>
            inside(desc) {
              case DescOut(_, _, _, fc, st, _) =>
                fc shouldBe FlowControlTypeValid
                st shouldBe StorageTypeReg
            }
        }

        cc.messages.loneElement should {
          beThe[Error]("Output port with 'sync' flow control cannot use output slices")
        }
      }
    }

    "reject case statements with multiple defaults" in {
      val tree = """case(1) {
                   | default: a;
                   | default: b;
                   |}""".stripMargin.asTree[Stmt]()

      tree rewrite checker shouldBe StmtError()

      cc.messages should have length 2
      cc.messages(0) should beThe[Error]("Multiple 'default' clauses specified in case statement")
      cc.messages(0).loc.line shouldBe 2
      cc.messages(1) should beThe[Error]("Multiple 'default' clauses specified in case statement")
      cc.messages(1).loc.line shouldBe 3
    }

    "reject disallowed entity contents" - {

      "instantiations in" - {
        for (variant <- List("fsm", "verbatim entity")) {
          variant in {
            val tree = s"""$variant a {
                          |  c = new d();
                          |}""".stripMargin.asTree[Desc]()

            tree rewrite checker should matchPattern {
              case DescEntity(_, _, _, Nil) =>
            }

            cc.messages.loneElement should beThe[Error](s"'$variant' cannot contain instantiations")
            cc.messages(0).loc.line shouldBe 2
          }
        }
      }

      "connections in" - {
        for (variant <- List("fsm", "verbatim entity")) {
          variant in {
            val tree = s"""$variant a {
                          |  a -> b;
                          |}""".stripMargin.asTree[Desc]()

            tree rewrite checker should matchPattern {
              case DescEntity(_, _, _, Nil) =>
            }

            cc.messages.loneElement should beThe[Error](s"'$variant' cannot contain connections")
            cc.messages(0).loc.line shouldBe 2
          }
        }
      }

      "nested entitiy definition" - {
        for (variant <- List("fsm", "verbatim entity")) {
          variant in {
            val tree = s"""$variant a {
                          |  fsm d {}
                          |}""".stripMargin.asTree[Desc]()

            tree rewrite checker should matchPattern {
              case DescEntity(_, _, _, Nil) =>
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
            val tree = s"""$variant a {
                          |  new fsm d {}
                          |}""".stripMargin.asTree[Desc]()

            tree rewrite checker should matchPattern {
              case DescEntity(_, _, _, Nil) =>
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
            val tree = s"""$variant a {
                          |  void main() {}
                          |}""".stripMargin.asTree[Desc]()

            tree rewrite checker should matchPattern {
              case DescEntity(_, _, _, Nil) =>
            }

            cc.messages.loneElement should beThe[Error](
              s"'$variant' cannot contain function definitions"
            )
            cc.messages(0).loc.line shouldBe 2
          }
        }
      }

      "static functions in" - {
        "fsm" in {
          val tree = s"""fsm a {
                        |  static void main() {}
                        |}""".stripMargin.asTree[Desc]()

          tree rewrite checker should matchPattern {
            case DescEntity(_, _, _, Nil) =>
          }

          cc.messages.loneElement should beThe[Error](
            s"'fsm' cannot contain static function definitions"
          )
          cc.messages(0).loc.line shouldBe 2
        }
      }

      s"fence blocks in" - {
        for (variant <- List("network", "verbatim entity")) {
          variant in {
            val tree = s"""$variant a {
                          |  fence {}
                          |}""".stripMargin.asTree[Desc]()

            tree rewrite checker should matchPattern {
              case DescEntity(_, _, _, Nil) =>
            }

            cc.messages.loneElement should beThe[Error](s"'$variant' cannot contain fence blocks")
            cc.messages(0).loc.line shouldBe 2
          }
        }
      }

      s"declarations in fsm" - {
        for {
          (decl, msg) <- List(
            ("pipeline i8 a", "pipeline variable")
          )
        } {
          decl in {
            val tree = s"""fsm a {
                          |  $decl;
                          |}""".stripMargin.asTree[Desc]()

            tree rewrite checker should matchPattern {
              case DescEntity(_, _, _, Nil) =>
            }

            cc.messages.loneElement should beThe[Error](s"'fsm' cannot contain $msg declarations")
            cc.messages(0).loc.line shouldBe 2
          }

        }
      }

      s"declarations in networks" - {
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
            val tree = s"""struct s_t {
                          |  bool s;
                          |}
                          |
                          |network a {
                          |  $decl;
                          |}""".stripMargin.asTree[DescPackage]()

            tree rewrite checker

            cc.messages.loneElement should beThe[Error](
              s"'network' cannot contain $msg declarations"
            )
            cc.messages(0).loc.line shouldBe 6
          }

        }
      }

      s"declarations in verbatim" - {
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
            val tree = s"""struct s_t {
                          |  bool s;
                          |}
                          |
                          |verbatim entity a {
                          |  $decl;
                          |}""".stripMargin.asTree[DescPackage]()

            tree rewrite checker

            cc.messages.loneElement should beThe[Error](
              s"'verbatim entity' cannot contain $msg declarations"
            )
            cc.messages(0).loc.line shouldBe 6
          }
        }
      }

      "non-static assertion in" - {
        for (variant <- List("network", "fsm", "verbatim entity")) {
          variant in {
            val tree = s"""$variant a {
                          |  assert 0;
                          |}""".stripMargin.asTree[Desc]()

            tree rewrite checker should matchPattern {
              case DescEntity(_, _, _, Nil) =>
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
            val tree = s"""network outer {
                          |  new $variant inner {
                          |    param u8 P = 0;
                          |  }
                          |}""".stripMargin.asTree[Desc]()

            tree rewrite checker should matchPattern {
              case DescEntity(_, _, _, EntSplice(DescSingleton(_, _, _, Nil)) :: Nil) =>
            }

            cc.messages.loneElement should beThe[Error](
              s"Singleton entity cannot have parameters. Use a 'const' declaration instead."
            )
            cc.messages.loneElement.loc.line shouldBe 3
          }
        }
      }

      "type parameters in" - {
        for (variant <- List("fsm", "network", "verbatim entity")) {
          variant in {
            val tree = s"""network outer {
                          |  new $variant inner {
                          |    param type P = bool;
                          |  }
                          |}""".stripMargin.asTree[Desc]()

            tree rewrite checker should matchPattern {
              case DescEntity(_, _, _, EntSplice(DescSingleton(_, _, _, Nil)) :: Nil) =>
            }

            cc.messages.loneElement should beThe[Error](
              s"Singleton entity cannot have type parameters. Use a 'typedef' instead."
            )
            cc.messages.loneElement.loc.line shouldBe 3
          }
        }
      }
    }

    "reject disallowed class contents" - {
      "declarations with disallowed type" - {
        for {
          (hint, decl) <- List(
            ("port", "in bool a;"),
            ("port", "out bool a;"),
            ("pipeline", "pipeline bool a;"),
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
            s"""struct a {
               |  $decl
               |}""".stripMargin.asTree[Desc]() rewrite checker should matchPattern {
              case DescRecord(_, _, Nil) =>
            }

            cc.messages.loneElement should beThe[Error] {
              s"'struct' cannot contain $hint declarations"
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
                  s"$lval $assign;".asTree[Stmt]() rewrite checker shouldBe a[StmtError]
                  cc.messages.loneElement should beThe[Error](
                    s"Invalid expression on left hand side of '$op'"
                  )
                }
              }
            }
            "invalid lvalues inside concatenation lvalue" - {
              for ((name, lval) <- badLvals) {
                name in {
                  s"{x, $lval} $assign;".asTree[Stmt]() rewrite checker shouldBe a[StmtError]
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
                  val stmt = s"$lval $assign;".asTree[Stmt]()
                  stmt rewrite checker should be theSameInstanceAs stmt
                  cc.messages shouldBe empty
                }
              }
            }
            "nested valid lvalues" - {
              for ((name, lval) <- goodLvals) {
                name in {
                  val stmt = s"{x, $lval} $assign;".asTree[Stmt]()
                  stmt rewrite checker should be theSameInstanceAs stmt
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
          val tree = s"""$entity a {
                        | in bool b;
                        | out bool c;
                        | param i8 e = 2;
                        | const i8 f = 2;
                        |
                        | verbatim verilog {}
                        |}""".stripMargin.asTree[Desc]()
          tree rewrite checker shouldBe a[Desc]
          cc.messages.loneElement should beThe[Warning](
            s"Entity contains only verbatim blocks, use a 'verbatim entity' instead"
          )
        }
      }

    }

    "Check initializer expressions" - {
      "are not present in declarations where disallowed" - {
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
            val tree = s"""struct s {
                          |  i8 b;
                          |}
                          |
                          |$entity x {
                          |  $decl;
                          |}""".stripMargin.asTree[DescPackage]()
            tree rewrite checker shouldBe a[DescPackage]
            if (msg.nonEmpty) {
              cc.messages.loneElement should beThe[Error](
                s"$msg cannot have an initializer"
              )
            } else {
              cc.messages shouldBe empty
            }
          }
        }
      }
    }

    "warn for concatenations" - {
      "containing only a single expression" in {
        val tree = "{1'b1}".asTree[Expr]()

        tree rewrite checker shouldBe tree

        cc.messages.loneElement should beThe[Warning](
          s"Single expression concatenation"
        )
      }

      "but not for 2 or more expressions" in {
        val tree = "{1'b1, 1'b1}".asTree[Expr]()

        tree rewrite checker shouldBe tree

        cc.messages shouldBe empty
      }
    }

    "check usage of break/continue statements" - {
      "accepting them in looping statements" - {
        for (word <- List("break", "continue")) {
          word in {
            val tree = s"""fsm b {
                          |  void main() {
                          |    loop {
                          |      $word;
                          |    }
                          |  }
                          |}""".stripMargin.asTree[Desc]()

            tree rewrite checker shouldBe tree

            cc.messages shouldBe empty
          }
        }
      }

      "rejecting them outside loops" - {
        for (word <- List("break", "continue")) {
          word in {
            val tree = s"""fsm b {
                          |  void main() {
                          |    loop {}
                          |    $word;
                          |  }
                          |}""".stripMargin.asTree[Desc]()

            val node = tree rewrite checker

            inside(node) {
              case DescEntity(_, _, _, List(EntSplice(main))) =>
                inside(main) {
                  case DescFunc(_, _, _, _, _, List(_, stmt)) =>
                    stmt shouldBe StmtError()
                }
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
          val tree = s"""verbatim entity a {
                        |  $decl;
                        |}""".stripMargin.asTree[Desc]()

          tree rewrite checker
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
          val tree = s"""fsm f {
                        |  void main() {
                        |    goto $tgt;
                        |  }
                        |}""".stripMargin.asTree[DescPackage]()

          tree rewrite checker

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
