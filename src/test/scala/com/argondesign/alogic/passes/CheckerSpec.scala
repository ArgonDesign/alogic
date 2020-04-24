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

package com.argondesign.alogic.passes

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeValid
import com.argondesign.alogic.core.StorageTypes.StorageTypeReg
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Warning
import org.scalatest.freespec.AnyFreeSpec

import scala.collection.immutable.ListMap

final class CheckerSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext
  val checker = new Checker

  "The Checker should" - {
    "check usage of read/write statements" - {
      "accepting them in nested entities" - {
        for (word <- List("read", "write")) {
          word in {
            val tree = s"""|network a {
                           |  new fsm b {
                           |    void main() {
                           |      $word;
                           |    }
                           |  }
                           |}""".stripMargin.asTree[Desc]

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
                           |    $word;
                           |  }
                           |}""".stripMargin.asTree[Desc]

            val node = tree rewrite checker

            inside(node) {
              case DescEntity(_, _, List(EntDesc(main))) =>
                inside(main) {
                  case DescFunc(_, _, _, _, List(stmt)) =>
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
            "Only variables can be declared in declaration statements"
          )
        }

        "output ports" in {
          val tree = "out bool a;".asTree[Stmt]

          tree rewrite checker shouldBe StmtError()

          cc.messages.loneElement should beThe[Error](
            "Only variables can be declared in declaration statements"
          )
        }

        "parameters" in {
          val tree = "param bool a = false;".asTree[Stmt]

          tree rewrite checker shouldBe StmtError()

          cc.messages.loneElement should beThe[Error](
            "Only variables can be declared in declaration statements"
          )
        }

        "constants" in {
          val tree = StmtDesc("const bool a = false;".asTree[Desc]) withLoc Loc.synthetic

          tree rewrite checker shouldBe StmtError()

          cc.messages.loneElement should beThe[Error](
            "Only variables can be declared in declaration statements"
          )
        }

        "arrays" in {
          val tree = "bool a[2];".asTree[Stmt]

          tree rewrite checker shouldBe StmtError()

          cc.messages.loneElement should beThe[Error](
            "Only variables can be declared in declaration statements"
          )
        }

        "pipeline variables" in {
          val tree = "pipeline bool a;".asTree[Stmt]

          tree rewrite checker shouldBe StmtError()

          cc.messages.loneElement should beThe[Error](
            "Only variables can be declared in declaration statements"
          )
        }

        "srams" in {
          val tree = "sram bool a[1];".asTree[Stmt]

          tree rewrite checker shouldBe StmtError()

          cc.messages.loneElement should beThe[Error](
            "Only variables can be declared in declaration statements"
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
          val tree = "i3[2] a;".asTree[Stmt]

          tree rewrite checker should not be StmtError()

          cc.messages shouldBe empty
        }
      }
    }

    // TODO: Move to Typer
    "ensure only compile time known values are declared as unsized numbers" ignore {
      for {
        (src, err) <- List(
          ("param int a = 1;", ""),
          ("param uint a = 1;", ""),
          ("const int a = 1;", ""),
          ("const uint a = 1;", ""),
          ("pipeline int a;", "int"),
          ("pipeline uint a;", "uint"),
          ("in int a;", "int"),
          ("in uint a;", "uint"),
          ("out int a;", "int"),
          ("out uint a;", "uint"),
          ("int a[1];", "int"),
          ("uint a[1];", "uint"),
          ("sram int a[1];", "int"),
          ("sram uint a[1];", "uint"),
          ("int[1] a;", "int"),
          ("uint[1] a;", "uint"),
          ("int[2][3] a[1];", "int"),
          ("uint[2][3] a[1];", "uint"),
          ("int a;", "int"),
          ("uint a;", "uint")
        )
      } {
        src in {
          val tree = src.asTree[Desc]
          tree rewrite checker

          if (err.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](
              s"Only compile time constant scalars can be declared with type '$err'"
            )
          }
        }
      }
    }

    "reject multiple fence blocks" in {
      val tree = """|fsm foo {
                    |  fence {}
                    |  fence {}
                    |}""".stripMargin.asTree[Desc]

      tree rewrite checker

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
                        |}""".stripMargin.asTree[Desc]

        val tree = entity rewrite checker

        inside(tree) {
          case DescEntity(_, _, List(EntDesc(desc))) =>
            inside(desc) {
              case DescOut(_, _, fc, st, _) =>
                fc shouldBe FlowControlTypeNone
                st shouldBe StorageTypeReg
            }
        }

        cc.messages.loneElement should {
          beThe[Error]("Output port 'a' without flow control cannot use output slices")
        }
      }

      "with valid flow control" in {
        val entity = """|network a {
                        |  out sync bubble i2 a;
                        |}""".stripMargin.asTree[Desc]

        val tree = entity rewrite checker

        inside(tree) {
          case DescEntity(_, _, List(EntDesc(desc))) =>
            inside(desc) {
              case DescOut(_, _, fc, st, _) =>
                fc shouldBe FlowControlTypeValid
                st shouldBe StorageTypeReg
            }
        }

        cc.messages.loneElement should {
          beThe[Error]("Output port 'a' with 'sync' flow control cannot use output slices")
        }
      }
    }

    "reject case statements with multiple defaults" in {
      val tree = """|case(1) {
                    | default: a;
                    | default: b;
                    |}""".stripMargin.asTree[Stmt]

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
            val tree = s"""|$variant a {
                           |  c = new d();
                           |}""".stripMargin.asTree[Desc]

            tree rewrite checker should matchPattern {
              case DescEntity(_, _, Nil) =>
            }

            cc.messages.loneElement should beThe[Error](s"'$variant' cannot contain instantiations")
            cc.messages(0).loc.line shouldBe 2
          }
        }
      }

      "connections in" - {
        for (variant <- List("fsm", "verbatim entity")) {
          variant in {
            val tree = s"""|$variant a {
                           |  a -> b;
                           |}""".stripMargin.asTree[Desc]

            tree rewrite checker should matchPattern {
              case DescEntity(_, _, Nil) =>
            }

            cc.messages.loneElement should beThe[Error](s"'$variant' cannot contain connections")
            cc.messages(0).loc.line shouldBe 2
          }
        }
      }

      "nested entitiy definition" - {
        for (variant <- List("fsm", "verbatim entity")) {
          variant in {
            val tree = s"""|$variant a {
                           |  fsm d {}
                           |}""".stripMargin.asTree[Desc]

            tree rewrite checker should matchPattern {
              case DescEntity(_, _, Nil) =>
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
            val tree = s"""|$variant a {
                           |  new fsm d {}
                           |}""".stripMargin.asTree[Desc]

            tree rewrite checker should matchPattern {
              case DescEntity(_, _, Nil) =>
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
            val tree = s"""|$variant a {
                           |  void main() {}
                           |}""".stripMargin.asTree[Desc]

            tree rewrite checker should matchPattern {
              case DescEntity(_, _, Nil) =>
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
          val tree = s"""|fsm a {
                         |  static void main() {}
                         |}""".stripMargin.asTree[Desc]

          tree rewrite checker should matchPattern {
            case DescEntity(_, _, Nil) =>
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
            val tree = s"""|$variant a {
                           |  fence {}
                           |}""".stripMargin.asTree[Desc]

            tree rewrite checker should matchPattern {
              case DescEntity(_, _, Nil) =>
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
            val tree = s"""|fsm a {
                           |  $decl;
                           |}""".stripMargin.asTree[Desc]

            tree rewrite checker should matchPattern {
              case DescEntity(_, _, Nil) =>
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
            val tree = s"""|struct s_t {
                           |  bool s;
                           |}
                           |
                           |network a {
                           |  $decl;
                           |}""".stripMargin.asTree[Root]

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
            val tree = s"""|struct s_t {
                           |  bool s;
                           |}
                           |
                           |verbatim entity a {
                           |  $decl;
                           |}""".stripMargin.asTree[Root]

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
            val tree = s"""|$variant a {
                           |  assert 0;
                           |}""".stripMargin.asTree[Desc]

            tree rewrite checker should matchPattern {
              case DescEntity(_, _, Nil) =>
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
            val tree = s"""|network outer {
                           |  new $variant inner {
                           |    param u8 P = 0;
                           |  }
                           |}""".stripMargin.asTree[Desc]

            tree rewrite checker should matchPattern {
              case DescEntity(_, _, EntDesc(DescSingleton(_, _, Nil)) :: Nil) =>
            }

            cc.messages.loneElement should beThe[Error](
              s"Singleton entity cannot have parameters. Use a 'const' declaration instead."
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
            s"""|struct a {
                |  $decl
                |}""".stripMargin.asTree[Desc] rewrite checker should matchPattern {
              case DescRecord(_, Nil) =>
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
                  s"$lval $assign;".asTree[Stmt] rewrite checker shouldBe a[StmtError]
                  cc.messages.loneElement should beThe[Error](
                    s"Invalid expression on left hand side of '$op'"
                  )
                }
              }
            }
            "invalid lvalues inside concatenation lvalue" - {
              for ((name, lval) <- badLvals) {
                name in {
                  s"{x, $lval} $assign;".asTree[Stmt] rewrite checker shouldBe a[StmtError]
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
                  val stmt = s"$lval $assign;".asTree[Stmt]
                  stmt rewrite checker should be theSameInstanceAs stmt
                  cc.messages shouldBe empty
                }
              }
            }
            "nested valid lvalues" - {
              for ((name, lval) <- goodLvals) {
                name in {
                  val stmt = s"{x, $lval} $assign;".asTree[Stmt]
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
          val tree = s"""|$entity a {
                         | in bool b;
                         | out bool c;
                         | param i8 e = 2;
                         | const i8 f = 2;
                         |
                         | verbatim verilog {}
                         |}""".stripMargin.asTree[Desc]
          tree rewrite checker shouldBe a[Desc]
          cc.messages.loneElement should beThe[Warning](
            s"Entity 'a' contains only verbatim blocks, use a 'verbatim entity' instead"
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
            ("fsm", "out sync ready i8 a = 0", "Output port with 'sync ready' flow control"),
            ("fsm", "out sync accept i8 a = 0", "Output port with 'sync accept' flow control")
            // format: on
          )
        } {
          decl in {
            val tree = s"""|struct s {
                           |  i8 b;
                           |}
                           |
                           |$entity x {
                           |  $decl;
                           |}""".stripMargin.asTree[Root]
            tree rewrite checker shouldBe a[Root]
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
        val tree = "{1'b1}".asTree[Expr]

        tree rewrite checker shouldBe tree

        cc.messages.loneElement should beThe[Warning](
          s"Single expression concatenation"
        )
      }

      "but not for 2 or more expressions" in {
        val tree = "{1'b1, 1'b1}".asTree[Expr]

        tree rewrite checker shouldBe tree

        cc.messages shouldBe empty
      }
    }

    "check usage of break/continue statements" - {
      "accepting them in looping statements" - {
        for (word <- List("break", "continue")) {
          word in {
            val tree = s"""|fsm b {
                           |  void main() {
                           |    loop {
                           |      $word;
                           |    }
                           |  }
                           |}""".stripMargin.asTree[Desc]

            tree rewrite checker shouldBe tree

            cc.messages shouldBe empty
          }
        }
      }

      "rejecting them outside loops" - {
        for (word <- List("break", "continue")) {
          word in {
            val tree = s"""|fsm b {
                           |  void main() {
                           |    loop {}
                           |    $word;
                           |  }
                           |}""".stripMargin.asTree[Desc]

            val node = tree rewrite checker

            inside(node) {
              case DescEntity(_, _, List(EntDesc(main))) =>
                inside(main) {
                  case DescFunc(_, _, _, _, List(_, stmt)) =>
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

    "check port, const and param declarations in entities precede" ignore {
      for {
        (decl, hint, genOK) <- List(
          ("in bool p", "Port", true),
          ("out bool p", "Port", true),
          ("param bool p", "Parameter", false),
          ("const bool p = false", "Constant", false)
        )
      } {
        decl - {
          "nested classes" in {
            val tree = s"""|network a {
                           |  struct b {}
                           |  $decl;
                           |}""".stripMargin.asTree[Desc]

            tree rewrite checker

            cc.messages.loneElement should beThe[Error](
              s"$hint declarations must appear before nested 'struct' definitions"
            )
          }

          "nested entities" in {
            val tree = s"""|network a {
                           |  fsm b {}
                           |  $decl;
                           |}""".stripMargin.asTree[Desc]

            tree rewrite checker

            cc.messages.loneElement should beThe[Error](
              s"$hint declarations must appear before nested entities"
            )
          }

          "instances" in {
            val tree = s"""|network a {
                           |  b = new c();
                           |  $decl;
                           |}""".stripMargin.asTree[Desc]

            tree rewrite checker

            cc.messages.loneElement should beThe[Error](
              s"$hint declarations must appear before instances"
            )
          }

          "connections" in {
            val tree = s"""|network a {
                           |  b -> c;
                           |  $decl;
                           |}""".stripMargin.asTree[Desc]

            tree rewrite checker

            cc.messages.loneElement should beThe[Error](
              s"$hint declarations must appear before connections"
            )
          }

          "functions" in {
            val tree = s"""|fsm a {
                           |  void main() { fence; }
                           |  $decl;
                           |}""".stripMargin.asTree[Desc]

            tree rewrite checker

            cc.messages.loneElement should beThe[Error](
              s"$hint declarations must appear before function definitions"
            )
          }

          "fence block" in {
            val tree = s"""|fsm a {
                           |  fence {}
                           |  $decl;
                           |}""".stripMargin.asTree[Desc]

            tree rewrite checker

            cc.messages.loneElement should beThe[Error](
              s"$hint declarations must appear before 'fence' block"
            )
          }

          "verbatim block" in {
            val tree = s"""|fsm a {
                           |  verbatim verilog {}
                           |  $decl;
                           |  void main() {fence;}
                           |}""".stripMargin.asTree[Desc]

            tree rewrite checker

            cc.messages.loneElement should beThe[Error](
              s"$hint declarations must appear before 'verbatim' blocks"
            )
          }

          if (!genOK) {
            "gen blocks" in {
              val tree = s"""|fsm a {
                             |  gen if (true) {}
                             |  $decl;
                             |}""".stripMargin.asTree[Desc]

              tree rewrite checker

              cc.messages.loneElement should beThe[Error](
                s"$hint declarations must appear before 'gen' blocks"
              )
            }
          }

          if (genOK) {
            "inside 'gen'" - {
              "'if' a" in {
                val tree = s"""|fsm a {
                               |  void main() { fence; }
                               |  gen if (1) { $decl; }
                               |}""".stripMargin.asTree[Desc]

                tree rewrite checker

                cc.messages.loneElement should beThe[Error](
                  s"$hint declarations must appear before function definitions"
                )
              }

              "'if' b" in {
                val tree = s"""|fsm a {
                               |  gen if (1) {
                               |   void main() { fence; }
                               |   $decl;
                               |  }
                               |}""".stripMargin.asTree[Desc]

                tree rewrite checker

                cc.messages.loneElement should beThe[Error](
                  s"$hint declarations must appear before function definitions"
                )
              }

              "'if' c" in {
                val tree = s"""|fsm a {
                               |  gen if (1) { void main() { fence; } }
                               |  $decl;
                               |}""".stripMargin.asTree[Desc]

                tree rewrite checker

                cc.messages.loneElement should beThe[Error](
                  s"$hint declarations must appear before function definitions"
                )
              }

              "'else' a" in {
                val tree = s"""|fsm a {
                               |  void main() { fence; }
                               |  gen if (1) {} else { $decl; }
                               |}""".stripMargin.asTree[Desc]

                tree rewrite checker

                cc.messages.loneElement should beThe[Error](
                  s"$hint declarations must appear before function definitions"
                )
              }

              "'else' b" in {
                val tree = s"""|fsm a {
                               |  gen if (1) {} else {
                               |   void main() { fence; }
                               |   $decl;
                               |  }
                               |}""".stripMargin.asTree[Desc]

                tree rewrite checker

                cc.messages.loneElement should beThe[Error](
                  s"$hint declarations must appear before function definitions"
                )
              }

              "'else' c" in {
                val tree = s"""|fsm a {
                               |  gen if (1) {} else { void main() { fence; } }
                               |  $decl;
                               |}""".stripMargin.asTree[Desc]

                tree rewrite checker

                cc.messages.loneElement should beThe[Error](
                  s"$hint declarations must appear before function definitions"
                )
              }

              "'for' standard a" in {
                val tree = s"""|fsm a {
                               |  void main() { fence; }
                               |  gen for (uint N = 0 ; N < 1 ; N++) { $decl; }
                               |}""".stripMargin.asTree[Desc]

                tree rewrite checker

                cc.messages.loneElement should beThe[Error](
                  s"$hint declarations must appear before function definitions"
                )
              }

              "'for' standard b" in {
                val tree = s"""|fsm a {
                               |  gen for (uint N = 0 ; N < 1 ; N++) {
                               |    void main() { fence; }
                               |    $decl;
                               |  }
                               |}""".stripMargin.asTree[Desc]

                tree rewrite checker

                cc.messages.loneElement should beThe[Error](
                  s"$hint declarations must appear before function definitions"
                )
              }

              "'for' standard c" in {
                val tree = s"""|fsm a {
                               |  gen for (uint N = 0 ; N < 1 ; N++) { void main() { fence; } }
                               |  $decl;
                               |}""".stripMargin.asTree[Desc]

                tree rewrite checker

                cc.messages.loneElement should beThe[Error](
                  s"$hint declarations must appear before function definitions"
                )
              }

              "'for' range a" in {
                val tree = s"""|fsm a {
                               |  void main() { fence; }
                               |  gen for (uint N < 1) { $decl; }
                               |}""".stripMargin.asTree[Desc]

                tree rewrite checker

                cc.messages.loneElement should beThe[Error](
                  s"$hint declarations must appear before function definitions"
                )
              }

              "'for' range b" in {
                val tree = s"""|fsm a {
                               |  gen for (uint N < 1) {
                               |    void main() { fence; }
                               |    $decl;
                               |  }
                               |}""".stripMargin.asTree[Desc]

                tree rewrite checker

                cc.messages.loneElement should beThe[Error](
                  s"$hint declarations must appear before function definitions"
                )
              }

              "'for' range c" in {
                val tree = s"""|fsm a {
                               |  gen for (uint N < 1) { void main() { fence; } }
                               |  $decl;
                               |}""".stripMargin.asTree[Desc]

                tree rewrite checker

                cc.messages.loneElement should beThe[Error](
                  s"$hint declarations must appear before function definitions"
                )
              }
            }
          }
        }
      }
    }

    "check const and param declarations in classes precede" - {
      for {
        (decl, hint) <- List(
          ("param bool p", "Parameter"),
          ("const bool p = false", "Constant")
        )
      } {
        decl - {
          "nested classes" ignore {
            val tree = s"""|struct a {
                           |  struct b {}
                           |  $decl;
                           |}""".stripMargin.asTree[Desc]

            tree rewrite checker

            cc.messages.loneElement should beThe[Error](
              s"$hint declarations must appear before nested 'struct' definitions"
            )
          }

          "functions" ignore {
            val tree = s"""|struct a {
                           |  void main() { fence; }
                           |  $decl;
                           |}""".stripMargin.asTree[Desc]

            tree rewrite checker

            cc.messages.loneElement should beThe[Error](
              s"$hint declarations must appear before function definitions"
            )
          }

          "gen blocks" ignore {
            val tree = s"""|struct a {
                           |  gen if (true) {}
                           |  $decl;
                           |}""".stripMargin.asTree[Desc]

            tree rewrite checker

            cc.messages.loneElement should beThe[Error](
              s"$hint declarations must appear before 'gen' blocks"
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
          val tree = s"""|verbatim entity a {
                         |  $decl;
                         |}""".stripMargin.asTree[Desc]

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
  }
}
