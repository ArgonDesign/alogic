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
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.core.Warning
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeValid
import com.argondesign.alogic.core.StorageTypes.StorageTypeReg
import com.argondesign.alogic.core.Types._
import org.scalatest.FreeSpec

import scala.collection.immutable.ListMap

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
              case EntityIdent(_, _, _, _, _, List(main), _, _) =>
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
            "Input ports must be declared in the enclosing entity"
          )
        }

        "output ports" in {
          val tree = "out bool a;".asTree[Stmt]

          tree rewrite checker shouldBe StmtError()

          cc.messages.loneElement should beThe[Error](
            "Only variables can be declared in declaration statements",
            "Output ports must be declared in the enclosing entity"
          )
        }

        "parameters" in {
          val tree = "param bool a = false;".asTree[Stmt]

          tree rewrite checker shouldBe StmtError()

          cc.messages.loneElement should beThe[Error](
            "Only variables can be declared in declaration statements",
            "Parameters must be declared in the enclosing entity"
          )
        }

        "constants" in {
          val tree = "const bool a = false;".asTree[Stmt]

          tree rewrite checker shouldBe StmtError()

          cc.messages.loneElement should beThe[Error](
            "Only variables can be declared in declaration statements",
            "Constants must be declared in the enclosing entity"
          )
        }

        "arrays" in {
          val tree = "bool a[2];".asTree[Stmt]

          tree rewrite checker shouldBe StmtError()

          cc.messages.loneElement should beThe[Error](
            "Only variables can be declared in declaration statements",
            "Arrays must be declared in the enclosing entity"
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

        "srams" in {
          val tree = "sram bool a[1];".asTree[Stmt]

          tree rewrite checker shouldBe StmtError()

          cc.messages.loneElement should beThe[Error](
            "Only variables can be declared in declaration statements",
            "SRAMs must be declared in the enclosing entity"
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

    "ensure only compile time known values are declared as unsized numbers" - {
      for {
        (src, err) <- List(
          ("param int a = 1", ""),
          ("param uint a = 1", ""),
          ("const int a = 1", ""),
          ("const uint a = 1", ""),
          ("pipeline int a", "Only compile time constant scalars can be declared with type 'int'"),
          ("pipeline uint a",
           "Only compile time constant scalars can be declared with type 'uint'"),
          ("in int a", "Only compile time constant scalars can be declared with type 'int'"),
          ("in uint a", "Only compile time constant scalars can be declared with type 'uint'"),
          ("out int a", "Only compile time constant scalars can be declared with type 'int'"),
          ("out uint a", "Only compile time constant scalars can be declared with type 'uint'"),
          ("int a[1]", "Only compile time constant scalars can be declared with type 'int'"),
          ("uint a[1]", "Only compile time constant scalars can be declared with type 'uint'"),
          ("sram int a[1]", "Only compile time constant scalars can be declared with type 'int'"),
          ("sram uint a[1]", "Only compile time constant scalars can be declared with type 'uint'"),
          ("int[1] a", "Only compile time constant scalars can be declared with type 'int'"),
          ("uint[1] a", "Only compile time constant scalars can be declared with type 'uint'"),
          ("int[2][3] a[1]", "Only compile time constant scalars can be declared with type 'int'"),
          ("uint[2][3] a[1]",
           "Only compile time constant scalars can be declared with type 'uint'"),
          ("int a", "Only compile time constant scalars can be declared with type 'int'"),
          ("uint a", "Only compile time constant scalars can be declared with type 'uint'"),
        )
      } {
        src in {
          val tree = src.asTree[DeclIdent]
          tree rewrite checker

          if (err.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](err)
          }
        }
      }
    }

    "reject multiple fence blocks" in {
      val tree = """|fsm foo {
                    |  fence {}
                    |  fence {}
                    |}""".asTree[Entity]

      tree rewrite checker should matchPattern {
        case EntityIdent(_, Nil, Nil, Nil, Nil, Nil, Nil, _) =>
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
          case EntityIdent(_, List(decl), Nil, Nil, Nil, Nil, Nil, _) =>
            inside(decl) {
              case DeclIdent(_, TypeOut(_, fc, st), _) =>
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
          case EntityIdent(_, List(decl), Nil, Nil, Nil, Nil, Nil, _) =>
            inside(decl) {
              case DeclIdent(_, TypeOut(_, fc, st), _) =>
                fc shouldBe FlowControlTypeValid
                st shouldBe StorageTypeReg
            }
        }

        cc.messages.loneElement should {
          beThe[Error](
            "Output port 'a' with 'sync' flow control specifier cannot use output slices")
        }
      }
    }

    "reject case statements with multiple defaults" in {
      val tree = """|case(1) {
                    | default: a;
                    | default: b;
                    |}""".asTree[Stmt]

      tree rewrite checker shouldBe StmtError()

      cc.messages should have length 2
      cc.messages(0) should beThe[Error]("Multiple 'default' clauses specified in case statement")
      cc.messages(0).loc.line shouldBe 2
      cc.messages(1) should beThe[Error]("Multiple 'default' clauses specified in case statement")
      cc.messages(1).loc.line shouldBe 3
    }

    "reject disallowed entity contents" - {
      val kwmap = Map(
        "fsm" -> "fsm",
        "network" -> "network",
        "verbatim" -> "verbatim entity"
      )

      "instantiations in" - {
        for (variant <- List("fsm", "verbatim")) {
          variant in {
            val tree = s"""|${kwmap(variant)} a {
                           |  c = new d();
                           |}""".stripMargin.asTree[Entity]

            tree rewrite checker should matchPattern {
              case EntityIdent(_, Nil, Nil, Nil, Nil, Nil, Nil, _) =>
            }

            cc.messages.loneElement should beThe[Error](
              s"'${variant}' entity cannot contain instantiations")
            cc.messages(0).loc.line shouldBe 2
          }
        }
      }

      "connections in" - {
        for (variant <- List("fsm", "verbatim")) {
          variant in {
            val tree = s"""|${kwmap(variant)} a {
                           |  a -> b;
                           |}""".stripMargin.asTree[Entity]

            tree rewrite checker should matchPattern {
              case EntityIdent(_, Nil, Nil, Nil, Nil, Nil, Nil, _) =>
            }

            cc.messages.loneElement should beThe[Error](
              s"'${variant}' entity cannot contain connections")
            cc.messages(0).loc.line shouldBe 2
          }
        }
      }

      "nested entities without autoinst in" - {
        for (variant <- List("fsm", "verbatim")) {
          variant in {
            val tree = s"""|${kwmap(variant)} a {
                           |  fsm d {}
                           |}""".stripMargin.asTree[Entity]

            tree rewrite checker should matchPattern {
              case EntityIdent(_, Nil, Nil, Nil, Nil, Nil, Nil, _) =>
            }

            cc.messages.loneElement should beThe[Error](
              s"'${variant}' entity cannot contain nested entities")
            cc.messages(0).loc.line shouldBe 2
          }
        }
      }

      "nested entities with autoinst in" - {
        for (variant <- List("fsm", "verbatim")) {
          variant in {
            val tree = s"""|${kwmap(variant)} a {
                           |  new fsm d {}
                           |}""".stripMargin.asTree[Entity]

            tree rewrite checker should matchPattern {
              case EntityIdent(_, Nil, Nil, Nil, Nil, Nil, Nil, _) =>
            }

            cc.messages should have length 2
            cc.messages(0) should beThe[Error](s"'${variant}' entity cannot contain instantiations")
            cc.messages(0).loc.line shouldBe 2
            cc.messages(1) should beThe[Error](
              s"'${variant}' entity cannot contain nested entities")
            cc.messages(1).loc.line shouldBe 2
          }
        }
      }

      "functions in" - {
        for (variant <- List("network", "verbatim")) {
          variant in {
            val tree = s"""|${kwmap(variant)} a {
                           |  void main() {}
                           |}""".stripMargin.asTree[Entity]

            tree rewrite checker should matchPattern {
              case EntityIdent(_, Nil, Nil, Nil, Nil, Nil, Nil, _) =>
            }

            cc.messages.loneElement should beThe[Error](
              s"'${variant}' entity cannot contain function definitions")
            cc.messages(0).loc.line shouldBe 2
          }
        }
      }

      s"fence blocks in" - {
        for (variant <- List("network", "verbatim")) {
          variant in {
            val tree = s"""|${kwmap(variant)} a {
                           |  fence {}
                           |}""".stripMargin.asTree[Entity]

            tree rewrite checker should matchPattern {
              case EntityIdent(_, Nil, Nil, Nil, Nil, Nil, Nil, _) =>
            }

            cc.messages.loneElement should beThe[Error](
              s"'${variant}' entity cannot contain fence blocks")
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
                           |  ${decl};
                           |}""".stripMargin.asTree[Entity]

            tree rewrite checker should matchPattern {
              case entity: EntityIdent =>
            }

            cc.messages.loneElement should beThe[Error](
              s"'fsm' entity cannot contain ${msg} declarations")
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
                           |};
                           |
                           |network a {
                           |  ${decl};
                           |}""".stripMargin.asTree[Root]

            tree rewrite checker

            cc.messages.loneElement should beThe[Error](
              s"'network' entity cannot contain ${msg} declarations")
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
            ("sram i8 a[2]", "registered SRAM"),
          )
        } {
          decl in {
            val tree = s"""|struct s_t {
                           |  bool s;
                           |};
                           |
                           |verbatim entity a {
                           |  ${decl};
                           |}""".stripMargin.asTree[Root]

            tree rewrite checker

            cc.messages.loneElement should beThe[Error](
              s"'verbatim' entity cannot contain ${msg} declarations")
            cc.messages(0).loc.line shouldBe 6
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
                  s"${lval} ${assign};".asTree[Stmt] rewrite checker shouldBe a[StmtError]
                  cc.messages.loneElement should beThe[Error](
                    s"Invalid expression on left hand side of '${op}'")
                }
              }
            }
            "invalid lvalues inside concatenation lvalue" - {
              for ((name, lval) <- badLvals) {
                name in {
                  s"{x, ${lval}} ${assign};".asTree[Stmt] rewrite checker shouldBe a[StmtError]
                  cc.messages.loneElement should beThe[Error](
                    s"Invalid expression on left hand side of '${op}'")
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
          "unary ' " in test("'a")
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
            tree shouldBe Connect(ExprIdent("a"), Nil)
            cc.messages.loneElement should beThe[Error](
              s"Invalid port reference on right hand side of '->'",
              "Only identifiers, optionally followed by a single field selector are allowed"
            )
          }
        }
        "right hand side of -> in second position" - {
          check { ref =>
            val tree = s"a -> b, ${ref}".asTree[Connect] rewrite checker
            tree shouldBe Connect(ExprIdent("a"), List(ExprIdent("b")))
            cc.messages.loneElement should beThe[Error](
              s"Invalid port reference on right hand side of '->'",
              "Only identifiers, optionally followed by a single field selector are allowed"
            )
          }
        }
      }

      "accept valid port references on " - {
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

    "warn for non-verbatim entities with only verbatim contents" - {
      for (entity <- List("fsm", "network")) {
        entity in {
          val tree = s"""|${entity} a {
                         | in bool b;
                         | out bool c;
                         | param i8 e = 2;
                         | const i8 f = 2;
                         |
                         | verbatim verilog {}
                         |}""".stripMargin.asTree[Entity]
          tree rewrite checker shouldBe a[Entity]
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
            ("fsm", "i8 a = 0", ""),
            ("fsm", "param i8 a = 0", ""),
            ("fsm", "const i8 a = 0", ""),
            ("fsm", "i2[8] a = 0", ""),
            ("fsm", "s a = 0", ""),
            ("fsm", "i8 a[2] = 0", "Array declarations"),
            ("fsm", "sram i8 a[2] = 0", "SRAM declarations"),
            ("fsm", "in i8 a = 0", "Input port declarations"),
            ("fsm", "out i8 a = 0", ""),
            ("fsm", "out wire i8 a = 0", "Output port with 'wire' storage specifier"),
            ("fsm", "out sync i8 a = 0", "Output port declarations with 'sync' flow control"),
            ("fsm",
             "out sync ready i8 a = 0",
             "Output port declarations with 'sync ready' flow control"),
            ("fsm",
             "out sync accept i8 a = 0",
             "Output port declarations with 'sync accept' flow control"),
            ("network", "pipeline i8 a = 0", "Pipeline variable declarations")
          )
        } {
          decl in {
            val tree = s"""|struct s {
                           |  i8 b;
                           |};
                           |
                           |${entity} x {
                           |  ${decl};
                           |}""".stripMargin.asTree[Root]
            tree rewrite checker shouldBe a[Root]
            if (msg.nonEmpty) {
              cc.messages.loneElement should beThe[Error](
                s"${msg} cannot have an initializer"
              )
            } else {
              cc.messages shouldBe empty
            }
          }
        }
      }

      "are present in declarations where required" - {
        for {
          (entity, decl, msg) <- List(
            ("fsm", "i8 a ", ""),
            ("fsm", "param i8 a", ""),
            ("fsm", "const i8 a", "Constant"),
            ("fsm", "i2[8] a", ""),
            ("fsm", "s a", ""),
            ("fsm", "i8 a[2]", ""),
            ("fsm", "in i8 a", ""),
            ("fsm", "out i8 a", ""),
            ("network", "pipeline i8 a", "")
          )
        } {
          decl in {
            val tree = s"""|struct s {
                           |  i8 b;
                           |};
                           |
                           |${entity} x {
                           |  ${decl};
                           |}""".stripMargin.asTree[Root]
            tree rewrite checker shouldBe a[Root]
            if (msg.nonEmpty) {
              cc.messages.loneElement should beThe[Error](
                s"${msg} declarations must have an initializer"
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
  }

}
