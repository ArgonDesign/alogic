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
// AllocateReturnStack tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.core.Warning
import com.argondesign.alogic.typer.Typer
import org.scalatest.FreeSpec

final class AllocateReturnStackSpec extends FreeSpec with AlogicTest {

  implicit val cc = new CompilerContext
  val namer = new Namer
  val typer = new Typer
  val fold = new FoldExpr(true)
  val lowerLoops = new LowerLoops
  val allocReturnStack = new AllocateReturnStack

  def xform(tree: Tree): Tree = {
    tree match {
      case root: Root     => cc.addGlobalEntity(root.entity)
      case entity: Entity => cc.addGlobalEntity(entity)
      case _              =>
    }
    tree rewrite namer rewrite typer rewrite fold rewrite lowerLoops rewrite allocReturnStack
  }

  "AllocateReturnStack should" - {
    "error for directly recursive functions without reclimit attribute" in {
      val tree = """|fsm a {
                    |  void main() { foo(); }
                    |  void foo() { foo(); }
                    |}""".stripMargin.asTree[Root]

      xform(tree)

      cc.messages.loneElement should beThe[Error](
        "Recursive function 'foo' requires 'reclimit' attribute"
      )
    }

    "do not error for directly recursive functions with reclimit attribute" in {
      val tree = """|fsm a {
                    |  void main() { foo(); }
                    |  (* reclimit = 2 *) void foo() { foo(); }
                    |}""".stripMargin.asTree[Root]

      xform(tree)

      cc.messages shouldBe empty
    }

    "do not error for functions that goto themselves without reclimit attribute" in {
      val tree = """|fsm a {
                    |  void main() { foo(); }
                    |  void foo() { goto foo; }
                    |}""".stripMargin.asTree[Root]

      xform(tree)

      cc.messages shouldBe empty
    }

    "error for indirectly recursive functions without reclimit attribute" - {
      "2 cycle" in {
        val tree = """|fsm a {
                      |  void main() { foo(); }
                      |  void foo() { bar(); }
                      |  void bar() { foo(); }
                      |}""".stripMargin.asTree[Root]

        xform(tree)

        cc.messages should have length 2
        cc.messages(0) should beThe[Error](
          "Indirectly recursive function 'foo' requires 'reclimit' attribute"
        )
        cc.messages(1) should beThe[Error](
          "Indirectly recursive function 'bar' requires 'reclimit' attribute"
        )
      }

      "3 cycle" in {
        val tree = """|fsm a {
                      |  void main() { foo(); }
                      |  void foo() { bar(); }
                      |  void bar() { main(); }
                      |}""".stripMargin.asTree[Root]

        xform(tree)

        cc.messages should have length 3
        cc.messages(0) should beThe[Error](
          "Indirectly recursive function 'main' requires 'reclimit' attribute"
        )
        cc.messages(1) should beThe[Error](
          "Indirectly recursive function 'foo' requires 'reclimit' attribute"
        )
        cc.messages(2) should beThe[Error](
          "Indirectly recursive function 'bar' requires 'reclimit' attribute"
        )
      }
    }

    "do not error for indirectly recursive functions with reclimit attribute" - {
      "2 cycle" in {
        val tree = """|fsm a {
                      |  void main() { foo(); }
                      |  (* reclimit = 2 *) void foo() { bar(); }
                      |  (* reclimit = 2 *) void bar() { foo(); }
                      |}""".stripMargin.asTree[Root]

        xform(tree)

        cc.messages shouldBe empty
      }

      "3 cycle" in {
        val tree = """|fsm a {
                      |  (* reclimit = 2 *) void main() { foo(); }
                      |  (* reclimit = 2 *) void foo() { bar(); }
                      |  (* reclimit = 2 *) void bar() { main(); }
                      |}""".stripMargin.asTree[Root]

        xform(tree)

        cc.messages shouldBe empty
      }
    }

    "do not error for recursion through gotos only without reclimit attributes" - {
      "direct" in {
        val tree = """|fsm a {
                      |  void main() { goto main; }
                      |}""".stripMargin.asTree[Root]

        xform(tree)

        cc.messages shouldBe empty
      }

      "2 cycle" in {
        val tree = """|fsm a {
                      |  void main() { goto foo; }
                      |  void foo() { goto main; }
                      |}""".stripMargin.asTree[Root]

        xform(tree)

        cc.messages shouldBe empty
      }

      "3 cycle" in {
        val tree = """|fsm a {
                      |  void main() { goto foo; }
                      |  void foo() { goto bar; }
                      |  void bar() { goto main; }
                      |}""".stripMargin.asTree[Root]

        xform(tree)

        cc.messages shouldBe empty
      }
    }

    "error for non-computable reclimit attribute" in {
      val tree = """|fsm a {
                    |  (* reclimit = @randbit() *) void main() { main(); }
                    |}""".stripMargin.asTree[Root]

      xform(tree)

      cc.messages.loneElement should beThe[Error](
        "Cannot compute value of 'reclimit' attribute of function 'main'"
      )
    }

    "error for reclimit 1" in {
      val tree = """|fsm a {
                    |  (* reclimit = 1 *) void main() { main(); }
                    |}""".stripMargin.asTree[Root]

      xform(tree)

      cc.messages.loneElement should beThe[Error](
        "Recursive function 'main' has 'reclimit' attribute equal to 1"
      )
    }

    "warn for ignored reclimit attribute" - {
      "non-recursive" in {
        val tree = """|fsm a {
                      |  void main() { foo(); }
                      |  (* reclimit = 2 *) void foo() { fence; }
                      |}""".stripMargin.asTree[Root]

        xform(tree)

        cc.messages.loneElement should beThe[Warning](
          "'reclimit' attribute ignored on function 'foo'"
        )
      }

      "tail recursion only" in {
        val tree = """|fsm a {
                      |  void main() { foo(); }
                      |  (* reclimit = 2 *) void foo() { goto foo; }
                      |}""".stripMargin.asTree[Root]

        xform(tree)

        cc.messages.loneElement should beThe[Warning](
          "'reclimit' attribute ignored on function 'foo'"
        )
      }

      "recursion through gotos only" - {
        "direct" in {
          val tree = """|fsm a {
                        |  (* reclimit = 2 *) void main() { goto main; }
                        |}""".stripMargin.asTree[Root]

          xform(tree)

          cc.messages.loneElement should beThe[Warning](
            "'reclimit' attribute ignored on function 'main'"
          )
        }

        "2 cycle" in {
          val tree = """|fsm a {
                        |  (* reclimit = 2 *) void main() { goto foo; }
                        |  (* reclimit = 2 *) void foo() { goto main; }
                        |}""".stripMargin.asTree[Root]

          xform(tree)

          cc.messages should have length 2
          cc.messages(0) should beThe[Warning](
            "'reclimit' attribute ignored on function 'main'"
          )
          cc.messages(1) should beThe[Warning](
            "'reclimit' attribute ignored on function 'foo'"
          )
        }

        "3 cycle" in {
          val tree = """|fsm a {
                        |  (* reclimit = 2 *) void main() { goto foo; }
                        |  (* reclimit = 2 *) void foo() { goto bar; }
                        |  (* reclimit = 2 *) void bar() { goto main; }
                        |}""".stripMargin.asTree[Root]

          xform(tree)

          cc.messages should have length 3
          cc.messages(0) should beThe[Warning](
            "'reclimit' attribute ignored on function 'main'"
          )
          cc.messages(1) should beThe[Warning](
            "'reclimit' attribute ignored on function 'foo'"
          )
          cc.messages(2) should beThe[Warning](
            "'reclimit' attribute ignored on function 'bar'"
          )
        }
      }

    }
  }
}
