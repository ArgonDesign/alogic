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
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.core.Warning
import org.scalatest.FreeSpec

final class AnalyseCallGraphSpec extends FreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  private def analyseCallGraph(text: String): Thicket = Thicket {
    transformWithPass(
      Namer andThen
        Elaborate andThen
        TypeCheck andThen
        ReplaceUnaryTicks andThen
        ResolvePolyFunc andThen
        AddCasts andThen
        FoldExpr andThen
        LowerLoops andThen
        AnalyseCallGraph,
      text
    ) map {
      _ flatMap {
        case (decl, defn) => List(decl, defn)
      }
    } getOrElse Nil
  }

  "AnalyseCallGraph should" - {
    "error for directly recursive functions without reclimit attribute" - {
      "simple" in {
        analyseCallGraph {
          s"""
          |fsm a {
          |  void main() { foo(); }
          |  void foo() { foo(); }
          |}"""
        }
        cc.messages.loneElement should beThe[Error](
          "Recursive function 'foo' requires 'reclimit' attribute"
        )
      }

      "even if stacklimit of enclosed entity is provided" in {
        analyseCallGraph {
          s"""
          |(* stacklimit = 2 *) fsm a {
          |  void main() { foo(); }
          |  void foo() { foo(); }
          |}"""
        }
        cc.messages.loneElement should beThe[Error](
          "Recursive function 'foo' requires 'reclimit' attribute"
        )
      }
    }

    "do not error for directly recursive functions with reclimit attribute" in {
      analyseCallGraph {
        s"""
        |fsm a {
        |  void main() { foo(); }
        |  (* reclimit = 2 *) void foo() { foo(); }
        |}"""
      }
      cc.messages shouldBe empty
    }

    "do not error for functions that goto themselves without reclimit attribute" in {
      analyseCallGraph {
        s"""
        |fsm a {
        |  void main() { foo(); }
        |  void foo() { goto foo; }
        |}"""
      }
      cc.messages shouldBe empty
    }

    "error for indirectly recursive functions without reclimit attribute" - {
      "2 cycle" in {
        analyseCallGraph {
          s"""
          |fsm a {
          |  void main() { foo(); }
          |  void foo() { bar(); }
          |  void bar() { foo(); }
          |}"""
        }
        cc.messages should have length 2
        cc.messages(0) should beThe[Error](
          "Indirectly recursive function 'foo' requires 'reclimit' attribute"
        )
        cc.messages(1) should beThe[Error](
          "Indirectly recursive function 'bar' requires 'reclimit' attribute"
        )
      }

      "3 cycle" in {
        analyseCallGraph {
          s"""
          |fsm a {
          |  void main() { foo(); }
          |  void foo() { bar(); }
          |  void bar() { main(); }
          |}"""
        }
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
        analyseCallGraph {
          s"""
          |fsm a {
          |  void main() { foo(); }
          |  (* reclimit = 2 *) void foo() { bar(); }
          |  (* reclimit = 2 *) void bar() { foo(); }
          |}"""
        }
        cc.messages shouldBe empty
      }

      "3 cycle" in {
        analyseCallGraph {
          s"""
          |fsm a {
          |  (* reclimit = 2 *) void main() { foo(); }
          |  (* reclimit = 2 *) void foo() { bar(); }
          |  (* reclimit = 2 *) void bar() { main(); }
          |}"""
        }
        cc.messages shouldBe empty
      }
    }

    "do not error for recursion through gotos only without reclimit attributes" - {
      "direct" in {
        analyseCallGraph {
          s"""
          |fsm a {
          |  void main() { goto main; }
          |}"""
        }
        cc.messages shouldBe empty
      }

      "2 cycle" in {
        analyseCallGraph {
          s"""
          |fsm a {
          |  void main() { goto foo; }
          |  void foo() { goto main; }
          |}"""
        }
        cc.messages shouldBe empty
      }

      "3 cycle" in {
        analyseCallGraph {
          s"""
          |fsm a {
          |  void main() { goto foo; }
          |  void foo() { goto bar; }
          |  void bar() { goto main; }
          |}"""
        }
        cc.messages shouldBe empty
      }
    }

    "error for non-computable reclimit attribute" in {
      analyseCallGraph {
        s"""
        |fsm a {
        |  (* reclimit = @randbit() *) void main() { main(); }
        |}"""
      }
      cc.messages.loneElement should beThe[Error](
        "Cannot compute value of 'reclimit' attribute of function 'main'"
      )
    }

    "error for reclimit 1" in {
      analyseCallGraph {
        s"""
        |fsm a {
        |  (* reclimit = 1 *) void main() { main(); }
        |}"""
      }
      cc.messages.loneElement should beThe[Error](
        "Recursive function 'main' has 'reclimit' attribute equal to 1"
      )
    }

    "warn for ignored reclimit attribute" - {
      "non-recursive" in {
        analyseCallGraph {
          s"""
          |fsm a {
          |  void main() { foo(); }
          |  (* reclimit = 2 *) void foo() { fence; }
          |}"""
        }
        cc.messages.loneElement should beThe[Warning](
          "'reclimit' attribute ignored on function 'foo'"
        )
      }

      "tail recursion only" in {
        analyseCallGraph {
          s"""
          |fsm a {
          |  void main() { foo(); }
          |  (* reclimit = 2 *) void foo() { goto foo; }
          |}"""
        }
        cc.messages.loneElement should beThe[Warning](
          "'reclimit' attribute ignored on function 'foo'"
        )
      }

      "recursion through gotos only" - {
        "direct" in {
          analyseCallGraph {
            s"""
            |fsm a {
            |  (* reclimit = 2 *) void main() { goto main; }
            |}"""
          }
          cc.messages.loneElement should beThe[Warning](
            "'reclimit' attribute ignored on function 'main'"
          )
        }

        "2 cycle" in {
          analyseCallGraph {
            s"""
            |fsm a {
            |  (* reclimit = 2 *) void main() { goto foo; }
            |  (* reclimit = 2 *) void foo() { goto main; }
            |}"""
          }
          cc.messages should have length 2
          cc.messages(0) should beThe[Warning](
            "'reclimit' attribute ignored on function 'main'"
          )
          cc.messages(1) should beThe[Warning](
            "'reclimit' attribute ignored on function 'foo'"
          )
        }

        "3 cycle" in {
          analyseCallGraph {
            s"""
            |fsm a {
            |  (* reclimit = 2 *) void main() { goto foo; }
            |  (* reclimit = 2 *) void foo() { goto bar; }
            |  (* reclimit = 2 *) void bar() { goto main; }
            |}"""
          }
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

      "even if stacklimit is provided" in {
        analyseCallGraph {
          s"""
          |(* stacklimit = 1 *) fsm a {
          |  (* reclimit = 2 *) void main() { foo(); main(); }
          |  (* reclimit = 2 *) void foo() { fence; }
          |}"""
        }
        cc.messages.loneElement should beThe[Warning](
          "'reclimit' attribute ignored on function 'foo'"
        )
      }

    }

    "error for non-computable stacklimit attribute" in {
      analyseCallGraph {
        s"""
        |(* stacklimit = @randbit() *) fsm a {
        |   (*reclimit = 2 *) void main() { main(); }
        |}"""
      }
      cc.messages.loneElement should beThe[Error](
        "Cannot compute value of 'stacklimit' attribute of entity 'a'"
      )
    }

    "error for stacklimit 0" in {
      analyseCallGraph {
        s"""
        |(* stacklimit = 0 *) fsm a {
        |   (*reclimit = 2 *) void main() { main(); }
        |}"""
      }
      cc.messages.loneElement should beThe[Error](
        "Entity 'a' has 'stacklimit' attribute equal to 0"
      )
    }

    "warn for ignored stacklimit attribute " - {
      "non-recursive fsm" in {
        analyseCallGraph {
          s"""
          |(* stacklimit = 2 *) fsm a {
          |  void main() { foo(); }
          |  void foo() { fence; }
          |}"""
        }
        cc.messages.loneElement should beThe[Warning](
          "'stacklimit' attribute ignored on entity 'a'"
        )
      }

      "no functions" in {
        analyseCallGraph {
          s"""
          |(* stacklimit = 2 *) network a {
          |}"""
        }
        cc.messages.loneElement should beThe[Warning](
          "'stacklimit' attribute ignored on entity 'a'"
        )
      }
    }

  }
}
