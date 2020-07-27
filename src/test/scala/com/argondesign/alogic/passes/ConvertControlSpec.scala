////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import org.scalatest.freespec.AnyFreeSpec

final class ConvertControlSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  private def convertControl(text: String): Thicket = Thicket {
    transformWithPass(
      FrontendPass andThen
        DropPackageAndParametrizedDescs andThen
        DescToDeclDefn andThen
        AnalyseCallGraph andThen
        ConvertControl,
      text
    ) map {
      _.toList flatMap { case (decl, defn) => List(decl, defn) }
    } getOrElse Nil
  }

  "ConvertControl should" - {

    def checkGoesTo(stateFrom: DefnState, stateToGolden: DefnState): Unit = {
      val stateTo = stateFrom getFirst {
        case StmtGoto(ExprSym(sym: Symbol)) => sym
      }
      stateTo shouldBe stateToGolden.symbol
    }

    def checkGoesToPop(state: DefnState): Unit = {
      val stack = state getFirst {
        case StmtExpr(ExprCall(ExprSel(s, "pop"), Nil)) => s
      }
      state.postOrderIterator exists {
        case StmtGoto(ExprSel(`stack`, "old")) => true
        case _                                 => false
      } shouldBe true
    }

    def checkPopsStack(state: DefnState): Unit = {
      val pop = state collectFirst {
        case e @ ExprCall(ExprSel(_, "pop"), Nil) => e
      }
      pop should matchPattern { case Some(_) => }
    }

    def checkDoesntPush(stateFrom: DefnState): Unit = {
      val hopefullyNone = stateFrom collectFirst {
        case e @ ExprCall(ExprSel(_, "push"), _) => e
      }
      hopefullyNone shouldBe None
    }

    def checkPushesState(stateFrom: DefnState, statePushedGolden: DefnState): Unit = {
      val stack = stateFrom getFirst {
        case StmtExpr(ExprCall(ExprSel(s, "push"), Nil)) => s
      }
      stateFrom.postOrderIterator exists {
        case StmtAssign(ExprSel(`stack`, "top"), ExprSym(g)) => g == statePushedGolden.symbol
        case _                                               => false
      } shouldBe true
    }

    "correctly assign static return points" in {
      val result = convertControl {
        """fsm fsm_e {
          |  void main() {
          |    fence;
          |    fence;
          |    a();
          |    fence;
          |    fence;
          |    b();
          |    fence;
          |  }
          |  void a() { return; } // State 7, should return to state 3
          |  void b() { return; } // State 8, should return to state 6
          |}""".stripMargin
      }
      val states = List from (result collect {
        case x: DefnState => x
      })

      states.size shouldBe 9

      checkGoesTo(states(0), states(1))
      checkGoesTo(states(1), states(2))
      checkGoesTo(states(2), states(7))
      checkGoesTo(states(7), states(3))
      checkGoesTo(states(3), states(4))
      checkGoesTo(states(4), states(5))
      checkGoesTo(states(5), states(8))
      checkGoesTo(states(8), states(6))
      checkGoesTo(states(6), states(0))

      states foreach checkDoesntPush
    }

    "correctly push and pop to stack" in {
      val result = convertControl {
        """fsm fsm_e {
          |  void main() {
          |    a();
          |    a();
          |  }
          |  void a() { return; }
          |}""".stripMargin
      }
      val states = List from (result collect {
        case x: DefnState => x
      })

      states.size shouldBe 3

      checkGoesTo(states(0), states(2))
      checkPushesState(states(0), states(1))

      checkGoesTo(states(1), states(2))
      checkPushesState(states(1), states(0))

      checkGoesToPop(states(2))
    }

    "correctly pops from stack even when return point is static" in {
      val result = convertControl {
        """fsm fsm_e {
          |  in bool pi;
          |  void main() { a(); b(); }
          |  void a() {
          |    if (pi) { goto c(); } else { goto b(); }
          |  }
          |  void b() { return; }
          |  void c() { return; }
          |}""".stripMargin
      }
      val states = List from (result collect {
        case x: DefnState => x
      })

      states.size shouldBe 5

      checkGoesTo(states(0), states(2))
      checkPushesState(states(0), states(1))

      checkGoesTo(states(1), states(3))
      checkPushesState(states(1), states(0))

      // b goes to pop
      checkGoesToPop(states(3))

      // c always returns to call site of a but also pops
      checkGoesTo(states(4), states(1))
      checkPopsStack(states(4))
    }

    "correctly returns to entry point of main" in {
      val result = convertControl {
        """fsm fsm_e {
          |  void main() {
          |    fence;
          |    goto a();
          |  }
          |  void a() { goto b(); }
          |  void b() { return; }
          |}""".stripMargin
      }
      val states = List from (result collect {
        case x: DefnState => x
      })

      states.size shouldBe 4

      checkGoesTo(states(0), states(1))
      checkGoesTo(states(1), states(2))
      checkGoesTo(states(2), states(3))
      checkGoesTo(states(3), states(0))

      states foreach checkDoesntPush
    }

    "deals with returns when stack empty" in {
      val result = convertControl {
        """fsm fsm_e {
          |  void main() {
          |    a();
          |    goto a();
          |  }
          |  void a() { return; }
          |}""".stripMargin
      }
      val states = List from (result collect {
        case x: DefnState => x
      })

      states.size shouldBe 3

      checkGoesTo(states(0), states(2))
      checkPushesState(states(0), states(1))

      checkGoesTo(states(1), states(2))
      checkDoesntPush(states(1))

      checkGoesToPop(states(2))
      checkPopsStack(states(2))
    }

  }
}
