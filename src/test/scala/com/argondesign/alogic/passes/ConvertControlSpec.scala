////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Thomas Brown
//
// DESCRIPTION:
//
// ConvertControl tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import org.scalatest.freespec.AnyFreeSpec

final class ConvertControlSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc = new CompilerContext

  private def convertControl(text: String): Thicket = Thicket {
    transformWithPass(
      Checker andThen
        Namer andThen
        Elaborate andThen
        TypeCheck andThen
        AnalyseCallGraph andThen
        ConvertControl,
      text
    ) map {
      _ flatMap {
        case (decl, defn) => List(decl, defn)
      }
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
      val goto = state collectFirst {
        case e @ StmtGoto(ExprCall(ExprSelect(_, "pop", Nil), Nil)) => e
      }
      goto should matchPattern { case Some(_) => }
    }

    def checkPopsStack(state: DefnState): Unit = {
      val pop = state collectFirst {
        case e @ ExprCall(ExprSelect(_, "pop", Nil), Nil) => e
      }
      pop should matchPattern { case Some(_) => }
    }

    def checkDoesntPush(stateFrom: DefnState): Unit = {
      val hopefullyNone = stateFrom collectFirst {
        case e @ ExprCall(ExprSelect(_, "push", Nil), _) => e
      }
      hopefullyNone shouldBe None
    }

    def checkPushesState(stateFrom: DefnState, statePushedGolden: DefnState): Unit = {
      val pushCall = stateFrom getFirst { case x: ExprCall => x }
      pushCall should matchPattern {
        case ExprCall(ExprSelect(_, "push", Nil), List(ArgP(ExprSym(symbol))))
            if symbol == statePushedGolden.symbol =>
      }
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
           |}"""
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
           |}"""
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
           |}"""
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
           |}"""
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
           |}"""
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
