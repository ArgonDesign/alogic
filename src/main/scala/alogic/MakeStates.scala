// We convert control statements into state labels and goto statements
//
// In the returned AST there will be no:
//   BreakStmt
//   ReturnStmt
//   ControlFor
//   ControlDo
//   ControlIf
//   ControlCaseStmt
//   ControlWhile
//   FunCall
//   ControlBlock
//   FenceStmt
//   GotoStmt
//
// These will be replaced with
//   StateBlock
//   GotoState
//   GotoStmt (this is used to target the callstack)

package alogic

import scala.collection._
import AstOps._
import scala.annotation.tailrec
import scala.collection.mutable.Stack

final class MakeStates {

  val state_alloc = Stream.from(0).toIterator; // Allocate sequential state numbers

  val fn2state = mutable.Map[String, Int]() // First state in each function

  val breakTargets = Stack[Int]() // Stack of targets when we see a break

  val extra = Stack[StateBlock]() // list of (state, commands) pairs

  // Transfer a batch of instructions to the states list
  def emit(state: Int, insns: List[AlogicAST]): Unit = {
    extra.push(StateBlock(state, insns))
  }

  def apply(fsm: FsmTask): StateTask = {
    val FsmTask(name, decls, fns, fencefn, vfns) = fsm

    if (fns exists (_.name == "main")) {
      // Ensure entry to main is state 0
      fn2state("main") = state_alloc.next

      // Allocate remaining function state numbers
      for (Function(name, _) <- fns if name != "main") {
        fn2state(name) = state_alloc.next
      }

      // Convert all functions (do not inline below -- side effect on state_alloc)
      val states = fns flatMap makeFnStates

      val newDecls = if (states.length > 1) {
        // Add a declaration for the state variable (reset to the entry state of main)
        VarDeclaration(State, DottedName("state" :: Nil), Some(makeNum(0))) :: decls
      } else {
        decls
      }

      // Create State Task
      StateTask(name, newDecls, states, fencefn, vfns)
    } else if (fencefn == None && fns == Nil) {
      // 'fsm' with only 'verilog' functions
      Message.warning(s"FSM '$name' contains only 'verilog' functions. Consider using a 'verilog' task.")
      StateTask(name, decls, Nil, None, vfns)
    } else {
      Message.fatal(s"No function named 'main' found in FSM '$name'")
    }
  }

  def makeFnStates(fn: Function): List[StateBlock] = {
    val Function(name, body) = fn

    val s = fn2state(name)
    val current = makeStates(s, s, body)

    emit(s, current)

    val result = extra.toList

    extra.clear()

    result
  }

  val callDepth = DottedName(List("call_depth"))
  val callStack = DottedName(List("call_stack"))

  def makeNum(x: Int) = Num(s"$x")

  // Create an AST that finishes by moving to finalState (unless break, return etc)
  // Returns a list of all the statements that should be appended to the list in the current state
  // If complete additional states are generated they are emitted
  // If startState >= 0 it means the current list is empty, and startState is the state we are starting to construct
  //    This can be useful to avoid emitting an empty state
  def makeStates(startState: Int, finalState: Int, tree: AlogicAST): List[AlogicAST] = tree match {
    ///////////////////////////////////////////////////////////////////////////
    // Simple control transfer statements
    ///////////////////////////////////////////////////////////////////////////

    case FenceStmt   => List(GotoState(finalState))
    case BreakStmt   => List(GotoState(breakTargets.head))
    case GotoStmt(t) => List(GotoState(fn2state(t))) // TODO check targets exist in AstBuilder to avoid exception here
    case ReturnStmt => {
      List(
        // Pop state
        Minusminus(callDepth),
        // Return to new top of stack
        GotoStmt("call_stack[call_depth_nxt]"))
    }
    case FunCall(n, args) => {
      val DottedName(name :: _) = n
      List(
        // Push return state
        Assign(ArrayLookup(callStack, callDepth), makeNum(finalState)),
        // increment depth
        Plusplus(callDepth),
        // branch to function - TODO check target exists in builder
        GotoState(fn2state(name)))
    }

    ///////////////////////////////////////////////////////////////////////////
    // ControlBlock
    ///////////////////////////////////////////////////////////////////////////

    case ControlBlock(cmds) => makeBlockStmts(startState, finalState, cmds)

    ///////////////////////////////////////////////////////////////////////////
    // Loops
    ///////////////////////////////////////////////////////////////////////////

    case ControlWhile(cond, body) => {
      val s = if (startState < 0) state_alloc.next else startState
      breakTargets push finalState
      val follow = makeStates(s, s, ControlBlock(body))
      breakTargets.pop
      val loop = CombinatorialIf(cond, CombinatorialBlock(follow), Some(GotoState(finalState)))
      if (startState < 0) {
        emit(s, loop :: Nil)
        List(GotoState(s))
      } else
        List(loop)
    }
    case ControlFor(init, cond, incr, body) => {
      val s = state_alloc.next
      val (i, f) = findLastStmts(-1, (s, Nil), finalState, body)
      emit(i, f ::: incr :: CombinatorialIf(cond, GotoState(s), Some(GotoState(finalState))) :: Nil)
      List(init, CombinatorialIf(cond, GotoState(s), Some(GotoState(finalState))))
    }
    case ControlDo(cond, body) => {
      val s = state_alloc.next
      breakTargets push finalState
      val (i, f) = findLastStmts(startState, (s, Nil), finalState, body)
      emit(i, f ::: CombinatorialIf(cond, GotoState(s), Some(GotoState(finalState))) :: Nil)
      breakTargets.pop
      List(GotoState(s))
    }

    ///////////////////////////////////////////////////////////////////////////
    // Branches
    ///////////////////////////////////////////////////////////////////////////

    case ControlIf(cond, body, None) => {
      val current = makeStates(-1, finalState, body)
      List(CombinatorialIf(cond, CombinatorialBlock(current), Some(GotoState(finalState))))
    }
    case ControlIf(cond, body, Some(elsebody)) => {
      val current = makeStates(-1, finalState, body)
      val current2 = makeStates(-1, finalState, elsebody)
      List(CombinatorialIf(cond, CombinatorialBlock(current), Some(CombinatorialBlock(current2))))
    }

    case ControlCaseStmt(value, cases) => {
      val hasDefault = cases.exists(isCaseDefault)
      breakTargets push finalState
      var combcases = for { c <- cases } yield c match {
        case ControlCaseLabel(cond, body) => {
          val a = makeStates(-1, finalState, body)
          val content: AlogicAST = {
            if (a.length == 1)
              a(0)
            else
              CombinatorialBlock(a)
          }
          CombinatorialCaseLabel(cond, content)
        }
        case _ => c // Trigger error here
      }
      if (!hasDefault)
        combcases = CombinatorialCaseLabel(Nil, GotoState(finalState)) :: combcases
      breakTargets.pop
      List(CombinatorialCaseStmt(value, combcases))
    }

    ///////////////////////////////////////////////////////////////////////////
    // Combinatorial statements
    ///////////////////////////////////////////////////////////////////////////

    case x => {
      assert(!is_control_stmt(x))
      x :: Nil
    }
  }

  def isCaseDefault(c: AlogicAST): Boolean = c match {
    case ControlCaseLabel(Nil, body) => true
    case _                           => false
  }

  //  We need to split the list of statements into control units
  //  A control unit is a set of combinatorial statements followed by a control statement
  //  We then create a new state for each intermediate control unit
  //  We return the statements to be appended to the initial list
  def makeBlockStmts(startState: Int, finalState: Int, tree: List[AlogicAST]): List[AlogicAST] = tree match {
    case Nil => Nil /// TODO: This is probably unreachable. Check in builder.
    case x :: Nil => {
      assert(is_control_stmt(x))
      makeStates(startState, finalState, x)
    }
    case x :: xs if (is_control_stmt(x)) => {
      val s = state_alloc.next
      val current = makeStates(startState, s, x)
      val current2 = makeBlockStmts(s, finalState, xs)
      emit(s, current2) // TODO can we make this function tail recursive somehow?
      current
    }
    case x :: xs => x :: makeBlockStmts(-1, finalState, xs) // x must be combinatorial
  }

  // Given a series of combinatorial and control statements this emits all control blocks, and returns the final combinatorial statements
  // initial is a list of statements in the current state
  def findLastStmts(startState: Int, initial: (Int, List[AlogicAST]), finalState: Int, tree: List[AlogicAST]): (Int, List[AlogicAST]) = tree match {
    case Nil      => initial
    case x :: Nil => (initial._1, initial._2 ::: makeStates(startState, finalState, x)) // TODO check this is not a combinatorial statement?
    case x :: xs if (is_control_stmt(x)) => {
      val s = state_alloc.next
      val current = makeStates(startState, s, x)
      emit(initial._1, initial._2 ::: current)
      findLastStmts(s, (s, Nil), finalState, xs)
    }
    case x :: xs => findLastStmts(-1, (initial._1, initial._2 ::: x :: Nil), finalState, xs) // x must be combinatorial
  }

}

object MakeStates {
  def apply(fsm: FsmTask): StateTask = {
    new MakeStates()(fsm)
  }
}
