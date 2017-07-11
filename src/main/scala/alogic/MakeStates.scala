// We convert control statements into state labels and goto statements
//
// In the returned AST there will be no:
//   ControlLoop
//   ControlWhile
//   ControlDo
//   ControlFor
//   ControlIf
//   ControlCaseStmt
//   ControlBlock
//   FenceStmt
//   BreakStmt
//   GotoStmt
//   CallStmt
//   ReturnStmt
//
// These will be replaced with
//   StateBlock
//   GotoState
//   CallState
//   PopState

package alogic

import scala.collection._
import scala.collection.mutable.Stack

import alogic.ast._
import alogic.ast.AstOps._

final class MakeStates {

  val state_alloc = Stream.from(0).toIterator; // Allocate sequential state numbers

  val fn2state = mutable.Map[String, Int]() // First state in each function

  val breakTargets = Stack[Int]() // Stack of targets when we see a break

  val extra = Stack[StateBlock]() // list of (state, commands) pairs

  // Transfer a batch of instructions to the states list
  def emit(state: Int, insns: List[CombStmt]): Unit = {
    extra.push(StateBlock(state, insns))
  }

  def apply(fsm: FsmTask): Option[StateTask] = {
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
        VarDeclaration(State, DottedName("state" :: Nil), Some(Num(Some(false), None, 0))) :: decls
      } else {
        decls
      }

      // Create State Task
      Some(StateTask(name, newDecls, states, fencefn, vfns))
    } else if (fencefn == None && fns == Nil) {
      Message.error(s"fsm '$name' contains only 'verilog' functions. Use a 'verilog' task instead.")
      None
    } else {
      Message.error(s"No function named 'main' found in FSM '$name'")
      None
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

  // Create an AST that finishes by moving to finalState (unless break, return etc)
  // Returns a list of all the statements that should be appended to the list in the current state
  // If complete additional states are generated they are emitted
  // If startState >= 0 it means the current list is empty, and startState is the state we are starting to construct
  //    This can be useful to avoid emitting an empty state
  def makeStates(startState: Int, finalState: Int, tree: Stmt): List[CombStmt] = tree match {
    ///////////////////////////////////////////////////////////////////////////
    // Simple control transfer statements
    ///////////////////////////////////////////////////////////////////////////

    case FenceStmt          => List(GotoState(finalState))
    case BreakStmt          => List(GotoState(breakTargets.head))
    case GotoStmt(t)        => List(GotoState(fn2state(t))) // TODO check targets exist in builder
    case CallStmt(t)        => List(CallState(fn2state(t), finalState)) // TODO check target exists in builder
    case ReturnStmt         => List(ReturnState)

    ///////////////////////////////////////////////////////////////////////////
    // ControlBlock
    ///////////////////////////////////////////////////////////////////////////

    case ControlBlock(cmds) => makeBlockStmts(startState, finalState, cmds)

    ///////////////////////////////////////////////////////////////////////////
    // Fundamental Loop
    ///////////////////////////////////////////////////////////////////////////

    case ControlLoop(body) => {
      val s = if (startState < 0) state_alloc.next else startState
      breakTargets push finalState
      val follow = makeStates(s, s, body)
      breakTargets.pop
      val loop = CombinatorialBlock(follow)
      if (startState < 0) {
        emit(s, loop :: Nil)
        List(GotoState(s))
      } else {
        List(loop)
      }
    }

    ///////////////////////////////////////////////////////////////////////////
    // Common Loops
    ///////////////////////////////////////////////////////////////////////////

    case ControlWhile(cond, body) => {
      val entryState = state_alloc.next
      breakTargets push finalState
      val (lastState, lastStmts) = findLastStmts(entryState, finalState, body)
      breakTargets.pop
      emit(lastState, lastStmts ::: CombinatorialIf(cond, GotoState(entryState), Some(GotoState(finalState))) :: Nil)
      List(CombinatorialIf(cond, GotoState(entryState), Some(GotoState(finalState))))
    }
    case ControlDo(cond, body) => {
      val entryState = state_alloc.next
      breakTargets push finalState
      val (lastState, lastStmts) = findLastStmts(entryState, finalState, body)
      breakTargets.pop
      emit(lastState, lastStmts ::: CombinatorialIf(cond, GotoState(entryState), Some(GotoState(finalState))) :: Nil)
      List(GotoState(entryState))
    }
    case ControlFor(init, cond, incr, body) => {
      val entryState = state_alloc.next
      breakTargets push finalState
      val (lastState, lastStmts) = findLastStmts(entryState, finalState, body)
      breakTargets.pop
      emit(lastState, lastStmts ::: incr :: CombinatorialIf(cond, GotoState(entryState), Some(GotoState(finalState))) :: Nil)
      List(init, CombinatorialIf(cond, GotoState(entryState), Some(GotoState(finalState))))
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

    case ControlCaseStmt(value, cases, default) => {
      def makeBodyStmt(body: CtrlStmt) = makeStates(-1, finalState, body) match {
        case a :: Nil => a
        case as       => CombinatorialBlock(as)
      }

      val combcases = cases map {
        case ControlCaseLabel(cond, body) => CombinatorialCaseLabel(cond, makeBodyStmt(body))
      }

      default match {
        case Some(d) => List(CombinatorialCaseStmt(value, combcases, Some(makeBodyStmt(d))))
        case None    => List(CombinatorialCaseStmt(value, combcases, Some(GotoState(finalState))))
      }
    }

    ///////////////////////////////////////////////////////////////////////////
    // Combinatorial statements
    ///////////////////////////////////////////////////////////////////////////

    case x: CombStmt => x :: Nil
  }

  //  We need to split the list of statements into control units
  //  A control unit is a set of combinatorial statements followed by a control statement
  //  We then create a new state for each intermediate control unit
  //  We return the statements to be appended to the initial list
  def makeBlockStmts(startState: Int, finalState: Int, tree: List[Stmt]): List[CombStmt] = tree match {
    case Nil                  => Nil /// TODO: This is probably unreachable. Check in builder.
    case (x: CtrlStmt) :: Nil => makeStates(startState, finalState, x)
    case (x: CtrlStmt) :: xs => {
      val s = state_alloc.next
      val current = makeStates(startState, s, x)
      val current2 = makeBlockStmts(s, finalState, xs)
      emit(s, current2) // TODO can we make this function tail recursive somehow?
      current
    }
    case (x: CombStmt) :: Nil => Message.ice("unreachable")
    case (x: CombStmt) :: xs  => x :: makeBlockStmts(-1, finalState, xs)
  }

  // Given a series of combinatorial and control statements this emits all control blocks,
  // and returns the last state and combinatorial statements
  def findLastStmts(initialState: Int, finalState: Int, stmts: List[Stmt]): (Int, List[CombStmt]) = {
    def loop(initialState: Int, initialStmts: List[CombStmt], stmts: List[Stmt]): (Int, List[CombStmt]) = stmts match {
      case Nil                 => (initialState, initialStmts)
      case (x: CombStmt) :: xs => loop(initialState, initialStmts ::: x :: Nil, xs)
      case (x: CtrlStmt) :: xs => {
        val s = state_alloc.next
        val current = makeStates(-1, s, x)
        emit(initialState, initialStmts ::: current)
        loop(s, Nil, xs)
      }
    }
    loop(initialState, Nil, stmts)
  }

}

object MakeStates {
  def apply(fsm: FsmTask): Option[StateTask] = {
    new MakeStates()(fsm)
  }
}
