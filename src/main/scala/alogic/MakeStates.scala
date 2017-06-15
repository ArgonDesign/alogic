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
//   GotoState
//   StateStmt
//   GotoStmt (this is used to target the callstack)

package alogic

import scala.collection._
import AstOps._
import scala.annotation.tailrec

final class MakeStates {

  var state_num = 0; // Track which state to use

  val fn2state = mutable.Map[String, Int]() // First state in each function

  var breakTargets: List[Int] = Nil // Stack of targets when we see a break

  var extra: List[List[AlogicAST]] = Nil // list of commands in each state

  // Transfer a batch of instructions to the states list
  def emit(insns: List[AlogicAST]): Unit = {
    extra = insns :: extra
  }

  def addTarget(x: Int): Unit = {
    breakTargets = x :: breakTargets
  }

  def removeTarget(): Unit = {
    breakTargets = breakTargets.tail
  }

  def apply(tree: FsmTask): StateTask = {
    tree.fns foreach {
      case Function(name, body) => createFnState(name);
    }

    val cmd = tree match {
      case FsmTask(n, decls, fns, fencefn, vfns) => StateTask(n, decls, fns.map(makeFnStates), fencefn, vfns, 0)
    }

    val prog = cmd match {
      case StateTask(n, decls, fns, fencefn, vfns, 0) => StateTask(n, decls, fns, fencefn, vfns, state_num)
    }

    // Add a declaration for the state variable
    if (fn2state contains "main") {
      val start = fn2state("main")
      val sd = VarDeclaration(State, DottedName("state" :: Nil), Some(makeNum(start)))
      prog rewrite {
        case StateTask(name, decls, fns, fencefn, vfns, states) => StateTask(name, sd :: decls, fns, fencefn, vfns, states)
      }
    } else {
      Message.error(s"No function named 'main' found in FSM '${tree.name}'")
      prog
    }
  }

  def makeFnStates(tree: Function): Function = tree match {
    case Function(name, body) => {
      val s = fn2state(name)
      val current = makeStates(s, s, body)
      emit(StateStmt(s) :: current)
      // Now convert sequences of state followed by no-state into a StateBlock
      val states = for { b <- extra } yield {
        val state = b.head.asInstanceOf[StateStmt].state
        StateBlock(state, b.tail)
      }
      extra = Nil
      Function(name, CombinatorialBlock(states))
    }
  }

  def createFnState(name: String): Int = {
    val s = state_num
    fn2state(name) = s
    state_num += 1
    s
  }

  val callDepth = DottedName(List("call_depth"))
  val callStack = DottedName(List("call_stack"))

  def makeNum(x: Int) = Num(s"$x")

  // Create an AST that finishes by moving to finalState
  // Returns a list of all the statements that should be appended to the current list (i.e. cmds in this state)
  // If complete additional commands are generated they are emitted
  // If startState >= 0 it means the current list consists simply of StateStmt(startState)
  //    This can be useful to avoid emitting an empty state
  def makeStates(startState: Int, finalState: Int, tree: AlogicAST): List[AlogicAST] = tree match {
    case FenceStmt => List(GotoState(finalState))
    case BreakStmt => List(GotoState(breakTargets.head))
    case ReturnStmt => List(
      Minusminus(callDepth),
      GotoStmt("call_stack[call_depth_nxt]"))
    case GotoStmt(t) => List(GotoState(fn2state(t))) // TODO check targets exist in AstBuilder to avoid exception here
    case FunCall(name, args) => List( // require(args.length==0)  // TODO check in builder
      // Push return state
      Assign(ArrayLookup(callStack, callDepth), makeNum(finalState)),
      // increment depth
      Plusplus(callDepth),
      // branch to function
      GotoState(fn2state(ExtractName(name))) // TODO check target exists in builder
      )
    case ControlBlock(cmds) => makeBlockStmts(startState, finalState, cmds)
    case ControlWhile(cond, body) => {
      val s = if (startState < 0) newState() else startState
      addTarget(finalState)
      val follow = makeStates(s, finalState, ControlBlock(body))
      removeTarget()
      val loop = CombinatorialIf(cond, CombinatorialBlock(follow), Some(GotoState(finalState)))
      if (startState < 0) {
        emit(StateStmt(s) :: loop :: Nil)
        List(GotoState(s))
      } else
        List(loop)
    }
    case ControlIf(cond, body, None) => {
      val current = makeStates(-1, finalState, body)
      List(CombinatorialIf(cond, CombinatorialBlock(current), Some(GotoState(finalState))))
    }
    case ControlIf(cond, body, Some(elsebody)) => {
      val current = makeStates(-1, finalState, body)
      val current2 = makeStates(-1, finalState, elsebody)
      List(CombinatorialIf(cond, CombinatorialBlock(current), Some(CombinatorialBlock(current2))))
    }
    case ControlFor(init, cond, incr, body) => {
      val s = newState()
      val f = findLastStmts(-1, StateStmt(s) :: Nil, finalState, body)
      emit(f ::: incr :: CombinatorialIf(cond, GotoState(s), Some(GotoState(finalState))) :: Nil)
      List(init, CombinatorialIf(cond, GotoState(s), Some(GotoState(finalState))))
    }
    case ControlDo(cond, body) => {
      val s = newState()
      addTarget(finalState)
      val f = findLastStmts(startState, StateStmt(s) :: Nil, finalState, body)
      emit(f ::: CombinatorialIf(cond, GotoState(s), Some(GotoState(finalState))) :: Nil)
      removeTarget()
      List(GotoState(s))
    }
    case ControlCaseStmt(value, cases) => {
      val hasDefault = cases.exists(isCaseDefault)
      addTarget(finalState)
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
      removeTarget()
      List(CombinatorialCaseStmt(value, combcases))
    }

    case x => x :: Nil
  }

  def isCaseDefault(c: AlogicAST): Boolean = c match {
    case ControlCaseLabel(Nil, body) => true
    case _                           => false
  }

  def newState(): Int = {
    val s = state_num
    state_num += 1
    s
  }

  //  We need to split into control units
  //  A control unit is a set of combinatorial statements followed by a control statement
  //  We then have a new state for each intermediate control unit
  //  We return the statements to be appended to the initial list
  def makeBlockStmts(startState: Int, finalState: Int, tree: List[AlogicAST]): List[AlogicAST] = tree match {
    case Nil      => Nil
    case x :: Nil => makeStates(startState, finalState, x)
    case x :: xs if (is_control_stmt(x)) => {
      val s = newState()
      val current = makeStates(startState, s, x)
      val current2 = makeBlockStmts(s, finalState, xs)
      emit(StateStmt(s) :: current2) // TODO can we make this function tail recursive somehow?
      current
    }
    case x :: xs => x :: makeBlockStmts(-1, finalState, xs) // x must be combinatorial
  }

  // Given a series of combinatorial and control statements this emits all control blocks, and returns the final combinatorial statements
  // initial is a list of statements in the current state
  def findLastStmts(startState: Int, initial: List[AlogicAST], finalState: Int, tree: List[AlogicAST]): List[AlogicAST] = tree match {
    case Nil      => initial
    case x :: Nil => initial ::: makeStates(startState, finalState, x) // TODO check this is not a combinatorial statement?
    case x :: xs if (is_control_stmt(x)) => {
      val s = newState()
      val current = makeStates(startState, s, x)
      emit(initial ::: current)
      findLastStmts(s, StateStmt(s) :: Nil, finalState, xs)
    }
    case x :: xs => findLastStmts(-1, initial ::: x :: Nil, finalState, xs) // x must be combinatorial
  }

}
