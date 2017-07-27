////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017 Argon Design Ltd.
// All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

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
import alogic.ast.ExprOps._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._

final class MakeStates {

  val state_alloc = Stream.from(0).toIterator; // Allocate sequential state numbers

  val fn2state = mutable.Map[String, Int]() // First state in each function

  val breakTargets = Stack[Int]() // Stack of targets when we see a break

  val extra = Stack[StateBlock]() // list of (state, commands) pairs

  // Transfer a batch of instructions to the states list
  def emit(state: Int, insns: List[CombStmt]): Unit = {
    extra.push(StateBlock(state, insns))
  }

  // TODO: factor out to some common place
  def ceillog2(x: Int): Int = {
    var y = 0
    while ((1 << y) < x)
      y += 1
    y
  }

  // Return the call stack size for this fsm and whether it is implicit or explicit
  def callStackSize(fsm: FsmTask): (Int, Boolean) = {
    val FsmTask(name, decls, fns, fencefn, vfns, hasnew) = fsm

    // Find the value of the CALL_STACK_SIZE constant, if any
    val cssOpt: Option[Int] = {
      val expr = decls collectFirst { case ConstDeclaration(_, "CALL_STACK_SIZE", value) => value }
      expr map { e =>
        if (!e.isConst) {
          Message.error("The value of 'CALL_STACK_SIZE' must be a compile time constant"); 1
        } else {
          val value = e.eval.toInt
          if (value < 1) {
            Message.error("The value of 'CALL_STACK_SIZE' must be >= 1"); 1
          } else {
            value
          }
        }
      }
    }

    // Collect the call graph
    val nodes = fns map { _.name }
    val edges = for {
      Function(caller, body) <- fns
      (callee, weight) <- body collect {
        case CallStmt(name) => (name, 1)
        case GotoStmt(name) => (name, 0)
      }
    } yield {
      caller ~> callee % weight
    }
    val callGraph = Graph.from(nodes, edges)

    // Find directly recursive functions or an indirectly recursive path
    val directlyRecursiveFns = callGraph.edges.toList.map { _.toOuter } collect { case s :~> t % _ if s == t => s }
    val cycle = callGraph.findCycle

    val recursive = directlyRecursiveFns.nonEmpty || cycle.isDefined

    // Emit error if CALL_STACK_SIZE definition is inconsistent
    if (recursive && !cssOpt.isDefined) {
      val msg = s"Recursive fsm '$name' must define the 'CALL_STACK_SIZE' constant" :: {
        directlyRecursiveFns map { n => s"directly recursive function: '$n'" }
      } ::: {
        cycle.toList flatMap { c =>
          s"indirectly recursive path:" :: {
            c.nodes.toList.init map { n => s"  $n ->" }
          } ::: {
            List("  " + c.nodes.last)
          }
        }
      }
      Message.error(msg: _*)
    } else if (!recursive && cssOpt.isDefined) {
      Message.error(s"Non-recursive fsm '$name' must not define the 'CALL_STACK_SIZE' constant")
    }

    // Get the value of CALL_STACK_SIZE if defined, or otherwise the length of the longest path in the call graph
    val css = cssOpt getOrElse {
      val maxWeightSum = mutable.Map[String, Long]("main" -> 1)

      callGraph.topologicalSort {
        case n: callGraph.InnerNode if n.toOuter != "main" => {
          val weightSums = for (edge <- n.incoming) yield {
            maxWeightSum(edge.source.toOuter) + edge.weight
          }
          maxWeightSum(n.toOuter) = weightSums.max
        }
        case _ =>
      }

      maxWeightSum.values.max.toInt
    }

    // Return css, and true if it's explicitly defined
    (css, cssOpt.isDefined)
  }

  def apply(fsm: FsmTask): Option[StateTask] = {
    val FsmTask(name, decls, fns, fencefn, vfns, hasnew) = fsm

    if (fns exists (_.name == "main")) {
      // Figure out the call stack size
      val (css, explicitCss) = callStackSize(fsm)

      // Ensure entry to main is state 0
      fn2state("main") = state_alloc.next

      // Allocate remaining function state numbers
      for (Function(name, _) <- fns if name != "main") {
        fn2state(name) = state_alloc.next
      }

      // Convert all functions (do not inline below -- side effect on state_alloc)
      val states = fns flatMap makeFnStates
      val stateType = IntType(false, ceillog2(states.length))

      // Add a declaration for the state variable if required (reset to the entry state of main)
      val stateDecl = if (states.length > 1) {
        Some(VarDeclaration(stateType, "state", Some(Num(Some(false), None, 0))))
      } else {
        None
      }

      // Add a declaration for the CALL_STACK_SIZE constant if required
      val cssDecl = if (!explicitCss && css > 1) {
        Some(ConstDeclaration(IntType(false, 32), "CALL_STACK_SIZE", Num(None, None, css)))
      } else {
        None
      }

      // Add declarations for 'call_stack' and 'call_depth'
      val callStackDecls = if (css > 1) {
        // TODO: the depth of 'call_stack' should be "CALL_STACK_SIZE" but MakeVerilog does not support
        // parametrizable depth arrays yet
        val csDecl = ArrayDeclaration(stateType, "call_stack", List(Num(None, None, css - 1)))
        val cdDecl = {
          val sz = ceillog2(css)
          VarDeclaration(IntType(false, sz), "call_depth", Some(Num(Some(false), Some(sz), 0)))
        }
        List(csDecl, cdDecl)
      } else {
        Nil
      }

      val newDecls = stateDecl.toList ::: cssDecl.toList ::: callStackDecls ::: decls

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
