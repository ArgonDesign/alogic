////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Peter de Rivaz/Geza Lore
//
// DESCRIPTION:
//
// Convert state functions to states:
//  - Converts control functions to state system
//  - Does NOT allocate state numbers, which will be done later
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Stack
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.FollowedBy

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class ConvertControl(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy {

  // The return stack symbol
  private[this] lazy val rsSymbol: TermSymbol = {
    entitySymbol.attr.returnStack.get getOrElse {
      cc.ice(entitySymbol, "Entity requires a return stack, but none was allocated")
    }
  }

  //////////////////////////////////////////////////////////////////////////
  // State for control conversion
  //////////////////////////////////////////////////////////////////////////

  // Map from function symbols to the entry state symbol of that function
  private[this] var func2state: Map[TermSymbol, TermSymbol] = _

  // Map from stmt.id to state symbol that is allocated after this state
  private[this] val allocStmts = mutable.Map[Int, TermSymbol]()

  // Map from stmt.id to state symbol if this is the first stmt in that state
  private[this] var entryStmts = mutable.Map[Int, TermSymbol]()

  // Stack of state symbols to go to when finished with this state
  private[this] val followingState = Stack[TermSymbol]()

  // Stack of break statement target state symbols
  private[this] val breakTargets = Stack[TermSymbol]()

  // Stack of states symbols in the order they are emitted. We keep these
  // as Options. A None indicates that the state does not actually needs
  // to be emitted, as it will be emitted by an enclosing list (which is empty),
  // in part, this is used to avoid emitting empty states for loop entry points.
  private[this] val pendingStates = Stack[Option[TermSymbol]]()

  override def skip(tree: Tree): Boolean = tree match {
    case entity: Entity => entity.functions.isEmpty
    case _              => false
  }

  // Allocate all intermediate states that are introduced
  // by a list of statements
  private[this] def allocateStates(stmts: List[Stmt]): Unit = {
    assert(stmts.last.tpe == TypeCtrlStmt)

    // Collect all but the last control statements, together with
    // the statements immediately following them, as pairs.
    // There is no need to allocate a state after the last control statement,
    // as it just goes to the follower of the enclosing control statement.
    val stateSymbols = for {
      (stmt, next) <- stmts zip stmts.tail
      if stmt.tpe == TypeCtrlStmt
    } yield {
      // Allocate a new state for the following statement
      val symbol = cc.newTermSymbol(s"l${next.loc.line}", next.loc, TypeState)
      allocStmts(stmt.id) = symbol
      entryStmts(next.id) = symbol
      symbol
    }

    // Push state symbols in reverse onto the pendingStates stack as
    // we want the first to be emitted to be at the top of the stack
    for (symbol <- stateSymbols.reverse) {
      pendingStates.push(Some(symbol))
    }
  }

  override def enter(tree: Tree): Unit = {
    if (tree.tpe == TypeCtrlStmt) {
      // Either push the state that is allocated after this statement,
      // or just double up the top of the followingState so control
      // statements can always pop when they have been converted
      followingState.push(allocStmts.getOrElse(tree.id, followingState.top))
    }

    tree match {
      //////////////////////////////////////////////////////////////////////////
      // Leave comb statements alone
      //////////////////////////////////////////////////////////////////////////

      case stmt: Stmt if stmt.tpe == TypeCombStmt => ()

      //////////////////////////////////////////////////////////////////////////
      // Entity
      //////////////////////////////////////////////////////////////////////////

      case entity: Entity => {
        // Allocate function entry state symbols up front so they can be
        // resolved in an arbitrary order, also add them to the entryStmts map
        val pairs = for (function <- entity.functions) yield {
          val Sym(functionSymbol: TermSymbol) = function.ref
          val stateSymbol = cc.newTermSymbol(
            s"l${functionSymbol.loc.line}_function_${functionSymbol.name}",
            functionSymbol.loc,
            TypeState
          )
          stateSymbol.attr.update(functionSymbol.attr)
          stateSymbol.attr.recLimit.clear
          entryStmts(function.body.head.id) = stateSymbol
          functionSymbol -> stateSymbol
        }

        // Construct the map from function symbols to entry state symbols
        func2state = Map(pairs: _*)
      }

      //////////////////////////////////////////////////////////////////////////
      // Allocate states where any List[Stmt] is involved
      //////////////////////////////////////////////////////////////////////////

      case StmtBlock(body) => {
        // Allocate states for body
        allocateStates(body)
      }

      case StmtLoop(body) => {
        // Set up the break target
        breakTargets.push(followingState.top)

        // Allocate a state for the entry point of the loop,
        // but only if a state does not yet exist there,
        // otherwise reuse the existing state
        val symbol = entryStmts.get(tree.id) match {
          case Some(symbol) => {
            // Let the outer list emit the state
            pendingStates.push(None)
            symbol
          }
          case None => {
            val symbol = cc.newTermSymbol(s"l${tree.loc.line}_loop", tree.loc, TypeState)
            entryStmts(tree.id) = symbol
            // Need to emit newly created state
            pendingStates.push(Some(symbol))
            symbol
          }
        }

        // Ensure loop body loops back to the loop entry
        followingState.push(symbol)

        // Allocate states for body
        allocateStates(body)
      }

      case Function(Sym(symbol: TermSymbol), body) => {
        val stateSymbol = func2state(symbol)

        // Set up the followingState to loop back to the function entry point
        followingState.push(stateSymbol)

        // Allocate states for body
        allocateStates(body)

        // Ensure the function entry state is emitted
        pendingStates.push(Some(stateSymbol))
      }

      //////////////////////////////////////////////////////////////////////////
      // Otherwise nothing interesting
      //////////////////////////////////////////////////////////////////////////

      case _ =>
    }
  }

  // Split the list after every control statement
  private[this] def splitControlUnits(stmts: List[Stmt]): List[List[Stmt]] = {
    assert(stmts.last.tpe == TypeCtrlStmt)

    @tailrec
    def loop(
        stmts: List[Stmt],
        current: ListBuffer[Stmt] = ListBuffer(),
        acc: ListBuffer[List[Stmt]] = ListBuffer()
    ): List[List[Stmt]] = {
      val head :: tail = stmts
      current append head
      if (head.tpe == TypeCombStmt) {
        loop(tail, current, acc)
      } else {
        acc append current.toList
        if (tail.nonEmpty) {
          loop(tail, ListBuffer(), acc)
        } else {
          acc.toList
        }
      }
    }

    loop(stmts)
  }

  // List of emitted states
  private[this] val emittedStates = ListBuffer[State]()

  // Emit current state with given body, returns symbol that was emitted
  private[this] def emitState(body: List[Stmt]): Option[TermSymbol] = {
    assert(body.last.tpe == TypeCtrlStmt)
    assert(body.init forall { _.tpe == TypeCombStmt })

    val symOpt = pendingStates.top followedBy pendingStates.pop()

    if (symOpt.isDefined) {
      val loc = body.head.loc
      val Some(symbol) = symOpt
      val ref = ExprRef(symbol) regularize loc
      val state = State(ref, body) withLoc loc
      TypeAssigner(state)

      emittedStates append state
    }

    symOpt
  }

  override def transform(tree: Tree): Tree = {
    val result = tree match {
      //////////////////////////////////////////////////////////////////////////
      // Leave combinatorial statements alone
      //////////////////////////////////////////////////////////////////////////

      case _: Stmt if tree.tpe == TypeCombStmt => tree

      //////////////////////////////////////////////////////////////////////////
      // Convert leaf statements
      //////////////////////////////////////////////////////////////////////////

      case _: StmtFence => {
        val ref = ExprRef(followingState.top)
        StmtGoto(ref) regularize tree.loc
      }

      case _: StmtBreak => {
        val ref = ExprRef(breakTargets.top)
        StmtGoto(ref) regularize tree.loc
      }

      case StmtGoto(ExprRef(symbol: TermSymbol)) => {
        val ref = ExprRef(func2state(symbol))
        StmtGoto(ref) regularize tree.loc
      }

      case _: StmtReturn => {
        val pop = ExprRef(rsSymbol) select "pop" call Nil
        StmtGoto(pop) regularize tree.loc
      }

      case StmtExpr(ExprCall(ExprRef(symbol: TermSymbol), Nil)) => {
        val ret = ExprRef(followingState.top)
        val push = ExprRef(rsSymbol) select "push" call List(ret)
        val ref = ExprRef(func2state(symbol))
        StmtBlock(List(StmtExpr(push), StmtGoto(ref))) regularize tree.loc
      }

      //////////////////////////////////////////////////////////////////////////
      // Convert if
      //////////////////////////////////////////////////////////////////////////

      case stmt @ StmtIf(_, _, elseStmtOpt) => {
        if (elseStmtOpt.nonEmpty) {
          tree
        } else {
          // Omitted else goes to the following state (i.e.: implicit fence)
          val ref = ExprRef(followingState.top)
          stmt.copy(elseStmt = Some(StmtGoto(ref))) regularize tree.loc
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // Convert case
      //////////////////////////////////////////////////////////////////////////

      case stmt @ StmtCase(_, cases) => {
        val hasDefault = cases exists {
          case _: DefaultCase => true
          case _              => false
        }
        if (hasDefault) {
          tree
        } else {
          // Omitted default goes to the following state (i.e.: implicit fence)
          val ref = ExprRef(followingState.top)
          stmt.copy(cases = DefaultCase(StmtGoto(ref)) :: cases) regularize tree.loc
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // Convert block
      //////////////////////////////////////////////////////////////////////////

      case StmtBlock(body) => {
        val head :: tail = splitControlUnits(body)
        tail foreach emitState
        TypeAssigner(StmtBlock(head) withLoc tree.loc)
      }

      //////////////////////////////////////////////////////////////////////////
      // Convert loop
      //////////////////////////////////////////////////////////////////////////

      case StmtLoop(body) => {
        val head :: tail = splitControlUnits(body)
        tail foreach emitState
        // Emit the loop entry state if necessary
        val stmt = emitState(head) match {
          case Some(symbol) => {
            // Loop entry state was emitted, so the containing state
            // needs to go to the emitted state
            val ref = ExprRef(symbol) regularize symbol.loc
            StmtGoto(ref)
          }
          case None => {
            // Loop entry state was not emitted (because the containing
            // state is empty), so the containing state becomes the loop
            // entry state
            StmtBlock(head)
          }
        }
        TypeAssigner(stmt withLoc tree.loc)
      } followedBy {
        breakTargets.pop()
        followingState.pop()
      }

      //////////////////////////////////////////////////////////////////////////
      // Handle function
      //////////////////////////////////////////////////////////////////////////

      case Function(_, body) => {
        splitControlUnits(body) foreach emitState
        // Don't bother rewriting, it will be discarded later
        tree
      } followedBy {
        followingState.pop()
      }

      //////////////////////////////////////////////////////////////////////////
      // Convert entity
      //////////////////////////////////////////////////////////////////////////

      case entity: Entity => {
        // Sort states by source location (for ease of debugging)
        val states = emittedStates.toList sortBy { _.loc.start }

        val result = entity.copy(
          functions = Nil,
          states = states
        ) withLoc entity.loc
        TypeAssigner(result)
      }

      //////////////////////////////////////////////////////////////////////////
      // Die if we missed a control statement
      //////////////////////////////////////////////////////////////////////////

      case node: Stmt if node.tpe == TypeCtrlStmt => {
        cc.ice(node, "Cannot convert control statement", node.toSource)
      }

      //////////////////////////////////////////////////////////////////////////
      // Otherwise nothing interesting
      //////////////////////////////////////////////////////////////////////////

      case _ => tree
    }

    // If we have just converted a control statement, pop the followingState
    if (tree.tpe == TypeCtrlStmt) {
      followingState.pop()
    }

    result
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(followingState.isEmpty)
    assert(breakTargets.isEmpty)
    assert(pendingStates.isEmpty)

    tree visit {
      case node: Tree if !node.hasTpe => cc.ice(node, "Lost tpe of", node.toString)
      case node: Function             => cc.ice(node, "Function remains")
      case node: StmtLoop             => cc.ice(node, "Loop remains")
      case node: StmtFence            => cc.ice(node, "Fence statement remains")
      case node: StmtBreak            => cc.ice(node, "Break statement remains")
      case node: StmtReturn           => cc.ice(node, "Return statement remains")
      case node @ ExprCall(ref, _) if ref.tpe == TypeCtrlStmt => {
        cc.ice(node, "Control function call remains")
      }
    }
  }
}

object ConvertControl extends TreeTransformerPass {
  val name = "convert-control"
  def create(implicit cc: CompilerContext) = new ConvertControl
}
