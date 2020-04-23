////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2019 Argon Design Ltd. All rights reserved.
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

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Bindings
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.transform.ReplaceTermRefs
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class ConvertControl(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  // The return stack symbol
  private[this] var rsSymbol: Symbol = _

  //////////////////////////////////////////////////////////////////////////
  // State for control conversion
  //////////////////////////////////////////////////////////////////////////

  // Current function we are in
  private[this] var currentFunction: Symbol = _

  // Map from function symbols to the entry state symbol of that function
  private[this] var func2state: Map[Symbol, Symbol] = _

  // Map from stmt.id to state symbol that is allocated after this statement
  private[this] val allocStmts = mutable.Map[Int, Symbol]()

  // Map from stmt.id to state symbol if this is the first stmt in that statement
  private[this] val entryStmts = mutable.Map[Int, Symbol]()

  // Stack of state symbols to go to when finished with this state
  private[this] val followingState = mutable.Stack[Symbol]()

  // Stack of break statement target state symbols
  private[this] val breakTargets = mutable.Stack[Symbol]()

  // Stack of continue statement target state symbols
  private[this] val continueTargets = mutable.Stack[Symbol]()

  // Stack of states symbols in the order they are emitted. We keep these
  // as Options. A None indicates that the state does not actually needs
  // to be emitted, as it will be emitted by an enclosing list (which is empty),
  // in part, this is used to avoid emitting empty states for loop entry points.
  private[this] val pendingStates = mutable.Stack[Option[Symbol]]()

  // Map from function symbols to the state symbol following the function call.
  // Only valid when function is only called once.
  private[this] val stateFollowingCallOf = mutable.Map[Symbol, Symbol]()

  // Map from state symbol alias to the function call to which it corresponds.
  // State aliases are created when resolving return statements with static return points
  // (the return point is the state immediately following some function call).
  // We cannot replace such a return statement with the correct goto statement since the
  // state may not yet have been allocated. So we create aliases which are resolved when
  // we finish transforming the entity.
  private[this] val functionCallReturningTo = mutable.Map[Symbol, Symbol]()

  // Allocate all intermediate states that are introduced
  // by a list of statements
  private[this] def allocateStates(stmts: List[Stmt]): Unit = if (stmts.nonEmpty) {
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
      val symbol = cc.newSymbol(s"l${next.loc.line}", next.loc) tap { _.kind = TypeState }
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

  override def skip(tree: Tree): Boolean = tree match {
    case DeclFunc(symbol, _, _, _) => !symbol.kind.isCtrlFunc
    case DefnFunc(symbol, _, _)    => !symbol.kind.isCtrlFunc
    case _                         => false
  }

  override def enter(tree: Tree): Option[Tree] = {
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

      case defn: DefnEntity =>
        // Allocate function entry state symbols up front so they can be
        // resolved in an arbitrary order, also add them to the entryStmts map
        func2state = Map from {
          defn.functions.iterator filter { _.symbol.kind.isCtrlFunc } map { funcDefn =>
            val funcSymbol = funcDefn.symbol
            val name = s"l${funcSymbol.loc.line}_function_${funcSymbol.name}"
            val stateSymbol = cc.newSymbol(name, funcSymbol.loc)
            stateSymbol.kind = TypeState
            stateSymbol.attr.update(funcSymbol.attr)
            stateSymbol.attr.recLimit.clear()
            entryStmts(funcDefn.body.head.id) = stateSymbol
            funcSymbol -> stateSymbol
          }
        }

      //////////////////////////////////////////////////////////////////////////
      // Keep hold of the return stack symbol (previously put at the beginning)
      //////////////////////////////////////////////////////////////////////////

      case Defn(symbol) if symbol.attr.returnStack.isSet =>
        rsSymbol = symbol

      //////////////////////////////////////////////////////////////////////////
      // Allocate states where any List[Stmt] is involved
      //////////////////////////////////////////////////////////////////////////

      case StmtBlock(body) => allocateStates(body)

      case StmtIf(_, thenStmts, elseStmts) =>
        // Allocate in reverse order so the pendingStates stack is
        // the right way around when emitting the states
        allocateStates(elseStmts)
        allocateStates(thenStmts)

      case StmtCase(_, cases) =>
        // Allocate in reverse order so the pendingStates stack is
        // the right way around when emitting the states
        cases.reverse foreach {
          case CaseRegular(_, stmts) => allocateStates(stmts)
          case CaseDefault(stmts)    => allocateStates(stmts)
          case _: CaseGen            => unreachable
        }

      case StmtLoop(body) =>
        // Set up the break target
        breakTargets.push(followingState.top)

        // Allocate a state for the entry point of the loop,
        // but only if a state does not yet exist there,
        // otherwise reuse the existing state
        val symbol = entryStmts.get(tree.id) match {
          case Some(symbol) =>
            // Let the outer list emit the state
            pendingStates.push(None)
            symbol
          case None =>
            val symbol = cc.newSymbol(s"l${tree.loc.line}_loop", tree.loc) tap {
              _.kind = TypeState
            }
            entryStmts(tree.id) = symbol
            // Need to emit newly created state
            pendingStates.push(Some(symbol))
            symbol
        }

        // Ensure loop body loops back to the loop entry
        followingState.push(symbol)

        // Set up the continue target
        continueTargets.push(symbol)

        // Allocate states for body
        allocateStates(body)

      case DefnFunc(symbol, _, body) =>
        currentFunction = symbol

        val stateSymbol = func2state(symbol)

        if (symbol.attr.entry.isSet) {
          // If we ever want to return to the start of the entry point function
          // (e.g. return stmt in main or after sequence of gotos from main).
          stateFollowingCallOf(symbol) = stateSymbol
        }

        // Set up the followingState to loop back to the function entry point
        followingState.push(stateSymbol)

        // Allocate states for body
        allocateStates(body)

        // Ensure the function entry state is emitted
        pendingStates.push(Some(stateSymbol))

      case StmtExpr(ExprCall(ExprSym(functionSymbol), Nil)) =>
        stateFollowingCallOf(functionSymbol) = followingState.top

      //////////////////////////////////////////////////////////////////////////
      // Otherwise nothing interesting
      //////////////////////////////////////////////////////////////////////////

      case _ =>
    }
    None
  }

  // Split the list after every control statement
  private[this] def splitControlUnits(stmts: List[Stmt]): List[List[Stmt]] = {
    assert(stmts.last.tpe == TypeCtrlStmt)

    @tailrec
    def loop(
        stmts: List[Stmt],
        current: ListBuffer[Stmt],
        acc: ListBuffer[List[Stmt]]
      ): List[List[Stmt]] = stmts match {
      case head :: tail =>
        current append head
        if (head.tpe == TypeCombStmt) {
          loop(tail, current, acc)
        } else {
          acc append current.toList
          if (tail.nonEmpty) loop(tail, ListBuffer(), acc) else acc.toList
        }
      case Nil => unreachable
    }

    loop(stmts, ListBuffer(), ListBuffer())
  }

  private[this] def convertControlUnits(stmts: List[Stmt]): List[Stmt] =
    stmts match {
      case Nil => unreachable
      case stmts =>
        splitControlUnits(stmts) match {
          case Nil => unreachable
          case head :: tail =>
            tail foreach emitState
            head
        }
    }

  // List of emitted states
  private[this] val emittedStates = ListBuffer[EntDefn]()

  private[this] lazy val finishedStates = emittedStates.toList sortBy { _.loc.start }

  // Emit current state with given body, returns symbol that was emitted
  private[this] def emitState(body: List[Stmt]): Option[Symbol] = {
    assert(body.last.tpe == TypeCtrlStmt)
    assert(body.init forall { _.tpe == TypeCombStmt })

    val symOpt = pendingStates.pop()

    symOpt foreach { symbol =>
      emittedStates append {
        EntDefn(DefnState(symbol, ExprSym(symbol), body)) regularize body.head.loc
      }
    }

    symOpt
  }

  override def transform(tree: Tree): Tree = {
    val result = tree match {
      //////////////////////////////////////////////////////////////////////////
      // Leave combinational statements alone
      //////////////////////////////////////////////////////////////////////////

      case _: Stmt if tree.tpe == TypeCombStmt => tree

      //////////////////////////////////////////////////////////////////////////
      // Convert leaf statements
      //////////////////////////////////////////////////////////////////////////

      case _: StmtFence =>
        val ref = ExprSym(followingState.top)
        StmtGoto(ref) regularize tree.loc

      case _: StmtBreak =>
        val ref = ExprSym(breakTargets.top)
        StmtGoto(ref) regularize tree.loc

      case _: StmtContinue =>
        val ref = ExprSym(continueTargets.top)
        StmtGoto(ref) regularize tree.loc

      case StmtGoto(ExprSym(symbol)) =>
        val ref = ExprSym(func2state(symbol))
        StmtGoto(ref) regularize tree.loc

      case stmt: StmtReturn =>
        def stmtGotoStateFollowing(returnee: Symbol): StmtGoto = {
          val returnStateAlias =
            cc.newSymbol(s"l${stmt.loc.line}_state_alias_following_${returnee.name}_call", stmt.loc)
          returnStateAlias.kind = TypeState

          functionCallReturningTo(returnStateAlias) = returnee

          StmtGoto(ExprSym(returnStateAlias))
        }

        lazy val pop = ExprSym(rsSymbol) select "pop" call Nil

        (
          currentFunction.attr.popStackOnReturn.value,
          currentFunction.attr.staticReturnPoint.value
        ) match {
          case (true, Some(returnee)) =>
            Thicket(List(StmtExpr(pop), stmtGotoStateFollowing(returnee))) regularize tree.loc
          case (false, Some(returnee)) =>
            stmtGotoStateFollowing(returnee) regularize tree.loc
          case (true, None) =>
            StmtGoto(pop) regularize tree.loc
          case (false, None) => unreachable
        }

      case StmtExpr(ExprCall(ExprSym(symbol), Nil)) =>
        val ref = ExprSym(func2state(symbol))
        if (symbol.attr.pushStackOnCall.value) {
          val ret = ArgP(ExprSym(followingState.top))
          val push = ExprSym(rsSymbol) select "push" call List(ret)
          Thicket(List(StmtExpr(push), StmtGoto(ref))) regularize tree.loc
        } else {
          StmtGoto(ref) regularize tree.loc
        }

      //////////////////////////////////////////////////////////////////////////
      // Convert if
      //////////////////////////////////////////////////////////////////////////

      case stmt @ StmtIf(_, thenStmts, elseStmts) =>
        val newThenStmts = convertControlUnits(thenStmts)
        val newElseStmts = convertControlUnits(elseStmts)
        stmt.copy(thenStmts = newThenStmts, elseStmts = newElseStmts) regularize tree.loc

      //////////////////////////////////////////////////////////////////////////
      // Convert case
      //////////////////////////////////////////////////////////////////////////

      case stmt @ StmtCase(_, cases) =>
        val newCases = cases map {
          case CaseRegular(cond, stmts) =>
            CaseRegular(cond, convertControlUnits(stmts))
          case CaseDefault(stmts) =>
            CaseDefault(convertControlUnits(stmts))
          case _: CaseGen => unreachable
        }

        stmt.copy(cases = newCases) regularize tree.loc

      //////////////////////////////////////////////////////////////////////////
      // Convert block
      //////////////////////////////////////////////////////////////////////////

      case StmtBlock(body) =>
        TypeAssigner(StmtBlock(convertControlUnits(body)) withLoc tree.loc)

      //////////////////////////////////////////////////////////////////////////
      // Convert loop
      //////////////////////////////////////////////////////////////////////////

      case StmtLoop(body) => {
          val head = convertControlUnits(body)
          // Emit the loop entry state if necessary
          val stmt = emitState(head) match {
            case Some(symbol) =>
              // Loop entry state was emitted, so the containing state
              // needs to go to the emitted state
              val ref = ExprSym(symbol) regularize symbol.loc
              StmtGoto(ref)
            case None =>
              // Loop entry state was not emitted (because the containing
              // state is empty), so the containing state becomes the loop
              // entry state
              StmtBlock(head)
          }
          TypeAssigner(stmt withLoc tree.loc)
        } tap { _ =>
          breakTargets.pop()
          followingState.pop()
          continueTargets.pop()
        }

      //////////////////////////////////////////////////////////////////////////
      // Handle control functions
      //////////////////////////////////////////////////////////////////////////

      case _: DeclFunc => Stump

      case DefnFunc(_, _, body) => {
          splitControlUnits(body) foreach emitState
          Stump
        } tap { _ =>
          followingState.pop()
        }

      //////////////////////////////////////////////////////////////////////////
      // Convert entity
      //////////////////////////////////////////////////////////////////////////

      case decl: DeclEntity =>
        val newDecls = List from {
          decl.decls.iterator ++ {
            finishedStates.iterator map { ent =>
              ent.defn.symbol.mkDecl regularize ent.loc
            }
          }
        }
        TypeAssigner(decl.copy(decls = newDecls) withLoc decl.loc)

      case defn: DefnEntity =>
        // Now all the return state aliases have been created, resolve
        // all the aliases within the emitted states.
        val bindings = Bindings from {
          functionCallReturningTo.iterator map {
            case (aSymbol, fSymbol) => aSymbol -> ExprSym(stateFollowingCallOf(fSymbol))
          }
        }

        val resolve = new ReplaceTermRefs(bindings)

        val newBody = List from {
          defn.body.iterator ++ (finishedStates.iterator map { _ rewrite resolve })
        }

        TypeAssigner(defn.copy(body = newBody) withLoc defn.loc)

      //////////////////////////////////////////////////////////////////////////
      // Die if we missed a control statement
      //////////////////////////////////////////////////////////////////////////

      case node: Stmt if node.tpe == TypeCtrlStmt =>
        cc.ice(node, "Cannot convert control statement", node.toSource)

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
    assert(continueTargets.isEmpty)
    assert(pendingStates.isEmpty)

    tree visit {
      case node: Tree if !node.hasTpe                    => cc.ice(node, "Lost tpe of", node.toString)
      case node: DeclFunc if node.symbol.kind.isCtrlFunc => cc.ice(node, "Control Function remains")
      case node: DefnFunc if node.symbol.kind.isCtrlFunc => cc.ice(node, "Control Function remains")
      case node: StmtLoop                                => cc.ice(node, "Loop remains")
      case node: StmtFence                               => cc.ice(node, "Fence statement remains")
      case node: StmtBreak                               => cc.ice(node, "Break statement remains")
      case node: StmtReturn                              => cc.ice(node, "Return statement remains")
      case node @ ExprCall(ref, _) if ref.tpe == TypeCtrlStmt =>
        cc.ice(node, "Control function call remains")
    }
  }

}

object ConvertControl extends PairTransformerPass {
  val name = "convert-control"

  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) = {
    (decl, defn) match {
      case (dcl: DeclEntity, _: DefnEntity) =>
        if (dcl.functions.isEmpty) {
          // If no functions, then there is nothing to do
          (decl, defn)
        } else {
          // Perform the transform
          val transformer = new ConvertControl
          // First transform the defn
          val newDefn = transformer(defn)
          // Then transform the decl
          val newDecl = transformer(decl)
          (newDecl, newDefn)
        }
      case _ => (decl, defn)
    }
  }

}
