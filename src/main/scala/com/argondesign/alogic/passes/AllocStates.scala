////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018-2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// - Allocate state numbers
// - Introduce state variable
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Math
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.SequenceNumbers
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

final class AllocStates(implicit cc: CompilerContext) extends TreeTransformer {

  // Number of states
  private[this] var nStates: Int = _
  // Number of bits in state variable
  private[this] lazy val stateBits = Math.clog2(nStates)
  // The state variable symbol
  private[this] lazy val stateVarSymbol: Symbol = {
    val loc = entitySymbol.loc
    val symbol = cc.newSymbol("state", loc)
    symbol.kind = TypeUInt(stateBits)
    entitySymbol.attr.stateVar set symbol
    symbol
  }

  // The return stack symbol
  private[this] var rsSymbol: Option[Symbol] = _

  // The number of the current state being processed
  private[this] var currStateNum: Int = _
  // Map from state symbol to state number
  private[this] val stateMap: mutable.Map[Symbol, Int] = mutable.Map()

  // TODO: Re-write without lazy vals now that the pass is more structured
  override def enter(tree: Tree): Option[Tree] = {
    tree match {
      case defn: DefnEntity =>
        nStates = defn.states.length

        if (nStates > 1) {
          // For now, just allocate state numbers linearly as binary coded
          val it = new SequenceNumbers

          // Ensure the entry symbol is allocated number 0
          val (entryStates, otherStates) = defn.states partition { _.symbol.attr.entry.isSet }

          assert(entryStates.length == 1)

          stateMap(entryStates.head.symbol) = it.next

          otherStates foreach { decl =>
            stateMap(decl.symbol) = it.next
          }
        }

        // compute the return stack symbol if exists and update it's type
        rsSymbol = defn.symbol.attr.returnStack.get map { symbol =>
          symbol.kind match {
            case TypeStack(_, depth) =>
              symbol.kind = TypeStack(TypeUInt(stateBits), depth)
              symbol
            case _ => unreachable
          }
        }

      case DefnState(symbol, _, _) if nStates > 1 => currStateNum = stateMap(symbol)

      case _ =>
    }
    None
  }

  override def transform(tree: Tree): Tree = {
    val result = if (nStates == 1) {
      // If there is only 1 state, optimize the design by
      // omitting the state variable altogether
      tree match {
        // Replace references to states with the state numbers (there is only 1 state)
        case ExprSym(symbol) if symbol.kind == TypeState =>
          ExprInt(false, 1, 0) withLoc tree.loc

        // Convert goto to fence
        case _: StmtGoto => StmtFence() withLoc tree.loc

        // Drop push to return stack
        case StmtExpr(ExprCall(ExprSelect(ExprSym(symbol), _, _), _)) if rsSymbol contains symbol =>
          Stump

        // Drop the return stack decl/defn if exists
        case DeclStack(symbol, _, _) if rsSymbol contains symbol => Stump
        case DefnStack(symbol) if rsSymbol contains symbol       => Stump

        //
        case _ => tree
      }
    } else if (nStates > 1) {
      tree match {
        // Replace references to states with the state numbers
        case ExprSym(symbol) if stateMap contains symbol =>
          ExprInt(false, stateBits, stateMap(symbol)) withLoc tree.loc

        // Convert goto <current state> to fence
        case StmtGoto(ExprInt(false, _, value)) if value == currStateNum =>
          StmtFence() withLoc tree.loc

        // Convert goto <other state> to state assignment and fence
        case StmtGoto(expr) =>
          Thicket(
            List(
              StmtAssign(ExprSym(stateVarSymbol), expr) regularize tree.loc,
              StmtFence() regularize tree.loc
            )
          )

        // Add state variable decl/defn
        case decl: DeclEntity =>
          val newDecl = stateVarSymbol.mkDecl regularize stateVarSymbol.loc
          decl.copy(decls = newDecl :: decl.decls) withLoc decl.loc
        case defn: DefnEntity =>
          // TODO: polish off entry state handling (currently always state 0)
          val init = ExprInt(false, stateVarSymbol.kind.width.toInt, 0)
          val newDefn = EntDefn(stateVarSymbol.mkDefn(init)) regularize stateVarSymbol.loc
          defn.copy(body = newDefn :: defn.body) withLoc defn.loc

        //
        case _ => tree
      }
    } else {
      tree
    }

    if ((result ne tree) && !result.hasTpe) {
      TypeAssigner(result)
    }

    result
  }

  override protected def finalCheck(tree: Tree): Unit = {
    tree visit {
      case DefnState(_, _: ExprInt, _) => ()
      case node: DefnState             => cc.ice(node, "Unallocated state remains")
    }
  }

}

object AllocStates extends PairTransformerPass {
  val name = "alloc-states"
  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) = {
    (decl, defn) match {
      case (dcl: DeclEntity, _: DefnEntity) =>
        if (dcl.states.isEmpty) {
          // If no states, then there is nothing to do
          (decl, defn)
        } else {
          // Perform the transform
          val transformer = new AllocStates()
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
