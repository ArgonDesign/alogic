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
// Allocate state numbers
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Math
import com.argondesign.alogic.typer.TypeAssigner

import scala.collection.mutable

final class AllocStates(implicit cc: CompilerContext) extends TreeTransformer {

  // Number of states
  private[this] var nStates: Int = _
  // Number of bits in state variable
  private[this] lazy val stateBits = Math.clog2(nStates)
  // The return stack symbol
  private[this] var rsSymbol: TermSymbol = _
  // The number of the current state being processed
  private[this] var currStateNum: Int = _
  // Map from state symbol to state number
  private[this] val stateMap: mutable.Map[TermSymbol, Int] = mutable.Map()

  override def enter(tree: Tree): Unit = tree match {
    case entity: Entity => {
      nStates = entity.states.length

      if (nStates > 1) {
        // For now, just allocate state numbers linearly as binary coded
        val it = Stream.from(0).iterator

        // Ensure the entry symbol is allocated number 0
        val (entryStates, otherStates) = entity.states partition { state =>
          val State(ExprRef(Sym(symbol: TermSymbol)), _) = state
          symbol.denot.attr contains "entry"
        }

        assert(entryStates.length == 1)

        val State(ExprRef(Sym(entrySymbol: TermSymbol)), _) = entryStates.head
        val num = it.next()
        entrySymbol.addAttr("statenumber" -> ExprInt(false, stateBits, num))
        stateMap(entrySymbol) = num

        for (State(ExprRef(Sym(symbol: TermSymbol)), _) <- otherStates) {
          val num = it.next()
          symbol.addAttr("statenumber" -> ExprInt(false, stateBits, num))
          stateMap(symbol) = num
        }
      }

      // Gather the return stack symbol if exists and update it's type
      entity.declarations.headOption foreach { decl =>
        val Sym(symbol: TermSymbol) = decl.ref
        if (symbol.denot.name.str == "return_stack") {
          val TypeStack(_, depth) = symbol.denot.kind
          val width = Expr(stateBits) regularize symbol.loc
          symbol withDenot symbol.denot.copy(kind = TypeStack(TypeUInt(width), depth))
          rsSymbol = symbol
        }
      }
    }

    case State(ExprRef(Sym(symbol: TermSymbol)), _) if nStates > 1 => {
      currStateNum = stateMap(symbol)
    }

    case _ =>
  }

  override def transform(tree: Tree): Tree = {
    // Nodes with children that have been rewritten need their types assigned
    if (!tree.hasTpe) {
      TypeAssigner(tree)
    }

    val result = if (nStates == 1) {
      // If there is only 1 state, optimize the design by
      // omitting the state variable altogether
      tree match {
        // Convert goto to fence
        case StmtGoto(_) => {
          StmtFence() withLoc tree.loc
        }

        // Drop push to return stack
        case StmtExpr(ExprCall(ExprSelect(ExprRef(Sym(symbol)), _), _)) if symbol == rsSymbol => {
          StmtBlock(Nil) withLoc tree.loc
        }

        // Drop the return stack definition if exists
        case entity: Entity if rsSymbol != null => {
          entity.copy(
            declarations = entity.declarations.tail
          ) withVariant entity.variant withLoc entity.loc
        }

        case _ => tree
      }
    } else {
      tree match {
        // Replace references to states with the state numbers
        case ExprRef(Sym(symbol: TermSymbol)) if stateMap contains symbol => {
          ExprInt(false, stateBits, stateMap(symbol)) withLoc tree.loc
        }

        // Convert goto <current state> to fence
        case StmtGoto(ExprInt(false, `stateBits`, value)) if value == currStateNum => {
          StmtFence() withLoc tree.loc
        }

        // Update the type of the return stack decl
        case decl @ Decl(Sym(symbol), _, _) if symbol == rsSymbol => {
          decl.copy(kind = symbol.denot.kind) withLoc tree.loc
        }

        case _ => tree
      }
    }

    if (result != tree) {
      TypeAssigner(result)
    }

    result
  }

  override def finalCheck(tree: Tree): Unit = {
    tree visit {
      case node: Tree if !node.hasTpe => cc.ice(node, "Lost node.tpe", node.toString)
    }
  }

}
