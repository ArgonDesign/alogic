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
  private[this] lazy val stateVarSymbol: TermSymbol = {
    val loc = entitySymbol.loc
    val kind = TypeAssigner(Expr(stateBits) withLoc loc)
    val symbol = cc.newTermSymbol("state", loc, TypeUInt(kind))
    entitySymbol.attr.stateVar set symbol
    symbol
  }

  // The return stack symbol
  private[this] lazy val rsSymbol: Option[TermSymbol] =
    entitySymbol.attr.returnStack.get map { symbol =>
      symbol.kind match {
        case TypeStack(_, depth) =>
          val width = Expr(stateBits) regularize symbol.loc
          symbol.kind = TypeStack(TypeUInt(width), depth)
          symbol
        case _ => unreachable
      }
    }

  // The number of the current state being processed
  private[this] var currStateNum: Int = _
  // Map from state symbol to state number
  private[this] val stateMap: mutable.Map[TermSymbol, Int] = mutable.Map()

  override def skip(tree: Tree): Boolean = tree match {
    case entity: EntityNamed => entity.states.isEmpty
    case _                   => false
  }

  override def enter(tree: Tree): Unit = tree match {
    case entity: EntityNamed => {
      nStates = entity.states.length

      if (nStates > 1) {
        // For now, just allocate state numbers linearly as binary coded
        val it = new SequenceNumbers

        // Ensure the entry symbol is allocated number 0
        val (entryStates, otherStates) = entity.states partition {
          case State(ExprRef(symbol: TermSymbol), _) => symbol.attr.entry.isSet
          case _                                     => unreachable
        }

        assert(entryStates.length == 1)

        entryStates.head match {
          case State(ExprRef(entrySymbol: TermSymbol), _) => stateMap(entrySymbol) = it.next
          case _                                          => unreachable
        }

        for (State(ExprRef(symbol: TermSymbol), _) <- otherStates) {
          stateMap(symbol) = it.next
        }
      }

      // force lazy val to gather the return stack symbol if exists and update it's type
      rsSymbol
    }

    case State(ExprRef(symbol: TermSymbol), _) if nStates > 1 => {
      currStateNum = stateMap(symbol)
    }

    case _ =>
  }

  override def transform(tree: Tree): Tree = {
    val result = if (nStates == 1) {
      // If there is only 1 state, optimize the design by
      // omitting the state variable altogether
      tree match {
        // Replace references to states with the state numbers (there is only 1 state)
        case ExprRef(symbol: TermSymbol) if symbol.kind == TypeState => {
          ExprInt(false, 1, 0) withLoc tree.loc
        }

        // Convert goto to fence
        case StmtGoto(_) => {
          StmtFence() withLoc tree.loc
        }

        // Drop push to return stack
        case StmtExpr(ExprCall(ExprSelect(ExprRef(symbol), _), _)) if rsSymbol contains symbol => {
          Thicket(Nil) withLoc tree.loc
        }

        // Drop the return stack definition if exists
        case entity: EntityNamed if rsSymbol.nonEmpty => {
          entity.copy(
            declarations = entity.declarations.tail
          ) withLoc entity.loc
        }

        case _ => tree
      }
    } else if (nStates > 1) {
      tree match {
        // Replace references to states with the state numbers
        case ExprRef(symbol: TermSymbol) if stateMap contains symbol => {
          ExprInt(false, stateBits, stateMap(symbol)) withLoc tree.loc
        }

        // Convert goto <current state> to fence
        case StmtGoto(ExprInt(false, _, value)) if value == currStateNum => {
          StmtFence() withLoc tree.loc
        }

        // Convert goto <other state> to state assignment and fence
        case StmtGoto(expr) => {
          StmtBlock(
            List(
              StmtAssign(ExprRef(stateVarSymbol), expr) regularize tree.loc,
              StmtFence() regularize tree.loc
            )
          ) withLoc tree.loc
        }

        // Emit state variable declaration
        case entity: EntityNamed => {
          // TODO: polish off entry state handling (currently always state 0)
          val init = ExprInt(false, stateVarSymbol.kind.width, 0)
          val decl = Decl(stateVarSymbol, Some(init)) regularize stateVarSymbol.loc
          entity.copy(
            declarations = decl :: entity.declarations
          ) withLoc entity.loc
        }

        case _ => tree
      }
    } else {
      tree
    }

    if (result ne tree) {
      TypeAssigner(result)
    }

    result
  }

  override protected def finalCheck(tree: Tree): Unit = {
    tree visit {
      case State(_: ExprInt, _) => ()
      case node @ State(expr, _) => {
        cc.ice(node, "Unallocated state remains")
      }
    }
  }

}

object AllocStates extends TreeTransformerPass {
  val name = "alloc-states"
  def create(implicit cc: CompilerContext) = new AllocStates
}
