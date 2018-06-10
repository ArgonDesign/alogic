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
// Any symbol driven combinatorially through the FSM logic must be assigned a
// value on all code paths to avoid latches. In this phase we add all such
// default assignments.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.analysis.Liveness
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.FollowedBy

import scala.collection.mutable

final class DefaultAssignments(implicit cc: CompilerContext)
    extends TreeTransformer
    with FollowedBy {

  private val needsDefault = mutable.Set[TermSymbol]()

  override def skip(tree: Tree): Boolean = tree match {
    case entity: Entity => entity.states.isEmpty
    case _              => false
  }

  override def enter(tree: Tree): Unit = tree match {
    case Decl(symbol, _) if symbol.kind.isIn || symbol.kind.isConst => ()

    case Decl(symbol, _) if !symbol.attr.flop.isSet && !symbol.attr.memory.isSet => {
      needsDefault += symbol
    }

    case _ => ()
  }

  override def transform(tree: Tree): Tree = tree match {
    case entity: Entity if needsDefault.nonEmpty => {
      // Remove any nets driven through a connect
      for (Connect(_, List(rhs)) <- entity.connects) {
        rhs.visit {
          case ExprRef(symbol: TermSymbol) => {
            needsDefault remove symbol
          }
        }
      }

      if (needsDefault.nonEmpty) {
        assert(entity.states.nonEmpty)

        // Remove symbols that are dead at the beginning of the cycle. To do
        // this, we build the case statement representing the state dispatch
        // (together with the fence statements), and do liveness analysis on it
        val deadSymbols = {
          // TODO factor out this construction
          val stateSystem = if (entity.states.lengthCompare(1) == 0) {
            entity.fenceStmts ::: entity.states.head.body
          } else {
            entity.fenceStmts :+ StmtCase(
              ExprRef(entitySymbol.attr.stateVar.value),
              DefaultCase(StmtBlock(entity.states.head.body)) :: {
                entity.states.tail map {
                  case State(expr, body) => RegularCase(List(expr), StmtBlock(body))
                }
              }
            )
          }

          // Perform the liveness analysis
          val deadSymbolBits = Liveness(stateSystem)._2

          // Keep only the symbols with all bits dead
          val it = deadSymbolBits collect {
            case (symbol, set) if set.size == symbol.kind.width => symbol
          }
          it.toSet
        }

        // Now retain only the symbols that are not dead
        needsDefault retain { symbol =>
          !(deadSymbols contains symbol)
        }
      }

      if (needsDefault.isEmpty) {
        tree
      } else {
        val newFenceStms = for {
          Decl(symbol, _) <- entity.declarations
          if needsDefault contains symbol
        } yield {
          // Initialize items to their default values, otherwise zero
          val init = symbol.attr.default.getOrElse {
            val kind = symbol.kind
            ExprInt(kind.isSigned, kind.width, 0)
          }
          StmtAssign(ExprRef(symbol), init) regularize symbol.loc
        }

        TypeAssigner {
          entity.copy(
            fenceStmts = newFenceStms ::: entity.fenceStmts
          ) withLoc tree.loc
        }
      }
    }

    case _ => tree
  }

}

object DefaultAssignments extends TreeTransformerPass {
  val name = "default-assignments"
  def create(implicit cc: CompilerContext) = new DefaultAssignments
}
