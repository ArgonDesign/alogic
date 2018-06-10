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
// Convert EntityNamed to EntityLowered
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.typer.TypeAssigner

final class CreateStateSystem(implicit cc: CompilerContext) extends TreeTransformer {

  override def skip(tree: Tree): Boolean = tree match {
    case _: EntityNamed => false
    case _: State       => false
    case _              => true
  }

  // TODO: Strip all fences
  override def transform(tree: Tree): Tree = tree match {

    case entity: EntityNamed => {
      // TODO: polish off entry state handling (currently always state 0 and first in the list)
      val dispatch = entity.states match {
        case Nil          => Nil
        case first :: Nil => first.body
        case first :: second :: Nil => {
          StmtComment("State dispatch") :: StmtIf(
            ~ExprRef(entitySymbol.attr.stateVar.value),
            StmtBlock(first.body),
            Some(StmtBlock(second.body))
          ) :: Nil
        }
        case first :: rest => {
          StmtComment("State dispatch") :: StmtCase(
            ExprRef(entitySymbol.attr.stateVar.value),
            DefaultCase(StmtBlock(first.body)) :: {
              rest map {
                case State(expr, body) => RegularCase(List(expr), StmtBlock(body))
              }
            }
          ) :: Nil
        }
      }

      dispatch foreach { _ regularize entity.loc }

      TypeAssigner {
        EntityLowered(
          entity.symbol,
          entity.declarations,
          entity.instances,
          entity.connects,
          entity.fenceStmts ::: dispatch,
          entity.verbatim
        ) withLoc entity.loc
      }
    }

    case State(expr @ ExprInt(_, _, value), body) => {
      State(expr, StmtComment(s"State ${value}") :: body) regularize tree.loc
    }

    case _ => tree
  }

  override protected def finalCheck(tree: Tree): Unit = {
    tree visit {
      case node: EntityNamed => cc.ice(node, "EntityNamed remains")
    }
  }
}

object CreateStateSystem extends TreeTransformerPass {
  val name = "create-state-system"
  def create(implicit cc: CompilerContext) = new CreateStateSystem
}
