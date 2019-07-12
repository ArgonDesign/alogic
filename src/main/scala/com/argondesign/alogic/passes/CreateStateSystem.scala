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
// - Convert EntityNamed to EntityLowered by constructing the state dispatch
// - Drop all StmtFence
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

final class CreateStateSystem(implicit cc: CompilerContext) extends TreeTransformer {

  override def skip(tree: Tree): Boolean = tree match {
    case entity: Entity => entity.states.isEmpty
    case _: EntState    => false
    case _: Stmt        => false
    case _: Case        => false
    case _              => true
  }

  override def transform(tree: Tree): Tree = tree match {

    case _: StmtFence => TypeAssigner(Thicket(Nil) withLoc tree.loc)

    case EntState(expr @ ExprInt(_, _, value), body) => {
      EntState(expr, StmtComment(s"State ${value} - line ${tree.loc.line}") :: body) regularize tree.loc
    }

    case entity: Entity => {
      val newBody = List from {
        // Drop states and the comb process
        entity.body.iterator filter {
          case _: EntCombProcess => false
          case _: EntState       => false
          case _                 => true
        } concat {
          // Add the comb process back with the state dispatch
          Iterator single {
            // Ensure entry state is the first
            val (entryState, otherStates) = entity.states partition {
              case EntState(ExprInt(_, _, value), _) => value == 0
              case _                                 => unreachable
            }

            val dispatch = entryState ::: otherStates match {
              case Nil          => Nil
              case first :: Nil => first.stmts
              case first :: second :: Nil => {
                StmtComment("State dispatch") :: StmtIf(
                  ~ExprRef(entitySymbol.attr.stateVar.value),
                  first.stmts,
                  second.stmts
                ) :: Nil
              }
              case first :: rest => {
                StmtComment("State dispatch") :: StmtCase(
                  ExprRef(entitySymbol.attr.stateVar.value),
                  CaseDefault(first.stmts) :: {
                    rest map {
                      case EntState(expr, body) => CaseRegular(List(expr), body)
                    }
                  }
                ) :: Nil
              }
            }

            dispatch foreach { _ regularize entity.loc }

            assert(entity.combProcesses.lengthIs <= 1)

            TypeAssigner {
              entity.combProcesses.headOption map { tree =>
                EntCombProcess(tree.stmts ::: dispatch) withLoc tree.loc
              } getOrElse {
                EntCombProcess(dispatch) withLoc dispatch.head.loc
              }
            }
          }
        }
      }

      TypeAssigner(entity.copy(body = newBody) withLoc entity.loc)
    }

    case _ => tree
  }

  override protected def finalCheck(tree: Tree): Unit = {
    tree visit {
      case node: EntState  => cc.ice(node, "Entity states remain")
      case node: StmtFence => cc.ice(node, "StmtFence remains")
    }
  }
}

object CreateStateSystem extends TreeTransformerPass {
  val name = "create-state-system"
  def create(implicit cc: CompilerContext) = new CreateStateSystem
}
