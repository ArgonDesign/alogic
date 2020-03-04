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
// - Constructing the state dispatch statement
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
    case _: Expr => true
    case _       => false
  }

  override def transform(tree: Tree): Tree = tree match {

    case _: StmtFence => Stump

    case _: DeclState => Stump

    case desc @ DefnState(_, ExprInt(_, _, value), body) =>
      val newBody = StmtComment(s"State $value - line ${tree.loc.line}") :: body
      desc.copy(body = newBody) regularize tree.loc

    case defn: DefnEntity =>
      val newBody = List from {
        // Drop states and the comb process
        defn.body.iterator filter {
          case _: EntCombProcess     => false
          case EntDefn(_: DefnState) => false
          case _                     => true
        } concat {
          // Add the comb process back with the state dispatch
          Iterator single {
            // Ensure entry state is the first
            val (entryState, otherStates) = defn.states partition {
              case DefnState(_, ExprInt(_, _, v), _) => v == 0
              case _                                 => unreachable
            }

            val dispatch = entryState ::: otherStates match {
              case Nil          => Nil
              case first :: Nil => first.body
              case first :: second :: Nil =>
                StmtComment("State dispatch") :: StmtIf(
                  ~ExprSym(defn.symbol.attr.stateVar.value),
                  first.body,
                  second.body
                ) :: Nil
              case first :: rest =>
                StmtComment("State dispatch") :: StmtCase(
                  ExprSym(defn.symbol.attr.stateVar.value),
                  CaseDefault(first.body) :: {
                    rest map {
                      case DefnState(_, expr, body) => CaseRegular(List(expr), body)
                    }
                  }
                ) :: Nil
            }

            dispatch foreach { _ regularize defn.loc }

            assert(defn.combProcesses.lengthIs <= 1)

            TypeAssigner {
              defn.combProcesses.headOption map { tree =>
                EntCombProcess(tree.stmts ::: dispatch) withLoc tree.loc
              } getOrElse {
                EntCombProcess(dispatch) withLoc dispatch.head.loc
              }
            }
          }
        }
      }

      TypeAssigner(defn.copy(body = newBody) withLoc defn.loc)

    case _ => tree
  }

  override protected def finalCheck(tree: Tree): Unit = {
    tree visit {
      case node: DeclState => cc.ice(node, "DeclState remains")
      case node: DefnState => cc.ice(node, "DefnState remains")
      case node: StmtFence => cc.ice(node, "StmtFence remains")
    }
  }
}

object CreateStateSystem extends PairTransformerPass {
  val name = "create-state-system"
  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) = {
    (decl, defn) match {
      case (dcl: DeclEntity, _: DefnEntity) =>
        if (dcl.states.isEmpty) {
          // If no states, then there is nothing to do
          (decl, defn)
        } else {
          // Perform the transform
          val transformer = new CreateStateSystem
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
