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
// Any symbol that is either:
//  - On the left hand side of an assignment statement
//  - Is an output not driven through a connect
//  - Is an interconnect symbol not driven through a connect
// must be assigned a value on all code paths to avoid latches. In this phase
// we mark all such symbols.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.FollowedBy
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

final class DefaultAssignments(implicit cc: CompilerContext)
    extends TreeTransformer
    with FollowedBy {

  // TODO: skip early for verbatim entities

  private val needsDefault = mutable.Set[TermSymbol]()

  // Given an expression, return an iterable of symbols that would be assigned
  // should this expression be used on the left hand side of an assignment
  private def assignmentTargets(expr: Expr): Iterator[TermSymbol] = {
    expr match {
      case ExprRef(Sym(symbol: TermSymbol)) => Iterator.single(symbol)
      case ExprCat(parts)                   => parts.toIterator flatMap assignmentTargets
      case ExprIndex(expr, _)               => assignmentTargets(expr)
      case ExprSlice(expr, _, _, _)         => assignmentTargets(expr)
      case _                                => Iterator.empty
    }
  }

  override def enter(tree: Tree): Unit = tree match {
    case StmtAssign(lhs, _) => {
      needsDefault ++= assignmentTargets(lhs)
    }

    case Decl(symbol, _) if symbol.denot.kind.isInstanceOf[TypeOut] => {
      needsDefault += symbol
    }

    case Decl(symbol, _) if symbol.attr.interconnect.isSet => {
      needsDefault += symbol
    }

    case _ =>
  }

  // Check whether a symbol is assigned on all
  // possible paths through a list of statements
  def isAssignedOnAllPaths(symbol: TermSymbol, stmts: List[Stmt]): Boolean = {
    def check(stmts: List[Stmt]): Boolean = {
      if (stmts.isEmpty) {
        false
      } else {
        val head :: tail = stmts

        val assignedHere = head match {
          case StmtAssign(lhs, _) => {
            assignmentTargets(lhs) contains symbol
          }
          case StmtIf(_, _, None) => false
          case StmtIf(_, thenStmt, Some(elseStmt)) => {
            check(List(thenStmt)) && check(List(elseStmt))
          }
          case StmtCase(_, _, Nil) => false
          case StmtCase(_, cases, default) => {
            (cases forall { case CaseClause(_, body) => check(List(body)) }) && check(default)
          }
          case StmtBlock(body) => check(body)
          case _: StmtStall    => false
          case _: StmtFence    => false
          case _               => unreachable
        }

        assignedHere || check(tail)
      }
    }

    check(stmts)
  }

  override def transform(tree: Tree): Tree = tree match {
    case entity: Entity if entity.variant != "verbatim" => {
      // Remove any nets driven through a connect
      for (Connect(_, List(rhs)) <- entity.connects) {
        rhs.visit {
          case ExprRef(Sym(symbol: TermSymbol)) => {
            needsDefault remove symbol
          }
        }
      }

      // Remove any symbols that are assigned through all
      // code paths through the state system
      needsDefault retain { symbol =>
        val assignedInFence = isAssignedOnAllPaths(symbol, entity.fenceStmts)

        lazy val assignedInAllStates = entity.states forall {
          case State(_, body) => isAssignedOnAllPaths(symbol, body)
        }

        !assignedInFence && !assignedInAllStates
      }

      val newFenceStms = for {
        Decl(symbol, _) <- entity.declarations
        if needsDefault contains symbol
      } yield {
        val kind = symbol.denot.kind
        val signed = kind.isSigned
        val width = kind.width.value.get.toInt
        StmtAssign(ExprRef(Sym(symbol)), ExprInt(signed, width, 0)) regularize symbol.loc
      }

      if (newFenceStms.isEmpty) {
        tree
      } else {
        TypeAssigner {
          entity.copy(
            fenceStmts = newFenceStms ::: entity.fenceStmts
          ) withVariant entity.variant withLoc tree.loc
        }
      }
    }

    case _ => tree
  }

}
