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
// Remove clearOnStall attributes from signals that for all code path,
// are only assigned the same values as the stall conditions in that code path,
// or if the signal is a don't care when that stall condition occurs
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.transform.StatementFilter
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

final class OptimizeClearOnStall(implicit cc: CompilerContext) extends StatelessTreeTransformer {

  // TODO: This assumes clearOnStall signals are alone on the lhs of assignments
  // If this is a problem, start by teaching SimplifyCat more cases. It also
  // assumes stall conditions and clearOnStall assignment sources are read-only
  // signals.

  override def skip(tree: Tree): Boolean = tree match {
    case defn: DefnEntity => defn.combProcesses.isEmpty
    case _                => true
  }

  // Given a list of statements, return a list of all linear paths through
  // these statements, without any branches or blocks
  private def enumeratePaths(stmts: List[Stmt]): List[List[Stmt]] = {
    val (init, rest) = stmts span {
      case _: StmtAssign => true
      case _: StmtWait   => true
      case _             => false
    }

    rest match {
      case Nil => List(init)
      case branch :: tail =>
        val branches = branch match {
          case StmtBlock(body) => enumeratePaths(body)
          case StmtIf(_, thenStmts, elseStmts) =>
            enumeratePaths(thenStmts) ::: enumeratePaths(elseStmts)
          case StmtCase(_, cases) =>
            cases flatMap {
              case CaseRegular(_, ss) => enumeratePaths(ss)
              case CaseDefault(ss)    => enumeratePaths(ss)
              case _: CaseGen         => unreachable
            }
          case _ => unreachable
        }

        val inits = (branches map { init ::: _ }).distinct

        val tails = enumeratePaths(tail).distinct

        for {
          init <- inits
          tail <- tails
        } yield {
          init ::: tail
        }
    }
  }

  override def enter(tree: Tree): Option[Tree] = {
    tree match {
      case defn: DefnEntity =>
        // Candidates for having clearOnStall removed
        val candidateSymbols = mutable.Set from {
          defn.defns collect {
            case Defn(symbol) if symbol.attr.clearOnStall contains true => symbol
          }
        }

        if (candidateSymbols.nonEmpty) {
          assert(defn.combProcesses.lengthIs == 1)
          val block = StmtBlock(defn.combProcesses.head.stmts) regularize tree.loc

          // Discard everything that is not a StallStmt
          // or an assignment to one of our candidates
          val filter = {
            def leafStatement(stmt: Stmt): Boolean = stmt.children forall {
              case _: Stmt => false
              case _: Case => false
              case _       => true
            }
            StatementFilter {
              case _: StmtWait                    => true
              case StmtAssign(ExprSym(symbol), _) => symbol.attr.clearOnStall contains true
              case stmt if leafStatement(stmt)    => false // Discard other leaf statements
            }
          }
          val trimmed = filter(block) match {
            case stmt: Stmt => List(stmt)
            case Stump      => Nil
            case _          => unreachable
          }

          // Check each path through the statements
          for (path <- enumeratePaths(trimmed)) {
            // Gather all the distinct stall conditions
            val stallConditions = (path collect { case StmtWait(cond) => cond }).distinct
            stallConditions match {
              case Nil                              => // No stall conditions through this path, we are safe
              case (cond @ ExprSym(sSymbol)) :: Nil =>
                // There is a single stall condition. Remove candidates that are
                // neither gated by this signal nor are assigned this signal.
                // The point being is that if the signal is gated by the stall
                // condition signal, then it's value is don't care if a stall is
                // required, hence the signal in question can be anything when we
                // stall, no need to clear it on stall.
                path filterNot {
                  case StmtAssign(ExprSym(cand), rhs) =>
                    (cand.attr.dontCareUnless.get contains sSymbol) || (rhs == cond)
                  case _ => false
                } foreach {
                  case StmtAssign(ExprSym(s), _) => candidateSymbols remove s
                  case _                         =>
                }
              case cond :: Nil =>
                // There is a single generic stall condition on this path, remove
                // all candidates that are not assigned this condition.
                path filter {
                  case StmtAssign(_: ExprSym, `cond`) => false
                  case _                              => true
                } foreach {
                  case StmtAssign(ExprSym(s), _) => candidateSymbols remove s
                  case _                         =>
                }
              case _ =>
                // There are 2 or more stall conditions on this path, remove all
                // candidates that are assigned anything on this path
                path foreach {
                  case StmtAssign(ExprSym(s), _) => candidateSymbols remove s
                  case _                         =>
                }
            }
          }

          // Remove attributes from the remaining candidates
          for (symbol <- candidateSymbols) {
            symbol.attr.clearOnStall.clear()
          }
        }

      case _ =>
    }
    None
  }

}

object OptimizeClearOnStall extends EntityTransformerPass(declFirst = true) {
  val name = "optimize-clear-on-stall"
  def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
    cc.optimizeClearOnStall
}
