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

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.transform.StatementFilter
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

final class OptimizeClearOnStall(implicit cc: CompilerContext) extends TreeTransformer {

  // TODO: This assumes clearOnStall signals are alone on the lhs of assignments
  // If this is a problem, start by teaching SimplifyCat more cases. It also
  // assumes stall conditions and clearOnStall assignment sources are read-only
  // signals.

  override def skip(tree: Tree): Boolean = tree match {
    case entity: Entity => entity.combProcesses.isEmpty
    case _              => true
  }

  // Given a list of statements, return a list of all linear paths through
  // these statements, without any branches or blocks
  private def enumeratePaths(stmts: List[Stmt]): List[List[Stmt]] = {
    val (init, rest) = stmts span {
      case _: StmtAssign => true
      case _: StmtStall  => true
      case _             => false
    }

    rest match {
      case Nil => List(init)
      case branch :: tail =>
        val branches = branch match {
          case StmtBlock(body) => enumeratePaths(body)
          case StmtIf(_, thenStmts, elseStmts) => {
            enumeratePaths(thenStmts) ::: enumeratePaths(elseStmts)
          }
          case StmtCase(_, cases) => {
            cases flatMap {
              case CaseRegular(_, stmts) => enumeratePaths(stmts)
              case CaseDefault(stmts)    => enumeratePaths(stmts)
              case _: CaseGen            => unreachable
            }
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

  override def enter(tree: Tree): Unit = tree match {
    case entity: Entity => {
      // Candidates for having clearOnStall removed
      val candidateSymbols = mutable.Set from {
        entity.declarations collect {
          case Decl(symbol, _) if symbol.attr.clearOnStall contains true => symbol
        }
      }

      if (candidateSymbols.nonEmpty) {
        assert(entity.combProcesses.lengthIs == 1)
        val block = StmtBlock(entity.combProcesses.head.stmts) regularize tree.loc

        // Discard everything that is not a StallStmt
        // or an assignment to one of our candidates
        val trimmed = block rewrite StatementFilter {
          case _: StmtStall                   => true
          case StmtAssign(ExprRef(symbol), _) => symbol.attr.clearOnStall contains true
          case _: StmtAssign                  => false
        }

        // Get the list of statements
        val body = trimmed match {
          case StmtBlock(body) => body
          case other: Stmt     => List(other)
          case _               => unreachable
        }

        // Check each path through the statements
        for (path <- enumeratePaths(body)) {
          // Gather all the distinct stall conditions
          val stallConditions = (path collect { case StmtStall(cond) => cond }).distinct
          stallConditions match {
            case Nil => {
              // No stall conditions through this path, we are safe
            }
            case (cond @ ExprRef(sSymbol)) :: Nil => {
              // There is a single stall condition. Remove candidates that are
              // neither gated by this signal nor are assigned this signal.
              // The point being is that if the signal is gated by the stall
              // condition signal, then it's value is don't care if a stall is
              // required, hence the signal in question can be anything when we
              // stall, no need to clear it on stall.
              path filterNot {
                case StmtAssign(ExprRef(cand), rhs) => {
                  (cand.attr.dontCareUnless.get contains sSymbol) || (rhs == cond)
                }
                case _ => false
              } foreach {
                case StmtAssign(ExprRef(s: TermSymbol), _) => candidateSymbols remove s
                case _                                     =>
              }
            }
            case cond :: Nil => {
              // There is a single generic stall condition on this path, remove
              // all candidates that are not assigned this condition.
              path filter {
                case StmtAssign(_: ExprRef, `cond`) => false
                case _                              => true
              } foreach {
                case StmtAssign(ExprRef(s: TermSymbol), _) => candidateSymbols remove s
                case _                                     =>
              }
            }
            case _ => {
              // There are 2 or more stall conditions on this path, remove all
              // candidates that are assigned anything on this path
              path foreach {
                case StmtAssign(ExprRef(s: TermSymbol), _) => candidateSymbols remove s
                case _                                     =>
              }
            }
          }
        }

        // Remove attributes from the remaining candidates
        for (symbol <- candidateSymbols) {
          symbol.attr.clearOnStall.clear()
        }
      }
    }

    case _ => unreachable
  }

}

object OptimizeClearOnStall extends TreeTransformerPass {
  val name = "optimize-clear-on-stall"
  def create(implicit cc: CompilerContext) = new OptimizeClearOnStall
}
