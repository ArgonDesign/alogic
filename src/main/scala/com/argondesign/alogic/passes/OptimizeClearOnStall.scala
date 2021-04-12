////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Remove clearOnStall attributes from signals that for all code path,
// are only assigned the same values as the stall conditions in that code path,
// or if the signal is a don't care when that stall condition occurs
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.transform.StatementFilter
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.mutable

private object OptimizeClearOnSallTransform extends StatelessTreeTransformer {

  // TODO: This assumes clearOnStall signals are alone on the lhs of assignments
  // If this is a problem, start by teaching SimplifyCat more cases. It also
  // assumes stall conditions and clearOnStall assignment sources are read-only
  // signals.

  // Given a list of statements, return a list of all linear paths through
  // these statements, without any branches or blocks
  private def enumeratePaths(stmts: List[Stmt]): LazyList[List[Stmt]] = {
    val (init, rest) = stmts span {
      case _: StmtAssign | _: StmtWait => true
      case _                           => false
    }

    rest match {
      case Nil => LazyList(init)
      case branch :: tail =>
        val branches = branch match {
          case StmtBlock(body) =>
            enumeratePaths(body)
          case StmtIf(_, thenStmts, Nil) =>
            // We can ignore the empty branch, as the non-empty branch will
            // always yield a more conservative result.
            enumeratePaths(thenStmts)
          case StmtIf(_, thenStmts, elseStmts) =>
            enumeratePaths(thenStmts) concat enumeratePaths(elseStmts)
          case StmtCase(_, cases) =>
            // We can ignore the empty branches, as the non-empty branch will
            // always yield a more conservative result.
            val ll = LazyList
              .from(cases)
              .collect {
                case CaseRegular(_, ss) if ss.nonEmpty => enumeratePaths(ss)
                case CaseDefault(ss) if ss.nonEmpty    => enumeratePaths(ss)
              }
              .flatten
            // Note: we still need to yield an empty path if all branches are
            // empty!
            if (ll.nonEmpty) ll else LazyList(Nil)
          case _ => unreachable
        }

        val tails = enumeratePaths(tail)

        for {
          branch <- branches
          tail <- tails
        } yield {
          init ::: branch ::: tail
        }
    }
  }

  override def start(tree: Tree): Unit = tree match {
    case defn: DefnEntity if defn.combProcesses.nonEmpty =>
      // Candidates for having clearOnStall removed
      val candidateSymbols = mutable.Set from {
        defn.defns collect {
          case Defn(symbol) if symbol.attr.clearOnStall contains true => symbol
        }
      }

      if (candidateSymbols.nonEmpty) {
        assert(defn.combProcesses.lengthIs == 1)

        // Discard everything that is not a StallStmt
        // or an assignment to one of our candidates
        val trimmed = {
          val block = StmtBlock(defn.combProcesses.head.stmts) regularize tree.loc

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

          filter(block) match {
            case stmt: StmtBlock => stmt.body
            case Stump           => Nil
            case _               => unreachable
          }
        }

        // Check each path through the statements
        @tailrec
        def loop(paths: Iterator[Iterable[Stmt]]): Unit =
          if (paths.hasNext && candidateSymbols.nonEmpty) { // Terminate early if no candidates left
            val path = paths.next()
            // Gather all the distinct stall conditions
            val stallConditions = path.iterator.collect { case StmtWait(cond) => cond }.distinct
            if (!stallConditions.hasNext) {
              // No stall conditions through this path, we are safe
            } else {
              val cond = stallConditions.next()
              if (!stallConditions.hasNext) {
                // Single stall condition
                cond match {
                  case ExprSym(sSymbol) =>
                    // There is a single stall condition. Remove candidates that are
                    // neither gated by this signal nor are assigned this signal.
                    // The point being is that if the signal is gated by the stall
                    // condition signal, then it's value is don't care if a stall is
                    // required, hence the signal in question can be anything when we
                    // stall, no need to clear it on stall.
                    path.foreach {
                      case StmtAssign(ExprSym(cand), rhs)
                          if !(cand.attr.dontCareUnless.get contains sSymbol) && (rhs != cond) =>
                        candidateSymbols remove cand
                      case _ => //
                    }
                  case _ =>
                    // There is a single generic stall condition on this path, remove
                    // all candidates that are not assigned this condition.
                    path.foreach {
                      case StmtAssign(ExprSym(cand), c) if c != cond => candidateSymbols remove cand
                      case _                                         =>
                    }
                }
              } else {
                // There are 2 or more stall conditions on this path, remove all
                // candidates that are assigned anything on this path
                path.foreach {
                  case StmtAssign(ExprSym(s), _) => candidateSymbols remove s
                  case _                         =>
                }
              }
            }
            loop(paths)
          }

        // If no wait statements we can clear all clearOnStall flags, otherwise
        // analyse the code.
        if (trimmed.exists(_.exists { case _: StmtWait => true })) {
          loop(enumeratePaths(trimmed).iterator)
        }

        // Remove attributes from the remaining candidates
        for (symbol <- candidateSymbols) {
          symbol.attr.clearOnStall.clear()
        }
      }

    case _ =>
  }

  // Skip all
  override def enter(tree: Tree): Option[Tree] = Some(tree)
}

object OptimizeClearOnStall extends EntityTransformerPass(declFirst = true) {
  val name = "optimize-clear-on-stall"

  def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
    OptimizeClearOnSallTransform
}
