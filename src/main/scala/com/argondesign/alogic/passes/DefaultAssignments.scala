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

  private val needsDefault = mutable.Set[TermSymbol]()

  // If 'lval' is true, given an expression, return an iterable of symbols that
  // would be read if this expression is used on on the left hand side of an
  // assignment. If 'lval' is false, return an iterable of all symbols
  // referenced (i.e.: read) in the expression
  private def readSymbols(lval: Boolean)(expr: Expr): Iterator[TermSymbol] = {
    if (!lval) {
      expr collect { case ExprRef(Sym(symbol: TermSymbol)) => symbol }
    } else {
      expr match {
        case _: ExprRef        => Iterator.empty
        case ExprCat(parts)    => parts.toIterator flatMap readSymbols(lval = true)
        case ExprIndex(_, idx) => readSymbols(lval = false)(idx)
        case ExprSlice(_, lidx, _, ridx) => {
          readSymbols(lval = false)(lidx) ++ readSymbols(lval = false)(ridx)
        }
        case _ => unreachable
      }
    }
  }

  // Given an expression, return an iterable of symbols that would be assigned
  // should this expression be used on the left hand side of an assignment
  private def writtenSymbols(expr: Expr): Iterator[TermSymbol] = {
    expr match {
      case ExprRef(Sym(symbol: TermSymbol)) => Iterator.single(symbol)
      case ExprCat(parts)                   => parts.toIterator flatMap writtenSymbols
      case ExprIndex(expr, _)               => writtenSymbols(expr)
      case ExprSlice(expr, _, _, _)         => writtenSymbols(expr)
      case _                                => Iterator.empty
    }
  }

  override def enter(tree: Tree): Unit = tree match {
    case StmtAssign(lhs, _) => {
      needsDefault ++= writtenSymbols(lhs)
    }

    case Decl(symbol, _) if symbol.denot.kind.isInstanceOf[TypeOut] => {
      needsDefault += symbol
    }

    case Decl(symbol, _) if symbol.attr.interconnect.isSet => {
      needsDefault += symbol
    }

    case _ =>
  }

  // Compute the liveness of a symbol through a list of statements. Returns:
  // - Some(true) iff there exists at least 1 path where the symbol
  //   is read before being writen (i.e.: the symbol is alive)
  // - Some(false) iff for all paths the symbol
  //   is written before being read (i.e.: the symbol is dead)
  // - None otherwise, which can happen if there are no reads, but there
  //   might be some writes. In this case, liveness is determined by the
  //   successors of these statements.
  def isAlive(symbol: TermSymbol, stmts: List[Stmt]): Option[Boolean] = {

    // Given the liveness across the parallel paths of a branching statement,
    // compute the liveness of the branching statement itself
    def combine(liveness: Iterator[Option[Boolean]]): Option[Boolean] = {
      def checkNext(): Option[Boolean] = {
        if (!liveness.hasNext) {
          None
        } else {
          liveness.next() match {
            // If it's alive at this branch, we can quickly conclude it's alive
            case option @ Some(true) => option
            // If if's dead at this branch, liveness is up to the other
            // branches. If there aren't any more, we conclude it's dead
            case option @ Some(false) => if (!liveness.hasNext) option else checkNext()
            // If it's liveness is indeterminate at this branch, then we
            // know it cannot be dead, but it might still be alive if it
            // is alive across any of the other branches
            case None => {
              checkNext() match {
                case option @ Some(true) => option
                case _                   => None
              }
            }
          }
        }
      }
      checkNext()
    }

    // Recursive lazy implementation
    def check(stmts: List[Stmt]): Option[Boolean] = {
      if (stmts.isEmpty) {
        None
      } else {
        val head :: tail = stmts

        val here: Option[Boolean] = head match {
          case StmtAssign(lhs, rhs) => {
            if (readSymbols(lval = false)(rhs) contains symbol) {
              Some(true)
            } else if (readSymbols(lval = true)(lhs) contains symbol) {
              Some(true)
            } else if (writtenSymbols(lhs) contains symbol) {
              Some(false)
            } else {
              None
            }
          }

          case StmtIf(cond, thenStmt, elseStmtOpt) => {
            if (readSymbols(lval = false)(cond) contains symbol) {
              Some(true)
            } else {
              combine {
                Iterator.single(check(List(thenStmt))) ++
                  Iterator.single(check(elseStmtOpt.toList))
              }
            }
          }

          case StmtCase(expr, cases, default) => {
            if (readSymbols(lval = false)(expr) contains symbol) {
              Some(true)
            } else {
              val alive = cases.view flatMap {
                case CaseClause(exprs, _) => exprs
              } exists { expr =>
                readSymbols(lval = false)(expr) contains symbol
              }
              if (alive) {
                Some(true)
              } else {
                combine {
                  Iterator.single(check(default)) ++
                    (cases.iterator map { case CaseClause(_, body) => check(List(body)) })
                }
              }
            }
          }

          case StmtBlock(body) => check(body)
          case _: StmtStall    => None
          case _: StmtFence    => None
          case _               => unreachable
        }

        here orElse check(tail)
      }
    }

    check(stmts)
  }

  override def transform(tree: Tree): Tree = tree match {
    // TODO: skip early for verbatim entities
    case entity: Entity if needsDefault.nonEmpty && entity.variant != "verbatim" => {
      // Remove any nets driven through a connect
      for (Connect(_, List(rhs)) <- entity.connects) {
        rhs.visit {
          case ExprRef(Sym(symbol: TermSymbol)) => {
            needsDefault remove symbol
          }
        }
      }

      if (needsDefault.nonEmpty) {
        assert(entity.states.nonEmpty)
        // TODO: This now walks the tree multiple times, once for each symbol,
        // it should just compute the alive/dead sets in one pass...

        // Remove symbols that are dead at the beginning of the cycle. To do
        // this, we build the case statement representing the state dispatch
        // (together with the fence statements), and do liveness analysis on it
        val stateSystem = if (entity.states.lengthCompare(1) == 0) {
          entity.fenceStmts ::: entity.states.head.body
        } else {
          entity.fenceStmts :+ StmtCase(
            ExprRef(Sym(entitySymbol.attr.stateVar.value)),
            entity.states.tail map {
              case State(expr, body) => CaseClause(List(expr), StmtBlock(body))
            },
            entity.states.head.body
          )
        }

        // Now retain only the symbols that are not dead
        needsDefault retain { symbol =>
          !(isAlive(symbol, stateSystem) contains false)
        }
      }

      if (needsDefault.isEmpty) {
        tree
      } else {
        // Collect the _d -> _q default map for flops
        val flopQ = {
          val pairs = for {
            Decl(symbol, _) <- entity.declarations
            if symbol.attr.flop.isSet
          } yield {
            symbol.attr.flop.value -> symbol
          }
          pairs.toMap
        }

        val newFenceStms = for {
          Decl(symbol, _) <- entity.declarations
          if needsDefault contains symbol
        } yield {
          // Initialize flop _d to _q, anything else to zeros
          val init = flopQ.get(symbol) map { qSymbol =>
            ExprRef(Sym(qSymbol))
          } getOrElse {
            val kind = symbol.denot.kind
            val signed = kind.isSigned
            val width = kind.width.value.get.toInt
            ExprInt(signed, width, 0)
          }
          StmtAssign(ExprRef(Sym(symbol)), init) regularize symbol.loc
        }

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
