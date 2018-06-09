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
// Given a statement, compute a map from the identity of each sub-statement
// to known bindings of term symbols before executing that statement
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.analysis

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.Expr.Integral
import com.argondesign.alogic.core.Bindings
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.util.PartialMatch._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object StaticEvaluation {

  // Given an expression that is known to be true, return a set
  // of bindings that can be inferred
  private def inferTrue(expr: Expr)(implicit cc: CompilerContext): Bindings = {
    if (expr.tpe.width == 1) {
      expr match {
        case ExprRef(symbol: TermSymbol) => {
          Map(symbol -> (ExprInt(symbol.kind.isSigned, 1, 1) regularize expr.loc))
        }
        case ExprUnary("!" | "~", expr)                         => inferFalse(expr)
        case ExprBinary(lhs, "&&" | "&", rhs)                   => inferTrue(lhs) ++ inferTrue(rhs)
        case ExprBinary(lhs, "==", ExprRef(symbol: TermSymbol)) => Map(symbol -> lhs)
        case ExprBinary(ExprRef(symbol: TermSymbol), "==", rhs) => Map(symbol -> rhs)
        case _                                                  => Bindings.empty
      }
    } else {
      Bindings.empty
    }
  }

  // Given an expression that is known to be false, return a set
  // of bindings that can be inferred
  private def inferFalse(expr: Expr)(implicit cc: CompilerContext): Bindings = {
    val eWidth = expr.tpe.width
    if (eWidth == 1) {
      expr match {
        case ExprRef(symbol: TermSymbol) => {
          Map(symbol -> (ExprInt(symbol.kind.isSigned, 1, 0) regularize expr.loc))
        }
        case ExprUnary("!" | "~", expr)                         => inferTrue(expr)
        case ExprBinary(lhs, "||" | "|", rhs)                   => inferFalse(lhs) ++ inferFalse(rhs)
        case ExprBinary(lhs, "!=", ExprRef(symbol: TermSymbol)) => Map(symbol -> lhs)
        case ExprBinary(ExprRef(symbol: TermSymbol), "!=", rhs) => Map(symbol -> rhs)
        case _                                                  => Bindings.empty
      }
    } else {
      expr match {
        case ExprRef(symbol: TermSymbol) => {
          Map(symbol -> (ExprInt(symbol.kind.isSigned, eWidth, 0) regularize expr.loc))
        }
        case _ => Bindings.empty
      }
    }
  }

  // Given the current bindings and some bindings inferred from a condition,
  // infer bindings for symbols defining the just inferred symbols
  @tailrec
  private def inferTransitive(
      curr: Bindings,
      inferred: Bindings
  )(implicit cc: CompilerContext): Bindings = {
    if (inferred.isEmpty) {
      curr
    } else {
      val transitives = curr flatMap {
        case (symbol, oldExpr) => {
          inferred.get(symbol) flatMap { newExpr =>
            newExpr partialMatch {
              case Integral(_, _, value) if value == 0 => inferFalse(oldExpr)
              case Integral(_, _, value) if value != 0 => inferTrue(oldExpr)
            }
          }
        }
      }

      val transitive = (Bindings.empty /: transitives)(_ ++ _)

      inferTransitive(curr ++ inferred, transitive)
    }
  }

  private def inferTrueTransitive(
      curr: Bindings,
      expr: Expr
  )(implicit cc: CompilerContext): Bindings = {
    inferTransitive(curr, inferTrue(expr))
  }

  private def inferFalseTransitive(
      curr: Bindings,
      expr: Expr
  )(implicit cc: CompilerContext): Bindings = {
    inferTransitive(curr, inferFalse(expr))
  }

  def apply(stmt: Stmt)(implicit cc: CompilerContext): (Map[Int, Bindings], Bindings) = {
    val res = mutable.Map[Int, Bindings]()

    def analyse(curr: Bindings, stmt: Stmt): Bindings = {
      // Annotate the current statement right at the beginning,
      // this side-effect builds the final map we are returning
      res(stmt.id) = curr
      // Compute the new bindings after this statement
      stmt match {
        // Simple assignment
        case StmtAssign(ExprRef(symbol: TermSymbol), rhs) => {
          // Add the new binding for the symbol
          val expanded = curr + (symbol -> rhs)

          // Remove all binding that referenced the just added symbol,
          // as these used the old value. TODO: use SSA form...
          expanded filterNot {
            case (_, v) => v exists { case ExprRef(`symbol`) => true }
          }
        }

        // Assignments with complex left hand side
        case StmtAssign(lhs, _) => {
          // TODO: could improve this by attempting to compute new bindings here
          val written = WrittenSymbols(lhs).toList

          // Remove bindings of the written symbols, and all
          // bindings that reference a written symbol
          curr filterNot {
            case (k, v) =>
              (written contains k) || (v exists { case ExprRef(s) => written contains s })
          }
        }

        case StmtBlock(body) => (curr /: body)(analyse)

        case StmtIf(cond, thenStmt, elseStmtOpt) => {
          val afterThen = analyse(inferTrueTransitive(curr, cond), thenStmt)
          val afterElse = elseStmtOpt match {
            case Some(elseStmt) => analyse(inferFalseTransitive(curr, cond), elseStmt)
            case None           => curr
          }

          // Keep only the bindings that are the same across both branches
          (afterThen.toSet intersect afterElse.toSet).toMap
        }

        case StmtCase(value, cases, defaults) => {
          val stmts = StmtBlock(defaults) :: (cases map { _.body })

          val befores = {
            val constraints = cases map {
              case node @ CaseClause(conds, _) =>
                conds map { ExprBinary(_, "==", value) } reduce { _ || _ } regularize node.loc
            }

            val buf = ListBuffer[Bindings]()

            @tailrec
            def loop(curr: Bindings, constraints: List[Expr]): List[Bindings] = {
              if (constraints.isEmpty) {
                curr :: buf.toList
              } else {
                buf append inferTrueTransitive(curr, constraints.head)
                loop(inferFalseTransitive(curr, constraints.head), constraints.tail)
              }
            }

            loop(curr, constraints)
          }

          val afters = for ((before, stmt) <- befores zip stmts) yield analyse(before, stmt)

          // Keep only the bindings that are the same across all branches
          (afters map { _.toSet } reduce { _ intersect _ }).toMap
        }

        case _: StmtStall         => curr // TODO: can we do better here?
        case _: StmtFence         => curr
        case _: StmtExpr          => curr
        case _: StmtDollarComment => curr
        case _                    => Bindings.empty
      }
    }

    val finalBindings = analyse(Bindings.empty, stmt)

    (res.toMap, finalBindings)
  }

}
