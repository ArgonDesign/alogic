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
// to known bindings of term symbols before executing that statement, and the
// final bindings after executing all statements
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.analysis

import com.argondesign.alogic.ast.Trees.Expr.Integral
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Bindings
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.util.PartialMatch._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object StaticEvaluation {

  // Given an expression that is known to be true, return a set
  // of bindings that can be inferred
  private def inferTrue(expr: Expr)(implicit cc: CompilerContext): Bindings = {
    if (expr.tpe.isPacked && expr.tpe.width == 1) {
      expr match {
        case ExprSym(symbol) => {
          val self =
            Iterator.single(symbol -> (ExprInt(symbol.kind.isSigned, 1, 1) regularize expr.loc))
          val implied = symbol.attr.implications.enumerate collect {
            case (true, true, iSymbol) =>
              iSymbol -> (ExprInt(iSymbol.kind.isSigned, 1, 1) regularize expr.loc)
            case (true, false, iSymbol) =>
              iSymbol -> (ExprInt(iSymbol.kind.isSigned, 1, 0) regularize expr.loc)
          }
          (self ++ implied).toMap
        }
        case ExprUnary("!" | "~", expr)             => inferFalse(expr)
        case ExprBinary(lhs, "&&" | "&", rhs)       => inferTrue(lhs) ++ inferTrue(rhs)
        case ExprBinary(lhs, "==", ExprSym(symbol)) => Map(symbol -> lhs)
        case ExprBinary(ExprSym(symbol), "==", rhs) => Map(symbol -> rhs)
        case _                                      => Bindings.empty
      }
    } else {
      Bindings.empty
    }
  }

  // Given an expression that is known to be false, return a set
  // of bindings that can be inferred
  private def inferFalse(expr: Expr)(implicit cc: CompilerContext): Bindings = {
    if (!expr.tpe.isPacked) {
      Bindings.empty
    } else {
      val eWidth = expr.tpe.width
      if (eWidth == 1) {
        expr match {
          case ExprSym(symbol) => {
            val self =
              Iterator.single(symbol -> (ExprInt(symbol.kind.isSigned, 1, 0) regularize expr.loc))
            val implied = symbol.attr.implications.enumerate collect {
              case (false, true, iSymbol) =>
                iSymbol -> (ExprInt(iSymbol.kind.isSigned, 1, 1) regularize expr.loc)
              case (false, false, iSymbol) =>
                iSymbol -> (ExprInt(iSymbol.kind.isSigned, 1, 0) regularize expr.loc)
            }
            (self ++ implied).toMap
          }
          case ExprUnary("!" | "~", expr)             => inferTrue(expr)
          case ExprBinary(lhs, "||" | "|", rhs)       => inferFalse(lhs) ++ inferFalse(rhs)
          case ExprBinary(lhs, "!=", ExprSym(symbol)) => Map(symbol -> lhs)
          case ExprBinary(ExprSym(symbol), "!=", rhs) => Map(symbol -> rhs)
          case _                                      => Bindings.empty
        }
      } else {
        expr match {
          case ExprSym(symbol) => {
            Map(symbol -> (ExprInt(symbol.kind.isSigned, eWidth.toInt, 0) regularize expr.loc))
          }
          //        case ExprCat(parts) => (Bindings.empty /: parts)(_ ++ inferFalse(_))
          case _ => Bindings.empty
        }
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

      val transitive = transitives.foldLeft(Bindings.empty)(_ ++ _)

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

  private def overwrite(curr: Bindings, symbol: Symbol, expr: Expr)(
      implicit cc: CompilerContext): Bindings = {
    // Add the new binding for the symbol, but first substitute the new
    // expression using the current binding of symbol to remove self references
    val expanded = {
      val selfBinding = Bindings(curr get symbol map { symbol -> _ })
      curr + (symbol -> (expr given selfBinding).simplify)
    }

    // Remove all binding that referenced the just added symbol,
    // as these used the old value. TODO: use SSA form...
    expanded filterNot {
      case (_, v) => v exists { case ExprSym(`symbol`) => true }
    }
  }

  private def removeWritten(curr: Bindings, lval: Expr): Bindings = {
    // TODO: could improve this by compute new bindings
    val written = WrittenSymbols(lval).toList

    // Remove bindings of the written symbols, and all
    // bindings that reference a written symbol
    curr filterNot {
      case (k, v) =>
        (written contains k) || (v exists { case ExprSym(s) => written contains s })
    }
  }

  def apply(stmt: Stmt, initialBindings: Bindings = Bindings.empty)(
      implicit cc: CompilerContext): (Map[Int, Bindings], Bindings) = {
    val res = mutable.Map[Int, Bindings]()

    def analyse(curr: Bindings, stmt: Stmt): Bindings = {
      // Annotate the current statement right at the beginning,
      // this side-effect builds the final map we are returning
      res(stmt.id) = curr
      // Compute the new bindings after this statement
      stmt match {
        // Simple assignment
        case StmtAssign(ExprSym(symbol), rhs) => overwrite(curr, symbol, rhs)
        // Simple update
        case StmtUpdate(lhs @ ExprSym(symbol), op, rhs) =>
          overwrite(curr, symbol, ExprBinary(lhs, op, rhs) regularize stmt.loc)
        // Simple postfix
        case StmtPost(expr @ ExprSym(symbol), "++") => overwrite(curr, symbol, expr + 1)
        case StmtPost(expr @ ExprSym(symbol), "--") => overwrite(curr, symbol, expr - 1)

        // Assignments with complex left hand side
        case StmtAssign(lhs, _) => removeWritten(curr, lhs)
        // Update with complex left hand side
        case StmtUpdate(lhs, _, _) => removeWritten(curr, lhs)
        // Postfix with complex argument
        case StmtPost(expr, _) => removeWritten(curr, expr)

        case StmtBlock(body) => body.foldLeft(curr)(analyse)

        case StmtIf(cond, thenStmts, elseStmts) => {
          val afterThen = thenStmts.foldLeft(inferTrueTransitive(curr, cond))(analyse)
          val afterElse = elseStmts.foldLeft(inferFalseTransitive(curr, cond))(analyse)

          // Keep only the bindings that are the same across both branches
          (afterThen.toSet intersect afterElse.toSet).toMap
        }

        case StmtCase(value, cases) => {
          // Ensure default comes at the front, as that's
          // how we are computing the 'befores' below
          val defaultStmt = cases.collectFirst {
            case CaseDefault(stmts) => StmtBlock(stmts)
          } getOrElse StmtBlock(Nil)
          val regularStmts = cases collect { case CaseRegular(_, stmts) => StmtBlock(stmts) }
          val stmts = defaultStmt :: regularStmts

          val befores = {
            val constraints = cases collect {
              case node @ CaseRegular(conds, _) =>
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

        case _: StmtStall   => curr // TODO: can we do better here?
        case _: StmtExpr    => curr
        case _: StmtComment => curr
        case _              => Bindings.empty
      }
    }

    val finalBindings = analyse(initialBindings, stmt)

    (res.toMap, finalBindings)
  }

}
