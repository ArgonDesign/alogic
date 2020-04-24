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
import com.argondesign.alogic.util.unreachable

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
    )(
      implicit
      cc: CompilerContext
    ): Bindings = {
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
    )(
      implicit
      cc: CompilerContext
    ): Bindings = {
    inferTransitive(curr, inferTrue(expr))
  }

  private def inferFalseTransitive(
      curr: Bindings,
      expr: Expr
    )(
      implicit
      cc: CompilerContext
    ): Bindings = {
    inferTransitive(curr, inferFalse(expr))
  }

  // Same as "expr exists { case ExprSym(symbol) => set(symbol) }" but faster
  private def uses(expr: Expr, set: Set[Symbol]): Boolean = {
    def p(expr: Expr): Boolean = expr match {
      case _: ExprInt             => false
      case _: ExprNum             => false
      case ExprSym(symbol)        => set(symbol)
      case ExprBinary(l, _, r)    => p(l) || p(r)
      case ExprUnary(_, e)        => p(e)
      case ExprTernary(c, ts, es) => p(c) || p(ts) || p(es)
      case ExprCall(e, as)        => p(e) || (as exists { case ArgN(_, e) => p(e); case ArgP(e) => p(e) })
      case ExprIndex(e, i)        => p(e) || p(i)
      case ExprSlice(e, l, _, r)  => p(e) || p(l) || p(r)
      case ExprCat(ps)            => ps exists p
      case ExprRep(c, e)          => p(e) || p(c)
      case ExprSelect(e, _, _)    => p(e)
      case ExprCast(_, e)         => p(e)
      case _: ExprType            => false
      case _: ExprStr             => false
      case _: ExprError           => false
      case _: ExprRef             => unreachable
      case _: ExprThis            => unreachable
    }
    p(expr)
  }

  private def overwrite(
      curr: Bindings,
      symbol: Symbol,
      expr: Expr
    )(
      implicit
      cc: CompilerContext
    ): Option[Bindings] = {
    // Add the new binding for the symbol, but first substitute the new
    // expression using the current binding of symbol to remove self references
    val selfBinding = Bindings(curr get symbol map { symbol -> _ })
    val newValue = (expr given selfBinding).simplify
    Option.unless(newValue.tpe.isError) {
      // Remove all bindings that reference the just added symbol,
      // as these used the old value. TODO: use SSA form...
      val written = Set(symbol)
      val remaining = (curr filterNot { case (_, v) => uses(v, written) })
      // If the new expression is at this point still self referential, then
      // the current bindings didn't cover the symbol, so we do not know the
      // new value as we did not know the old value, so only add it to the
      // binding if no self references and hence we might know the value.
      if (newValue exists { case ExprSym(`symbol`) => true }) {
        remaining
      } else {
        remaining + (symbol -> newValue)
      }
    }
  }

  private def removeWritten(curr: Bindings, lval: Expr): Bindings = {
    // Remove bindings of the written symbols, and all
    // bindings that reference a written symbol
    val written = WrittenSymbols(lval).toSet
    curr filterNot { case (k, v) => written(k) || uses(v, written) }
  }

  def apply(
      stmt: Stmt,
      initialBindings: Bindings = Bindings.empty
    )(
      implicit
      cc: CompilerContext
    ): Option[(Map[Int, Bindings], Bindings)] = {
    val res = mutable.Map[Int, Bindings]()

    def analyse(curr: Bindings, stmt: Stmt): Option[Bindings] = {
      // Annotate the current statement right at the beginning,
      // this side-effect builds the final map we are returning
      res(stmt.id) = curr
      //
      def analyseOpt(bindingsOpt: Option[Bindings], stmt: Stmt): Option[Bindings] =
        bindingsOpt flatMap { bindings =>
          analyse(bindings, stmt)
        }
      // Compute the new bindings after this statement
      stmt match {
        // Simple assignment
        case StmtAssign(ExprSym(symbol), rhs) => overwrite(curr, symbol, rhs)
        // Simple update
        case StmtUpdate(lhs @ ExprSym(symbol), op, rhs) =>
          overwrite(curr, symbol, ExprBinary(lhs, op, rhs) regularize stmt.loc)
        // Simple postfix
        case StmtPost(expr @ ExprSym(symbol), "++") => overwrite(curr, symbol, expr.inc)
        case StmtPost(expr @ ExprSym(symbol), "--") => overwrite(curr, symbol, expr.dec)

        // Initialization is like assignment. This is only used early when
        // inlining combinational functions. As the symbol is being introduced
        // by this statement, and in statements definitions must precede use,
        // we know that the symbol is not in the bindings, so just add it
        case StmtDefn(DefnVal(symbol, init))       => Some(curr + (symbol -> init))
        case StmtDefn(DefnVar(symbol, Some(init))) => Some(curr + (symbol -> init))
        case StmtDefn(DefnVar(_, None))            => Some(curr)

        // TODO: these could be improved by computing new bindings
        // Assignments with complex left hand side
        case StmtAssign(lhs, _) => Some(removeWritten(curr, lhs))
        // Update with complex left hand side
        case StmtUpdate(lhs, _, _) => Some(removeWritten(curr, lhs))
        // Postfix with complex argument
        case StmtPost(expr, _) => Some(removeWritten(curr, expr))

        // Outcall
        case StmtOutcall(output, _, _) => Some(removeWritten(curr, output))

        case StmtBlock(body) => body.foldLeft(Option(curr))(analyseOpt)

        case StmtIf(cond, thenStmts, elseStmts) =>
          val afterThen = thenStmts.foldLeft(Option(inferTrueTransitive(curr, cond)))(analyseOpt)
          val afterElse = elseStmts.foldLeft(Option(inferFalseTransitive(curr, cond)))(analyseOpt)

          // Keep only the bindings that are the same across both branches
          afterThen flatMap { at =>
            afterElse map { ae =>
              (at.toSet intersect ae.toSet).toMap
            }
          }

        case StmtCase(value, cases) =>
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

          Option.when(afters forall { _.isDefined }) {
            // Keep only the bindings that are the same across all branches
            (afters map { _.get.toSet } reduce { _ intersect _ }).toMap
          }

        // Infer assumption is true
        case StmtAssertion(AssertionAssume(cond, _)) => Some(inferTrueTransitive(curr, cond))

        case _: StmtStall   => Some(curr) // TODO: can we do better here?
        case _: StmtExpr    => Some(curr)
        case _: StmtComment => Some(curr)
        case _: StmtDecl    => Some(curr)

        // TODO: This is only used by InlineMethods and could be improved by
        // indicating this branch does not join hence shouldn't constrain
        // subsequent statements
        case _: StmtReturn => Some(curr)

        case _ => unreachable
      }
    }

    analyse(initialBindings, stmt) map { finalBindings =>
      (res.toMap, finalBindings)
    }
  }

}
