////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Given a statement, compute a map from the identity of each sub-statement
// to known bindings of term symbols before executing that statement, and the
// final bindings after executing all statements
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.analysis

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.Expr.Integral
import com.argondesign.alogic.core.Bindings
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.util.PartialMatch._
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.chaining.scalaUtilChainingOps

object StaticEvaluation {

  // Evaluation context used throughout the traversal. 'bindings' just
  // contains the expressions that define the values of symbols and forms
  // directly the result of the evaluation. The 'used' set is only used
  // to speed up the algorithm as it can be used to eliminate an expensive
  // "filter map by predicate on value" type operation required when
  // removing or replacing a binding. In order to do this, the 'used' set
  // must always be a superset of
  // 'bindings.valuesIterator.flatMap(ReadSymbols.rval).toSet',
  // i.e.: the set of all symbols used in an expression in the bindings.
  // It is ok if 'used' is a proper superset and contains symbols which are
  // not actually used. These will be removed from the 'used' set the next
  // time time we try to remove bindings using that symbol (which do not
  // exist), so the worst case waste is one full traversal of the bindings
  // which is also what would often be needed to keep the 'used' set precise,
  // but we only pay this if we actually want to remove that symbol, which is
  // optimal.
  case class Ctx(bindings: Bindings, used: immutable.Set[Symbol]) {

    def add(symbol: Symbol, expr: Expr): Ctx = {
      // Note: The used set of the result is approximate (because we don't
      // drop overwritten bindings), but is conservative which is sufficient.
      Ctx(bindings + (symbol -> expr), used ++ ReadSymbols.rval(expr))
    }

    def addAll(newBindings: Bindings): Ctx = {
      newBindings.iterator.foldLeft(this) { case (ctx, (symbol, expr)) => ctx.add(symbol, expr) }
    }

    def fastIntersect(f: Ctx => Option[IterableOnce[Ctx]]): Option[Ctx] = {
      var acc: immutable.Set[Symbol] = Set.empty
      val newBindingsOpt = bindings.fastIntersect { bindings =>
        f(Ctx(bindings, used)) map {
          _.iterator map { ctx =>
            acc = acc union ctx.used
            ctx.bindings
          }
        }
      }

      // Note: used set is approximate again, but is conservative.
      newBindingsOpt map { Ctx(_, acc) }
    }

  }

  object Ctx {
    def from(bindings: Bindings): Ctx =
      Ctx(bindings, bindings.valuesIterator.flatMap(ReadSymbols.rval).toSet)
  }

  // Same as "expr exists { case ExprSym(symbol) => set(symbol) }" but faster
  private def uses(expr: Expr, set: Set[Symbol]): Boolean = {
    def p(expr: Expr): Boolean = expr match {
      case _: ExprInt            => false
      case _: ExprNum            => false
      case ExprSym(symbol)       => set(symbol)
      case ExprBinary(l, _, r)   => p(l) || p(r)
      case ExprUnary(_, e)       => p(e)
      case ExprCond(c, ts, es)   => p(c) || p(ts) || p(es)
      case ExprCall(e, as)       => p(e) || (as exists { a => p(a.expr) })
      case ExprBuiltin(_, as)    => as exists { a => p(a.expr) }
      case ExprIndex(e, i)       => p(e) || p(i)
      case ExprSlice(e, l, _, r) => p(e) || p(l) || p(r)
      case ExprCat(ps)           => ps exists p
      case ExprRep(c, e)         => p(e) || p(c)
      case ExprSel(e, _)         => p(e)
      case ExprCast(_, e)        => p(e)
      case ExprOld(e)            => p(e)
      case _: ExprType | _: ExprStr | _: ExprError =>
        false
      case _: ExprDot | _: ExprSymSel | _: ExprIdent | _: ExprThis =>
        unreachable
    }
    p(expr)
  }

  // Given an expression that is known to be true, return a set
  // of bindings that can be inferred
  private def inferTrue(expr: Expr): Bindings = {
    if (expr.tpe.isPacked && expr.tpe.width == 1) {
      expr match {
        case ExprSym(symbol) =>
          val self =
            Iterator.single(symbol -> (ExprInt(symbol.kind.isSigned, 1, 1) regularize expr.loc))
          val implied = symbol.attr.implications.enumerate collect {
            case (true, true, iSymbol) =>
              iSymbol -> (ExprInt(iSymbol.kind.isSigned, 1, 1) regularize expr.loc)
            case (true, false, iSymbol) =>
              iSymbol -> (ExprInt(iSymbol.kind.isSigned, 1, 0) regularize expr.loc)
          }
          Bindings.from(self ++ implied)
        case ExprUnary("!" | "~", expr)       => inferFalse(expr)
        case ExprBinary(lhs, "&&" | "&", rhs) => inferTrue(lhs) ++ inferTrue(rhs)
        case ExprBinary(lhs, "==", ExprSym(symbol)) if !uses(lhs, Set(symbol)) =>
          Bindings.from(Iterator.single(symbol -> lhs))
        case ExprBinary(ExprSym(symbol), "==", rhs) if !uses(rhs, Set(symbol)) =>
          Bindings.from(Iterator.single(symbol -> rhs))
        case _ => Bindings.empty
      }
    } else {
      Bindings.empty
    }
  }

  // Given an expression that is known to be false, return a set
  // of bindings that can be inferred
  private def inferFalse(expr: Expr): Bindings = {
    if (!expr.tpe.isPacked) {
      Bindings.empty
    } else {
      val eWidth = expr.tpe.width
      if (eWidth == 1) {
        expr match {
          case ExprSym(symbol) =>
            val self =
              Iterator.single(symbol -> (ExprInt(symbol.kind.isSigned, 1, 0) regularize expr.loc))
            val implied = symbol.attr.implications.enumerate collect {
              case (false, true, iSymbol) =>
                iSymbol -> (ExprInt(iSymbol.kind.isSigned, 1, 1) regularize expr.loc)
              case (false, false, iSymbol) =>
                iSymbol -> (ExprInt(iSymbol.kind.isSigned, 1, 0) regularize expr.loc)
            }
            Bindings.from(self ++ implied)
          case ExprUnary("!" | "~", expr)       => inferTrue(expr)
          case ExprBinary(lhs, "||" | "|", rhs) => inferFalse(lhs) ++ inferFalse(rhs)
          case ExprBinary(lhs, "!=", ExprSym(symbol)) if !uses(lhs, Set(symbol)) =>
            Bindings.from(Iterator.single(symbol -> lhs))
          case ExprBinary(ExprSym(symbol), "!=", rhs) if !uses(rhs, Set(symbol)) =>
            Bindings.from(Iterator.single(symbol -> rhs))
          case _ => Bindings.empty
        }
      } else {
        expr match {
          case ExprSym(symbol) =>
            Bindings.from(
              Iterator.single(
                symbol -> (ExprInt(symbol.kind.isSigned, eWidth.toInt, 0) regularize expr.loc)
              )
            )
          //        case ExprCat(parts) => (Bindings.empty /: parts)(_ ++ inferFalse(_))
          case _ => Bindings.empty
        }
      }
    }
  }

  // Given the current bindings and some bindings inferred from a condition,
  // infer bindings for symbols defining the just inferred symbols
  @tailrec
  private def inferTransitive(curr: Ctx, inferred: Bindings): Ctx = {
    if (inferred.isEmpty) {
      curr
    } else {
      val transitives = curr.bindings.iterator flatMap {
        case (symbol, oldExpr) =>
          inferred.get(symbol) flatMap { newExpr =>
            newExpr partialMatch {
              case Integral(_, _, value) if value == 0 => inferFalse(oldExpr)
              case Integral(_, _, value) if value != 0 => inferTrue(oldExpr)
            }
          }
      }

      val transitive = transitives.foldLeft(Bindings.empty)(_ ++ _)

      inferTransitive(curr addAll inferred, transitive)
    }
  }

  private def inferTrueTransitive(curr: Ctx, expr: Expr): Ctx =
    inferTransitive(curr, inferTrue(expr))

  private def inferFalseTransitive(curr: Ctx, expr: Expr): Ctx =
    inferTransitive(curr, inferFalse(expr))

  private def overwrite(curr: Ctx, symbol: Symbol, expr: Expr): Option[Ctx] = {
    // Add the new binding for the symbol, but first substitute the new
    // expression using the current binding of symbol to remove self references
    val newValue = {
      val selfBinding = Bindings from { curr.bindings get symbol map { symbol -> _ } }
      val replaced = expr substitute selfBinding
      val signFixed = if (symbol.kind.isSigned != replaced.tpe.isSigned) {
        if (symbol.kind.isSigned) replaced.castSigned else replaced.castUnsigned
      } else {
        replaced
      }
      signFixed.simplify
    }
    // TODO: Get rid of this Option by catching all errors currently reported
    //       in SimplifyExpr in the TypeChecker
    Option.unless(newValue.tpe.isError) {
      val remaining = if (!curr.used(symbol)) {
        // The written symbol is not used in the value of any of the current
        // bindings, so there is no need to filter
        curr
      } else {
        // Remove all bindings that reference the just added symbol,
        // as these used the old value. Also update the 'used' set.
        // TODO: use SSA form...
        // TODO: Specialize 'uses' for single symbol
        Ctx(curr.bindings filterNot { case (_, v) => uses(v, Set(symbol)) }, curr.used.excl(symbol))
      }
      // If the new expression is at this point still self referential, then
      // the current bindings didn't cover the symbol, so we do not know the
      // new value as we did not know the old value, so only add it to the
      // binding if no self references and hence we might know the value.
      // We also know that if the new value is self-referential, then remaining
      // does not contain a binding for the symbol, because if it did, then
      // the result could not be self referential due to the above substitution.
      if (newValue exists { case ExprSym(`symbol`) => true }) {
        remaining
      } else {
        remaining.add(symbol, newValue)
      }
    }
  }

  private def removeWritten(curr: Ctx, lval: Expr): Ctx = {
    // Remove bindings of the written symbols, and all
    // bindings that reference a written symbol
    val written = WrittenSymbols(lval).toSet
    val writtenAndUsed = written filter curr.used
    if (writtenAndUsed.isEmpty) {
      // None of he written symbols are used, just drop based on keys which
      // is a lot faster
      Ctx(curr.bindings.removedAll(written), curr.used)
    } else {
      // Do the hard work of filtering based on value as well
      val newBingings = curr.bindings filterNot {
        case (k, v) => written(k) || uses(v, writtenAndUsed)
      }
      val newUsed = curr.used diff writtenAndUsed
      Ctx(newBingings, newUsed)
    }
  }

  def apply(
      stmt: Stmt,
      initialBindings: Bindings = Bindings.empty
    ): Option[(Map[Int, Bindings], Bindings, Map[Symbol, Int], Map[Symbol, Int])] = {
    val res = mutable.Map[Int, Bindings]()

    // We count read and write accesses to each symbol. We do this here to save
    // on a second traversal doing the same, though that would probably not be
    // very expensive either, so this can be factored out if required.
    val writeCount = mutable.Map[Symbol, Int]() withDefaultValue 0
    val readCount = mutable.Map[Symbol, Int]() withDefaultValue 0

    def countAccessesLValue(expr: Expr): Unit = {
      WrittenSymbols(expr) foreach { writeCount(_) += 1 }
      ReadSymbols.lval(expr) foreach { readCount(_) += 1 }
    }

    def countAccessesRValue(expr: Expr): Unit =
      ReadSymbols.rval(expr) foreach { readCount(_) += 1 }

    def analyse(curr: Ctx, stmt: Stmt): Option[Ctx] = {
      // Annotate the current statement right at the beginning,
      // this side-effect builds the final map we are returning
      res(stmt.id) = curr.bindings

      // Count assignments and references
      stmt pipe {
        case StmtAssign(lhs, rhs) =>
          countAccessesLValue(lhs)
          countAccessesRValue(rhs)
        case StmtUpdate(lhs, _, rhs) =>
          countAccessesLValue(lhs)
          countAccessesRValue(rhs)
        case StmtPost(lhs, _) => countAccessesLValue(lhs)
        case StmtOutcall(lhs, f, rhss) =>
          countAccessesLValue(lhs)
          countAccessesRValue(f)
          rhss foreach countAccessesRValue
        case StmtSplice(s) =>
          s.children foreach {
            case expr: Expr => countAccessesRValue(expr)
            case _          =>
          }
        case other =>
          other.children foreach {
            case expr: Expr            => countAccessesRValue(expr)
            case CaseRegular(conds, _) => conds foreach countAccessesRValue
            case _: CaseDefault        => // Will count when 'analyze' called on children
            case _: Stmt               => // Will count when 'analyze' called on it
            case _                     => unreachable
          }
      }

      //
      def analyseOpt(ctxOpt: Option[Ctx], stmt: Stmt): Option[Ctx] =
        ctxOpt.flatMap(analyse(_, stmt))

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
        case StmtSplice(DefnVal(symbol, init))       => Some(curr.add(symbol, init))
        case StmtSplice(DefnVar(symbol, Some(init))) => Some(curr.add(symbol, init))
        case StmtSplice(DefnVar(_, None))            => Some(curr)

        // Constant definitions can be created in statement position during
        // elaboration of 'gen' loops as loop variable instances. These however
        // can be directly folded by SimplifyExpr so we need not track them
        // explicitly through the static evaluation.
        case StmtSplice(_: DefnConst) => Some(curr)

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
          // At the end, keep only the bindings that are the same across all branches
          curr fastIntersect { curr =>
            val afterThen = thenStmts.foldLeft(Option(inferTrueTransitive(curr, cond)))(analyseOpt)
            val afterElse = elseStmts.foldLeft(Option(inferFalseTransitive(curr, cond)))(analyseOpt)

            afterThen flatMap { at =>
              afterElse map { ae =>
                Iterator(at, ae)
              }
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

          // At the end, keep only the bindings that are the same across all branches
          curr fastIntersect { curr =>
            val befores = {
              val constraints = cases collect {
                case node @ CaseRegular(conds, _) =>
                  conds map {
                    ExprBinary(_, "==", value)
                  } reduce {
                    _ || _
                  } regularize node.loc
              }

              val buf = ListBuffer[Ctx]()

              @tailrec
              def loop(curr: Ctx, constraints: List[Expr]): List[Ctx] = {
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

            Option.when(afters forall { _.isDefined })(afters.map(_.get))
          }

        // Infer assertion/assumption is true
        case StmtSplice(AssertionAssert(cond, _)) => Some(inferTrueTransitive(curr, cond))
        case StmtSplice(AssertionAssume(cond, _)) => Some(inferTrueTransitive(curr, cond))

        // Infer stall condition will be true (by the time subsequent statements execute
        case StmtWait(cond) => Some(inferTrueTransitive(curr, cond))

        case _: StmtExpr             => Some(curr)
        case _: StmtComment          => Some(curr)
        case StmtSplice(_: Decl)     => Some(curr)
        case StmtSplice(_: DefnFunc) => Some(curr)

        // TODO: This could be improved by indicating this branch does not join
        // hence shouldn't constrain subsequent statements.
        case _: StmtReturn => Some(curr)
        case _: StmtGoto   => Some(curr)

        case _ => unreachable
      }
    }

    analyse(Ctx.from(initialBindings), stmt) map { finalCtx =>
      (res.toMap, finalCtx.bindings, writeCount.toMap, readCount.toMap)
    }
  }

}
