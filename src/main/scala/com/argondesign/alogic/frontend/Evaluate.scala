////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Compute value of expressions/symbols.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.Messages.Note
import com.argondesign.alogic.util.unreachable
import com.argondesign.alogic.util.BigIntOps.BigIntClassOps

import scala.util.chaining.scalaUtilChainingOps

private[frontend] object Evaluate {

  def apply(
      expr: Expr
    )(
      implicit
      fe: Frontend
    ): FinalResult[Either[Seq[Note], BigInt]] =
    fe.typeCheck(expr) flatMap { _ =>
      object Transform extends StatelessTreeTransformer {
        // Replaces references to symbols with their value. We do it via the
        // frontend for 2 reasons. One is to track dependencies for the
        // circularity check, but more importantly the definitions have not
        // been through Clarify yet, so SimplifyExpr cannot deal with them.

        var bad: Option[FinalResult[Either[Seq[Note], BigInt]]] = None

        private def evaluate(symbol: Symbol, loc: Loc): Option[Expr] =
          fe.tryEvaluate(symbol, loc) match {
            // Walk recursively so there are no symbols left anywhere
            case Complete(Right(e))    => Some(walkSame(e))
            case Complete(Left(notes)) => bad = Some(Complete(Left(notes))); None
            case failure: Failure      => bad = Some(failure); None
            case unknown: Unknown      => bad = Some(unknown); None
          }

        override protected def enter(tree: Tree): Option[Tree] = tree match {
          case _ if bad.isDefined     => Some(tree) // Bail quickly, on issue
          case ExprSym(symbol)        => evaluate(symbol, expr.loc) orElse Some(tree)
          case ExprSymSel(_, tSymbol) =>
            // TODO: Van we just evaluate(tSymbol, expr.loc) orElse Some(tree) ???
            tSymbol.desc match {
              case _: DescVal | _: DescParam | _: DescConst | _: DescGenVar =>
                evaluate(tSymbol, expr.loc) orElse Some(tree)
              case DescAlias(_, _, expr, _) => Some(walk(expr))
              case _                        => None
            }
          case _: ExprIdent | _: ExprDot | _: ExprSel => unreachable
          case _: ExprCall =>
            bad = Some(
              Complete(Left(Seq(Note(tree, "Cannot evaluate function call at elaboration time"))))
            )
            Some(tree)
          case _ => None
        }
      }

      val clarified = Clarify(expr)
      val transformed = clarified rewrite Transform
      Transform.bad getOrElse {
        transformed.valueOption match {
          case Some(v) => Complete(Right(v))
          case None    => Complete(Left(Seq.empty))
        }
      }
    }

  def apply(
      symbol: Symbol,
      markUsed: Boolean,
      paramCheck: Boolean
    )(
      implicit
      fe: Frontend
    ): FinalResult[Either[Seq[Note], Expr]] =
    fe.typeCheck(symbol.desc) flatMap { _ =>
      Clarify(symbol.desc) pipe {
        case DescVal(_, _, _, init)                     => Complete(Right(init))
        case d: DescParam if !d.finished && !paramCheck => Unknown(ReasonUnelaborated(d))
        case DescParam(_, _, _, initOpt, _)             => Complete(Right(initOpt.get))
        case DescConst(_, _, _, init)                   => Complete(Right(init))
        case DescGenVar(_, _, _, init)                  => Complete(Right(init))
        case DescAlias(_, _, expr, _)                   => Complete(Right(expr))
        case _                                          => Complete(Left(Seq.empty))
      } flatMap {
        case l @ Left(_) => Complete(l)
        case Right(init) =>
          fe.tryEvaluate(init).map {
            _.map { value =>
              value.asExpr(symbol.kind.underlying) tap {
                _ visitAll { case tree => tree withLocOf init }
              }
            }
          }
      }
    } tapEach { _ =>
      // Mark as used during elaboration if required
      if (markUsed) {
        symbol.attr.wasUsed set true
      }
    }

}
