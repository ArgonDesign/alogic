////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Compute value of expressions/symbols.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.util.unreachable
import com.argondesign.alogic.util.BigIntOps.BigIntClassOps

import scala.util.chaining.scalaUtilChainingOps

private[frontend] object Evaluate {

  def apply(
      expr: Expr,
      hint: => String
    )(
      implicit
      fe: Frontend
    ): FinalResult[BigInt] =
    fe.typeCheck(expr) flatMap { _ =>
      var bad: Option[FinalResult[BigInt]] = None

      object Transform extends StatelessTreeTransformer {
        // Bail quickly, if there was an issue..
        override def skip(tree: Tree): Boolean = bad.isDefined

        private def evaluate(symbol: Symbol, loc: Loc): Option[Expr] =
          fe.evaluate(symbol, loc, hint) match {
            case Complete(e) =>
              // Walk recursively so there are no symbols left anywhere
              Some(walkSame(e))
            case failure: Failure =>
              bad = Some(failure)
              None
            case unknown: Unknown =>
              bad = Some(unknown)
              None
          }

        // Replace references to symbols with their value (we do it via the
        // frontend for 2 reasons. One is to track dependencies, but more
        // importantly the definitions haven't been through Clarify yet, so
        // the standard constant folder cannot deal with them yet).

        override protected def enter(tree: Tree): Option[Tree] = tree match {
          case _: ExprDot => unreachable
          case ExprSymSel(_, tSymbol) =>
            tSymbol.desc match {
              case _: DescVal | _: DescParam | _: DescConst | _: DescGenVar =>
                evaluate(tSymbol, tree.loc)
              case DescAlias(_, _, expr, _) => Some(walk(expr))
              case _                        => None
            }
          case _ => None
        }

        override def transform(tree: Tree): Tree = tree match {
          case ExprSym(symbol) => evaluate(symbol, tree.loc) getOrElse tree
          case _: ExprIdent    => throw Ice(tree, "Attempting to evaluate unresolved reference")
          case _               => tree
        }
      }

      Clarify(expr) pipe { clarified =>
        val transformed = clarified rewrite Transform
        bad getOrElse {
          transformed.valueOption match {
            case Some(v) => Complete(v)
            case None    => Failure(expr, s"${hint.capitalize} must be a compile time constant")
          }
        }
      }
    }

  def apply(
      symbol: Symbol,
      loc: Loc,
      hint: => String,
      markUsed: Boolean,
      paramCheck: Boolean
    )(
      implicit
      fe: Frontend
    ): FinalResult[Expr] =
    fe.typeCheck(symbol.desc) flatMap { _ =>
      Clarify(symbol.desc) pipe {
        case DescVal(_, _, _, init)                     => Complete(init)
        case d: DescParam if !d.finished && !paramCheck => Unknown(ReasonUnelaborated(d))
        case DescParam(_, _, _, Some(init), _)          => Complete(init)
        case DescParam(_, _, _, None, _)                => unreachable // Initializer provided by Elaborate
        case DescConst(_, _, _, init)                   => Complete(init)
        case DescGenVar(_, _, _, init)                  => Complete(init)
        case DescAlias(_, _, expr, _)                   => Complete(expr)
        case _                                          => Failure(loc, s"${hint.capitalize} must be a compile time constant")
      } flatMap { init =>
        fe.evaluate(init, hint) map { value =>
          value.asExpr(symbol.kind.underlying) tap {
            _ visitAll { case tree => tree withLocOf init }
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
