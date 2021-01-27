////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Builtin '@bits'
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.frontend.Complete
import com.argondesign.alogic.frontend.Failure
import com.argondesign.alogic.frontend.FinalResult
import com.argondesign.alogic.frontend.Frontend
import com.argondesign.alogic.util.unreachable

import scala.util.chaining.scalaUtilChainingOps

object AtBits extends Builtin("@bits", isPure = true) {

  def typeCheck(expr: ExprBuiltin, args: List[Expr])(implicit fe: Frontend): FinalResult[TypeFund] =
    checkArgCount(expr, 1) flatMap { _ =>
      args.head.tpe match {
        case TypeType(kind) if kind.isPacked => Complete(TypeNum(false))
        case TypeNone(kind) if kind.isPacked => Complete(TypeNum(false))
        case kind if kind.isPacked           => Complete(TypeNum(false))
        case _ =>
          Failure(args.head, s"Argument to '$name' must be a packed type or a packed value")
      }
    }

  def clarify(expr: ExprBuiltin)(implicit fe: Frontend): Expr =
    expr.args.head.expr.tpe pipe {
      case TypeType(kind) => kind
      case TypeNone(kind) => kind
      case kind           => kind
    } pipe { kind =>
      markSymbolsUsed(expr.args.head.expr)
      Expr(kind.width)
    }

  def returnType(args: List[Arg]): TypeFund = unreachable

  def simplify(expr: ExprBuiltin): Expr = unreachable
}
