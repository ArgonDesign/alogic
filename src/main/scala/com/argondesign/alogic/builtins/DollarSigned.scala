////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Builtin '$signed'
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.frontend.Clarify
import com.argondesign.alogic.frontend.Complete
import com.argondesign.alogic.frontend.Failure
import com.argondesign.alogic.frontend.FinalResult
import com.argondesign.alogic.frontend.Frontend
import com.argondesign.alogic.util.BigIntOps.BigIntClassOps

object DollarSigned extends Builtin("$signed", isPure = true) {

  def typeCheck(expr: ExprBuiltin, args: List[Expr])(implicit fe: Frontend): FinalResult[TypeFund] =
    checkArgCount(expr, 1) flatMap { _ =>
      val arg = args.head
      checkNumericOrPacked(arg, s"Argument to '$name") match {
        case Some(error) => Failure(error :: Nil)
        case None        => Complete(if (arg.tpe.isPacked) TypeSInt(arg.tpe.width) else TypeNum(true))
      }
    }

  def clarify(expr: ExprBuiltin)(implicit fe: Frontend): Expr =
    Clarify(expr.args.head.expr).asSigned

  def returnType(args: List[Arg]): TypeFund = if (args.head.expr.tpe.underlying.isNum) {
    TypeNum(true)
  } else {
    TypeSInt(args.head.expr.tpe.width)
  }

  def simplify(expr: ExprBuiltin): Expr = expr.args.head.expr match {
    case ExprInt(false, w, v) => ExprInt(true, w, v.asI(w))
    case ExprNum(false, v)    => ExprNum(true, v)
    case _                    => expr
  }

}
