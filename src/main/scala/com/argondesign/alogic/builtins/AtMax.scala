////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Builtin '@max'
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.frontend.Complete
import com.argondesign.alogic.frontend.Failure
import com.argondesign.alogic.frontend.FinalResult
import com.argondesign.alogic.frontend.Frontend
import com.argondesign.alogic.util.unreachable

object AtMax extends Builtin("@max", isPure = true) {

  def typeCheck(expr: ExprBuiltin, args: List[Expr])(implicit fe: Frontend): FinalResult[TypeFund] =
    if (args.isEmpty) {
      Failure(expr, s"'$name' requires 1 or more unsized integer arguments")
    } else {
      args.collect {
        case expr if !expr.tpe.underlying.isNum =>
          Error(expr, s"All arguments of '$name' must be unsized integers")
      } match {
        case Nil    => Complete(TypeNum(args.forall(_.tpe.isSigned)))
        case errors => Failure(errors)
      }
    }

  def clarify(expr: ExprBuiltin)(implicit fe: Frontend): Expr = {
    val signed = expr.args.forall(_.expr.tpe.isSigned)
    val value = expr.args.map(arg => fe.evaluate(arg.expr, unreachable).get).max
    ExprNum(signed, value)
  }

  def returnType(args: List[Arg]): TypeFund = unreachable

  def simplify(expr: ExprBuiltin): Expr = unreachable
}
