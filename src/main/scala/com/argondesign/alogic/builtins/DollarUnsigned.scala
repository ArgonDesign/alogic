////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Builtin '$unsigned'
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

object DollarUnsigned extends Builtin("$unsigned", isPure = true) {

  def typeCheck(expr: ExprBuiltin, args: List[Expr])(implicit fe: Frontend): FinalResult[TypeFund] =
    checkArgCount(expr, 1) flatMap { _ =>
      val arg = args.head
      checkNumericOrPacked(arg, s"Argument to '$name") match {
        case Some(error) => Failure(error :: Nil)
        case None =>
          if (arg.tpe.underlying.isNum) {
            fe.evaluate(arg, "Expression of unsized integer type") flatMap {
              case v if v < 0 => Failure(expr, s"'$name' applied to negative int value")
              case _          => Complete(TypeNum(false))
            }
          } else {
            Complete(TypeUInt(arg.tpe.width))
          }
      }
    }

  def clarify(expr: ExprBuiltin)(implicit fe: Frontend): Expr =
    Clarify(expr.args.head.expr).asUnsigned

  def returnType(args: List[Arg]): TypeFund = if (args.head.expr.tpe.underlying.isNum) {
    TypeNum(false)
  } else {
    TypeUInt(args.head.expr.tpe.width)
  }

  def simplify(expr: ExprBuiltin): Expr = expr.args.head.expr match {
    case ExprInt(true, w, v) => ExprInt(false, w, v.asU(w))
    case ExprNum(true, v)    => ExprNum(false, v)
    case _                   => expr
  }

}
