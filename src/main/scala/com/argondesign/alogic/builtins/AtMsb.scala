////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Builtin '@msb'
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.frontend.Clarify
import com.argondesign.alogic.frontend.Complete
import com.argondesign.alogic.frontend.Failure
import com.argondesign.alogic.frontend.FinalResult
import com.argondesign.alogic.frontend.Frontend
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec

object AtMsb extends Builtin("@msb", isPure = true) {

  def typeCheck(expr: ExprBuiltin, args: List[Expr])(implicit fe: Frontend): FinalResult[TypeFund] =
    checkArgCount(expr, 1) flatMap { _ =>
      checkPacked(args.head, s"Argument of '$name'") match {
        case Some(error) => Failure(error)
        case None        => Complete(TypeUInt(1))
      }
    }

  def clarify(expr: ExprBuiltin)(implicit fe: Frontend): Expr = {
    val arg = Clarify(expr.args.head.expr)
    @tailrec
    def loop(sub: Expr): Expr = sub.tpe.underlying match {
      case TypeVector(_, size) => loop(sub index (size - 1))
      case other               => sub index (other.width - 1)
    }
    loop(arg)
  }

  def returnType(args: List[Arg]): TypeFund = unreachable

  def simplify(expr: ExprBuiltin): Expr = unreachable
}
