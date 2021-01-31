////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Builtin '@ex(bit, width, expr)'
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
import com.argondesign.alogic.util.unreachable

object AtEx extends Builtin("@ex", isPure = true) {

  def typeCheck(expr: ExprBuiltin, args: List[Expr])(implicit fe: Frontend): FinalResult[TypeFund] =
    checkArgCount(expr, 3) flatMap { _ =>
      fe.evaluate(args(1), s"second argument of '$name' (result width)")
    } flatMap { width =>
      Iterator(
        checkWidth(1, args(0), s"first argument of '$name'"),
        checkPacked(args(2), s"third argument of '$name'")
      ).flatten.toList match {
        case Nil =>
          if (width >= args(2).tpe.width) {
            Complete(TypeInt(args(2).tpe.isSigned, width.asLong))
          } else {
            Failure(
              expr,
              s"'$name' causes narrowing: result width is $width, but width of third argument is ${args(2).tpe.width}"
            )
          }
        case errors => Failure(errors)
      }
    }

  def clarify(expr: ExprBuiltin)(implicit fe: Frontend): Expr = {
    val subject = Clarify(expr.args(2).expr)
    val topBit = Clarify(expr.args(0).expr)
    val widthDiff = fe.evaluate(expr.args(1).expr, unreachable).get - subject.tpe.width
    assert(widthDiff >= 0)
    if (widthDiff == 0) {
      markSymbolsUsed(topBit)
      subject
    } else if (widthDiff == 1) {
      (topBit cat subject).withSignedness(subject.tpe.isSigned)
    } else {
      (topBit rep widthDiff cat subject).withSignedness(subject.tpe.isSigned)
    }
  }

  def returnType(args: List[Arg]): TypeFund = unreachable

  def simplify(expr: ExprBuiltin): Expr = unreachable
}
