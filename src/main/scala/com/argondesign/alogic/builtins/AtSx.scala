////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Builtin '@sx'
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

import scala.annotation.tailrec

object AtSx extends Builtin("@sx", isPure = true) {

  def typeCheck(expr: ExprBuiltin, args: List[Expr])(implicit fe: Frontend): FinalResult[TypeFund] =
    checkArgCount(expr, 2) flatMap { _ =>
      fe.evaluate(args(0), s"first argument of '$name' (result width)")
    } flatMap { width =>
      checkPacked(args(1), s"second argument of '$name'") match {
        case Some(error) => Failure(error :: Nil)
        case None =>
          if (width >= args(1).tpe.width) {
            Complete(TypeInt(args(1).tpe.isSigned, width.asLong))
          } else {
            Failure(
              expr,
              s"'$name' causes narrowing: result width is $width, but width of second argument is ${args(1).tpe.width}"
            )
          }
      }
    }

  def clarify(expr: ExprBuiltin)(implicit fe: Frontend): Expr =
    expr.copy(args = clarifyUintArg(expr.args.head) :: Clarify(expr.args(1)) :: Nil)

  def returnType(args: List[Arg]): TypeFund =
    TypeInt(args(1).expr.tpe.isSigned, args(0).expr.value.asLong)

  def simplify(expr: ExprBuiltin): Expr = {
    val subject = expr.args(1).expr
    val topBit = {
      @tailrec
      def loop(sub: Expr): Expr = sub.tpe.underlying match {
        case TypeVector(_, size) => loop(sub index (size - 1))
        case other               => sub index (other.width - 1)
      }
      loop(subject)
    }
    val widthDiff = expr.args(0).expr.value - subject.tpe.width
    assert(widthDiff >= 0)
    if (widthDiff == 0) {
      subject
    } else if (widthDiff == 1) {
      (topBit cat subject).withSignedness(subject.tpe.isSigned)
    } else {
      (topBit rep widthDiff cat subject).withSignedness(subject.tpe.isSigned)
    }
  }

}
