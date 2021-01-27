////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Utilities generally useful in implementations of builtins.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.frontend.Complete
import com.argondesign.alogic.frontend.Failure
import com.argondesign.alogic.frontend.FinalResult
import com.argondesign.alogic.frontend.Frontend
import com.argondesign.alogic.util.unreachable

package object builtins {

  // TODO: this is copied from the TypeChecker, refactor to reuse
  private def pluralize(value: BigInt, singular: String, plural: String): String = {
    if (value == 1) s"$value $singular" else s"$value $plural"
  }

  def checkArgCount(invocation: ExprBuiltin, expected: Int): FinalResult[Unit] = {
    require(expected >= 0)
    val len = invocation.args.length
    if (len == expected) {
      Complete(())
    } else {
      val name = invocation.builtin.name
      val what = pluralize(expected, "argument", "arguments")
      Failure(invocation, s"'$name' requires $what ($len provided)'")
    }
  }

  // TODO: this is copied from the TypeChecker, refactor to reuse
  def checkWidth(width: Long, expr: Expr, subject: => String): Option[Error] = {
    def complain(problem: String): Some[Error] =
      Some(Error(expr, s"$subject $problem, a $width bit value is expected"))
    if (expr.tpe.underlying.isNum) {
      complain("yields an unsized value")
    } else if (!expr.tpe.isPacked) {
      complain("is of non-packed type")
    } else if (expr.tpe.width != width) {
      complain(s"yields ${pluralize(expr.tpe.width, "bit", "bits")}")
    } else {
      None
    }
  }

  // TODO: this is copied from the TypeChecker, refactor to reuse
  def checkPacked(expr: Expr, subject: => String): Option[Error] =
    Option.when(!expr.tpe.isPacked) {
      Error(expr, s"$subject is of non-packed type")
    }

  // TODO: this is copied from the TypeChecker, refactor to reuse
  def checkNumericOrPacked(expr: Expr, subject: => String): Option[Error] =
    Option.when(!expr.tpe.underlying.isNumeric && !expr.tpe.isPacked) {
      Error(expr, s"$subject is of neither numeric nor packed type")
    }

  def clarifyUintArg(arg: Arg)(implicit fe: Frontend): ArgP = {
    val value = fe.evaluate(arg.expr, unreachable).get
    val newExpr = TypeAssigner(ExprNum(false, value) withLocOf arg.expr)
    TypeAssigner(ArgP(newExpr) withLocOf arg)
  }

  def markSymbolsUsed(tree: Tree): Unit = tree visit {
    case ExprSym(symbol) => symbol.attr.wasUsed set true
  }

}
