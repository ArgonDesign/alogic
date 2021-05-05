////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Builtin '@finish'
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.frontend.Complete
import com.argondesign.alogic.frontend.Failure
import com.argondesign.alogic.frontend.FinalResult
import com.argondesign.alogic.frontend.Frontend

object AtFinish extends Builtin("@finish", isPure = false) {

  def typeCheck(expr: ExprBuiltin, args: List[Expr])(implicit fe: Frontend): FinalResult[TypeFund] =
    args match {
      case Nil => Complete(TypeVoid)
      case expr :: Nil =>
        fe.evaluate(expr, s"Argument to '$name'") flatMap { _ =>
          Complete(TypeVoid)
        }
      case _ => Failure(expr, s"Too many arguments to '$name'")
    }

  def clarify(expr: ExprBuiltin)(implicit fe: Frontend): Expr =
    expr.copy(args = expr.args map clarifyUintArg)

  def returnType(args: List[Arg]): TypeFund = TypeVoid

  def simplify(expr: ExprBuiltin): Expr = expr
}
