////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Builtin '@unknownu(n)', used for testing only, which has a return value of
// uint(n), but acts as if it was a compile time unknown but pure value.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.frontend.Complete
import com.argondesign.alogic.frontend.FinalResult
import com.argondesign.alogic.frontend.Frontend
import com.argondesign.alogic.util.BigIntOps.BigIntClassOps

object AtUnknownU extends Builtin("@unknownu", isPure = true) {

  def typeCheck(expr: ExprBuiltin, args: List[Expr])(implicit fe: Frontend): FinalResult[TypeFund] =
    checkArgCount(expr, 1) flatMap { _ =>
      fe.evaluate(args.head, s"argument if '$name'")
    } flatMap { width =>
      Complete(TypeUInt(width.asLong))
    }

  def clarify(expr: ExprBuiltin)(implicit fe: Frontend): Expr =
    expr.copy(args = clarifyUintArg(expr.args.head) :: Nil)

  def returnType(args: List[Arg]): TypeFund = TypeUInt(args.head.expr.value.asLong)

  def simplify(expr: ExprBuiltin): Expr = expr
}
