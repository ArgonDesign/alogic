////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Builtin '$clog2'
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.frontend.Complete
import com.argondesign.alogic.frontend.Failure
import com.argondesign.alogic.frontend.FinalResult
import com.argondesign.alogic.frontend.Frontend
import com.argondesign.alogic.lib.Math
import com.argondesign.alogic.util.unreachable

object DollarClog2 extends Builtin("$clog2", isPure = true) {

  def typeCheck(expr: ExprBuiltin, args: List[Expr])(implicit fe: Frontend): FinalResult[TypeFund] =
    checkArgCount(expr, 1) flatMap { _ =>
      fe.evaluate(args.head, s"argument of '$name'")
    } flatMap {
      case v if v < 0 => Failure(args.head, s"'$name' invoked on negative argument $v")
      case _          => Complete(TypeNum(false))
    }

  def clarify(expr: ExprBuiltin)(implicit fe: Frontend): Expr =
    ExprNum(false, Math.clog2(fe.evaluate(expr.args.head.expr, unreachable).get))

  def returnType(args: List[Arg]): TypeFund = unreachable

  def simplify(expr: ExprBuiltin): Expr = unreachable
}
