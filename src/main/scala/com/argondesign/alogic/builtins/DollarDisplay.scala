////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Builtin '$display'
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.frontend.Clarify
import com.argondesign.alogic.frontend.Complete
import com.argondesign.alogic.frontend.Failure
import com.argondesign.alogic.frontend.FinalResult
import com.argondesign.alogic.frontend.Frontend

import scala.util.chaining.scalaUtilChainingOps

object DollarDisplay extends Builtin("$display", isPure = false) {

  def typeCheck(expr: ExprBuiltin, args: List[Expr])(implicit fe: Frontend): FinalResult[TypeFund] =
    if (
      args pipe {
        case Nil => true
        case str :: rest =>
          str.tpe.isStr && rest.map(_.tpe.underlying).forall {
            case TypeVoid   => false
            case _: TypeNum => true
            case kind       => kind.isPacked
          }
      }
    ) {
      Complete(TypeVoid)
    } else {
      Failure(
        expr,
        s"First argument to '$name' must be a string, the rest must be packed (or unsized)"
      )
    }

  def clarify(expr: ExprBuiltin)(implicit fe: Frontend): Expr =
    expr.copy(args = expr.args map Clarify.apply)

  def returnType(args: List[Arg]): TypeFund = TypeVoid

  def simplify(expr: ExprBuiltin): Expr = expr
}
