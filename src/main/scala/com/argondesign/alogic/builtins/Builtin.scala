////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Base class for the implementations of builtin function. Builtin functions
// are special because they can be variadic, polymorphic and have dependent
// return type.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.Expr
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.Types.TypeFund
import com.argondesign.alogic.frontend.Complete
import com.argondesign.alogic.frontend.Failure
import com.argondesign.alogic.frontend.FinalResult
import com.argondesign.alogic.frontend.Frontend

import scala.util.chaining.scalaUtilChainingOps

abstract class Builtin(val name: String, val isPure: Boolean) {

  // Implementation of type checking for given positional arguments. 'expr'
  // is the original invocation with arguments unchanged.
  protected def typeCheck(
      expr: ExprBuiltin,
      args: List[Expr]
    )(
      implicit
      fe: Frontend
    ): FinalResult[TypeFund]

  //////////////////////////////////////////////////////////////////////////////
  // Public interface
  //////////////////////////////////////////////////////////////////////////////

  // Type check invocation, assuming all arguments are well typed,
  // yields the type fo the result
  final def typeCheck(expr: ExprBuiltin)(implicit fe: Frontend): FinalResult[TypeFund] =
    expr.args flatMap {
      case _: ArgP => None
      case arg     => Some(Error(arg, s"Builtin function '$name' requires positional arguments"))
    } pipe {
      case Nil    => Complete(())
      case errors => Failure(errors)
    } flatMap { _ =>
      typeCheck(expr, expr.args.map(_.expr))
    }

  // Clarify invocation, assuming it is well typed, but note that the arguments
  // have not been clarified yet, so they need to be clarified or dropped
  def clarify(expr: ExprBuiltin)(implicit fe: Frontend): Expr

  // Type of return value for given arguments, assuming it's clarified
  def returnType(args: List[Arg]): TypeFund

  // Simplify invocation, assuming it's well formed
  def simplify(expr: ExprBuiltin): Expr
}
