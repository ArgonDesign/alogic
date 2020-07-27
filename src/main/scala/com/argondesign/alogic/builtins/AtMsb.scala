////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Builtin '@msb'
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.frontend.Frontend

private[builtins] class AtMsb(implicit cc: CompilerContext) extends BuiltinPolyFunc {

  val name = "@msb"

  def returnType(args: List[Expr], feOpt: Option[Frontend]): Option[TypeFund] = args partialMatch {
    case List(expr) if expr.tpe.isPacked && expr.tpe.width > 0 => TypeInt(false, 1)
  }

  val isPure: Boolean = true

  def simplify(loc: Loc, args: List[Expr]) = AtMsb.fold(loc, args(0))

}

private[builtins] object AtMsb {

  def fold(loc: Loc, expr: Expr)(implicit cc: CompilerContext): Option[Expr] =
    Some {
      if (expr.tpe.width == 1) {
        if (expr.tpe.isSigned) expr.castUnsigned else expr
      } else {
        (expr index (expr.tpe.width.toInt - 1)).simplify
      }
    }

}
