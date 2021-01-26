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
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.frontend.Frontend
import com.argondesign.alogic.util.PartialMatch.PartialMatchImpl

object AtMsb extends BuiltinPolyFunc("@msb") {

  def returnType(args: List[Expr], feOpt: Option[Frontend]): Option[TypeFund] = args partialMatch {
    case List(expr) if expr.tpe.isPacked && expr.tpe.width > 0 => TypeInt(false, 1)
  }

  val isPure: Boolean = true

  def simplify(loc: Loc, args: List[Expr]): Option[Expr] = fold(loc, args(0))

  def fold(loc: Loc, expr: Expr): Option[Expr] = Some {
    if (expr.tpe.width == 1) {
      if (expr.tpe.isSigned) expr.castUnsigned else expr
    } else {
      (expr index (expr.tpe.width.toInt - 1)).simplify
    }
  }

}
