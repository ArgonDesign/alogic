////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Builtin '$signed'
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.frontend.Frontend
import com.argondesign.alogic.util.PartialMatch.PartialMatchImpl

object DollarSigned extends BuiltinPolyFunc("$signed") {

  def returnType(args: List[Expr], feOpt: Option[Frontend]): Option[TypeFund] = args partialMatch {
    case List(arg) if arg.tpe.isPacked         => TypeSInt(arg.tpe.width)
    case List(arg) if arg.tpe.underlying.isNum => TypeNum(true)
  }

  val isPure: Boolean = true

  def simplify(loc: Loc, args: List[Expr]): Option[Expr] = args partialMatch {
    case List(e @ ExprNum(s, v)) =>
      if (s) {
        e
      } else {
        ExprNum(true, v)
      }
    case List(e @ ExprInt(s, w, v)) =>
      if (s) {
        e
      } else if (v.testBit(w - 1)) {
        ExprInt(true, w, v - (BigInt(1) << w))
      } else {
        ExprInt(true, w, v)
      }
  }

}
