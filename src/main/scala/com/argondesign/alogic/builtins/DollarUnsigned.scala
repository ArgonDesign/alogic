////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Builtin '$unsigned'
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Messages.Fatal
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.frontend.Frontend
import com.argondesign.alogic.util.PartialMatch.PartialMatchImpl

object DollarUnsigned extends BuiltinPolyFunc("$unsigned") {

  def returnType(args: List[Expr], feOpt: Option[Frontend]): Option[TypeFund] = args partialMatch {
    case List(arg) if arg.tpe.isPacked         => TypeUInt(arg.tpe.width)
    case List(arg) if arg.tpe.underlying.isNum => TypeNum(false)
  }

  val isPure: Boolean = true

  def simplify(loc: Loc, args: List[Expr]) = args partialMatch {
    case List(e @ ExprNum(s, v)) =>
      if (!s) {
        e
      } else if (v < 0) {
        throw Fatal(loc, "Cannot cast negative unsized integer to unsigned")
      } else {
        ExprNum(false, v)
      }
    case List(e @ ExprInt(s, w, v)) =>
      if (!s) {
        e
      } else if (v.testBit(w - 1)) {
        ExprInt(false, w, v + (BigInt(1) << w))
      } else {
        ExprInt(false, w, v)
      }
  }

}
