////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Builtin '$clog2'
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.frontend.Frontend
import com.argondesign.alogic.lib.Math

private[builtins] class DollarClog2(implicit cc: CompilerContext) extends BuiltinPolyFunc {

  val name = "$clog2"

  // TODO: die when non-const argument

  def returnType(args: List[Expr], feOpt: Option[Frontend]): Option[TypeFund] = args partialMatch {
    case List(arg) if arg.tpe.isPacked || arg.tpe.underlying.isNum => TypeNum(false)
  }

  val isPure: Boolean = true

  def simplify(loc: Loc, args: List[Expr]) = {
    args(0).value map { value =>
      if (value < 0) {
        cc.error(loc, s"'$name' invoked on negative value $value")
        ExprError()
      } else {
        if (value == 0) {
          cc.warning(loc, s"'$name' invoked on value 0")
        }
        ExprNum(false, Math.clog2(value))
      }
    }
  }

}
