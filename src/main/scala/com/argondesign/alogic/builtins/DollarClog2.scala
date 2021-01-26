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
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Messages.Fatal
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.frontend.Frontend
import com.argondesign.alogic.lib.Math
import com.argondesign.alogic.util.PartialMatch.PartialMatchImpl

object DollarClog2 extends BuiltinPolyFunc("$clog2") {

  // TODO: die when non-const argument

  def returnType(args: List[Expr], feOpt: Option[Frontend]): Option[TypeFund] = args partialMatch {
    case List(arg) if arg.tpe.isPacked || arg.tpe.underlying.isNum => TypeNum(false)
  }

  val isPure: Boolean = true

  def simplify(loc: Loc, args: List[Expr]): Option[Expr] = {
    args(0).valueOption map { value =>
      if (value < 0) {
        throw Fatal(loc, s"'$name' invoked on negative value $value")
      } else {
        ExprNum(false, Math.clog2(value))
      }
    }
  }

}
