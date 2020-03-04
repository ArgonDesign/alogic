////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Builtin '$clog2'
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Math

private[builtins] class DollarClog2(implicit cc: CompilerContext)
    extends BuiltinPolyFunc(isValidConnLhs = true) {

  val name = "$clog2"

  // TODO: die when non-const argument

  def returnType(args: List[Expr]): Option[TypeFund] = args partialMatch {
    case List(arg) if arg.tpe.isPacked || arg.tpe.isNum => TypeNum(false)
  }

  def isKnown(args: List[Expr]) = args(0).isKnownConst // TODO: should be always true

  def simplify(loc: Loc, args: List[Expr]) = {
    args(0).value map { value =>
      if (value < 0) {
        cc.error(loc, s"'${name}' invoked on negative value ${value}")
        ExprError()
      } else {
        if (value == 0) {
          cc.warning(loc, s"'${name}' invoked on value 0")
        }
        ExprNum(false, Math.clog2(value))
      }
    }
  }
}
