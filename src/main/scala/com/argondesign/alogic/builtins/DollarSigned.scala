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
// Builtin '$signed'
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Types._

private[builtins] class DollarSigned(implicit cc: CompilerContext)
    extends BuiltinPolyFunc(isValidConnLhs = true) {

  val name = "$signed"

  def returnType(args: List[Expr]): Option[TypeFund] = args partialMatch {
    case List(arg) if arg.tpe.isPacked => TypeSInt(arg.tpe.width)
    case List(arg) if arg.tpe.isNum    => TypeNum(true)
  }

  def isKnown(args: List[Expr]) = args(0).isKnownConst

  val isPure: Boolean = true

  def simplify(loc: Loc, args: List[Expr]) = args partialMatch {
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
