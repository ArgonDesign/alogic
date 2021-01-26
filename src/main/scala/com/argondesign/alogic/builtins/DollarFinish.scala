////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Builtin '$finish'
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.frontend.Frontend
import com.argondesign.alogic.util.PartialMatch.PartialMatchImpl

object DollarFinish extends BuiltinPolyFunc("$finish") {

  def returnType(args: List[Expr], feOpt: Option[Frontend]): Option[TypeFund] = args partialMatch {
    case Nil                           => TypeVoid
    case expr :: Nil if expr.tpe.isNum => TypeVoid
  }

  def isKnown(args: List[Expr]) = false

  val isPure: Boolean = false

  def simplify(loc: Loc, args: List[Expr]): Option[Expr] = None
}
