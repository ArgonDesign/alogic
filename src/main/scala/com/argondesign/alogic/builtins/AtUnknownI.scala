////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Builtin '@unknowni(n)', used for testing only, which has a return value of
// int(n), but acts as if it was a compile time unknown value.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.frontend.Frontend
import com.argondesign.alogic.util.BigIntOps.BigIntClassOps
import com.argondesign.alogic.util.PartialMatch.PartialMatchImpl

object AtUnknownI extends BuiltinPolyFunc("@unknowni") {

  def returnType(args: List[Expr], fe: Option[Frontend]): Option[TypeFund] = args partialMatch {
    case List(ExprNum(_, width)) => TypeSInt(width.asLong)
  }

  def isKnown(args: List[Expr]) = false

  val isPure: Boolean = false

  def simplify(loc: Loc, args: List[Expr]): Option[Expr] = None
}
