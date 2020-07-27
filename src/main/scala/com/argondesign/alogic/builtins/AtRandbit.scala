////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Builtin '@randbit' that returns a random bit. Useful for compiler testing.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.frontend.Frontend

private[builtins] class AtRandbit(implicit cc: CompilerContext) extends BuiltinPolyFunc {

  val name = "@randbit"

  def returnType(args: List[Expr], fe: Option[Frontend]): Option[TypeFund] = args partialMatch {
    case Nil => TypeUInt(1)
  }

  def isKnown(args: List[Expr]) = false

  val isPure: Boolean = false

  def simplify(loc: Loc, args: List[Expr]) = None
}
