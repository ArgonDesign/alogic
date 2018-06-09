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
// Builtin '@randbit' that returns a random bit. Useful for testing.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Types._

private[builtins] class AtRandbit(implicit cc: CompilerContext) extends BuiltinPolyFunc {

  val name = "@randbit"

  def returnType(args: List[Expr]) = args partialMatch {
    case Nil => TypeUInt(Expr(1))
  }

  def isKnownConst(args: List[Expr]) = false

  def fold(loc: Loc, args: List[Expr]) = None
}
