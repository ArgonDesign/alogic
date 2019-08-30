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
// Builtin '@zx'
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Types._

private[builtins] class AtZx(implicit cc: CompilerContext) extends BuiltinPolyFunc {

  val name = "@zx"

  def returnType(args: List[Expr]) = args partialMatch {
    case List(width, expr) if width.isKnownConst && expr.tpe.isPacked => {
      TypeInt(expr.tpe.isSigned, width)
    }
  }

  def combArgs(args: List[Expr]) = List(args(1))

  def fold(loc: Loc, args: List[Expr]) = {
    val List(width, expr) = args
    AtEx.fold(loc, ExprInt(false, 1, 0) withLoc loc, width, expr)
  }
}
