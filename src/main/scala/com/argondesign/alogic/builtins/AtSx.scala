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
// Builtin '@sx'
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Types._

private[builtins] class AtSx(implicit cc: CompilerContext) extends BuiltinPolyFunc {

  val name = "@sx"

  def returnType(args: List[Expr]): Option[TypeFund] = args partialMatch {
    case List(width, expr) if width.isKnownConst && expr.tpe.isPacked =>
      TypeInt(expr.tpe.isSigned, width.value.get.toInt)
  }

  def isKnown(args: List[Expr]) = args(1).isKnownConst

  val isPure: Boolean = true

  def simplify(loc: Loc, args: List[Expr]) = {
    val List(width, expr) = args
    AtMsb.fold(loc, expr) flatMap { msb =>
      AtEx.fold(loc, msb, width, expr)
    }
  }

}
