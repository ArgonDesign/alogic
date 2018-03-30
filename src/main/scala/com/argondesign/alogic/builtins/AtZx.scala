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
import com.argondesign.alogic.core.Types._

object AtZx extends BuiltinPolyFunc {

  protected def name = "@zx"

  protected def retType(args: List[Expr])(implicit cc: CompilerContext): Type = {
    TypeInt(args(1).tpe.isSigned, args(0))
  }

  protected def validArgs(args: List[Expr])(implicit cc: CompilerContext) = {
    args.lengthCompare(2) == 0 && args(0).isKnownConst && args(1).tpe.isPacked
  }

  private[builtins] override def fold(call: ExprCall)(implicit cc: CompilerContext): Expr = {
    val folded = call.args match {
      case List(width: ExprNum, value: ExprInt) => foldNumInt(call, width, value)
      case _                                    => None
    }

    folded map { expr =>
      if (!expr.hasLoc) expr withLoc call.loc else expr
    } getOrElse call
  }

  private def foldNumInt(call: ExprCall, width: ExprNum, value: ExprInt)(
      implicit cc: CompilerContext): Option[Expr] = {
    if (width.value > value.width) {
      val mask = (BigInt(1) << value.width) - 1
      Some(ExprInt(value.signed, width.value.toInt, value.value & mask))
    } else if (width.value == value.width) {
      Some(value)
    } else {
      cc.error(
        call,
        s"Result width (${width.value}) of ${name} is less than argument width (${value.width})"
      )
      Some(ExprError())
    }
  }

}
