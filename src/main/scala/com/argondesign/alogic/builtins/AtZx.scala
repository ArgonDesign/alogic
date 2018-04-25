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

object AtZx extends BuiltinPolyFunc {

  protected def name = "@zx"

  protected def retType(args: List[Expr])(implicit cc: CompilerContext): Type = {
    TypeInt(args(1).tpe.isSigned, args(0))
  }

  protected def validArgs(args: List[Expr])(implicit cc: CompilerContext) = {
    args.lengthCompare(2) == 0 && args(0).isKnownConst && args(1).tpe.isPacked
  }

  private[builtins] override def fold(call: ExprCall)(implicit cc: CompilerContext): Expr = {
    call.args(0).value flatMap { dstWidth =>
      call.args(1) match {
        case int: ExprInt => foldNumInt(call.loc, dstWidth, int)
        case ref: ExprRef => foldNumRef(call.loc, dstWidth, ref)
        case _            => None
      }
    } map { expr =>
      if (!expr.hasLoc) expr withLoc call.loc else expr
    } getOrElse {
      call
    }
  }

  private def foldNumInt(
      loc: Loc,
      width: BigInt,
      value: ExprInt
  )(
      implicit cc: CompilerContext
  ): Option[Expr] = {
    Some {
      if (width > value.width) {
        val mask = (BigInt(1) << value.width) - 1
        ExprInt(value.signed, width.toInt, value.value & mask)
      } else if (width == value.width) {
        value
      } else {
        cc.error(
          loc,
          s"Result width (${width}) of ${name} is less than argument width (${value.width})"
        )
        ExprError()
      }
    }
  }

  private def foldNumRef(
      loc: Loc,
      dstWidth: BigInt,
      ref: ExprRef
  )(
      implicit cc: CompilerContext
  ): Option[Expr] = {
    val ExprRef(Sym(symbol)) = ref
    symbol.denot.kind.width.value map { srcWidth =>
      if (dstWidth > srcWidth) {
        ExprCat(List(ExprInt(false, (dstWidth - srcWidth).toInt, 0), ref)) regularize loc
      } else if (dstWidth == srcWidth) {
        ref
      } else {
        cc.error(
          loc,
          s"Result width (${dstWidth}) of ${name} is less than argument width (${srcWidth})"
        )
        ExprError()
      }
    }
  }

}
