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
// Builtin '@ex(bit, width, expr)'
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.builtins

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Types._

private[builtins] class AtEx(implicit cc: CompilerContext)
    extends BuiltinPolyFunc(isValidConnLhs = true) {

  val name = "@ex"

  def validArgs(args: List[Expr]) = args match {
    case List(bit, width, expr) => (bit.tpe.width == 1) && width.isKnownConst && expr.tpe.isPacked
    case _                      => false
  }

  def returnType(args: List[Expr]): Option[TypeFund] = Some {
    TypeInt(args(2).tpe.isSigned, args(1).value.get.toInt)
  }

  def isKnown(args: List[Expr]) = args(0).isKnownConst && args(2).isKnownConst

  def simplify(loc: Loc, args: List[Expr]) = AtEx.fold(loc, args(0), args(1), args(2))

}

private[builtins] object AtEx {

  def fold(
      loc: Loc,
      bitExpr: Expr,
      width: Expr,
      expr: Expr
  )(
      implicit cc: CompilerContext
  ): Option[Expr] = {

    def fixSign(result: Expr): Expr = {
      if (expr.tpe.isSigned) {
        // Resolving the polymorphic builtin $signed needs types,
        // so assign types if we are folding a typed expression,
        // otherwise the TypeAssigner will choke on the unresolved
        // reference to the polymorphic builtin
        if (expr.hasTpe) {
          result regularize loc
        }
        cc.makeBuiltinCall("$signed", result.loc, List(result))
      } else {
        result
      }
    }

    for {
      dstWidth <- width.value map { _.toInt }
    } yield {
      val srcWidth = expr.tpe.width.toInt
      val d = dstWidth - srcWidth
      if (d < 0) {
        val msg = s"Result width ${dstWidth} of extension is less than argument width ${srcWidth}"
        cc.error(loc, msg)
        ExprError()
      } else if (d == 0) {
        expr
      } else {
        bitExpr.value map { _.abs } map { bit =>
          // Known bit value
          assert(bit == 0 || bit == 1)
          lazy val msbs = (bit << d) - bit
          lazy val bits = msbs << srcWidth
          lazy val mask = (BigInt(1) << srcWidth) - 1
          expr match {
            case ExprInt(true, _, v) if v >= 0 && bit == 0 => ExprInt(true, dstWidth, v)
            case ExprInt(true, _, v) if v >= 0 && bit == 1 => ExprInt(true, dstWidth, bits | v)
            case ExprInt(true, _, v) if v < 0 && bit == 0  => ExprInt(true, dstWidth, mask & v)
            case ExprInt(true, _, v) if v < 0 && bit == 1  => ExprInt(true, dstWidth, v)
            case ExprInt(false, _, v)                      => ExprInt(false, dstWidth, bits | v)
            case _                                         => fixSign(ExprInt(false, d, msbs) cat expr)
          }
        } getOrElse {
          // Variable bit value
          fixSign {
            if (d == 1) {
              bitExpr cat expr
            } else {
              bitExpr rep d cat expr
            }
          }
        }
      }
    }
  }

}
