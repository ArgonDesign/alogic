////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Build an expression AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.util.unreachable
import com.argondesign.alogic.lib.Math
import org.antlr.v4.runtime.ParserRuleContext

import scala.math.BigInt.int2bigInt

object ExprBuilder extends BaseBuilder[ExprContext, Expr] {

  private def signedRest(text: String): (Boolean, String) = {
    if (text contains 's') (true, text filter { _ != 's' }) else (false, text)
  }

  private def parseBaseValue(ctx: ParserRuleContext, text: String)(
      implicit cc: CompilerContext): Option[BigInt] = {
    val (base, rest) = if (text.head != '\'') {
      (10, text)
    } else {
      text(1) match {
        case 'b' => (2, text drop 2)
        case 'd' => (10, text drop 2)
        case 'h' => (16, text drop 2)
        case _   => unreachable
      }
    }
    val digits = rest filter { _ != '_' }
    try {
      Some(BigInt(digits, base))
    } catch {
      case _: NumberFormatException => {
        cc.error(ctx, s"Invalid digit for base ${base} value")
        None
      }
    }
  }

  def apply(ctx: ExprContext)(implicit cc: CompilerContext): Expr = {
    object Visitor extends AlogicScalarVisitor[Expr] {
      // Bracket
      override def visitExprBracket(ctx: ExprBracketContext) = visit(ctx.expr)

      // Call
      override def visitExprCall(ctx: ExprCallContext) = {
        ExprCall(visit(ctx.expr), this(ctx.commaexpr)) withLoc ctx.loc
      }

      // Operators
      override def visitExprUnary(ctx: ExprUnaryContext) = {
        ExprUnary(ctx.op, visit(ctx.expr)) withLoc ctx.loc
      }
      override def visitExprBinary(ctx: ExprBinaryContext) = {
        val loc = ctx.loc.copy(point = ctx.op.getStartIndex)
        ExprBinary(visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1))) withLoc loc
      }
      override def visitExprTernary(ctx: ExprTernaryContext) = {
        val loc = ctx.loc.copy(point = ctx.op.getStartIndex)
        ExprTernary(visit(ctx.expr(0)), visit(ctx.expr(1)), visit(ctx.expr(2))) withLoc loc
      }
      override def visitExprRep(ctx: ExprRepContext) = {
        ExprRep(visit(ctx.expr(0)), visit(ctx.expr(1))) withLoc ctx.loc
      }
      override def visitExprCat(ctx: ExprCatContext) = {
        ExprCat(this(ctx.commaexpr)) withLoc ctx.loc
      }
      override def visitExprIndex(ctx: ExprIndexContext) = {
        ExprIndex(visit(ctx.expr(0)), visit(ctx.idx)) withLoc ctx.loc
      }
      override def visitExprSlice(ctx: ExprSliceContext) = {
        ExprSlice(visit(ctx.expr(0)), visit(ctx.lidx), ctx.op, visit(ctx.ridx)) withLoc ctx.loc
      }

      // Select
      override def visitExprSelect(ctx: ExprSelectContext) = {
        val loc = ctx.loc.copy(point = ctx.IDENTIFIER.getStartIndex - 1)
        ExprSelect(visit(ctx.expr), ctx.IDENTIFIER) withLoc loc
      }

      // Literals
      override def visitExprTrue(ctx: ExprTrueContext) = {
        ExprInt(false, 1, 1) withLoc ctx.loc
      }
      override def visitExprFalse(ctx: ExprFalseContext) = {
        ExprInt(false, 1, 0) withLoc ctx.loc
      }
      override def visitExprString(ctx: ExprStringContext) = {
        ExprStr(ctx.STRING.text.tail.init) withLoc ctx.loc
      }

      override def visitExprSizedInt(ctx: ExprSizedIntContext) = {
        lazy val errorExpr = ExprError() withLoc ctx.loc
        val neg = ctx.sign != null && ctx.sign.text == "-"
        val text = ctx.SIZEDINT.text
        val (signed, rest) = signedRest(text)
        val (widthDigits, baseValue) = rest span {
          _ != '\''
        }
        val width = widthDigits.toInt
        if (width == 0) {
          cc.error(ctx, "0 width integer literal")
          errorExpr
        } else {
          parseBaseValue(ctx, baseValue) map { abs =>
            val reqWidth = Math.clog2(abs + 1)
            if (reqWidth > width) {
              cc.error(ctx, s"Value specifier for ${width} bit literal requires ${reqWidth} bits")
              errorExpr
            } else {
              val mask = (BigInt(1) << width) - 1
              val bits = (if (neg) -abs else abs) & mask
              val value = if (!signed) {
                bits
              } else {
                val offset = (BigInt(-1) << width)
                if (bits.testBit(width - 1)) bits + offset else bits
              }
              if (neg && value > 0) {
                cc.warning(ctx, s"Apparently negative literal stands for positive value ${value}")
              } else if (!neg && value < 0) {
                cc.warning(ctx, s"Apparently positive literal stands for negative value ${value}")
              }
              ExprInt(signed, width, value) withLoc ctx.loc
            }
          } getOrElse {
            errorExpr
          }
        }
      }

      override def visitExprUnsizedInt(ctx: ExprUnsizedIntContext) = {
        lazy val errorExpr = ExprError() withLoc ctx.loc
        val neg = ctx.sign != null && ctx.sign.text == "-"
        val text = ctx.UNSIZEDINT.text
        val (signed, baseValue) = signedRest(text)
        parseBaseValue(ctx, baseValue) map { abs =>
          if (neg && abs > 0 && !signed) {
            cc.error(ctx, "Negative unsigned literal")
            errorExpr
          } else {
            val value = if (neg) -abs else abs
            ExprNum(signed, value) withLoc ctx.loc
          }
        } getOrElse {
          errorExpr
        }
      }

      // Identifiers
      override def visitExprIdent(ctx: ExprIdentContext) = {
        ExprRef(ctx.IDENTIFIER.toIdent) withLoc ctx.loc
      }
      override def visitExprAtid(ctx: ExprAtidContext) = {
        val ident = Ident(ctx.ATID.text) withLoc ctx.ATID.loc
        ExprRef(ident) withLoc ctx.loc
      }
      override def visitExprDollarid(ctx: ExprDollaridContext) = {
        val ident = Ident(ctx.DOLLARID.text) withLoc ctx.DOLLARID.loc
        ExprRef(ident) withLoc ctx.loc
      }

      // Type
      override def visitExprType(ctx: ExprTypeContext) = {
        ExprType(TypeBuilder(ctx.kind)) withLoc ctx.loc
      }
    }

    Visitor(ctx)
  }

}
