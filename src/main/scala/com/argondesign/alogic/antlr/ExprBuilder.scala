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
import com.argondesign.alogic.lib.Math
import com.argondesign.alogic.util.unreachable

import scala.math.BigInt.int2bigInt

object ExprBuilder extends BaseBuilder[ExprContext, Expr] {

  def apply(ctx: ExprContext)(implicit cc: CompilerContext): Expr = {
    object Visitor extends AlogicScalarVisitor[Expr] {
      // Bracket
      override def visitExprBracket(ctx: ExprBracketContext) = visit(ctx.expr)

      // Call
      override def visitExprCall(ctx: ExprCallContext) = {
        val args = Option(ctx.commaexpr) map { this(_) } getOrElse Nil
        ExprCall(visit(ctx.expr), args) withLoc ctx.loc
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
        val rep = visit(ctx.expr)
        val parts = this(ctx.commaexpr)
        lazy val cat = ExprCat(parts) withLoc ctx.commaexpr.loc
        parts match {
          case one :: Nil => ExprRep(rep, one) withLoc ctx.loc
          case _          => ExprRep(rep, cat) withLoc ctx.loc
        }
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
        val loc = ctx.loc.copy(point = ctx.ident.start.getStartIndex - 1)
        val Ident(name, idxs) = IdentBuilder(ctx.ident)
        ExprSelect(visit(ctx.expr), name, idxs) withLoc loc
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
        val neg = ctx.sign != null && ctx.sign.text == "-"
        val text = ctx.SIZEDINT.text
        val (widthDigits, tickRest) = text span { _ != '\'' }
        val width = widthDigits.toInt
        if (width == 0) {
          cc.error(ctx, "0 width integer literal")
          ExprError() withLoc ctx.loc
        } else {
          val signed = tickRest(1) == 's'
          val baseRest = if (signed) tickRest drop 2 else tickRest drop 1
          val base = baseRest(0) match {
            case 'b' => 2
            case 'd' => 10
            case 'h' => 16
            case _   => unreachable
          }
          val digits = baseRest drop 1 filter { _ != '_' }
          try {
            val abs = BigInt(digits, base)
            val reqWidth = Math.clog2(abs + 1)
            if (reqWidth > width) {
              cc.error(ctx, s"Value specifier for ${width} bit literal requires ${reqWidth} bits")
              ExprError() withLoc ctx.loc
            } else if (neg && abs > 0 && !signed) {
              cc.error(ctx, "Negative unsigned literal")
              ExprError() withLoc ctx.loc
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
          } catch {
            case _: NumberFormatException =>
              cc.error(ctx, s"Invalid digit for base ${base} value")
              ExprError() withLoc ctx.loc
          }
        }
      }

      override def visitExprUnsizedInt(ctx: ExprUnsizedIntContext) = {
        val neg = ctx.sign != null && ctx.sign.text == "-"
        val text = ctx.UNSIZEDINT.text
        if (text.length > 1 && text(0) == '0' && text(1).isDigit) {
          cc.error(ctx,
                   s"Invalid literal '${text}',",
                   "use prefix '0o' for octal or '0d' for decimal with leading zeros")
          ExprError() withLoc ctx.loc
        } else {
          val signed = text.last == 's'
          val rest = if (signed || text.last == 'u') text.init else text
          val literal = rest filter { _ != '_' }
          val base = if (literal.length == 1) {
            10
          } else {
            literal(1) match {
              case 'b' => 2
              case 'o' => 8
              case 'd' => 10
              case 'x' => 16
              case _   => 10
            }
          }
          val digits = if (literal.length > 1 && !literal(1).isDigit) literal drop 2 else literal
          try {
            val abs = BigInt(digits, base)
            if (neg && abs > 0 && !signed) {
              cc.error(ctx, "Negative unsigned literal")
              ExprError() withLoc ctx.loc
            } else {
              val value = if (neg) -abs else abs
              ExprNum(signed, value) withLoc ctx.loc
            }
          } catch {
            case _: NumberFormatException =>
              cc.error(ctx, s"Invalid digit for base ${base} value")
              ExprError() withLoc ctx.loc
          }
        }
      }

      // Identifiers
      override def visitExprIdent(ctx: ExprIdentContext) = {
        ExprRef(IdentBuilder(ctx.ident)) withLoc ctx.loc
      }
      override def visitExprAtid(ctx: ExprAtidContext) = {
        ExprRef(IdentBuilder(ctx.ATID)) withLoc ctx.loc
      }
      override def visitExprDollarid(ctx: ExprDollaridContext) = {
        ExprRef(IdentBuilder(ctx.DOLLARID)) withLoc ctx.loc
      }

      // Type
      override def visitExprType(ctx: ExprTypeContext) = {
        ExprType(TypeBuilder(ctx.kind)) withLoc ctx.loc
      }
    }

    Visitor(ctx)
  }

}
