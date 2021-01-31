////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Build an expression AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.SourceContext
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Math
import com.argondesign.alogic.util.unreachable

import scala.math.BigInt.int2bigInt

object ExprBuilder extends BaseBuilder[ExprContext, Expr] {

  def apply(ctx: ExprContext)(implicit mb: MessageBuffer, sc: SourceContext): Expr = {
    object Visitor extends AlogicScalarVisitor[Expr] {
      //////////////////////////////////////////////////////////////////////////
      // Bracket
      //////////////////////////////////////////////////////////////////////////

      override def visitExprBracket(ctx: ExprBracketContext): Expr = visit(ctx.expr)

      //////////////////////////////////////////////////////////////////////////
      // Literals
      //////////////////////////////////////////////////////////////////////////

      override def visitExprLitTrue(ctx: ExprLitTrueContext): Expr =
        ExprInt(false, 1, 1) withLoc ctx.loc

      override def visitExprLitFalse(ctx: ExprLitFalseContext): Expr =
        ExprInt(false, 1, 0) withLoc ctx.loc

      override def visitExprLitSizedInt(ctx: ExprLitSizedIntContext): Expr = {
        val neg = ctx.sign != null && ctx.sign.txt == "-"
        val text = ctx.SIZEDINT.txt
        val (widthDigits, tickRest) = text span { _ != '\'' }
        val width = widthDigits.toInt
        if (width == 0) {
          mb.error(ctx, "0 width integer literal")
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
              mb.error(ctx, s"Value specifier for $width bit literal requires $reqWidth bits")
              ExprError() withLoc ctx.loc
            } else if (neg && abs > 0 && !signed) {
              mb.error(ctx, "Negative unsigned literal")
              ExprError() withLoc ctx.loc
            } else {
              val mask = (BigInt(1) << width) - 1
              val bits = (if (neg) -abs else abs) & mask
              val value = if (!signed) {
                bits
              } else {
                val offset = BigInt(-1) << width
                if (bits.testBit(width - 1)) bits + offset else bits
              }
              if (neg && value > 0) {
                mb.warning(ctx, s"Apparently negative literal stands for positive value $value")
              } else if (!neg && value < 0) {
                mb.warning(ctx, s"Apparently positive literal stands for negative value $value")
              }
              ExprInt(signed, width, value) withLoc ctx.loc
            }
          } catch {
            case _: NumberFormatException =>
              mb.error(ctx, s"Invalid digit for base $base value")
              ExprError() withLoc ctx.loc
          }
        }
      }

      override def visitExprLitUnsizedInt(ctx: ExprLitUnsizedIntContext): Expr = {
        val neg = ctx.sign != null && ctx.sign.txt == "-"
        val text = ctx.UNSIZEDINT.txt
        if (text.length > 1 && text(0) == '0' && text(1).isDigit) {
          mb.error(
            ctx,
            s"Invalid literal '$text',",
            "use prefix '0o' for octal or '0d' for decimal with leading zeros"
          )
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
              mb.error(ctx, "Negative unsigned literal")
              ExprError() withLoc ctx.loc
            } else {
              val value = if (neg) -abs else abs
              ExprNum(signed, value) withLoc ctx.loc
            }
          } catch {
            case _: NumberFormatException =>
              mb.error(ctx, s"Invalid digit for base $base value")
              ExprError() withLoc ctx.loc
          }
        }
      }

      override def visitExprLitString(ctx: ExprLitStringContext): Expr =
        ExprStr(ctx.STRING.txt.tail.init) withLoc ctx.loc

      //////////////////////////////////////////////////////////////////////////
      // Primitive types
      //////////////////////////////////////////////////////////////////////////

      override def visitExprTypeBool(ctx: ExprTypeBoolContext): Expr =
        ExprType(TypeUInt(1)) withLoc ctx.loc

      override def visitExprTypeSInt(ctx: ExprTypeSIntContext): Expr =
        ExprType(TypeSInt(ctx.INTTYPE.txt.tail.toInt)) withLoc ctx.loc

      override def visitExprTypeUInt(ctx: ExprTypeUIntContext): Expr =
        ExprType(TypeUInt(ctx.UINTTYPE.txt.tail.toInt)) withLoc ctx.loc

      override def visitExprTypeSNum(ctx: ExprTypeSNumContext): Expr =
        ExprType(TypeNum(true)) withLoc ctx.loc

      override def visitExprTypeUNum(ctx: ExprTypeUNumContext): Expr =
        ExprType(TypeNum(false)) withLoc ctx.loc

      override def visitExprTypeVoid(ctx: ExprTypeVoidContext): Expr =
        ExprType(TypeVoid) withLoc ctx.loc

      //////////////////////////////////////////////////////////////////////////
      // Keywords
      //////////////////////////////////////////////////////////////////////////

      override def visitExprKeyword(ctx: ExprKeywordContext): Expr =
        ExprIdent(ctx.txt, Nil) withLoc ctx.loc

      override def visitExprThis(ctx: ExprThisContext): Expr = {
        mb.error(ctx.loc, "'this' reference is not user accessible")
        ExprError() withLoc ctx.loc
      }

      //////////////////////////////////////////////////////////////////////////
      // Names
      //////////////////////////////////////////////////////////////////////////

      override def visitExprIdent(ctx: ExprIdentContext): Expr =
        ExprIdent(ctx.ident.IDENTIFIER.txt, ExprBuilder(ctx.ident.expr)) withLoc ctx.loc

      override def visitExprAtid(ctx: ExprAtidContext): Expr =
        ExprIdent(ctx.ATID.txt, Nil) withLoc ctx.loc

      override def visitExprDollarid(ctx: ExprDollaridContext): Expr =
        ExprIdent(ctx.DOLLARID.txt, Nil) withLoc ctx.loc

      //////////////////////////////////////////////////////////////////////////
      // Call
      //////////////////////////////////////////////////////////////////////////

      override def visitExprCall(ctx: ExprCallContext): Expr = {
        val loc = ctx.loc.copy(point = ctx.point.getStartIndex)
        ExprCall(visit(ctx.expr), ArgBuilder(ctx.args)) withLoc loc
      }

      //////////////////////////////////////////////////////////////////////////
      // Index/Slice
      //////////////////////////////////////////////////////////////////////////

      override def visitExprIndex(ctx: ExprIndexContext): Expr = {
        val loc = ctx.loc.copy(point = ctx.point.getStartIndex)
        ExprIndex(visit(ctx.expr(0)), visit(ctx.idx)) withLoc loc
      }

      override def visitExprSlice(ctx: ExprSliceContext): Expr = {
        val loc = ctx.loc.copy(point = ctx.point.getStartIndex)
        ExprSlice(visit(ctx.expr(0)), visit(ctx.lidx), ctx.op.txt, visit(ctx.ridx)) withLoc loc
      }

      //////////////////////////////////////////////////////////////////////////
      // Select
      //////////////////////////////////////////////////////////////////////////

      override def visitExprDot(ctx: ExprDotContext): Expr = {
        val expr = visit(ctx.expr)
        val loc = ctx.loc.copy(point = ctx.point.getStartIndex)
        if (ctx.ident != null) {
          val Ident(name, idxs) = IdentBuilder(ctx.ident)
          ExprDot(expr, name, idxs) withLoc loc
        } else {
          ExprDot(expr, ctx.inout.txt, Nil) withLoc loc
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // Operators
      //////////////////////////////////////////////////////////////////////////

      override def visitExprUnary(ctx: ExprUnaryContext): Expr =
        ExprUnary(ctx.op.txt, visit(ctx.expr)) withLoc ctx.loc

      override def visitExprBinary(ctx: ExprBinaryContext): Expr = {
        val loc = ctx.loc.copy(point = ctx.op.getStartIndex)
        ExprBinary(visit(ctx.expr(0)), ctx.op.txt, visit(ctx.expr(1))) withLoc loc
      }

      override def visitExprTernary(ctx: ExprTernaryContext): Expr = {
        val loc = ctx.loc.copy(point = ctx.point.getStartIndex)
        ExprCond(visit(ctx.expr(0)), visit(ctx.expr(1)), visit(ctx.expr(2))) withLoc loc
      }

      override def visitExprRep(ctx: ExprRepContext): Expr = visit(ctx.expr) match {
        case Nil                => unreachable
        case _ :: Nil           => unreachable
        case rep :: part :: Nil => ExprRep(rep, part) withLoc ctx.loc
        case rep :: parts =>
          val cat = ExprCat(parts) withLoc ctx.s.loc.copy(end = ctx.e.loc.end)
          ExprRep(rep, cat) withLoc ctx.loc
      }

      override def visitExprCat(ctx: ExprCatContext): Expr =
        ExprCat(visit(ctx.expr)) withLoc ctx.loc
    }

    Visitor(ctx)
  }

}
