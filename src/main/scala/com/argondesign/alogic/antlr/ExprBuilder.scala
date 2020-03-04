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
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Math
import com.argondesign.alogic.util.unreachable

import scala.math.BigInt.int2bigInt

object ExprBuilder extends BaseBuilder[ExprContext, Expr] {

  def apply(ctx: ExprContext)(implicit cc: CompilerContext): Expr = {
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
              cc.error(ctx, s"Value specifier for $width bit literal requires $reqWidth bits")
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
                val offset = BigInt(-1) << width
                if (bits.testBit(width - 1)) bits + offset else bits
              }
              if (neg && value > 0) {
                cc.warning(ctx, s"Apparently negative literal stands for positive value $value")
              } else if (!neg && value < 0) {
                cc.warning(ctx, s"Apparently positive literal stands for negative value $value")
              }
              ExprInt(signed, width, value) withLoc ctx.loc
            }
          } catch {
            case _: NumberFormatException =>
              cc.error(ctx, s"Invalid digit for base $base value")
              ExprError() withLoc ctx.loc
          }
        }
      }

      override def visitExprLitUnsizedInt(ctx: ExprLitUnsizedIntContext): Expr = {
        val neg = ctx.sign != null && ctx.sign.text == "-"
        val text = ctx.UNSIZEDINT.text
        if (text.length > 1 && text(0) == '0' && text(1).isDigit) {
          cc.error(ctx,
                   s"Invalid literal '$text',",
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
              cc.error(ctx, s"Invalid digit for base $base value")
              ExprError() withLoc ctx.loc
          }
        }
      }

      override def visitExprLitString(ctx: ExprLitStringContext): Expr =
        ExprStr(ctx.STRING.text.tail.init) withLoc ctx.loc

      //////////////////////////////////////////////////////////////////////////
      // Primitive types
      //////////////////////////////////////////////////////////////////////////

      override def visitExprTypeBool(ctx: ExprTypeBoolContext): Expr =
        ExprType(TypeUInt(1)) withLoc ctx.loc

      override def visitExprTypeSInt(ctx: ExprTypeSIntContext): Expr =
        ExprType(TypeSInt(ctx.INTTYPE.text.tail.toInt)) withLoc ctx.loc

      override def visitExprTypeUInt(ctx: ExprTypeUIntContext): Expr =
        ExprType(TypeUInt(ctx.UINTTYPE.text.tail.toInt)) withLoc ctx.loc

      override def visitExprTypeSNum(ctx: ExprTypeSNumContext): Expr =
        ExprType(TypeNum(true)) withLoc ctx.loc

      override def visitExprTypeUNum(ctx: ExprTypeUNumContext): Expr =
        ExprType(TypeNum(false)) withLoc ctx.loc

      override def visitExprTypeVoid(ctx: ExprTypeVoidContext): Expr =
        ExprType(TypeVoid) withLoc ctx.loc

      //////////////////////////////////////////////////////////////////////////
      // Names
      //////////////////////////////////////////////////////////////////////////

      override def visitExprIdent(ctx: ExprIdentContext): Expr =
        ExprRef(IdentBuilder(ctx.ident)) withLoc ctx.loc

      override def visitExprAtid(ctx: ExprAtidContext): Expr =
        ExprRef(IdentBuilder(ctx.ATID)) withLoc ctx.loc

      override def visitExprDollarid(ctx: ExprDollaridContext): Expr =
        ExprRef(IdentBuilder(ctx.DOLLARID)) withLoc ctx.loc

      //////////////////////////////////////////////////////////////////////////
      // Call
      //////////////////////////////////////////////////////////////////////////

      override def visitExprCall(ctx: ExprCallContext): Expr = {
        object ArgVisitor extends AlogicScalarVisitor[Arg] {
          override def visitArgNamed(ctx: ArgNamedContext): Arg = {
            val loc = ctx.loc.copy(point = ctx.point.getStartIndex)
            ArgN(ctx.IDENTIFIER, ExprBuilder(ctx.expr)) withLoc loc
          }

          override def visitArgPositional(ctx: ArgPositionalContext): Arg =
            ArgP(ExprBuilder(ctx.expr)) withLoc ctx.loc
        }

        val args = if (ctx.args == null) Nil else ArgVisitor(ctx.args.arg)
        ExprCall(visit(ctx.expr), args) withLoc ctx.loc
      }

      //////////////////////////////////////////////////////////////////////////
      // Index/Slice
      //////////////////////////////////////////////////////////////////////////

      override def visitExprIndex(ctx: ExprIndexContext): Expr =
        ExprIndex(visit(ctx.expr(0)), visit(ctx.idx)) withLoc ctx.loc

      override def visitExprSlice(ctx: ExprSliceContext): Expr =
        ExprSlice(visit(ctx.expr(0)), visit(ctx.lidx), ctx.op, visit(ctx.ridx)) withLoc ctx.loc

      //////////////////////////////////////////////////////////////////////////
      // Select
      //////////////////////////////////////////////////////////////////////////

      override def visitExprSelect(ctx: ExprSelectContext): Expr = {
        val loc = ctx.loc.copy(point = ctx.ident.start.getStartIndex - 1)
        val Ident(name, idxs) = IdentBuilder(ctx.ident)
        ExprSelect(visit(ctx.expr), name, idxs) withLoc loc
      }

      //////////////////////////////////////////////////////////////////////////
      // Operators
      //////////////////////////////////////////////////////////////////////////

      override def visitExprUnary(ctx: ExprUnaryContext): Expr =
        ExprUnary(ctx.op, visit(ctx.expr)) withLoc ctx.loc

      override def visitExprBinary(ctx: ExprBinaryContext): Expr = {
        val loc = ctx.loc.copy(point = ctx.op.getStartIndex)
        ExprBinary(visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1))) withLoc loc
      }

      override def visitExprTernary(ctx: ExprTernaryContext): Expr = {
        val loc = ctx.loc.copy(point = ctx.op.getStartIndex)
        ExprTernary(visit(ctx.expr(0)), visit(ctx.expr(1)), visit(ctx.expr(2))) withLoc loc
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
