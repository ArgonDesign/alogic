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

import scala.BigInt
import scala.math.BigInt.int2bigInt

import com.argondesign.alogic.antlr.AlogicParser.ExprAtCallContext
import com.argondesign.alogic.antlr.AlogicParser.ExprBinaryContext
import com.argondesign.alogic.antlr.AlogicParser.ExprBracketContext
import com.argondesign.alogic.antlr.AlogicParser.ExprCallContext
import com.argondesign.alogic.antlr.AlogicParser.ExprCatContext
import com.argondesign.alogic.antlr.AlogicParser.ExprConstContext
import com.argondesign.alogic.antlr.AlogicParser.ExprConstTickNumContext
import com.argondesign.alogic.antlr.AlogicParser.ExprContext
import com.argondesign.alogic.antlr.AlogicParser.ExprDollarCallContext
import com.argondesign.alogic.antlr.AlogicParser.ExprFalseContext
import com.argondesign.alogic.antlr.AlogicParser.ExprIdentContext
import com.argondesign.alogic.antlr.AlogicParser.ExprIndexContext
import com.argondesign.alogic.antlr.AlogicParser.ExprRepContext
import com.argondesign.alogic.antlr.AlogicParser.ExprSelectContext
import com.argondesign.alogic.antlr.AlogicParser.ExprSliceContext
import com.argondesign.alogic.antlr.AlogicParser.ExprStringContext
import com.argondesign.alogic.antlr.AlogicParser.ExprTernaryContext
import com.argondesign.alogic.antlr.AlogicParser.ExprTickNumContext
import com.argondesign.alogic.antlr.AlogicParser.ExprTrueContext
import com.argondesign.alogic.antlr.AlogicParser.ExprTypeContext
import com.argondesign.alogic.antlr.AlogicParser.ExprUnaryContext
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext

object ExprBuilder extends BaseBuilder[ExprContext, Expr] {

  private val baseMap = Map('b' -> 2, 'd' -> 10, 'h' -> 16)

  private def ticknum2SignValue(ticknum: String): (Boolean, BigInt) = {
    val signed = ticknum(1) == 's'
    val rest = if (signed) ticknum.drop(2) else ticknum.drop(1)
    val base = baseMap(rest.head)
    val digits = rest.tail filter (_ != '_')
    (signed, BigInt(digits, base))
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
        ExprBinary(visit(ctx.expr(0)), ctx.op, visit(ctx.expr(1))) withLoc ctx.loc
      }
      override def visitExprTernary(ctx: ExprTernaryContext) = {
        ExprTernary(visit(ctx.expr(0)), visit(ctx.expr(1)), visit(ctx.expr(2))) withLoc ctx.loc
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
        ExprSelect(visit(ctx.expr), ctx.IDENTIFIER) withLoc ctx.loc
      }

      // Builtins
      override def visitExprAtCall(ctx: ExprAtCallContext) = {
        ExprAtCall(ctx.ATID.text.tail, this(ctx.commaexpr)) withLoc ctx.loc
      }
      override def visitExprDollarCall(ctx: ExprDollarCallContext) = {
        ExprDollarCall(ctx.DOLLARID.text.tail, this(ctx.commaexpr)) withLoc ctx.loc
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
      override def visitExprConst(ctx: ExprConstContext) = {
        val value = BigInt(ctx.CONSTANT.text filter (_ != '_'))
        ExprNum(true, value) withLoc ctx.loc
      }
      override def visitExprTickNum(ctx: ExprTickNumContext) = {
        val (sign, value) = ticknum2SignValue(ctx.TICKNUM)
        ExprNum(sign, value) withLoc ctx.loc
      }
      override def visitExprConstTickNum(ctx: ExprConstTickNumContext) = {
        val (sign, value) = ticknum2SignValue(ctx.TICKNUM)
        val width = (ctx.CONSTANT.text filter (_ != '_')).toInt
        ExprInt(sign, width, value) withLoc ctx.loc
      }

      // Identifier
      override def visitExprIdent(ctx: ExprIdentContext) = {
        ExprRef(ctx.IDENTIFIER.toIdent) withLoc ctx.loc
      }

      // Type
      override def visitExprType(ctx: ExprTypeContext) = {
        ExprType(TypeBuilder(ctx.kind)) withLoc ctx.loc
      }
    }

    Visitor(ctx)
  }

}
