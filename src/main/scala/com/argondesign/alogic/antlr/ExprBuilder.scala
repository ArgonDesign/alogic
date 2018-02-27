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

import com.argondesign.alogic.antlr.AlogicParser.ConnectRefIdentContext
import com.argondesign.alogic.antlr.AlogicParser.ConnectRefSelectContext
import com.argondesign.alogic.antlr.AlogicParser.ExprAtCallContext
import com.argondesign.alogic.antlr.AlogicParser.ExprBinaryContext
import com.argondesign.alogic.antlr.AlogicParser.ExprBracketContext
import com.argondesign.alogic.antlr.AlogicParser.ExprCallContext
import com.argondesign.alogic.antlr.AlogicParser.ExprCatContext
import com.argondesign.alogic.antlr.AlogicParser.ExprConstContext
import com.argondesign.alogic.antlr.AlogicParser.ExprConstTickNumContext
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
import com.argondesign.alogic.antlr.AlogicParser.ExprUnaryContext
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees.Expr
import com.argondesign.alogic.ast.Trees.ExprAtCall
import com.argondesign.alogic.ast.Trees.ExprBinary
import com.argondesign.alogic.ast.Trees.ExprBracket
import com.argondesign.alogic.ast.Trees.ExprCall
import com.argondesign.alogic.ast.Trees.ExprCat
import com.argondesign.alogic.ast.Trees.ExprDollarCall
import com.argondesign.alogic.ast.Trees.ExprIndex
import com.argondesign.alogic.ast.Trees.ExprNum
import com.argondesign.alogic.ast.Trees.ExprRef
import com.argondesign.alogic.ast.Trees.ExprRep
import com.argondesign.alogic.ast.Trees.ExprSelect
import com.argondesign.alogic.ast.Trees.ExprSlice
import com.argondesign.alogic.ast.Trees.ExprStr
import com.argondesign.alogic.ast.Trees.ExprTernary
import com.argondesign.alogic.ast.Trees.ExprUnary
import com.argondesign.alogic.core.CompilerContext

import org.antlr.v4.runtime.ParserRuleContext

object ExprBuilder extends BaseBuilder[ParserRuleContext, Expr] {

  private val baseMap = Map('b' -> 2, 'd' -> 10, 'h' -> 16)

  private def const2Num(const: String) = ExprNum(true, None, BigInt(const filter (_ != '_')))

  private def tickNum2Num(width: Option[String], tickNum: String): ExprNum = {
    assert(tickNum(0) == '\'')
    val widthVal = width filter (_ != '_') map { _.toInt }
    val signed = tickNum(1) == 's'
    val baseChar = if (signed) tickNum(2) else tickNum(1)
    val base = baseMap(baseChar)
    val rest = if (signed) tickNum drop 3 else tickNum drop 2
    val digits = rest filter (_ != '_')
    val value = BigInt(digits, base)
    // TODO: check value fits in width
    ExprNum(signed, widthVal, value)
  }

  def apply(ctx: ParserRuleContext)(implicit cc: CompilerContext): Expr = {
    object Visitor extends AlogicScalarVisitor[Expr] {
      // TODO: remove
      override def visitExprBracket(ctx: ExprBracketContext) = {
        ExprBracket(visit(ctx.expr)) withLoc ctx.loc
      }

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
        ExprNum(false, Some(1), 1) withLoc ctx.loc
      }
      override def visitExprFalse(ctx: ExprFalseContext) = {
        ExprNum(false, Some(1), 0) withLoc ctx.loc
      }
      override def visitExprString(ctx: ExprStringContext) = {
        ExprStr(ctx.STRING.text.tail.init) withLoc ctx.loc
      }
      override def visitExprTickNum(ctx: ExprTickNumContext) = {
        tickNum2Num(None, ctx.TICKNUM) withLoc ctx.loc
      }
      override def visitExprConstTickNum(ctx: ExprConstTickNumContext) = {
        tickNum2Num(Some(ctx.CONSTANT), ctx.TICKNUM) withLoc ctx.loc
      }
      override def visitExprConst(ctx: ExprConstContext) = {
        const2Num(ctx.CONSTANT) withLoc ctx.loc
      }

      // Identifier
      override def visitExprIdent(ctx: ExprIdentContext) = {
        ExprRef(ctx.IDENTIFIER.toIdent) withLoc ctx.loc
      }

      // Connect refs
      override def visitConnectRefSelect(ctx: ConnectRefSelectContext) = {
        ExprSelect(visit(ctx.ref), ctx.IDENTIFIER) withLoc ctx.loc
      }

      override def visitConnectRefIdent(ctx: ConnectRefIdentContext) = {
        ExprRef(ctx.IDENTIFIER.toIdent) withLoc ctx.loc
      }
    }

    Visitor(ctx)
  }

}
