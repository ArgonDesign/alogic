////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017 Argon Design Ltd. All rights reserved.
//
// Module : Scala Alogic Compiler
// Author : Geza Lore
//
// DESCRIPTION:
//
// Visitor to convert left value parse trees to left value ASTs
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic.ast

import alogic.Antlr4Conversions._
import alogic.VScalarVisitor
import org.antlr.v4.runtime.ParserRuleContext
import alogic.antlr.VParser._
import alogic.Message

class LValVisitor(symtab: Option[Symtab], typedefs: scala.collection.Map[String, Type])
    extends VScalarVisitor[LVal] { self =>

  private[this] lazy val exprVisitor = new ExprVisitor(symtab, typedefs)

  override def visitLValIndex(ctx: LValIndexContext) = {
    val idx = exprVisitor(ctx.idx)
    val attr = Attr(ctx.loc)
    visit(ctx.ref) match {
      case x: LValName                       => LValArrayLookup(attr, x, idx :: Nil)
      case LValArrayLookup(a, name, indices) => LValArrayLookup(a, name, indices ::: idx :: Nil)
      case _                                 => Message.fatal(ctx, s"Cannot index lvalue '${ctx.ref.sourceText}'")
    }
  }

  override def visitLValSlice(ctx: LValSliceContext) = {
    val attr = Attr(ctx.loc)
    visit(ctx.ref) match {
      case x: LValName => LValSlice(attr, x, exprVisitor(ctx.lidx), ctx.op, exprVisitor(ctx.ridx))
      case _           => Message.fatal(ctx, s"Cannot slice lvalue '${ctx.ref.sourceText}'")
    }
  }

  override def visitLValDot(ctx: LValDotContext) = visit(ctx.ref) match {
    case LValName(a, names) => LValName(a, names ::: ctx.IDENTIFIER.text :: Nil)
    case _                  => Message.fatal(ctx, s"Cannot access member of '${ctx.ref.sourceText}'")
  }

  override def visitLValId(ctx: LValIdContext) = {
    val attr = Attr(ctx.loc)
    symtab match {
      case Some(st) => st(ctx, ctx.text) match {
        case Left(decl) => LValName(attr, decl.id :: Nil)
        case Right(id)  => LValName(attr, id :: Nil)
      }
      case None => unreachable
    }
  }

  override def visitLValCat(ctx: LValCatContext) = LValCat(Attr(ctx.loc), visit(ctx.lval))
}
