////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017 Argon Design Ltd. All rights reserved.
//
// Module : Scala Alogic Compiler
// Author : Geza Lore
//
// DESCRIPTION:
//
// Visitor to convert type parse trees to type ASTs
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic.ast

import alogic.Antlr4Conversions._
import alogic.VScalarVisitor
import alogic.antlr.VParser._
import alogic.Message

class KnownTypeVisitor(symtab: Option[Symtab],
                       typedefs: scala.collection.Map[String, Type]) extends VScalarVisitor[Type] {
  override def visitBoolType(ctx: BoolTypeContext) = IntType(false, 1)
  override def visitIntType(ctx: IntTypeContext) = IntType(true, ctx.INTTYPE.text.tail.toInt)
  override def visitUintType(ctx: UintTypeContext) = IntType(false, ctx.UINTTYPE.text.tail.toInt)
  override def visitIntVType(ctx: IntVTypeContext) = {
    val visitor = new ExprVisitor(symtab, typedefs)
    IntVType(true, visitor(ctx.commaexpr))
  }
  override def visitUintVType(ctx: UintVTypeContext) = {
    val visitor = new ExprVisitor(symtab, typedefs)
    IntVType(false, visitor(ctx.commaexpr))
  }
  override def visitIdentifierType(ctx: IdentifierTypeContext) = {
    val s = ctx.IDENTIFIER.text
    typedefs.getOrElse(s, {
      Message.error(ctx, s"Unknown type '$s'")
      IntType(false, 1)
    })
  }
}
