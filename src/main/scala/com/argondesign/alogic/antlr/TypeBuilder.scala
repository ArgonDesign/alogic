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
// Build a Type from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types._

object TypeBuilder extends BaseBuilder[KindContext, Type] {

  def apply(ctx: KindContext)(implicit cc: CompilerContext): Type = {
    object Visitor extends AlogicScalarVisitor[Type] {
      override def visitTypeBool(ctx: TypeBoolContext) = {
        TypeUInt(Expr(1) withLoc ctx.loc)
      }
      override def visitTypeInt(ctx: TypeIntContext) = {
        TypeSInt(Expr(ctx.INTTYPE.text.tail.toInt) withLoc ctx.loc)
      }
      override def visitTypeUInt(ctx: TypeUIntContext) = {
        TypeUInt(Expr(ctx.UINTTYPE.text.tail.toInt) withLoc ctx.loc)
      }
      override def visitTypeIntN(ctx: TypeIntNContext) = {
        TypeSInt(ExprBuilder(ctx.expr))
      }
      override def visitTypeUIntN(ctx: TypeUIntNContext) = {
        TypeUInt(ExprBuilder(ctx.expr))
      }
      override def visitTypeSNum(ctx: TypeSNumContext) = {
        TypeNum(true)
      }
      override def visitTypeUNum(ctx: TypeUNumContext) = {
        TypeNum(false)
      }
      override def visitTypeIdent(ctx: TypeIdentContext) = {
        TypeIdent(ctx.IDENTIFIER.toIdent)
      }
      override def visitTypeVoid(ctx: TypeVoidContext) = {
        TypeVoid
      }
      override def visitTypeVec(ctx: TypeVecContext): Type = {
        val elementType = visit(ctx.kind)
        val sizes = ExprBuilder(ctx.expr).reverse
        (elementType /: sizes) { TypeVector }
      }
    }

    Visitor(ctx)
  }

}
