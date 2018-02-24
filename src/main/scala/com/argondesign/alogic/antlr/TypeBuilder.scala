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

import com.argondesign.alogic.antlr.AlogicParser.CommaexprContext
import com.argondesign.alogic.antlr.AlogicParser.KindContext
import com.argondesign.alogic.antlr.AlogicParser.TypeBoolContext
import com.argondesign.alogic.antlr.AlogicParser.TypeIdentContext
import com.argondesign.alogic.antlr.AlogicParser.TypeIntContext
import com.argondesign.alogic.antlr.AlogicParser.TypeIntVContext
import com.argondesign.alogic.antlr.AlogicParser.TypeUIntContext
import com.argondesign.alogic.antlr.AlogicParser.TypeUIntVContext
import com.argondesign.alogic.antlr.AlogicParser.TypeVoidContext
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees.Expr
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types.Type
import com.argondesign.alogic.core.Types.TypeInt
import com.argondesign.alogic.core.Types.TypeRef
import com.argondesign.alogic.core.Types.TypeVector
import com.argondesign.alogic.core.Types.TypeVoid

object TypeBuilder extends BaseBuilder[KindContext, Type] {

  def apply(ctx: KindContext)(implicit cc: CompilerContext): Type = {
    object Visitor extends AlogicScalarVisitor[Type] {
      def buildVector(signed: Boolean, ctx: CommaexprContext): Type = {
        val lastSize :: restSizes = ExprBuilder(ctx.expr)
        restSizes.foldLeft[Type](TypeInt(signed, lastSize)) { (elem, size) => TypeVector(elem, size) }
      }

      override def visitTypeBool(ctx: TypeBoolContext) = TypeInt(false, Expr(1))
      override def visitTypeInt(ctx: TypeIntContext) = TypeInt(true, Expr(ctx.INTTYPE.text.tail.toInt))
      override def visitTypeUInt(ctx: TypeUIntContext) = TypeInt(false, Expr(ctx.UINTTYPE.text.tail.toInt))
      override def visitTypeIntV(ctx: TypeIntVContext) = buildVector(signed = true, ctx.commaexpr)
      override def visitTypeUIntV(ctx: TypeUIntVContext) = buildVector(signed = false, ctx.commaexpr)
      override def visitTypeIdent(ctx: TypeIdentContext) = TypeRef(ctx.IDENTIFIER.toIdent)
      override def visitTypeVoid(ctx: TypeVoidContext) = TypeVoid
    }

    Visitor(ctx)
  }

}
