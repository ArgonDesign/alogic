////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018-2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Build a Desc AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.SourceContext
import com.argondesign.alogic.core.SourceAttribute

import scala.util.ChainingSyntax

object AttrBuilder extends BaseBuilder[AttrContext, (String, SourceAttribute)] with ChainingSyntax {

  def apply(
      ctx: AttrContext
    )(
      implicit
      cc: CompilerContext,
      sc: SourceContext
    ): (String, SourceAttribute) = {
    object Visitor extends AlogicScalarVisitor[(String, SourceAttribute)] {
      override def visitAttrFlag(ctx: AttrFlagContext): (String, SourceAttribute) =
        (ctx.IDENTIFIER.txt, SourceAttribute.Flag() withLoc ctx.loc)
      override def visitAttrExpr(ctx: AttrExprContext): (String, SourceAttribute) =
        (ctx.IDENTIFIER.txt, SourceAttribute.Expr(ExprBuilder(ctx.expr)) withLoc ctx.loc)
      override def visitAttrSlices(ctx: AttrSlicesContext): (String, SourceAttribute) =
        (ctx.IDENTIFIER.txt, SourceAttribute.Slices(SlicesBuilder(ctx.slices)) withLoc ctx.loc)
    }

    Visitor(ctx)
  }

}
