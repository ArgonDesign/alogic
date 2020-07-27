////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Build an Attr AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.SourceContext

import scala.util.ChainingSyntax

object AttrBuilder extends BaseBuilder[AttrContext, Attr] with ChainingSyntax {

  def apply(
      ctx: AttrContext
    )(
      implicit
      mb: MessageBuffer,
      sc: SourceContext
    ): Attr = {
    object Visitor extends AlogicScalarVisitor[Attr] {
      override def visitAttrBool(ctx: AttrBoolContext): Attr =
        AttrBool(ctx.IDENTIFIER.txt) withLoc ctx.loc
      override def visitAttrExpr(ctx: AttrExprContext): Attr =
        AttrExpr(ctx.IDENTIFIER.txt, ExprBuilder(ctx.expr)) withLoc ctx.loc
    }

    Visitor(ctx)
  }

}
