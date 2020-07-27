////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Build an Import AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.SourceContext

object FromBuilder extends BaseBuilder[FromContext, From] {

  def apply(ctx: FromContext)(implicit mb: MessageBuffer, sc: SourceContext): From = {
    object Visitor extends AlogicScalarVisitor[From] {
      override def visitFromOne(ctx: FromOneContext): From =
        FromOne(
          if (ctx.relative == null) 0 else ctx.relative.size,
          ExprBuilder(ctx.expr(0)),
          ExprBuilder(ctx.expr(1)),
          Option.when(ctx.ident != null)(IdentBuilder(ctx.ident))
        ) withLoc ctx.loc

      override def visitFromAll(ctx: FromAllContext): From =
        FromAll(
          if (ctx.relative == null) 0 else ctx.relative.size,
          ExprBuilder(ctx.expr)
        ) withLoc ctx.loc
    }

    Visitor(ctx)
  }

}
