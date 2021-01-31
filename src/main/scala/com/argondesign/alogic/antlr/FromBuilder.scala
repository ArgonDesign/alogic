////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Build a From AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.SourceContext
import com.argondesign.alogic.antlr.AntlrConverters._

object FromBuilder extends BaseBuilder[FromContext, From] {

  def apply(ctx: FromContext)(implicit mb: MessageBuffer, sc: SourceContext): From = {
    object Visitor extends AlogicScalarVisitor[From] {
      override def visitFromOne(ctx: FromOneContext): From =
        FromOne(
          ctx.STRING.txt.slice(1, ctx.STRING.txt.length - 1),
          ExprBuilder(ctx.expr),
          Option.when(ctx.ident != null)(IdentBuilder(ctx.ident))
        ) withLoc ctx.loc.copy(point = ctx.STRING.loc.start)

      override def visitFromAll(ctx: FromAllContext): From =
        FromAll(
          ctx.STRING.txt.slice(1, ctx.STRING.txt.length - 1)
        ) withLoc ctx.loc.copy(point = ctx.STRING.loc.start)
    }

    Visitor(ctx)
  }

}
