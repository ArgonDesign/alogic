////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Build a Using AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.SourceContext

object UsingBuilder extends BaseBuilder[UsngContext, Using] {

  def apply(ctx: UsngContext)(implicit mb: MessageBuffer, sc: SourceContext): Using = {
    object Visitor extends AlogicScalarVisitor[Using] {
      override def visitUsingOne(ctx: UsingOneContext): Using =
        UsingOne(
          ExprBuilder(ctx.expr),
          Option.when(ctx.ident != null)(IdentBuilder(ctx.ident))
        ) withLoc ctx.loc

      override def visitUsingAll(ctx: UsingAllContext): Using =
        UsingAll(ExprBuilder(ctx.expr), exprt = false) withLoc ctx.loc
    }

    Visitor(ctx)
  }

}
