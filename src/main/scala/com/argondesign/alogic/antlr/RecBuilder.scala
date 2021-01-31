////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Build a Rec AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.SourceContext

object RecBuilder extends BaseBuilder[RecContext, Rec] {

  def apply(ctx: RecContext)(implicit mb: MessageBuffer, sc: SourceContext): Rec = {
    object Visitor extends AlogicScalarVisitor[Rec] {
      override def visitRecDesc(ctx: RecDescContext): Rec =
        RecSplice(DescBuilder(ctx.desc)(mb, SourceContext.Record)) withLoc ctx.loc

      override def visitRecImport(ctx: RecImportContext): Rec =
        RecSplice(ImportBuilder(ctx.imprt)) withLoc ctx.loc

      override def visitRecUsing(ctx: RecUsingContext): Rec =
        RecSplice(UsingBuilder(ctx.usng)) withLoc ctx.loc

      override def visitRecFrom(ctx: RecFromContext): Rec =
        RecSplice(FromBuilder(ctx.from)) withLoc ctx.loc

      override def visitRecAssertion(ctx: RecAssertionContext): Rec =
        RecSplice(AssertionBuilder(ctx.assertion)) withLoc ctx.loc
    }

    Visitor(ctx)
  }

}
