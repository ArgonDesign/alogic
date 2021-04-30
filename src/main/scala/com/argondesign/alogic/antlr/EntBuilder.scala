////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Build a List[Ent] AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.SourceContext

object EntBuilder extends BaseBuilder[EntContext, Ent] {

  def apply(ctx: EntContext)(implicit mb: MessageBuffer, sc: SourceContext): Ent = {
    object Visitor extends AlogicScalarVisitor[Ent] {
      override def visitEntDesc(ctx: EntDescContext): Ent =
        EntSplice(DescBuilder(ctx.desc)(mb, SourceContext.Entity)) withLoc ctx.loc

      override def visitEntImport(ctx: EntImportContext): Ent =
        EntSplice(ImportBuilder(ctx.imprt)) withLoc ctx.loc

      override def visitEntUsing(ctx: EntUsingContext): Ent =
        EntSplice(UsingBuilder(ctx.usng)) withLoc ctx.loc

      override def visitEntFrom(ctx: EntFromContext): Ent =
        EntSplice(FromBuilder(ctx.from)) withLoc ctx.loc

      override def visitEntAssertion(ctx: EntAssertionContext): Ent =
        EntSplice(AssertionBuilder(ctx.assertion)) withLoc ctx.loc

      override def visitEntConnect(ctx: EntConnectContext): Ent = {
        val loc = ctx.loc.copy(point = ctx.point.getStartIndex)
        EntConnect(ExprBuilder(ctx.lhs), ExprBuilder(ctx.rhs)) withLoc loc
      }

      override def visitEntConnectInputs(ctx: EntConnectInputsContext): Ent = {
        val loc = ctx.loc.copy(point = ctx.point.getStartIndex)
        EntConnectInputs(ExprBuilder(ctx.expr)) withLoc loc
      }

      override def visitEntFenceBlock(ctx: EntFenceBlockContext): Ent =
        EntCombProcess(StmtBuilder(ctx.stmt)) withLoc ctx.loc

      override def visitEntVerbatimBlock(ctx: EntVerbatimBlockContext): Ent =
        EntVerbatim(ctx.IDENTIFIER.txt, ctx.VERBATIM_BODY.txt.tail.init) withLoc ctx.loc
    }

    Visitor(ctx)
  }

}
