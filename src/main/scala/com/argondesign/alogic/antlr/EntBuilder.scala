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
// Build a List[Ent] AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FuncVariant

object EntBuilder extends BaseBuilder[EntContext, Ent] {
  def apply(ctx: EntContext)(implicit cc: CompilerContext): Ent = {
    object Visitor extends AlogicScalarVisitor[Ent] {
      override def visitEntDesc(ctx: EntDescContext): Ent = {
        val desc = DescBuilder(ctx.desc) match {
          case func: DescFunc =>
            func.copy(variant = FuncVariant.Ctrl) withLoc func.loc
          case other => other
        }
        EntDesc(desc) withLoc ctx.loc
      }

      override def visitEntGen(ctx: EntGenContext): Ent =
        EntGen(GenBuilder(ctx.gen)) withLoc ctx.loc

      override def visitEntConnect(ctx: EntConnectContext): Ent =
        EntConnect(ExprBuilder(ctx.lhs), ExprBuilder(ctx.rhs)) withLoc ctx.loc

      override def visitEntFenceBlock(ctx: EntFenceBlockContext): Ent =
        EntCombProcess(StmtBuilder(ctx.stmt)) withLoc ctx.loc

      override def visitEntAssertion(ctx: EntAssertionContext): Ent =
        EntAssertion(AssertionBuilder(ctx.assertion)) withLoc ctx.loc

      override def visitEntVerbatimBlock(ctx: EntVerbatimBlockContext): Ent =
        EntVerbatim(ctx.IDENTIFIER, ctx.VERBATIM_BODY.text.tail.init) withLoc ctx.loc
    }

    Visitor(ctx)
  }
}
