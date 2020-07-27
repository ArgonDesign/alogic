////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Build an Assertion AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.SourceContext

object AssertionBuilder extends BaseBuilder[AssertionContext, Assertion] {

  def apply(
      ctx: AssertionContext
    )(
      implicit
      mb: MessageBuffer,
      sc: SourceContext
    ): Assertion = {
    object Visitor extends AlogicScalarVisitor[Assertion] {
      override def visitAssertionAssert(ctx: AssertionAssertContext): Assertion = {
        val msgOpt = Option.when(ctx.STRING != null) {
          ctx.STRING.txt.slice(1, ctx.STRING.txt.length - 1)
        }
        AssertionAssert(ExprBuilder(ctx.expr), msgOpt) withLoc ctx.loc
      }

      override def visitAssertionStatic(ctx: AssertionStaticContext): Assertion = {
        val msgOpt = Option.when(ctx.STRING != null) {
          ctx.STRING.txt.slice(1, ctx.STRING.txt.length - 1)
        }
        AssertionStatic(ExprBuilder(ctx.expr), msgOpt) withLoc ctx.loc
      }
    }

    Visitor(ctx)
  }

}
