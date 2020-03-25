////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Build a Rec AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FuncVariant

object RecBuilder extends BaseBuilder[RecContext, Rec] {
  def apply(ctx: RecContext)(implicit cc: CompilerContext): Rec = {
    object Visitor extends AlogicScalarVisitor[Rec] {
      override def visitRecDesc(ctx: RecDescContext): Rec = {
        val desc = DescBuilder(ctx.desc) match {
          case func: DescFunc if func.variant == FuncVariant.None =>
            func.copy(variant = FuncVariant.Comb) withLoc func.loc
          case other => other
        }
        RecDesc(desc) withLoc ctx.loc
      }

      override def visitRecGen(ctx: RecGenContext): Rec =
        RecGen(GenBuilder(ctx.gen)) withLoc ctx.loc

      override def visitRecAssertion(ctx: RecAssertionContext): Rec =
        RecAssertion(AssertionBuilder(ctx.assertion)) withLoc ctx.loc
    }

    Visitor(ctx)
  }
}
