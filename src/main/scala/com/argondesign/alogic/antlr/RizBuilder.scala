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

object RizBuilder extends BaseBuilder[RizContext, Riz] {
  def apply(ctx: RizContext)(implicit cc: CompilerContext): Riz = {
    object Visitor extends AlogicScalarVisitor[Riz] {
      override def visitRizDesc(ctx: RizDescContext): Riz = {
        val decl = DescBuilder(ctx.desc) match {
          case func: DescFunc =>
            func.copy(variant = FuncVariant.Comb) withLoc func.loc
          case other => other
        }
        RizDesc(decl) withLoc ctx.loc
      }
    }

    Visitor(ctx)
  }
}
