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
// Build an Assert AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext

object AssertBuilder extends BaseBuilder[AssertionContext, Assert] {
  def apply(ctx: AssertionContext)(implicit cc: CompilerContext): Assert = {
    val msgOpt = Option.when(ctx.STRING != null) {
      ctx.STRING.text.slice(1, ctx.STRING.text.length - 1)
    }
    Assert(ExprBuilder(ctx.expr), msgOpt) withLoc ctx.loc
  }
}
