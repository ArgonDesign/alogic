////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Build a Connect AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser.ConnectContext
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext

object ConnectBuilder extends BaseBuilder[ConnectContext, Connect] {

  def apply(ctx: ConnectContext)(implicit cc: CompilerContext): Connect = {
    object Visitor extends AlogicScalarVisitor[Connect] {
      override def visitConnect(ctx: ConnectContext) = {
        val lhs = ExprBuilder(ctx.lhs)
        val rhss = ExprBuilder(ctx.rhs)
        Connect(lhs, rhss) withLoc ctx.loc
      }
    }

    Visitor(ctx)
  }

}
