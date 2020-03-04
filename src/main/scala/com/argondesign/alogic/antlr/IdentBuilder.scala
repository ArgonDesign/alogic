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
// Build a Definition AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser.IdentContext
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import org.antlr.v4.runtime.Token

object IdentBuilder extends BaseBuilder[IdentContext, Ident] {
  def apply(ctx: IdentContext)(implicit cc: CompilerContext): Ident =
    Ident(ctx.IDENTIFIER.text, ExprBuilder(ctx.expr)) withLoc ctx.loc

  def apply(token: Token): Ident =
    Ident(token, Nil) withLoc token.loc
}
