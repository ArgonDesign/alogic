////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Build an Ident AST from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicParser.IdentContext
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.SourceContext
import org.antlr.v4.runtime.Token

object IdentBuilder extends BaseBuilder[IdentContext, Ident] {
  def apply(ctx: IdentContext)(implicit mb: MessageBuffer, sc: SourceContext): Ident =
    Ident(ctx.IDENTIFIER.txt, ExprBuilder(ctx.expr)) withLoc ctx.loc

  def apply(token: Token): Ident =
    Ident(token.txt, Nil) withLoc token.loc
}
