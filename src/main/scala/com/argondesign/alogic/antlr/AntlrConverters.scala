////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.tree.TerminalNode

import scala.language.implicitConversions

// Implicit conversions for Antrl4 classes
object AntlrConverters extends {
  // All Token are AlogicToken (see AlogicTokenFactory)
  implicit def t2at(token: Token): AlogicToken = token.asInstanceOf[AlogicToken]

  // All ParserRuleContext are AlogicParserRuleContext (see contextSuperClass grammar option)
  implicit def prc2aprc(ctx: ParserRuleContext): AlogicParserRuleContext =
    ctx.asInstanceOf[AlogicParserRuleContext]

  // TerminalNode is just a parse tree node holding a Token
  implicit def tb2at(node: TerminalNode): AlogicToken = node.getSymbol.asInstanceOf[AlogicToken]
}
