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
// The parser is generated using Antlr4, this is just a small wrapper to
// invoke it to build a parse tree.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.antlr.AlogicLexer
import com.argondesign.alogic.antlr.AlogicParser
import com.argondesign.alogic.antlr.ParserErrorListener
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Source

import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.ParserRuleContext

object Parser {
  def apply(source: Source)(implicit cc: CompilerContext): Option[ParserRuleContext] = {
    val inputStream = CharStreams.fromString(source.text, source.name)

    val lexer = new AlogicLexer(inputStream)
    val tokenStream = new CommonTokenStream(lexer)
    tokenStream.fill()

    val parser = new AlogicParser(tokenStream)
    parser.removeErrorListeners()
    parser.addErrorListener(new ParserErrorListener)

    val parseTree = parser.start()

    if (parser.getNumberOfSyntaxErrors != 0) None else Some(parseTree)
  }
}
