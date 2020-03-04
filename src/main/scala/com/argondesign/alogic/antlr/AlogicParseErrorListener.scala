////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogicr)

// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Error handler for Antlr4 generated Alogic language parser
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.core.CompilerContext

import org.antlr.v4.runtime.RecognitionException
import org.antlr.v4.runtime.Recognizer

import com.argondesign.alogic.antlr.AlogicLexer._

class AlogicParseErrorListener(implicit cc: CompilerContext) extends ParseErrorListener {
  override def syntaxError(
      recogniser: Recognizer[_, _],
      offendingSymbol: Object,
      line: Int,
      charPositionInLine: Int,
      defaultMessage: String,
      e: RecognitionException
  ): Unit = {
    val parser = recogniser.asInstanceOf[AlogicParser]

    val tokenStream = parser.getInputStream

    parser.getTokenFactory

    val message = if (tokenStream.LT(-2) != null) {
      List(-2, -1, 1) map tokenStream.LA match {
        case List(`LET`, `LEFTBRACKET`, `RIGHTBRACKET`)   => "empty 'let ()' statement"
        case List(`WHILE`, `LEFTBRACKET`, `RIGHTBRACKET`) => "empty 'while ()' condition"
        case List(`NEW`, `IDENTIFIER`, token) if token != LEFTBRACKET => {
          s"missing parameter list '()' after entity name in instantiation 'new ${tokenStream.LT(-1).getText}'"
        }
        case _ => defaultMessage
      }
    } else {
      defaultMessage
    }

    super.syntaxError(recogniser, offendingSymbol, line, charPositionInLine, message, e)
  }
}
