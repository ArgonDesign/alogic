////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AlogicLexer._
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.util.unreachable
import org.antlr.v4.runtime.BaseErrorListener
import org.antlr.v4.runtime.RecognitionException
import org.antlr.v4.runtime.Recognizer
import org.antlr.v4.runtime.Token

class AlogicParseErrorListener(implicit mb: MessageBuffer) extends BaseErrorListener {

  override def syntaxError[T <: Token](
      recogniser: Recognizer[T, _],
      offendingSymbol: T,
      line: Int,
      charPositionInLine: Int,
      defaultMessage: String,
      e: RecognitionException
    ): Unit = {
    val message = recogniser match {
      case parser: AlogicParser =>
        val tokenStream = parser.getInputStream
        if (tokenStream.LT(-2) != null) {
          List(-2, -1, 1) map tokenStream.LA match {
            case List(`LET`, `LEFTBRACKET`, `RIGHTBRACKET`)   => "empty 'let ()' statement"
            case List(`WHILE`, `LEFTBRACKET`, `RIGHTBRACKET`) => "empty 'while ()' condition"
            case _                                            => defaultMessage
          }
        } else {
          defaultMessage
        }
      case _ => unreachable
    }

    val loc = offendingSymbol match {
      case token: AlogicToken => token.loc
      case _                  => unreachable
    }

    mb.error(loc, s"Syntax error: $message")
  }

}
