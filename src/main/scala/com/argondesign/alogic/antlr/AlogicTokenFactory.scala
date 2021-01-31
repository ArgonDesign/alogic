////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Token factory used by the lexer. Also implements #line.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.util.unreachable
import org.antlr.v4.runtime.CharStream
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.TokenFactory
import org.antlr.v4.runtime.TokenSource

import scala.util.ChainingSyntax

class AlogicTokenFactory(val alogicSource: Source)(implicit mb: MessageBuffer)
    extends TokenFactory //[AlogicToken]
    with ChainingSyntax {

  // Used to implement #line
  private var fileName: String = alogicSource.path
  private var lineOffset: Int = 0

  // Need to keep the original filename as well
  private val trueFileName: String = fileName

  // Preprocessor state (only implements #line)
  sealed private trait PPState
  private case object PPNormal extends PPState
  private case object PPExpectLineNumber extends PPState
  private case object PPExpectLineName extends PPState
  private case object PPSkipLine extends PPState

  private var ppState: PPState = PPNormal

  private def adjustLine(line: Int): Int = line - lineOffset

  private def eol(kind: Int): Boolean = kind == AlogicLexer.NL

  var hadError: Boolean = false

  private def error(token: AlogicToken, msg: String): Unit = {
    hadError = true
    mb.error(token.loc, msg)
  }

  // New line offset and file name only committed at end of line
  private var newLineOffset: Int = _
  private var newFileName: String = _

  def create(
      source: org.antlr.v4.runtime.misc.Tuple2[_ <: TokenSource, CharStream],
      kind: Int,
      text: String,
      channel: Int,
      start: Int,
      stop: Int,
      line: Int,
      charPositionInLine: Int
    ): Token = {
    require(channel == Token.DEFAULT_CHANNEL || channel == Token.HIDDEN_CHANNEL)
    require(source.getItem1.isInstanceOf[AlogicLexer])

    def mkToken(channel: Int): AlogicToken = {
      val token =
        new AlogicToken(source, kind, channel, alogicSource, start, stop, fileName, trueFileName)
      token.setLine(adjustLine(line))
      token.setCharPositionInLine(charPositionInLine)
      if (text != null) {
        token.setText(text)
      }
      token
    }
    // Creates normal token passed to the parser
    def normalToken: AlogicToken = mkToken(Token.DEFAULT_CHANNEL)
    // Creates hidden token not passed to the parser
    def hiddenToken: AlogicToken = mkToken(Token.HIDDEN_CHANNEL)

    if (channel == Token.HIDDEN_CHANNEL) {
      // Hidden tokens, nothing special
      hiddenToken
    } else if (kind == Token.EOF) {
      // Pass through EOF
      normalToken
    } else {
      // #line state machine
      ppState match {
        case PPNormal =>
          if (kind == AlogicLexer.HASHLINE) {
            // '#line' token, expect line number
            ppState = PPExpectLineNumber
            hiddenToken
          } else if (eol(kind)) {
            // Normal mode, but hide newlines
            hiddenToken
          } else {
            // Normal mode
            normalToken
          }
        case PPExpectLineNumber =>
          hiddenToken tap { token =>
            if (kind == AlogicLexer.UNSIZEDINT && (token.getText forall { _.isDigit })) {
              // Line number given, expect optional file name
              ppState = PPExpectLineName
              // Set line offset such that the next line - lineOffset yields the
              // specified line number, i.e.: 'line + 1 - lineOffset == given'
              newLineOffset = line + 1 - token.getText.toInt
            } else {
              // Unexpected token, skip rest of line
              ppState = PPSkipLine
              error(token, "'#line' requires a positive decimal integer as first argument")
            }
          }
        case PPExpectLineName =>
          hiddenToken tap { token =>
            if (eol(kind)) {
              // End of line, return to normal mode, commit new line offset
              ppState = PPNormal
              lineOffset = newLineOffset
            } else if (kind == AlogicLexer.STRING) {
              // Filename given, skip rest of line
              ppState = PPSkipLine
              newFileName = hiddenToken.getText.tail.init
            } else {
              // Unexpected token, skip rest of line
              ppState = PPSkipLine
              error(token, "Second argument to '#line' must be a string literal")
            }
          }
        case PPSkipLine =>
          hiddenToken tap { token =>
            if (eol(kind)) {
              // End of line, return to normal mode, commit new line offset and file name
              ppState = PPNormal
              lineOffset = newLineOffset
              fileName = newFileName
            } else {
              // Stray input at end of line
              error(token, "Extraneous arguments to '#line'")
            }
          }
      }
    }
  }.ensuring(!eol(kind) || _.getChannel == Token.HIDDEN_CHANNEL, "Failed to hide newline")

  // This is never be used by Alogic but is part of the TokenFactory interface
  def create(kind: Int, text: String): AlogicToken = unreachable

}
