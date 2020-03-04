////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
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
// Automatic/convenience converters for Antrl4 classes
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.core.Loc
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.misc.Interval
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.tree.TerminalNode

import scala.language.implicitConversions

object AntlrConverters extends {
  implicit class RichParserRuleContext(val ctx: ParserRuleContext) extends AnyVal {

    def sourceText: String = {
      val inputStream = ctx.start.getInputStream
      val startIdx = ctx.start.getStartIndex
      val stopIdx = ctx.stop.getStopIndex

      val leadingLen = ctx.start.getCharPositionInLine
      val trailingLen =
        inputStream.getText(Interval.of(stopIdx + 1, stopIdx + 200)).takeWhile(_ != '\n').length

      val filler = "_"

      val leading = filler * leadingLen
      val source = inputStream.getText(Interval.of(startIdx, stopIdx))
      val trailing = filler * trailingLen

      leading + source + trailing
    }

    def text: String = ctx.getText

    def loc: Loc = {
      val sLoc = ctx.start.loc
      val eLoc = ctx.stop.loc
      Loc(sLoc.source, sLoc.start, eLoc.end, sLoc.point)
    }
  }

  implicit class RichToken(val token: Token) extends AnyVal {

    def loc: Loc = {
      val source = token.getTokenSource.asInstanceOf[SourceMixin].source
      Loc(source, token.getStartIndex, token.getStopIndex + 1, token.getStartIndex)
    }

    def text: String = token.getText

    def index: Int = token.getTokenIndex

    def isHidden: Boolean = token.getChannel != Token.DEFAULT_CHANNEL
  }

  implicit class RichParseTree(val node: ParseTree) extends AnyVal {
    def text: String = node.getText

    def children: List[ParseTree] = {
      for (n <- 0 until node.getChildCount)
        yield node.getChild(n)
    }.toList
  }

  implicit def terminalNodeToString(node: TerminalNode): String = node.text
  implicit def terminalNodeToToken(node: TerminalNode): Token = node.getSymbol
  implicit def terminalNodeToRichToken(node: TerminalNode): RichToken =
    new RichToken(node.getSymbol)

  implicit def parserRuleContextToString(ctx: ParserRuleContext): String = ctx.text

  implicit def tokenToString(token: Token): String = token.text
}
