package alogic

import scala.collection.convert.WrapAsJava
import scala.collection.convert.WrapAsScala
import scala.language.implicitConversions

import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.misc.Interval
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.tree.TerminalNode
import org.antlr.v4.runtime.tree.Tree

object Antlr4Conversions extends WrapAsScala with WrapAsJava {
  implicit class ParserRuleContextWrapper(val ctx: ParserRuleContext) extends AnyVal {
    def sourceText: String = {
      val inputStream = ctx.start.getInputStream
      val startIdx = ctx.start.getStartIndex
      val stopIdx = ctx.stop.getStopIndex

      val leadingLen = ctx.start.getCharPositionInLine
      val trailingLen = inputStream.getText(Interval.of(stopIdx + 1, stopIdx + 200)).takeWhile(_ != '\n').length

      val filler = "\u2591"

      val leading = filler * leadingLen
      val source = inputStream.getText(Interval.of(startIdx, stopIdx))
      val trailing = filler * trailingLen

      leading + source + trailing
    }

    def text = ctx.getText

    def loc = ctx.start.loc
  }

  implicit class TokenWrapper(val token: Token) extends AnyVal {
    def loc = Loc(token.getTokenSource.getSourceName, token.getLine)

    def text = token.getText

    def index = token.getTokenIndex

    def isHidden = token.getChannel != Token.DEFAULT_CHANNEL
  }

  implicit class ParseTreeWrapper(val node: ParseTree) extends AnyVal {
    def text = node.getText

    def children: List[ParseTree] = {
      for (n <- 0 to node.getChildCount - 1)
        yield node.getChild(n)
    }.toList
  }

  implicit class TreeWrapper(val node: Tree) extends AnyVal {
    private[this] def descendantOf(self: Tree, that: Tree): Boolean = {
      var parent = self.getParent
      while (parent != null) {
        if (parent == that) return true
        parent = parent.getParent
      }
      false
    }

    def -<-(that: Tree): Boolean = descendantOf(node, that)
    def !<-(that: Tree): Boolean = !descendantOf(node, that)
    def ->-(that: Tree): Boolean = descendantOf(that, node)
    def !>-(that: Tree): Boolean = !descendantOf(that, node)
  }

  implicit def terminalNodeToString(node: TerminalNode): String = node.text
  implicit def terminalNodeToToken(node: TerminalNode): Token = node.getSymbol
  implicit def terminalNodeToTokenWrapper(node: TerminalNode): TokenWrapper = new TokenWrapper(node.getSymbol)

  implicit def parserRuleContextToString(ctx: ParserRuleContext): String = ctx.text

  implicit def tokenToString(token: Token): String = token.text
}
