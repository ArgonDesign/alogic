package alogic

import org.antlr.v4.runtime.tree.RuleNode
import alogic.antlr.VParserBaseVisitor
import org.antlr.v4.runtime.tree.ParseTree

import Antlr4Conversions._
import org.antlr.v4.runtime.tree.TerminalNode

class VCollectVisitor[T](initialResult: T, combine: (T, T) => T) extends VBaseVisitor[T, T, T] {

  override def defaultResult = Message.ice("unreachable")
  override def aggregateResult(a: T, b: T) = Message.ice("unreachable")

  var result = initialResult

  override def visit(tree: ParseTree): T = {
    result = tree.accept(this) match {
      case null => result
      case res  => combine(result, res)
    }
    result
  }

  override def visitChildren(node: RuleNode): T = {
    for (i <- 0 until node.getChildCount) {
      visit(node.getChild(i))
    }
    result
  }

  override def visitTerminal(node: TerminalNode) = result

  def visit[U <: RuleNode](ctxList: List[U]): T = {
    ctxList foreach visit
    result
  }

  override def visit[U <: RuleNode](ctxOpt: Option[U]): T = ctxOpt match {
    case Some(ctx) => visit(ctx)
    case None      => result
  }
}
