package alogic

import org.antlr.v4.runtime.tree.RuleNode
import alogic.antlr.VParserBaseVisitor
import org.antlr.v4.runtime.tree.ParseTree

import Antlr4Conversions._

class VCollectVisitor[T](override val defaultResult: T, aggregate: (T, T) => T) extends VBaseVisitor[T, T] {
  override def aggregateResult(prev: T, next: T) = aggregate(prev, next)

  def visit[U <: RuleNode](ctxList: List[U]): T = {
    var result: T = defaultResult;
    for (ctx <- ctxList if shouldVisitNextChild(ctx, result)) {
      result = aggregateResult(result, visit(ctx));
    }
    result
  }
}
