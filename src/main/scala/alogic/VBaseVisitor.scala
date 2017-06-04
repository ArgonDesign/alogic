package alogic

import org.antlr.v4.runtime.tree.RuleNode
import alogic.antlr.VParserBaseVisitor
import org.antlr.v4.runtime.tree.ParseTree

import Antlr4Conversions._

class VBaseVisitor[T] extends VParserBaseVisitor[T] {
  override def visit(tree: ParseTree): T = {
    if (null == tree) defaultResult else super.visit(tree)
  } ensuring (null != _)

  def apply[U <: RuleNode](ctx: U): T = visit(ctx)

  def apply[U <: RuleNode](ctxList: List[U]): List[T] = ctxList.map(apply(_))

  def apply[U <: RuleNode](ctxList: java.util.List[U]): List[T] = apply(ctxList.toList)

  def apply[U <: RuleNode](ctxOpt: Option[U]): Option[T] = ctxOpt.map(apply(_))

  def visit[U <: RuleNode](ctxList: List[U]): List[T] = apply(ctxList)

  def visit[U <: RuleNode](ctxList: java.util.List[U]): List[T] = apply(ctxList)

  def visit[U <: RuleNode](ctxOpt: Option[U]): Option[T] = apply(ctxOpt)
}
