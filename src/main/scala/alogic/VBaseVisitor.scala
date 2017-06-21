package alogic

import org.antlr.v4.runtime.tree.RuleNode
import alogic.antlr.VParserBaseVisitor
import org.antlr.v4.runtime.tree.ParseTree

import Antlr4Conversions._

abstract class VBaseVisitor[T, C] extends VParserBaseVisitor[T] {
  override def visit(tree: ParseTree): T = {
    if (null == tree) defaultResult else super.visit(tree)
  } ensuring (null != _)

  def visit[U <: RuleNode](ctxList: List[U]): C

  def visit[U <: RuleNode](ctxList: java.util.List[U]): C = apply(ctxList.toList)

  def visit[U <: RuleNode](ctxOpt: Option[U]): Option[T] = ctxOpt map visit

  def apply[U <: RuleNode](ctx: U): T = visit(ctx)

  def apply[U <: RuleNode](ctxList: List[U]): C = visit(ctxList)

  def apply[U <: RuleNode](ctxList: java.util.List[U]): C = visit(ctxList.toList)

  def apply[U <: RuleNode](ctxOpt: Option[U]): Option[T] = visit(ctxOpt)
}
