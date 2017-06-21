package alogic

import org.antlr.v4.runtime.tree.RuleNode
import alogic.antlr.VParserBaseVisitor
import org.antlr.v4.runtime.tree.ParseTree

import Antlr4Conversions._

abstract class VBaseVisitor[T, L, O] extends VParserBaseVisitor[T] {
  override def visit(tree: ParseTree): T = {
    if (null == tree) defaultResult else super.visit(tree)
  } ensuring (null != _)

  def visit[U <: RuleNode](ctxList: List[U]): L

  def visit[U <: RuleNode](ctxOpt: Option[U]): O

  def visit[U <: RuleNode](ctxList: java.util.List[U]): L = apply(ctxList.toList)

  def apply[U <: RuleNode](ctx: U): T = visit(ctx)

  def apply[U <: RuleNode](ctxList: List[U]): L = visit(ctxList)

  def apply[U <: RuleNode](ctxOpt: Option[U]): O = visit(ctxOpt)

  def apply[U <: RuleNode](ctxList: java.util.List[U]): L = visit(ctxList.toList)

}
