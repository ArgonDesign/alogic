////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Adapter to ease use of Antlr generated visitors with collections and options
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.tree.RuleNode

import scala.jdk.CollectionConverters._

// T: result of application to single node
// L: result of application to list of nodes
// O: result of application to Option[node]
abstract class AlogicBaseVisitor[T, L, O] extends AlogicParserBaseVisitor[T] {

  // scalastyle:off null
  override def visit(tree: ParseTree): T = {
    if (null == tree) defaultResult else super.visit(tree)
  } ensuring {
    null != _
  }
  // scalastyle:on

  def visit[U <: RuleNode](ctxList: List[U]): L

  def visit[U <: RuleNode](ctxOpt: Option[U]): O

  def visit[U <: RuleNode](ctxList: java.util.List[U]): L = visit(ctxList.asScala.toList)

  def apply[U <: RuleNode](ctx: U): T = visit(ctx)

  def apply[U <: RuleNode](ctxList: List[U]): L = visit(ctxList)

  def apply[U <: RuleNode](ctxOpt: Option[U]): O = visit(ctxOpt)

  def apply[U <: RuleNode](ctxList: java.util.List[U]): L = visit(ctxList)
}
