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
abstract class AlogicBaseVisitor[T, L] extends AlogicParserBaseVisitor[T] {

  final override def visit(tree: ParseTree): T = {
    if (null == tree) defaultResult else super.visit(tree)
  } ensuring { _ != null }

  def visit[U <: RuleNode](ctxList: List[U]): L

  final def visit[U <: RuleNode](ctxList: java.util.List[U]): L = visit(ctxList.asScala.toList)

  final def apply[U <: RuleNode](ctx: U): T = visit(ctx)

  final def apply[U <: RuleNode](ctxList: java.util.List[U]): L = visit(ctxList)
}
