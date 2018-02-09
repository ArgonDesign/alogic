////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017 Argon Design Ltd. All rights reserved.
//
// Module : Scala Alogic Compiler
// Author : Peter de Rivaz/Geza Lore
//
// DESCRIPTION:
//
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic

import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.tree.RuleNode
import org.antlr.v4.runtime.tree.TerminalNode

class VCollectVisitor[T](initialResult: T, combine: (T, T) => T) extends VBaseVisitor[T, T, T] {

  override def defaultResult = unreachable
  override def aggregateResult(a: T, b: T) = unreachable

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
