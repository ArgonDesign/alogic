////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Replace unary ticks with casts, this must be run straight after the typer
// as unary tick nodes cannot be typed out of context and hence cannot be
// re-written by typed tree transformers. This pass keeps track of the original
// types of unary ' nodes on a stack in order to handle expressions with nested
// ticks like 'a['i].
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types.Type
import com.argondesign.alogic.typer.TypeAssigner

import scala.collection.mutable

final class ReplaceUnaryTicks(implicit cc: CompilerContext) extends TreeTransformer {

  override val typed = false

  private val kindStack = mutable.Stack[Type]()

  override protected def enter(tree: Tree): Unit = tree match {
    case expr @ ExprUnary("'", _) => kindStack push expr.tpe
    case _                        =>
  }

  override def transform(tree: Tree): Tree = tree match {
    case ExprUnary("'", op)  => TypeAssigner(ExprCast(kindStack.pop(), op) withLoc tree.loc)
    case node if node.hasTpe => tree
    case node                => TypeAssigner(node)
  }

  override protected def finalCheck(tree: Tree): Unit = {
    assert(kindStack.isEmpty)
    assert(tree forallAll { case node: Tree                       => node.hasTpe })
    val unaryTicks = tree collect { case node @ ExprUnary("'", _) => node }
    assert(unaryTicks.isEmpty)
  }
}

object ReplaceUnaryTicks extends TreeTransformerPass {
  val name = "replace-unary-ticks"
  def create(implicit cc: CompilerContext) = new ReplaceUnaryTicks
}
