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
import com.argondesign.alogic.typer.TypeAssigner

final class ReplaceUnaryTicks(implicit cc: CompilerContext) extends TreeTransformer {

  override protected def enter(tree: Tree): Option[Tree] = tree match {
    case expr @ ExprUnary("'", op) =>
      Some {
        TypeAssigner(ExprCast(expr.tpe.asFund, walk(op).asInstanceOf[Expr]) withLoc tree.loc)
      }

    case _ => None
  }

  override protected def finalCheck(tree: Tree): Unit = {
    assert(tree forallAll { case node: Tree => node.hasTpe })
    assert((tree collect { case node @ ExprUnary("'", _) => node }).isEmpty)
  }
}

object ReplaceUnaryTicks extends PairTransformerPass {
  val name = "replace-unary-ticks"
  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) = {
    val transform = new ReplaceUnaryTicks
    // The decl should contain no ticks
    (decl, transform(defn))
  }
}
