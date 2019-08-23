////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Replace stacks of depth 1 without accesses to empty/full with local flops
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._

import scala.collection.mutable

final class Replace1Stacks(implicit cc: CompilerContext) extends TreeTransformer {

  // Set of stack symbols to replace
  private[this] val stackSet = mutable.Set[TermSymbol]()

  override def skip(tree: Tree): Boolean = tree match {
    case entity: Entity => entity.declarations.isEmpty
    case _              => false
  }

  override def enter(tree: Tree): Unit = tree match {
    case Decl(symbol, _) =>
      symbol.kind match {
        case TypeStack(kind, depth) if depth.value contains BigInt(1) =>
          // TODO: iff no access to empty/full ports
          // Add to set of symbols to replace
          stackSet add symbol
          // Change type to element type
          symbol.kind = kind
        case _ =>
      }

    case _ =>
  }

  override def transform(tree: Tree): Tree = {
    val result: Tree = tree match {

      //////////////////////////////////////////////////////////////////////////
      // Rewrite statements
      //////////////////////////////////////////////////////////////////////////

      case StmtExpr(ExprCall(ExprSelect(ExprSym(symbol: TermSymbol), "push" | "set", _), args))
          if stackSet contains symbol => {
        StmtAssign(ExprSym(symbol), args.head)
      }

      //////////////////////////////////////////////////////////////////////////
      // Rewrite expressions
      //////////////////////////////////////////////////////////////////////////

      case ExprCall(ExprSelect(ExprSym(symbol: TermSymbol), "pop" | "top", _), Nil)
          if stackSet contains symbol => {
        ExprSym(symbol)
      }

      case ExprSelect(ExprSym(symbol: TermSymbol), "full" | "empty", _)
          if stackSet contains symbol => {
        cc.ice(tree, "Replacing 1 deep steck with full access")
      }

      case _ => tree
    }

    // If we did modify the node, regularize it
    if (result ne tree) {
      result regularize tree.loc
    }

    // Done
    result
  }

}

object Replace1Stacks extends TreeTransformerPass {
  val name = "replace-1-stacks"
  def create(implicit cc: CompilerContext) = new Replace1Stacks
}
