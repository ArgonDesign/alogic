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
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.FollowedBy

import scala.collection.mutable

final class Replace1Stacks(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy {

  // Set of stack symbols to replace
  private[this] val stackSet = mutable.Set[TermSymbol]()

  override def enter(tree: Tree): Unit = tree match {
    case Decl(Sym(symbol: TermSymbol), TypeStack(kind, depth), _)
        if depth.value contains BigInt(1) => {
      // TODO: iff no access to empty/full ports
      // Add to set of symbols to replace
      stackSet add symbol
      // Change type to element type
      symbol withDenot symbol.denot.copy(kind = kind)
    }

    case _ =>
  }

  override def transform(tree: Tree): Tree = {
    // Nodes with children that have been rewritten need their types assigned
    if (!tree.hasTpe) {
      TypeAssigner(tree)
    }

    val result: Tree = tree match {

      //////////////////////////////////////////////////////////////////////////
      // Rewrite statements
      //////////////////////////////////////////////////////////////////////////

      case StmtExpr(ExprCall(ExprSelect(ExprRef(Sym(symbol: TermSymbol)), "push" | "set"), args))
          if stackSet contains symbol => {
        StmtAssign(ExprRef(Sym(symbol)), args.head)
      }

      //////////////////////////////////////////////////////////////////////////
      // Rewrite expressions
      //////////////////////////////////////////////////////////////////////////

      case ExprCall(ExprSelect(ExprRef(Sym(symbol: TermSymbol)), "pop" | "top"), Nil)
          if stackSet contains symbol => {
        ExprRef(Sym(symbol))
      }

      case ExprSelect(ExprRef(Sym(symbol: TermSymbol)), "full" | "empty")
          if stackSet contains symbol => {
        cc.ice(tree, "Replacing 1 deep steck with full access")
      }

      //////////////////////////////////////////////////////////////////////////
      // Update declaration type
      //////////////////////////////////////////////////////////////////////////

      case decl @ Decl(Sym(symbol: TermSymbol), _, _) if stackSet contains symbol => {
        decl.copy(kind = symbol.denot.kind)
      }

      case _ => tree
    }

    // If we did modify the node, regularize it
    if (result != tree) {
      result regularize tree.loc
    }

    // Done
    result
  }

  override def finalCheck(tree: Tree): Unit = {
    tree visit {
      case node: Tree if !node.hasTpe => cc.ice(node, "Lost node.tpe", node.toString)
    }
  }

}
