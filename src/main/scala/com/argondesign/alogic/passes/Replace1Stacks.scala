////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018-2019 Argon Design Ltd. All rights reserved.
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

import scala.collection.mutable

final class Replace1Stacks(implicit cc: CompilerContext) extends TreeTransformer {

  // Map from original stack variable symbol to the corresponding replacement,
  private[this] val stackMap = mutable.LinkedHashMap[Symbol, Symbol]()

  override def transform(tree: Tree): Tree = {
    val result: Tree = tree match {

      //////////////////////////////////////////////////////////////////////////
      // Replace the stack decl/defn with the decl/defn of the new symbol
      //////////////////////////////////////////////////////////////////////////

      // TODO: iff no access to empty/full ports
      case DeclStack(symbol, _, depth) if depth.value contains BigInt(1) =>
        val newSymbol = cc.newSymbolLike(symbol) tap { _.kind = symbol.kind.asStack.kind }
        stackMap(symbol) = newSymbol
        newSymbol.mkDecl

      case DefnStack(symbol) =>
        stackMap.get(symbol) match {
          case None            => tree
          case Some(newSymbol) => newSymbol.mkDefn
        }

      //////////////////////////////////////////////////////////////////////////
      // Rewrite statements
      //////////////////////////////////////////////////////////////////////////

      case StmtExpr(ExprCall(ExprSelect(ExprSym(s), "push" | "set", _), List(ArgP(arg)))) =>
        stackMap.get(s) map { symbol =>
          StmtAssign(ExprSym(symbol), arg)
        } getOrElse tree

      //////////////////////////////////////////////////////////////////////////
      // Rewrite expressions
      //////////////////////////////////////////////////////////////////////////

      case ExprCall(ExprSelect(ExprSym(s), "pop" | "top", _), Nil) =>
        stackMap.get(s) map { symbol =>
          ExprSym(symbol)
        } getOrElse tree

      case ExprSelect(ExprSym(s), "full" | "empty", _) if stackMap contains s =>
        cc.ice(tree, "Replacing 1 deep stack with full access")

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

object Replace1Stacks extends PairTransformerPass {
  val name = "replace-1-stacks"
  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) = {
    (decl, defn) match {
      case (dcl: DeclEntity, _: DefnEntity) =>
        if (dcl.decls.isEmpty) {
          // If no decls, then there is nothing to do
          (decl, defn)
        } else {
          // Perform the transform
          val transformer = new Replace1Stacks
          // First transform the decl
          val newDecl = transformer(decl)
          // Then transform the defn
          val newDefn = transformer(defn)
          (newDecl, newDefn)
        }
      case _ => (decl, defn)
    }
  }
}
