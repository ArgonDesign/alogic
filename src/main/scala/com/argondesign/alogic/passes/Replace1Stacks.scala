////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Replace stacks of depth 1 with a simple variable
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.TypeAssigner

import scala.collection.mutable

final class Replace1Stacks(implicit cc: CompilerContext) extends StatelessTreeTransformer {

  // Map from original stack variable symbol to the corresponding replacement,
  private val stackMap = mutable.LinkedHashMap[Symbol, Symbol]()

  override def transform(tree: Tree): Tree = {
    val result: Tree = tree match {

      //////////////////////////////////////////////////////////////////////////
      // Replace the stack decl/defn with the decl/defn of the new symbol
      //////////////////////////////////////////////////////////////////////////

      case DeclStack(symbol, _, 1) =>
        val newSymbol = symbol.dup tap { _.kind = symbol.kind.asStack.kind }
        stackMap(symbol) = newSymbol
        newSymbol.mkDecl

      case DefnStack(symbol) =>
        stackMap.get(symbol) map { _.mkDefn } getOrElse tree

      //////////////////////////////////////////////////////////////////////////
      // Replace push/pop statements
      //////////////////////////////////////////////////////////////////////////

      case StmtExpr(ExprCall(ExprSel(ExprSym(s), "push"), _)) =>
        stackMap.get(s) map { _ => Stump } getOrElse tree

      case StmtExpr(ExprCall(ExprSel(ExprSym(s), "pop"), _)) =>
        stackMap.get(s) map { symbol => TypeAssigner(ExprSym(symbol)) assign 0 } getOrElse tree

      //////////////////////////////////////////////////////////////////////////
      // Rewrite expressions
      //////////////////////////////////////////////////////////////////////////

      case ExprSel(ExprSym(s), "top") =>
        stackMap.get(s) map ExprSym.apply getOrElse tree

      case ExprSel(ExprSym(s), "old") =>
        stackMap.get(s) map { symbol => ExprOld(ExprSym(symbol)) } getOrElse tree

      //
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

object Replace1Stacks extends EntityTransformerPass(declFirst = true, parallel = true) {
  val name = "replace-1-stacks"

  override protected def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
    new Replace1Stacks
}
