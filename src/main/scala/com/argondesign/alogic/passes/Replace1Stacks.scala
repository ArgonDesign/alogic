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

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._

import scala.collection.mutable

final class Replace1Stacks(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  // Map from original stack variable symbol to the corresponding replacement,
  private[this] val stackMap = mutable.LinkedHashMap[Symbol, Symbol]()

  private[this] var poppedStack: Option[Symbol] = None

  override def transform(tree: Tree): Tree = {
    val result: Tree = tree match {

      //////////////////////////////////////////////////////////////////////////
      // Replace the stack decl/defn with the decl/defn of the new symbol
      //////////////////////////////////////////////////////////////////////////

      case DeclStack(symbol, _, depth) if depth.value contains BigInt(1) =>
        val newSymbol = symbol.dup tap { _.kind = symbol.kind.asStack.kind }
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

      case StmtExpr(ExprCall(ExprSel(ExprSym(s), "push" | "set", _), List(ArgP(arg)))) =>
        stackMap.get(s) map { symbol =>
          StmtAssign(ExprSym(symbol), arg)
        } getOrElse tree

      //////////////////////////////////////////////////////////////////////////
      // Rewrite expressions
      //////////////////////////////////////////////////////////////////////////

      case ExprCall(ExprSel(ExprSym(s), "top", _), Nil) =>
        stackMap.get(s) map ExprSym getOrElse tree

      case ExprCall(ExprSel(ExprSym(s), "pop", _), Nil) =>
        stackMap.get(s) map { symbol =>
          poppedStack = Some(symbol)
          ExprSym(symbol)
        } getOrElse tree

      case stmt: Stmt if poppedStack.nonEmpty =>
        val symbol = poppedStack.get
        poppedStack = None
        Thicket(
          List(
            stmt,
            StmtAssign(ExprSym(symbol), ExprInt(symbol.kind.isSigned, symbol.kind.width.toInt, 0))
          )
        )

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

object Replace1Stacks extends EntityTransformerPass(declFirst = true) {
  val name = "replace-1-stacks"

  override protected def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
    new Replace1Stacks
}
