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
// Fold expressions inside types, so const references are removed
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.TreeInTypeTransformer
import com.argondesign.alogic.typer.TypeAssigner

final class FoldExprInTypes(implicit cc: CompilerContext) extends TreeTransformer {

  private[this] val foldExpr = new FoldExpr(foldRefs = true)
  private[this] object TypeFoldExpr extends TreeInTypeTransformer(foldExpr)

  override def enter(tree: Tree): Unit = tree match {
    case Decl(symbol, _) => {
      // Fold expressions within the type of the symbol
      symbol.kind = symbol.kind rewrite TypeFoldExpr
    }
    case _ =>
  }

  override def transform(tree: Tree): Tree = tree match {
    case ExprCast(kind, expr) => {
      TypeAssigner(ExprCast(kind rewrite TypeFoldExpr, expr) withLoc tree.loc)
    }
    case _: ExprType => ??? // Should be none left
    case _           => tree
  }

  override protected def finalCheck(tree: Tree): Unit = {
    tree visit {
      case decl @ Decl(symbol, _) => {
        symbol.kind visit {
          case ref @ ExprRef(cSymbol: TermSymbol) if cSymbol.kind.isConst =>
            cc.ice(ref, s"const reference remains in type of '${symbol.name}'", decl.toSource)
        }
      }
    }
  }
}

object FoldExprInTypes extends TreeTransformerPass {
  val name = "fold-expr-in-types"
  def create(implicit cc: CompilerContext) = new FoldExprInTypes
}
