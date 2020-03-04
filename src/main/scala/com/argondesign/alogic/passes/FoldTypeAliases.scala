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
// Remove all definitions with DescType
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types._

final class FoldTypeAliases(implicit cc: CompilerContext) extends TreeTransformer {

  override def transform(tree: Tree): Tree = tree match {

    ////////////////////////////////////////////////////////////////////////////
    // Fold references to type symbols (unless it's directly to the named type)
    ////////////////////////////////////////////////////////////////////////////

    case expr @ ExprSym(symbol) if symbol.kind.isType =>
      val newExpr = symbol.kind.asType.kind match {
        case kind: TypeInt           => ExprType(kind)
        case kind: TypeNum           => ExprType(kind)
        case kind: TypeVector        => ExprType(kind)
        case TypeVoid                => ExprType(TypeVoid)
        case TypeStr                 => ExprType(TypeStr)
        case TypeEntity(`symbol`, _) => expr
        case TypeRecord(`symbol`, _) => expr
        case TypeEntity(s, _)        => ExprSym(s)
        case TypeRecord(s, _)        => ExprSym(s)
      }
      newExpr regularize tree.loc

    ////////////////////////////////////////////////////////////////////////////
    // Drop type definitions
    ////////////////////////////////////////////////////////////////////////////

    case _: DeclType => Stump
    case _: DefnType => Stump

    ////////////////////////////////////////////////////////////////////////////
    // Otherwise
    ////////////////////////////////////////////////////////////////////////////

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    // Should have removed all StmtLet, StmtUpdate, StmtPost
    tree visit {
      case node: DeclType => cc.ice(node, s"DeclType remains")
    }
  }

}

object FoldTypeAliases extends PairTransformerPass {
  val name = "fold-type-aliases"
  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) = {
    val transformer = new FoldTypeAliases
    (transformer(decl), transformer(defn))
  }
}
