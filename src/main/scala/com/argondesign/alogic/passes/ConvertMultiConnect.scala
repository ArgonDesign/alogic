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
// Rewrite Connect instances with multiple right hand sides as multiple connect
// expressions
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext

final class ConvertMultiConnect(implicit cc: CompilerContext) extends TreeTransformer {

  override def skip(tree: Tree): Boolean = tree match {
    case _: Decl => true
    case _: Expr => true
    case _: Stmt => true
    case _       => false
  }

  override def transform(tree: Tree): Tree = tree match {
    case EntConnect(lhs, rhss) if rhss.length > 1 =>
      Thicket {
        for (rhs <- rhss) yield {
          EntConnect(lhs, List(rhs))
        }
      } regularize tree.loc

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    tree visit {
      case node @ EntConnect(_, rhss) if rhss.length > 1 =>
        cc.ice(node, "Connect with multiple rhs remains")
    }
  }

}

object ConvertMultiConnect extends PairTransformerPass {
  val name = "convert-multi-connect"
  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) = {
    val transformer = new ConvertMultiConnect
    (transformer(decl), transformer(defn))
  }
}
