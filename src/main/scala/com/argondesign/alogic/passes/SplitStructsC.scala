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
// Split structures to constituent signals
//   - Remove struct ports
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.typer.TypeAssigner

final class SplitStructsC(implicit cc: CompilerContext) extends TreeTransformer {

  override def skip(tree: Tree): Boolean = tree match {
    case _: Entity => false
    case _         => true
  }

  override def transform(tree: Tree): Tree = tree match {
    //////////////////////////////////////////////////////////////////////////
    // Entity
    //////////////////////////////////////////////////////////////////////////

    case entity: Entity => {
      // Drop original port declarations
      val newBody = entity.body filterNot {
        case EntDecl(Decl(symbol, _)) => symbol.attr.fieldSymbols.isSet
        case _                        => false
      }

      TypeAssigner(entity.copy(body = newBody) withLoc tree.loc)
    } tap { result =>
      // Update type of entity to drop new ports.
      entitySymbol.kind = result.typeBasedOnContents
    }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    tree visit {
      case node @ Decl(symbol, _) if symbol.kind.deref.underlying.isStruct => {
        cc.ice(node, "Struct declaration remains")
      }
      case node: Tree if node.tpe.deref.underlying.isStruct => {
        cc.ice(node, "Tree of type struct remains", node.toString)
      }
    }
  }

}

object SplitStructsC extends TreeTransformerPass {
  val name = "split-structs-c"
  def create(implicit cc: CompilerContext) = new SplitStructsC
}
