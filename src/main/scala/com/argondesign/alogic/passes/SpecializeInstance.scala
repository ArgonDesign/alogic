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
// Replace instantiations of parametrized entities with instantiations of the
// specialized entities
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._

final class SpecializeInstance(implicit cc: CompilerContext) extends TreeTransformer {

  override def transform(tree: Tree): Tree = tree match {
    case Instance(Sym(iSymbol: TermSymbol), Sym(eSymbol), _, _)
        if iSymbol.attr.paramBinding.isSet => {
      // Find the specialized entity
      val Sym(sSymbol) = {
        val sEntity = eSymbol.attr.specMap.value(iSymbol.attr.paramBinding.value)
        sEntity.ref
      }
      // Update type of instance
      iSymbol withDenot iSymbol.denot.copy(kind = sSymbol.denot.kind)
      // Remove attribute
      iSymbol.attr.paramBinding.clear
      // Rewrite tree
      Instance(Sym(iSymbol), Sym(sSymbol), Nil, Nil) regularize tree.loc
    }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    tree visit {
      case node @ Instance(_, _, n, e) if n.nonEmpty || e.nonEmpty => {
        cc.ice(node, "Instance with parameters remain")
      }
    }
  }

}
