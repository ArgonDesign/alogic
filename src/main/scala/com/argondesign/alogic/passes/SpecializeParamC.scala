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
import com.argondesign.alogic.core.Types._

final class SpecializeParamC(implicit cc: CompilerContext) extends TreeTransformer {

  override def enter(tree: Tree): Unit = tree match {
    case Decl(symbol, _) => symbol.attr.owner.clear()

    case _ => ()
  }

  override def transform(tree: Tree): Tree = tree match {
    case Instance(Sym(iSymbol: TermSymbol), Sym(eSymbol), paramNames, paramExprs) => {
      val eKind = eSymbol.denot.kind.asInstanceOf[TypeEntity]
      if (eKind.paramSymbols.isEmpty) {
        // Instantiated entity has no parameters
        tree
      } else {
        // Find the specialized entity
        val Sym(sSymbol: TypeSymbol) = {
          // Gather (now specialized) particular parameter bindings
          val paramBindings = {
            val paramMap = (paramNames zip paramExprs).toMap
            val pairs = for {
              symbol <- eKind.paramSymbols
              expr <- paramMap.get(symbol.name)
            } yield {
              symbol -> expr.simplify
            }
            pairs.toMap
          }
          // Complete them with the default bindings
          val completeBindings = eSymbol.attr.defaultParamBindings.value map {
            case (symbol, expr) => symbol -> paramBindings.getOrElse(symbol, expr)
          }
          // Find the specialized entity
          eSymbol.attr.specMap.value(completeBindings).ref
        }

        // Update type of instance
        iSymbol withDenot iSymbol.denot.copy(kind = TypeInstance(sSymbol))
        // Remove attribute
        iSymbol.attr.paramBinding.clear()
        // Rewrite tree
        Instance(Sym(iSymbol), Sym(sSymbol), Nil, Nil) regularize tree.loc
      }
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

object SpecializeParamC extends TreeTransformerPass {
  val name = "specialize-param-c"
  def create(implicit cc: CompilerContext) = new SpecializeParamC
}
