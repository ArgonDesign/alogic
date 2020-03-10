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
// A pure analysis pass that propagates signal implication relationships
// through entity boundaries
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees.Expr.InstancePortRef
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._

import scala.collection.mutable

final class PropagateImplications(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  override def skip(tree: Tree): Boolean = tree match {
    case _: DefnEntity => false
    case _             => true
  }

  override def enter(tree: Tree): Option[Tree] = {
    tree match {

      case defn: DefnEntity =>
        // Create empty instance -> port -> local maps
        val maps = Map from {
          defn.instances.iterator collect {
            case Defn(iSymbol) => iSymbol -> mutable.Map[Symbol, Symbol]()
          }
        }

        // populate them
        defn.connects foreach {
          case EntConnect(InstancePortRef(iSymbol, pSymbol), List(ExprSym(nSymbol))) =>
            maps(iSymbol)(pSymbol) = nSymbol
          case EntConnect(ExprSym(nSymbol), List(InstancePortRef(iSymbol, pSymbol))) =>
            maps(iSymbol)(pSymbol) = nSymbol
          case _ =>
        }

        // Lift the implication relations
        for {
          map <- maps.values
          (paSymbol, naSymbol) <- map
          (a, b, pbSymbol) <- paSymbol.attr.implications.enumerate
          nbSymbol <- map get pbSymbol
          lifted = (a, b, nbSymbol)
          if !(naSymbol.attr.implications.enumerate contains lifted)
        } {
          naSymbol.attr.implications append lifted
        }

      case _ =>
    }
    None
  }

}

object PropagateImplications extends EntityTransformerPass(declFirst = true) {
  val name = "propagate-implications"
  def create(symbol: Symbol)(implicit cc: CompilerContext) = new PropagateImplications
}
