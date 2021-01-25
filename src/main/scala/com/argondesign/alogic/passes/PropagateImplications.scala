////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// A pure analysis pass that propagates signal implication relationships
// through entity boundaries
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.Expr.InstancePortSel
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbol

import scala.collection.mutable

object PropagateImplicationsTransform extends StatelessTreeTransformer {

  override def start(tree: Tree): Unit = tree match {
    case defn: DefnEntity =>
      // Create empty instance -> port -> local maps
      val maps = Map from {
        defn.instances.iterator collect {
          case Defn(iSymbol) => iSymbol -> mutable.Map[Symbol, Symbol]()
        }
      }

      // populate them
      defn.assigns foreach {
        case EntAssign(ExprSym(nSymbol), InstancePortSel(iSymbol, pSymbol)) =>
          maps(iSymbol)(pSymbol) = nSymbol
        case EntAssign(InstancePortSel(iSymbol, pSymbol), ExprSym(nSymbol)) =>
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

  // Skip all
  override def enter(tree: Tree): Option[Tree] = Some(tree)
}

object PropagateImplications extends EntityTransformerPass(declFirst = true) {
  val name = "propagate-implications"

  def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
    PropagateImplicationsTransform
}
