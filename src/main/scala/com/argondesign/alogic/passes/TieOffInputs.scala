////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Tie off undriven inputs that have a default attribute
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.Expr.InstancePortSel
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.Symbol

import scala.collection.mutable

object TieOffInputs extends PairTransformerPass(parallel = true) {
  val name = "tie-off-inputs"

  override def skip(decl: Decl, defn: Defn)(implicit cc: CompilerContext): Boolean = defn match {
    case d: DefnEntity => d.instances.isEmpty
    case _             => true
  }

  override def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) = {

    val entityDecl = decl.asInstanceOf[DeclEntity]
    val entityDefn = defn.asInstanceOf[DefnEntity]

    val needsTieOff = mutable.LinkedHashSet[(Symbol, Symbol)]()

    // Gather all instance flow control input ports
    entityDecl.instances.iterator foreach {
      case DeclInstance(iSymbol, _) =>
        iSymbol.kind.asEntity.publicSymbols filter { pSymbol =>
          pSymbol.kind.isIn && pSymbol.attr.default.isSet
        } foreach { pSymbol =>
          needsTieOff += ((iSymbol, pSymbol))
        }
    }

    // Remove all that are driven
    entityDefn.assigns.iterator foreach {
      case EntAssign(InstancePortSel(iSymbol, pSymbol), _) =>
        needsTieOff -= ((iSymbol, pSymbol))
      case _ =>
    }

    if (needsTieOff.isEmpty) {
      (decl, defn)
    } else {
      // Drive un-driven inputs zero
      val newBody = List from {
        entityDefn.body.iterator concat {
          needsTieOff.iterator map {
            case (iSymbol, pSymbol) =>
              EntAssign(
                ExprSym(iSymbol) sel pSymbol.name,
                pSymbol.attr.default.value
              ) regularize iSymbol.loc
          }
        }
      }

      val newDefn = TypeAssigner(entityDefn.copy(body = newBody) withLoc entityDefn.loc)

      (decl, newDefn)
    }
  }

}
