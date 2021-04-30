////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Tie off undriven inputs
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.Expr.InstancePortSel
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.TypeAssigner

object TieOffInputs extends PairTransformerPass {
  val name = "tie-off-inputs"

  override def skip(decl: Decl, defn: Defn)(implicit cc: CompilerContext): Boolean = defn match {
    case d: DefnEntity => d.instances.isEmpty
    case _             => true
  }

  override def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) = {

    val entityDecl = decl.asInstanceOf[DeclEntity]
    val entityDefn = defn.asInstanceOf[DefnEntity]

    // Gather all instance input ports
    val instanceInputPorts = Set from {
      entityDecl.instances.iterator.flatMap {
        case DeclInstance(iSymbol, _, _) =>
          iSymbol.kind.asEntity.publicSymbols.iterator.collect {
            case pSymbol if pSymbol.kind.isIn => (iSymbol, pSymbol)
          }
      }
    }

    // Remove all that are driven
    val needsTieOff = instanceInputPorts.removedAll {
      entityDefn.assigns.iterator.collect {
        case EntAssign(InstancePortSel(iSymbol, pSymbol), _) => (iSymbol, pSymbol)
      }
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
                pSymbol.attr.default
                  .getOrElse(ExprInt(pSymbol.kind.isSigned, pSymbol.kind.width.toInt, 0))
              ) regularize iSymbol.loc
          }
        }
      }

      val newDefn = TypeAssigner(entityDefn.copy(body = newBody) withLoc entityDefn.loc)

      (decl, newDefn)
    }
  }

}
