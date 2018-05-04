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
// Do:
// - Remove expanded ports
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.StorageTypes.StorageTypeSlices
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

final class LowerFlowControlC(implicit cc: CompilerContext) extends TreeTransformer {

  override def transform(tree: Tree): Tree = tree match {
    //////////////////////////////////////////////////////////////////////////
    // Entity
    //////////////////////////////////////////////////////////////////////////

    case entity: Entity => {
      // Drop original port declarations
      val declarations = entity.declarations filterNot {
        case Decl(symbol, _) => symbol.attr.expandedPort.isSet
        case _               => unreachable
      }

      if (declarations == entity.declarations) {
        entity
      } else {
        // Update type of entity to drop new ports.
        val portSymbols = declarations collect {
          case Decl(symbol, _) if symbol.denot.kind.isInstanceOf[TypeIn]  => symbol
          case Decl(symbol, _) if symbol.denot.kind.isInstanceOf[TypeOut] => symbol
        }

        val newKind = entitySymbol.denot.kind match {
          case kind: TypeEntity => kind.copy(portSymbols = portSymbols)
          case _                => unreachable
        }
        entitySymbol withDenot entitySymbol.denot.copy(kind = newKind)

        TypeAssigner {
          entity.copy(declarations = declarations) withVariant entity.variant withLoc tree.loc
        }
      }
    }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {

    def check(node: Tree, kind: Type) = {
      kind visit {
        case TypeOut(_, fc, _) if fc != FlowControlTypeNone => {
          cc.ice(node, "Port with flow control remains")
        }
        case TypeOut(_, _, _: StorageTypeSlices) => {
          cc.ice(node, "Port with output slices remains")
        }
      }
    }

    tree visit {
      case node @ Decl(symbol, _) => check(node, symbol.denot.kind)
    }
  }

}

object LowerFlowControlC extends TreeTransformerPass {
  val name = "lower-flow-control-c"
  def create(implicit cc: CompilerContext) = new LowerFlowControlC
}
