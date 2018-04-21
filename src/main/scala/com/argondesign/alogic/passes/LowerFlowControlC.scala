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
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

final class LowerFlowControlC(implicit cc: CompilerContext) extends TreeTransformer {

  override def enter(tree: Tree): Unit = tree match {
    case entity: Entity => {
      // Update instance types to reflect ports about to be removes
      for (Instance(Sym(iSymbol: TermSymbol), Sym(eSymbol), _, _) <- entity.instances) {
        val TypeEntity(name, portSymbols, Nil) = eSymbol.denot.kind
        val newPortSymbols = portSymbols filterNot { _.hasAttr("expanded-port") }
        iSymbol withDenot iSymbol.denot.copy(kind = TypeEntity(name, newPortSymbols, Nil))
      }
    }

    case _ => ()
  }

  override def transform(tree: Tree): Tree = tree match {
    //////////////////////////////////////////////////////////////////////////
    // Entity
    //////////////////////////////////////////////////////////////////////////

    case entity: Entity => {
      // Drop original port declarations
      val declarations = entity.declarations filterNot {
        case Decl(Sym(symbol), _, _) => symbol hasAttr "expanded-port"
        case _                       => unreachable
      }

      if (declarations == entity.declarations) {
        entity
      } else {
        // Update type of entity to drop new ports.
        val portSymbols = declarations collect {
          case Decl(Sym(symbol: TermSymbol), _: TypeIn, _)  => symbol
          case Decl(Sym(symbol: TermSymbol), _: TypeOut, _) => symbol
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
    tree visit {
      case node @ Decl(_, TypeOut(_, fc, _), _) if fc != FlowControlTypeNone => {
        cc.ice(node, "Port with flow control remains")
      }
      case node @ Decl(_, TypeOut(_, _, _: StorageTypeSlices), _) => {
        cc.ice(node, "Port with output slices remains")
      }
    }
  }

}
