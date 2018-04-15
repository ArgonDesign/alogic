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
// - Update Connects
// - Replace naked port references yet
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.SliceFactory
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Stack
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.FollowedBy
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class LowerFlowControlB(implicit cc: CompilerContext) extends TreeTransformer {

  override def transform(tree: Tree): Tree = {
    val result: Tree = tree match {

      //////////////////////////////////////////////////////////////////////////
      // Expressions
      //////////////////////////////////////////////////////////////////////////

      case ExprRef(Sym(symbol: TermSymbol)) => {
        // Rewrite references to ports with fc as references to the payload
        symbol.getAttr[(Symbol, TermSymbol)]("fcv").map {
          case (pSymbol, _) => ExprRef(Sym(pSymbol))
        } orElse symbol.getAttr[(Symbol, TermSymbol, TermSymbol)]("fcr").map {
          case (pSymbol, _, _) => ExprRef(Sym(pSymbol))
        } orElse symbol.getAttr[(Symbol, TermSymbol, TermSymbol)]("fca").map {
          case (pSymbol, _, _) => ExprRef(Sym(pSymbol))
        } getOrElse {
          tree
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // Connect
      //////////////////////////////////////////////////////////////////////////

      // Expand inter-entity connections and
      // TODO

      //////////////////////////////////////////////////////////////////////////
      // Entity
      //////////////////////////////////////////////////////////////////////////

      case entity: Entity => {
        // Drop original port declarations
        val declarations = entity.declarations filterNot {
          case Decl(Sym(symbol), _, _) => {
            symbol.hasAttr("fcv") || symbol.hasAttr("fcr") || symbol.hasAttr("fca")
          }
          case _ => unreachable
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

          entity.copy(declarations = declarations) withVariant entity.variant
        }
      }

      case _ => tree
    }

    // If we did modify the node, regularize it
    if (result ne tree) {
      result regularize tree.loc
    }

    // Done
    result
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
