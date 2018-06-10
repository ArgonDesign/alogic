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
// Convert StorageTypeDefault to the appropriate concrete value
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes._
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.core.Types.TypeOut
import com.argondesign.alogic.util.FollowedBy

import scala.collection.mutable

final class DefaultStorage(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy {

  // Set of output ports accessed through port methods or directly
  private[this] val accessedSet = mutable.Set[TermSymbol]()

  // Set of output ports connected with '->'
  private[this] val connectedSet = mutable.Set[TermSymbol]()

  private[this] var inConnect = false

  override def enter(tree: Tree): Unit = tree match {

    case _: Connect => {
      assert(!inConnect)
      inConnect = true
    }

    case ExprRef(symbol: TermSymbol) if symbol.kind.isInstanceOf[TypeOut] => {
      if (inConnect) {
        connectedSet add symbol
      } else {
        accessedSet add symbol
      }
    }

    case _ =>
  }

  override def transform(tree: Tree): Tree = {
    tree match {
      case _: Connect => {
        tree
      } followedBy {
        assert(inConnect)
        inConnect = false
      }

      case entity: Entity => {
        // Collect all output declarations that use the default storage type
        val (outs, rest) = entity.declarations.partition {
          case Decl(symbol, _) if symbol.kind.isInstanceOf[TypeOut] => {
            symbol.kind.asInstanceOf[TypeOut].st == StorageTypeDefault
          }
          case _ => false
        }

        // Update their types
        for (Decl(symbol, _) <- outs) {
          val kind = symbol.kind.asInstanceOf[TypeOut]
          // Compute the appropriate default storage type
          val newSt =
            if ((connectedSet contains symbol) || entitySymbol.attr.variant.value == "verbatim") {
              StorageTypeWire
            } else {
              kind.fct match {
                case FlowControlTypeReady  => StorageTypeSlices(List(StorageSliceFwd))
                case FlowControlTypeAccept => StorageTypeWire
                case _                     => StorageTypeReg
              }
            }
          symbol.kind = kind.copy(st = newSt)
        }

        tree
      }

      case _ => tree
    }
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(!inConnect)
    tree visitAll {
      case Decl(symbol, _) => {
        symbol.kind visit {
          case TypeOut(_, _, StorageTypeDefault) => cc.ice(tree, "Default storage type remains")
        }
      }
    }
  }

}

object DefaultStorage extends TreeTransformerPass {
  val name = "default-storages"
  def create(implicit cc: CompilerContext) = new DefaultStorage
}
