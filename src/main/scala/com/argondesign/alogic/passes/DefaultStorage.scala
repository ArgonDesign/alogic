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

    case ExprRef(Sym(symbol: TermSymbol)) if symbol.denot.kind.isInstanceOf[TypeOut] => {
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
          case Decl(symbol, _) if symbol.denot.kind.isInstanceOf[TypeOut] => {
            symbol.denot.kind.asInstanceOf[TypeOut].st == StorageTypeDefault
          }
          case _ => false
        }

        // Update symbol types them
        for (Decl(symbol, _) <- outs) {
          val kind = symbol.denot.kind.asInstanceOf[TypeOut]
          // Compute the appropriate default storage type
          val newSt = if ((connectedSet contains symbol) || entity.variant == "verbatim") {
            StorageTypeWire
          } else {
            kind.fct match {
              case FlowControlTypeReady  => StorageTypeSlices(List(StorageSliceFwd))
              case FlowControlTypeAccept => StorageTypeWire
              case _                     => StorageTypeReg
            }
          }
          val newKind = kind.copy(st = newSt)
          symbol withDenot symbol.denot.copy(kind = newKind)
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
        symbol.denot.kind visit {
          case TypeOut(_, _, StorageTypeDefault) => cc.ice(tree, "Default storage type remains")
        }
      }
    }
  }

}
