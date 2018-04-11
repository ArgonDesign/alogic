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
// Convert register output ports to local register connected to wire output port
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes._
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.typer.TypeAssigner

import scala.collection.mutable

final class LowerRegPorts(implicit cc: CompilerContext) extends TreeTransformer {

  // Map from original output port symbol with StorageTypeReg to the
  // corresponding local register symbol
  private[this] val rMap = mutable.Map[TermSymbol, TermSymbol]()

  override def enter(tree: Tree): Unit = tree match {
    case Decl(Sym(symbol: TermSymbol), TypeOut(kind, FlowControlTypeNone, StorageTypeReg), _) => {
      // Allocate local register
      val loc = tree.loc
      val rName = "oreg__" + symbol.denot.name.str
      val fcn = FlowControlTypeNone
      val stw = StorageTypeWire
      val rSymbol = cc.newTermSymbol(rName, loc, TypeOut(kind, fcn, stw))
      rMap(symbol) = rSymbol
    }

    case _ =>
  }

  override def transform(tree: Tree): Tree = {
    // Nodes with children that have been rewritten need their types assigned
    if (!tree.hasTpe) {
      TypeAssigner(tree)
    }

    tree match {
      case ExprRef(Sym(symbol: TermSymbol)) => {
        // Rewrite all references to the registered output
        // port as references to the local reg
        rMap.get(symbol) map { rSymbol =>
          ExprRef(Sym(rSymbol)) regularize tree.loc
        } getOrElse {
          tree
        }
      }

      case decl @ Decl(Sym(symbol: TermSymbol), TypeOut(kind, _, _), _) if rMap contains symbol => {
        // Change storage type to wire
        val result = decl.copy(kind = TypeOut(kind, FlowControlTypeNone, StorageTypeWire))
        TypeAssigner(result withLoc tree.loc)
      }

      case entity: Entity if rMap.nonEmpty => {
        // Add assignments to outputs in the beginning of the fence block
        val assignments = for ((oSymbol, rSymbol) <- rMap) yield {
          StmtAssign(ExprRef(Sym(oSymbol)), ExprRef(Sym(rSymbol))) regularize oSymbol.loc
        }

        val result = entity.copy(
          fenceStmts = assignments.toList ::: entity.fenceStmts
        ) withVariant entity.variant
        TypeAssigner(result withLoc tree.loc)
      }

      case _ => tree
    }
  }

  override def finalCheck(tree: Tree): Unit = {
    tree visit {
      case node: Tree if !node.hasTpe => cc.ice(node, "Lost node.tpe", node.toString)
      case node @ ExprCall(ExprSelect(ref, sel), _) if ref.tpe.isInstanceOf[TypeOut] => {
        cc.ice(node, s"Output port .${sel} remains")
      }
      case node @ ExprCall(ExprSelect(ref, sel), _) if ref.tpe.isInstanceOf[TypeIn] => {
        cc.ice(node, s"Input port .${sel} remains")
      }
    }
  }

}
