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
    case Decl(Sym(symbol: TermSymbol), TypeOut(kind, FlowControlTypeNone, StorageTypeReg), None) => {
      // Allocate local register
      val loc = tree.loc
      val rName = "oreg__" + symbol.denot.name.str
      rMap(symbol) = cc.newTermSymbol(rName, loc, kind)
    }

    case _ =>
  }

  override def transform(tree: Tree): Tree = tree match {
    case ExprRef(Sym(symbol: TermSymbol)) => {
      // Rewrite all references to the registered output
      // port as references to the local reg
      rMap.get(symbol) map { rSymbol =>
        ExprRef(Sym(rSymbol)) regularize tree.loc
      } getOrElse {
        tree
      }
    }

    case Decl(Sym(oSymbol: TermSymbol), TypeOut(kind, FlowControlTypeNone, StorageTypeReg), None) => {
      // Change storage type to wire and add register declaration
      rMap get oSymbol map { rSymbol =>
        val declOut = Decl(Sym(oSymbol), TypeOut(kind, FlowControlTypeNone, StorageTypeWire), None)
        val declReg = Decl(Sym(rSymbol), kind, None)
        Thicket(List(declOut, declReg)) regularize tree.loc
      } getOrElse {
        tree
      }
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

  override def finalCheck(tree: Tree): Unit = {
    tree visit {
      case node @ Decl(_, TypeOut(_, _, StorageTypeReg), _) => {
        cc.ice(node, s"Output port with non-wire storage remains")
      }
    }
  }

}
