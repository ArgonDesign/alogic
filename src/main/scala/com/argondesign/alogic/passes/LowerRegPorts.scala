////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018-2020 Argon Design Ltd. All rights reserved.
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

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.enums.EntityVariant

import scala.collection.mutable

final class LowerRegPorts(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  val oRegs = mutable.Map[Symbol, Symbol]()

  override def enter(tree: Tree): Option[Tree] = {
    tree match {
      case DeclOut(symbol, _, _, StorageTypeReg) =>
        // Allocate local register
        val rName = "`oreg" + cc.sep + symbol.name
        val rSymbol = cc.newSymbol(rName, tree.loc) tap { _.kind = symbol.kind.underlying }
        // Move the clearOnStall attribute to the register symbol
        symbol.attr.clearOnStall.get foreach { attr =>
          rSymbol.attr.clearOnStall set attr
          symbol.attr.clearOnStall.clear()
        }
        // Move the default attribute to the register symbol
        symbol.attr.default.get foreach { attr =>
          rSymbol.attr.default set attr
          symbol.attr.default.clear()
        }
        oRegs(symbol) = rSymbol

      case _ =>
    }
    None
  }

  override def transform(tree: Tree): Tree = tree match {
    case ExprSym(symbol) =>
      // Rewrite all references to the registered output
      // port as references to the local reg
      oRegs.get(symbol) map { rSymbol =>
        ExprSym(rSymbol) regularize tree.loc
      } getOrElse tree

    case decl @ DeclOut(symbol, _, _, StorageTypeReg) =>
      // Change storage type to wire, add register declaration
      val oDecl = decl.copy(st = StorageTypeWire)
      val rSymbol = oRegs(symbol)
      val rDecl = rSymbol.mkDecl
      Thicket(List(oDecl, rDecl)) regularize tree.loc

    case EntDefn(defn @ DefnOut(symbol, initOpt)) if oRegs contains symbol =>
      oRegs.get(symbol) map { rSymbol =>
        // Add register definition and connect
        val oDefn = EntDefn(defn.copy(initOpt = None))
        val rDefn = EntDefn(rSymbol.mkDefn(initOpt))
        val conn = EntConnect(ExprSym(rSymbol), List(ExprSym(symbol)))
        Thicket(List(oDefn, rDefn, conn)) regularize tree.loc
      } getOrElse tree

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    tree visit {
      case node @ DeclOut(_, _, _, st) if st != StorageTypeWire =>
        cc.ice(node, s"Output port with non-wire storage remains")
    }
  }

}

object LowerRegPorts extends PairTransformerPass {
  val name = "lower-reg-ports"
  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) = {
    (decl, defn) match {
      case (dcl: DeclEntity, dfn: DefnEntity) =>
        if (dcl.decls.isEmpty || dfn.variant == EntityVariant.Net) {
          // If no decls, or network, then there is nothing to do
          (decl, defn)
        } else {
          // Perform the transform
          val transformer = new LowerRegPorts
          // First transform the decl
          val newDecl = transformer(decl)
          // Then transform the defn
          val newDefn = transformer(defn)
          (newDecl, newDefn)
        }
      case _ => (decl, defn)
    }
  }
}
