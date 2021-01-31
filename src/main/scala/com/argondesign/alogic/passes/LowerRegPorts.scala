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

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.enums.EntityVariant

import scala.collection.mutable

final class LowerRegPorts(implicit cc: CompilerContext) extends StatelessTreeTransformer {

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

    case EntSplice(defn @ DefnOut(symbol, initOpt)) if oRegs contains symbol =>
      oRegs.get(symbol) map { rSymbol =>
        // Add register definition and assignment
        val oDefn = EntSplice(defn.copy(initOpt = None))
        val rDefn = EntSplice(rSymbol.mkDefn(initOpt))
        val assign = EntAssign(ExprSym(symbol), ExprSym(rSymbol))
        Thicket(List(oDefn, rDefn, assign)) regularize tree.loc
      } getOrElse tree

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    // $COVERAGE-OFF$ Debug code
    tree visit {
      case node @ DeclOut(_, _, _, st) if st != StorageTypeWire =>
        throw Ice(node, s"Output port with non-wire storage remains")
    }
    // $COVERAGE-ON$
  }

}

object LowerRegPorts extends EntityTransformerPass(declFirst = true, parallel = true) {
  val name = "lower-reg-ports"

  override def skip(decl: DeclEntity, defn: DefnEntity): Boolean = defn.variant != EntityVariant.Fsm

  override protected def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
    new LowerRegPorts()
}
