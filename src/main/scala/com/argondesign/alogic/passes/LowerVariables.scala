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
// Lower variables into flops and combinatorial nets. At this stage, anything
// that is of TypeInt is a flop, unless it is driven through a connect, or is
// one of the control signals of a memory, in which case it is a net.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.analysis.WrittenSymbols
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.util.FollowedBy

final class LowerVariables(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy {

  // TODO: Generate clock enables

  override def skip(tree: Tree): Boolean = tree match {
    case entity: EntityLowered => entity.symbol.attr.variant.value == "network"
    // We do not replace _q with _d in connects, connects always use the _q
    case _: Connect => true
    case _          => false
  }

  override def enter(tree: Tree): Unit = tree match {

    case entity: EntityLowered => {
      // Drop the oreg prefix from the flops allocated for registered outputs,
      // These will now gain _d and _q, so the names will become unique.
      val prefix = s"oreg${cc.sep}"
      val prefixLen = prefix.length
      for {
        Decl(oSymbol, _) <- entity.declarations
        rSymbol <- oSymbol.attr.oReg.get
      } {
        val name = rSymbol.name
        assert(name startsWith prefix)
        rSymbol rename (name drop prefixLen)
        oSymbol.attr.oReg.clear()
      }

      // Mark local symbols driven by Connect as combinatorial nets
      for {
        Connect(_, List(rhs)) <- entity.connects
        symbol <- WrittenSymbols(rhs)
        if symbol.kind.isInt
      } {
        symbol.attr.combSignal set true
      }

      // Mark memory control signals as combinatorial nets
      for {
        Decl(mSymbol, _) <- entity.declarations
        (weSymbol, waSymbol, wdSymbol) <- mSymbol.attr.memory.get
      } {
        weSymbol.attr.combSignal set true
        waSymbol.attr.combSignal set true
        wdSymbol.attr.combSignal set true
      }
    }

    case Decl(symbol, _) if symbol.kind.isInt && !(symbol.attr.combSignal contains true) => {
      val loc = tree.loc
      val name = symbol.name
      // Append _q to the name of the symbol
      symbol rename s"${name}_q"
      // Create the _d symbol
      val dSymbol = cc.newTermSymbol(s"${name}_d", loc, symbol.kind)
      // Move the clearOnStall attribute to the _d symbol
      symbol.attr.clearOnStall.get foreach { attr =>
        dSymbol.attr.clearOnStall set attr
        symbol.attr.clearOnStall.clear()
      }
      // If the symbol has a default attribute, move that to the _d,
      // otherwise use the _q as the default initializer
      val default = symbol.attr.default.getOrElse {
        ExprRef(symbol) regularize loc
      }
      dSymbol.attr.default set default
      symbol.attr.default.clear()
      // Set attributes
      symbol.attr.flop set dSymbol
    }

    case _ =>
  }

  override def transform(tree: Tree): Tree = tree match {

    //////////////////////////////////////////////////////////////////////////
    // Rewrite references
    //////////////////////////////////////////////////////////////////////////

    case ExprRef(qSymbol) => {
      // Rewrite references to flops as references to the _d,
      qSymbol.attr.flop.get map { dSymbol =>
        ExprRef(dSymbol) regularize tree.loc
      } getOrElse {
        tree
      }
    }

    //////////////////////////////////////////////////////////////////////////
    // Add declarations
    //////////////////////////////////////////////////////////////////////////

    case decl @ Decl(qSymbol, _) => {
      qSymbol.attr.flop.get map { dSymbol =>
        val decls = List(
          decl,
          Decl(dSymbol, None)
        )
        Thicket(decls) regularize tree.loc
      } getOrElse {
        tree
      }
    }

    //////////////////////////////////////////////////////////////////////////
    // Note: Initial _d = _q fence statements will be added in
    // DefaultAssignments as required
    //////////////////////////////////////////////////////////////////////////

    case _ => tree
  }

}

object LowerVariables extends TreeTransformerPass {
  val name = "lower-variables"
  def create(implicit cc: CompilerContext) = new LowerVariables
}
