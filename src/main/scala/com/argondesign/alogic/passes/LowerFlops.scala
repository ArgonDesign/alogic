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
// Lower flops (anything that is TypeInt at this stage)
// to constituent _q and _d signals
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Names.TermName
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.FollowedBy

final class LowerFlops(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy {

  // TODO: Generate clock enables

  private var inConnect = false

  override def enter(tree: Tree): Unit = tree match {

    case entity: Entity => {
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
        rSymbol withDenot rSymbol.denot.copy(name = TermName(name drop prefixLen))
        oSymbol.attr.oReg.clear()
      }
    }

    case Decl(symbol, _) if symbol.denot.kind.isInstanceOf[TypeInt] => {
      val loc = tree.loc
      val name = symbol.name
      // Append _q to the name of the symbol
      symbol withDenot symbol.denot.copy(name = TermName(s"${name}_q"))
      // Create the _d symbol
      val dSymbol = cc.newTermSymbol(s"${name}_d", loc, symbol.denot.kind)
      // Move the clearOnStall attribute to the _d symbol
      symbol.attr.clearOnStall.get foreach { attr =>
        dSymbol.attr.clearOnStall set attr
        symbol.attr.clearOnStall.clear()
      }
      // If the symbol has a default attribute, move that to the _d,
      // otherwise use the _q as the default initializer
      val default = symbol.attr.default.getOrElse {
        ExprRef(Sym(symbol)) regularize loc
      }
      dSymbol.attr.default set default
      symbol.attr.default.clear()
      // Set attributes
      symbol.attr.flop set dSymbol
    }

    case _: Connect => inConnect = true

    case _ =>
  }

  override def transform(tree: Tree): Tree = tree match {

    //////////////////////////////////////////////////////////////////////////
    // Rewrite references
    //////////////////////////////////////////////////////////////////////////

    case ExprRef(Sym(qSymbol)) if !inConnect => {
      // Rewrite references to flops as references to the _d,
      // but not in connect expressions, connects always use the _q
      qSymbol.attr.flop.get map { dSymbol =>
        ExprRef(Sym(dSymbol)) regularize tree.loc
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
    // Exit connect
    //////////////////////////////////////////////////////////////////////////

    case _: Connect => {
      tree
    } followedBy {
      inConnect = false
    }

    //////////////////////////////////////////////////////////////////////////
    // Note: Initial _d = _q fence statements will be added in
    // DefaultAssignments as required
    //////////////////////////////////////////////////////////////////////////

    case _ => tree
  }

}

object LowerFlops extends TreeTransformerPass {
  val name = "lower-flops"
  def create(implicit cc: CompilerContext) = new LowerFlops
}
