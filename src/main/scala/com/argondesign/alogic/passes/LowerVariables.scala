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
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.enums.EntityVariant

final class LowerVariables(implicit cc: CompilerContext) extends TreeTransformer {

  // TODO: Generate clock enables

  override def skip(tree: Tree): Boolean = tree match {
    // We do not replace _q with _d in connects, connects always use the _q
    case _: EntConnect => true
    case _             => false
  }

  override def enter(tree: Tree): Option[Tree] = {
    tree match {

      case decl: DeclEntity =>
        // Drop the oreg prefix from the flops allocated for registered outputs,
        // These will now gain _d and _q, so the names will become unique.
        val prefix = s"`oreg${cc.sep}"
        val prefixLen = prefix.length
        for {
          Decl(symbol) <- decl.decls
          if symbol.name startsWith prefix
        } {
          symbol.name = symbol.name drop prefixLen
        }

        // Mark local symbols driven by Connect as combinational nets
        for {
          EntConnect(_, List(rhs)) <- decl.symbol.defn.asInstanceOf[DefnEntity].connects
          symbol <- WrittenSymbols(rhs)
          if symbol.kind.isInt
        } {
          symbol.attr.combSignal set true
        }

        // Mark memory control signals as combinatorial nets
        for {
          Decl(mSymbol) <- decl.decls
          (weSymbol, waSymbol, wdSymbol) <- mSymbol.attr.memory.get
        } {
          weSymbol.attr.combSignal set true
          waSymbol.attr.combSignal set true
          wdSymbol.attr.combSignal set true
        }

        decl.decls foreach {
          case decl @ Decl(symbol)
              if symbol.kind.isInt && !(symbol.attr.combSignal contains true) =>
            val loc = decl.loc
            val name = symbol.name
            // Append _q to the name of the symbol
            symbol.name = s"${name}_q"
            // Create the _d symbol
            val dSymbol = cc.newSymbol(s"${name}_d", loc) tap {
              _.kind = symbol.kind
            }
            // Move the clearOnStall attribute to the _d symbol
            symbol.attr.clearOnStall.get foreach { attr =>
              dSymbol.attr.clearOnStall set attr
              symbol.attr.clearOnStall.clear()
            }
            // If the symbol has a default attribute, move that to the _d,
            // otherwise use the _q as the default initializer
            val default = symbol.attr.default.getOrElse {
              ExprSym(symbol) regularize loc
            }
            dSymbol.attr.default set default
            symbol.attr.default.clear()
            // Set attributes
            symbol.attr.flop set dSymbol

          case _ =>
        }

      case _ =>
    }
    None
  }

  override def transform(tree: Tree): Tree = tree match {

    //////////////////////////////////////////////////////////////////////////
    // Rewrite references
    //////////////////////////////////////////////////////////////////////////

    case ExprSym(qSymbol) =>
      // Rewrite references to flops as references to the _d,
      qSymbol.attr.flop.get map { dSymbol =>
        ExprSym(dSymbol) regularize tree.loc
      } getOrElse {
        tree
      }

    //////////////////////////////////////////////////////////////////////////
    // Add declarations and definitions
    //////////////////////////////////////////////////////////////////////////

    case decl @ Decl(qSymbol) =>
      qSymbol.attr.flop.get map { dSymbol =>
        Thicket(List(decl, dSymbol.mkDecl)) regularize tree.loc
      } getOrElse tree

    case defn @ Defn(qSymbol) =>
      qSymbol.attr.flop.get map { dSymbol =>
        Thicket(List(defn, dSymbol.mkDefn)) regularize tree.loc
      } getOrElse tree

    //////////////////////////////////////////////////////////////////////////
    // Note: Initial _d = _q fence statements will be added in
    // DefaultAssignments as required
    //////////////////////////////////////////////////////////////////////////

    case _ => tree
  }

}

object LowerVariables extends EntityTransformerPass(declFirst = true) {
  val name = "lower-variables"

  override def skip(decl: Decl, defn: Defn)(implicit cc: CompilerContext): Boolean =
    super.skip(decl, defn) || {
      defn match {
        case DefnEntity(_, EntityVariant.Net, _) => true
        case _                                   => false
      }
    }

  def create(symbol: Symbol)(implicit cc: CompilerContext) = new LowerVariables
}
