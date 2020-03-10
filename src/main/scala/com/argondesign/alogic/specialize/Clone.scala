////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.specialize

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext

private[specialize] object Clone {

  def apply(desc: Desc)(implicit cc: CompilerContext): Desc = {
    // Create the symbol map by creating a new symbol for each symbol defined
    // within the input desc. Don't do the symbol defined by desc itself, as
    // we want to allow parametric self referential definitions.
    val symbolMap = Map from {
      desc collectAll { case d: Desc if d.symbol != desc.symbol => d.symbol -> d.symbol.dup }
    }

    // Transform that replaces references to cloned symbols
    val transform = new StatefulTreeTransformer {
      override val typed: Boolean = false

      override def transform(tree: Tree): Tree = tree match {
        case node: Sym =>
          symbolMap.get(node.symbol) match {
            case Some(symbol) => node.copy(symbol = symbol) withLoc node.loc
            case None         => tree
          }
        case node: ExprSym =>
          symbolMap.get(node.symbol) match {
            case Some(symbol) => node.copy(symbol = symbol) withLoc node.loc
            case None         => tree
          }
        case node => node
      }
    }

    // Apply transform
    val cloned = desc rewrite transform

    // Finally clone the symbol introduced by this Desc
    val sym = cloned.ref.asInstanceOf[Sym]
    cloned.cpy(ref = sym.copy(symbol = sym.symbol.dup) withLoc sym.loc) withLoc desc.loc
  }

}
