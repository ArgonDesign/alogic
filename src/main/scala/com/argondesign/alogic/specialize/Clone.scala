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

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext

private[specialize] object Clone {

  def apply(desc: Desc)(implicit cc: CompilerContext): Desc = {
    require(desc.symbol.desc eq desc, "Not canonical desc")

    // Clone every symbol within the input desc, but the symbol introduced
    // within the input desc itself, as we want to allow parametric self
    // referential definitions. Build mapping
    val symbolMap = Map from {
      desc collectAll { case d: Desc if d ne desc => d.symbol -> d.symbol.dup }
    }

    // Transform that replaces symbols with the cloned symbols
    object Transform extends StatelessTreeTransformer {
      override val typed: Boolean = false
      override def transform(tree: Tree): Tree = tree match {
        // Clone the symbol in the input desc here separately in order to avoid
        // creating a temporary desc node changing the desc of the input symbol
        case node: Sym if node eq desc.ref => node.copy(symbol = node.symbol.dup) withLoc tree.loc
        case node: Sym =>
          symbolMap.get(node.symbol) match {
            case Some(symbol) => node.copy(symbol = symbol) withLoc tree.loc
            case None         => tree
          }
        case node: ExprSym =>
          symbolMap.get(node.symbol) match {
            case Some(symbol) => node.copy(symbol = symbol) withLoc tree.loc
            case None         => tree
          }
        case _ => tree
      }
    }

    // Apply transform
    desc rewrite Transform
  } ensuring (desc.symbol.desc eq desc, "Cloning changed desc of input")

}
