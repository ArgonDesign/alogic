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
// Replace term references based on the given bindings
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.transform

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.TermSymbol

final class ReplaceTermRefs(
    bindings: Map[TermSymbol, Expr]
)(
    implicit cc: CompilerContext
) extends TreeTransformer {

  override val typed: Boolean = true

  override def transform(tree: Tree): Tree = tree match {
    case ExprRef(Sym(symbol: TermSymbol)) => bindings.getOrElse(symbol, tree)

    case _ => tree
  }
}
