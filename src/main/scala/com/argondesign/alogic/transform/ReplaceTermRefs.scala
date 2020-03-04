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
import com.argondesign.alogic.core.Bindings
import com.argondesign.alogic.core.CompilerContext

final class ReplaceTermRefs(
    bindings: Bindings,
    override val typed: Boolean = true
)(
    implicit cc: CompilerContext
) extends TreeTransformer {

  override def transform(tree: Tree): Tree = tree match {
    case ExprSym(symbol) => bindings.getOrElse(symbol, tree)

    case _ => tree
  }
}
