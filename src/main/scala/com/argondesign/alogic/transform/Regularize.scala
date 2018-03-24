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
// Regularize tree by assigning loc where missing, and applying type assigner
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.transform

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees.Tree
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.typer.TypeAssigner

final class Regularize(loc: Loc)(implicit cc: CompilerContext) extends TreeTransformer {

  override def transform(tree: Tree): Tree = {
    assert(!tree.hasTpe)
    if (!tree.hasLoc) {
      tree withLoc loc
    }
    TypeAssigner(tree)
  }
}
