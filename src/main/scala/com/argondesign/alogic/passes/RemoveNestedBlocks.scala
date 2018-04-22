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
// Strip redundant nested StmtBlock instances. This should only ever be applied
// past the Namer, and at that point this transform is purely cosmetic.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.typer.TypeAssigner

final class RemoveNestedBlocks(implicit cc: CompilerContext) extends TreeTransformer {

  private[this] def flatten(body: List[Stmt]): List[Stmt] = {
    body flatMap {
      case StmtBlock(nested) => nested.toIterator
      case other             => Iterator.single(other)
    }
  }

  override def transform(tree: Tree): Tree = {
    val result = tree match {

      case Function(ref, body) => Function(ref, flatten(body))

      case State(expr, body) => State(expr, flatten(body))

      case StmtBlock(body) => StmtBlock(flatten(body))

      case StmtLoop(body) => StmtLoop(flatten(body))

      case StmtCase(expr, cases, default) => StmtCase(expr, cases, flatten(default))

      case entity: Entity => {
        entity.copy(fenceStmts = flatten(entity.fenceStmts)) withVariant entity.variant
      }

      case _ => tree
    }

    if (result ne tree) {
      TypeAssigner(result withLoc tree.loc)
    }

    result
  }

}
