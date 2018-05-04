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
// Strip redundant StmtBlock instances. This should only ever be applied
// past the Namer, and at that point this transform is purely cosmetic.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.typer.TypeAssigner

final class RemoveRedundantBlocks(implicit cc: CompilerContext) extends TreeTransformer {

  private[this] def flatten(body: List[Stmt]): List[Stmt] = {
    body flatMap {
      case StmtBlock(nested) => nested.toIterator
      case other             => Iterator.single(other)
    }
  }

  override def transform(tree: Tree): Tree = tree match {
    case Function(ref, body) => {
      TypeAssigner {
        Function(ref, flatten(body)) withLoc tree.loc
      }
    }

    case State(expr, body) => {
      TypeAssigner {
        State(expr, flatten(body)) withLoc tree.loc
      }
    }

    case StmtBlock(List(stmt)) => stmt

    case StmtBlock(body) => {
      TypeAssigner {
        StmtBlock(flatten(body)) withLoc tree.loc
      }
    }

    case StmtLoop(body) => {
      TypeAssigner {
        StmtLoop(flatten(body)) withLoc tree.loc
      }
    }

    case StmtCase(expr, cases, default) => {
      TypeAssigner {
        StmtCase(expr, cases, flatten(default)) withLoc tree.loc
      }
    }

    case entity: Entity => {
      TypeAssigner {
        entity.copy(
          fenceStmts = flatten(entity.fenceStmts)
        ) withVariant entity.variant withLoc tree.loc
      }
    }

    case _ => tree
  }

}

object RemoveRedundantBlocks extends TreeTransformerPass {
  val name = "remove-redundant-blocks"
  def create(implicit cc: CompilerContext) = new RemoveRedundantBlocks
}
