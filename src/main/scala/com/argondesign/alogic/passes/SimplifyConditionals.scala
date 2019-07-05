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
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.util.FollowedBy

final class SimplifyConditionals(implicit cc: CompilerContext)
    extends TreeTransformer
    with FollowedBy {

  override def transform(tree: Tree): Tree = tree match {

    case StmtIf(cond, Nil, elseStmts) => {
      walk(StmtIf(!cond, elseStmts, Nil) regularize tree.loc)
    }

    // TODO: What is the point of this?
    case stmt @ StmtIf(cond, _, _) => {
      cond.simplify match {
        case `cond`  => tree
        case simpler => stmt.copy(cond = simpler) regularize tree.loc
      }
    }

    case StmtStall(cond) => {
      cond.simplify match {
        case `cond`  => tree
        case simpler => StmtStall(simpler) regularize tree.loc
      }
    }

    case expr @ ExprTernary(cond, _, _) => {
      cond.simplify match {
        case `cond`  => tree
        case simpler => expr.copy(cond = simpler) regularize tree.loc
      }
    }

    case _ => tree
  }

}

object SimplifyConditionals extends TreeTransformerPass {
  val name = "simplify-conditionals"
  def create(implicit cc: CompilerContext) = new SimplifyConditionals
}
