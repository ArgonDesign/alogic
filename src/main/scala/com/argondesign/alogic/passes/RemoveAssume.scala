////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Remove assumptions
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol

object RemoveAssumeTransform extends StatelessTreeTransformer {

  override protected def enter(tree: Tree): Option[Tree] = tree match {
    case _: Expr | _: EntAssign | _: Decl => Some(tree)
    case _                                => None
  }

  override def transform(tree: Tree): Tree = tree match {
    case _: AssertionAssume => Stump
    case _                  => tree
  }

}

object RemoveAssume extends EntityTransformerPass(declFirst = false, parallel = true) {
  val name = "remove-assume"

  def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer = RemoveAssumeTransform
}
