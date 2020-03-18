////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Check and strip static assertions
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol

final class StaticAssertions(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  override val typed = false

  override protected def skip(tree: Tree): Boolean = tree match {
    case _: Expr => true
    case _       => false
  }

  override def transform(tree: Tree): Tree = tree match {
    case AssertionStatic(cond, msgOpt) =>
      if (cc.typeCheck(tree)) {
        cond.value match {
          case Some(v) if v != 0 => // OK
          case Some(_) =>
            val suffix = msgOpt map { ": " + _ } getOrElse ""
            cc.error(cond, s"Static assertion failure$suffix")
          case None =>
            cc.error(cond, "Cannot evaluate condition of static assertion at compilation time")
        }
      }
      Stump

    //
    case _ => tree
  }

}

object StaticAssertions extends EntityTransformerPass(declFirst = false) {
  val name = "static-assertions"

  override def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
    new StaticAssertions
}
