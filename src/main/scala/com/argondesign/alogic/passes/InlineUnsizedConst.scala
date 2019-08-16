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
// Remove declarations of unsized const symbols, and replace references with
// the const value
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types.TypeConst
import com.argondesign.alogic.core.Types.TypeNum
import com.argondesign.alogic.typer.TypeAssigner

final class InlineUnsizedConst(implicit cc: CompilerContext) extends TreeTransformer {

  override def transform(tree: Tree): Tree = tree match {
    case ExprRef(symbol) =>
      symbol.kind match {
        case TypeConst(_: TypeNum) => walk(symbol.attr.init.value)
        case _                     => tree
      }

    case Decl(symbol, Some(init)) if symbol.kind.isConst => {
      symbol.attr.init set init
      tree
    }

    case entity: Entity => {
      val newBody = entity.body filter {
        case EntDecl(Decl(symbol, _)) =>
          symbol.kind match {
            case TypeConst(_: TypeNum) => false
            case _                     => true
          }
        case _ => true
      }

      TypeAssigner(entity.copy(body = newBody) withLoc entity.loc)
    }

    case _ => tree
  }
}

object InlineUnsizedConst extends TreeTransformerPass {
  val name = "inline-unsized-const"
  def create(implicit cc: CompilerContext) = new InlineUnsizedConst
}
