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

final class InlineUnsizedConst(implicit cc: CompilerContext) extends TreeTransformer {

  override def transform(tree: Tree): Tree = tree match {
    case ExprSym(symbol) =>
      symbol.kind match {
        case TypeConst(_: TypeNum) => walk(symbol.defn.initializer.get)
        case _                     => tree
      }

    case DeclConst(symbol, _) if symbol.kind.underlying.isNum => Stump
    case DefnConst(symbol, _) if symbol.kind.underlying.isNum => Stump

    case _ => tree
  }
}

object InlineUnsizedConst extends PairTransformerPass {
  val name = "inline-unsized-const"
  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) = {
    val transformer = new InlineUnsizedConst
    (transformer(decl), transformer(defn))
  }
}
