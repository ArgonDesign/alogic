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
// Transform parameters of definition to constants using given bindings
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.specialize

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol

private[specialize] object Replace {

  private class Transform(
      mapping: collection.Map[Symbol, Symbol]
  )(implicit cc: CompilerContext)
      extends StatefulTreeTransformer {
    override val typed: Boolean = false
    override def transform(tree: Tree): Tree = tree match {
      case ExprSym(symbol) =>
        mapping.get(symbol) match {
          case Some(newSymbol) => ExprSym(newSymbol) withLoc tree.loc
          case None            => tree
        }
      case ExprRef(Sym(symbol, _)) if mapping contains symbol => ???
      case tree                                               => tree
    }
  }

  // Replace every symbol reference with a reference to the mapped symbol
  def apply[T <: Tree](
      tree: T,
      mapping: collection.Map[Symbol, Symbol]
  )(implicit cc: CompilerContext): T =
    if (mapping.isEmpty) tree else tree rewrite new Transform(mapping)

  def apply(
      input: Either[Desc, (Decl, Defn)],
      mapping: collection.Map[Symbol, Symbol]
  )(implicit cc: CompilerContext): Either[Desc, (Decl, Defn)] = {
    if (mapping.isEmpty) {
      input
    } else {
      input match {
        case Left(desc)          => Left(this(desc, mapping))
        case Right((decl, defn)) => Right((decl, this(defn, mapping)))
      }
    }
  }
}
