////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Replace term references based on the given bindings
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.transform

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Symbols.Symbol

final class ReplaceTermRefs(
    bindings: Symbol => Option[Expr],
    override val typed: Boolean = true)
    extends StatelessTreeTransformer {

  override def skip(tree: Tree): Boolean = tree match {
    case _: ExprType => true
    case _: ExprInt  => true
    case _: ExprNum  => true
    case _: ExprStr  => true
    case _           => false
  }

  override def enter(tree: Tree): Option[Tree] = tree match {
    case ExprSym(symbol) =>
      bindings(symbol) match {
        case some @ Some(replacement) => replacement regularize tree.loc; some
        case None                     => Some(tree)
      }
    case _ => None
  }

}
