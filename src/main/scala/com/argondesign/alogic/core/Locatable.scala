////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Type class for types from which we can extract a source location
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.antlr.{AlogicParserRuleContext => AlogicPRC}
import com.argondesign.alogic.ast.Trees.Tree
import com.argondesign.alogic.core.Symbols.Symbol

abstract class Locatable[T] {
  def apply(item: T): Loc // Extract location from item
}

object Locatable {

  implicit val locatableLoc: Locatable[Loc] = loc => loc

  implicit def locatableTree[T <: Tree]: Locatable[T] = _.loc

  implicit val locatableSymbol: Locatable[Symbol] = _.loc

  implicit def locatableAPRC[T <: AlogicPRC]: Locatable[T] = _.loc

}
