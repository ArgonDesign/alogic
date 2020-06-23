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
// A data structure representing a map from symbols to their value as
// expressions (Scala 3: type Bindings = Map[Sybol, Expr]
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.ast.Trees.Expr
import com.argondesign.alogic.core.Symbols.Symbol

import scala.language.implicitConversions

class Bindings(val underlying: Map[Symbol, Expr]) extends AnyVal {
  def +(pair: (Symbol, Expr)): Bindings = underlying + pair

  override def toString: String = s"Bindings($underlying)"
}

object Bindings {

  val empty = new Bindings(Map.empty)

  def from(pairs: IterableOnce[(Symbol, Expr)]): Bindings = {
    new Bindings(Map.from(pairs))
  }

  implicit def underlyingToBindings(underlying: Map[Symbol, Expr]): Bindings = {
    new Bindings(underlying)
  }

  implicit def bindingsToUnderlying(bindings: Bindings): Map[Symbol, Expr] = {
    bindings.underlying
  }

}
