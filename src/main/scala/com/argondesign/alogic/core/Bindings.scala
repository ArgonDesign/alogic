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
// expressions
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.ast.Trees.Expr
import com.argondesign.alogic.ast.Trees.ExprSym
import com.argondesign.alogic.core.Symbols.Symbol

import scala.annotation.tailrec
import scala.language.implicitConversions

class Bindings(val underlying: Map[Symbol, Expr]) extends AnyVal {
  // Expand bindings by replacing references to symbols within the bindings
  // with their values. i.e.: If any value in the bindings map references a
  // key in the same bindings map, replace that reference with the value for
  // that key.
  @tailrec
  final def expand(implicit cc: CompilerContext): Bindings = {
    // Simplify the expressions
    val simplified = this mapValues { _.simplify }

    // Collect any symbols referenced that have a value in the bindings
    val referenced = simplified.valuesIterator flatMap {
      _ collect { case ExprSym(symbol) if symbol.kind.isParam => symbol }
    } filter {
      this.contains
    }

    if (referenced.isEmpty) {
      // If no symbols are referenced, we are done
      simplified
    } else {
      // Otherwise expand the bindings using themselves
      val expanded = this map {
        case (symbol, expr) => symbol -> (expr given this)
      }
      // Go again until the bindings are flat
      expanded.expand
    }
  }

  def mapValues(f: Expr => Expr): Bindings = {
    (underlying.view mapValues f).toMap
  }

  def map(f: ((Symbol, Expr)) => (Symbol, Expr)): Bindings = {
    underlying map f
  }

  def +(pair: (Symbol, Expr)): Bindings = underlying + pair
}

object Bindings {

  val empty = new Bindings(Map.empty)

  implicit def apply(pairs: Seq[(Symbol, Expr)]): Bindings = {
    new Bindings(pairs.toMap)
  }

  implicit def apply(map: scala.collection.Map[Symbol, Expr]): Bindings = {
    new Bindings(map.toMap)
  }

  implicit def apply(underlying: Map[Symbol, Expr]): Bindings = {
    new Bindings(underlying)
  }

  implicit def apply(pairOpt: Option[(Symbol, Expr)]): Bindings = {
    new Bindings(pairOpt.toMap)
  }

  implicit def toUnderlying(symbolBitSet: Bindings): Map[Symbol, Expr] = {
    symbolBitSet.underlying
  }

}
