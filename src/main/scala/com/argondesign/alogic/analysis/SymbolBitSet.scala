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
// A data structure representing a set of (symbol, bit index) pairs in an
// efficient and convenient way
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.analysis

import com.argondesign.alogic.core.Symbols.Symbol

import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.language.implicitConversions

class SymbolBitSet(val underlying: Map[Symbol, BitSet]) extends AnyVal {

  def union(that: SymbolBitSet): SymbolBitSet = {
    if (this.isEmpty) {
      that
    } else if (that.isEmpty) {
      this
    } else {
      val acc = mutable.Map[Symbol, BitSet]() ++ underlying
      for ((symbol, bits) <- that) {
        acc.get(symbol) match {
          case Some(curr) => acc(symbol) = curr union bits
          case None       => acc(symbol) = bits
        }
      }
      SymbolBitSet(acc.toMap)
    }
  }

  def intersect(that: SymbolBitSet): SymbolBitSet = {
    if (this.isEmpty || that.isEmpty) {
      SymbolBitSet.empty
    } else {
      val acc = mutable.Map[Symbol, BitSet]()
      acc ++= underlying filter { case (symbol, _) => that contains symbol }
      if (acc.isEmpty) {
        SymbolBitSet.empty
      } else {
        acc mapValuesInPlace {
          case (symbol, curr) => curr intersect that(symbol)
        } filterInPlace {
          case (_, bits) => bits.nonEmpty
        }
        SymbolBitSet(acc.toMap)
      }
    }
  }

  def diff(that: SymbolBitSet): SymbolBitSet = {
    if (this.isEmpty || that.isEmpty) {
      this
    } else {
      val acc = mutable.Map[Symbol, BitSet]() ++ underlying
      // TODO: early termination if acc is empty
      for ((symbol, bits) <- that) {
        acc.get(symbol) match {
          case Some(curr) => acc(symbol) = curr diff bits
          case None       => ()
        }
      }
      acc filterInPlace { case (_, bits) => bits.nonEmpty }
      SymbolBitSet(acc.toMap)
    }
  }

  def subsetOf(that: SymbolBitSet): Boolean = {
    this forall {
      case (symbol, bits) => bits.isEmpty || (that.get(symbol) exists { bits.subsetOf })
    }
  }

}

object SymbolBitSet {

  val empty = new SymbolBitSet(Map.empty)

  implicit def apply(underlying: Map[Symbol, BitSet]): SymbolBitSet = {
    new SymbolBitSet(underlying)
  }

  implicit def toUnderlying(symbolBitSet: SymbolBitSet): Map[Symbol, BitSet] = {
    symbolBitSet.underlying
  }

}
