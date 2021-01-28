////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// A data structure representing a set of (symbol, bit index) pairs in an
// efficient and convenient way
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.analysis

import com.argondesign.alogic.core.Symbols.Symbol

import scala.collection.immutable.BitSet
import scala.language.implicitConversions

final class SymbolBitSet(val underlying: Map[Symbol, BitSet]) extends AnyVal {

  private def unionSingle(single: SymbolBitSet, big: SymbolBitSet): SymbolBitSet =
    SymbolBitSet {
      val (symbol, other) = single.iterator.next()
      big.updatedWith(symbol) {
        case Some(bits) => Some(bits union other)
        case None       => Some(other)
      }
    }

  def union(that: SymbolBitSet): SymbolBitSet =
    if (that.isEmpty) {
      this
    } else if (this.isEmpty) {
      that
    } else if (that.sizeIs == 1) {
      unionSingle(that, this)
    } else if (this.sizeIs == 1) {
      unionSingle(this, that)
    } else {
      SymbolBitSet from {
        {
          underlying.iterator map {
            case (symbol, bits) =>
              symbol -> {
                that.get(symbol) match {
                  case Some(other) => bits union other
                  case None        => bits
                }
              }
          }
        } concat {
          that.underlying.iterator filterNot {
            case (symbol, _) => this contains symbol
          }
        }
      }
    }

  def intersect(that: SymbolBitSet): SymbolBitSet =
    if (this.isEmpty || that.isEmpty) {
      SymbolBitSet.empty
    } else {
      val (sml, big) = if ((this sizeCompare that) < 0) (this, that) else (that, this)
      SymbolBitSet from {
        sml.underlying.iterator map {
          case (symbol, bits) =>
            symbol -> {
              big.get(symbol) match {
                case Some(other) => bits intersect other
                case None        => BitSet.empty
              }
            }
        } filter {
          _._2.nonEmpty
        }
      }
    }

  def diff(that: SymbolBitSet): SymbolBitSet =
    if (this.isEmpty || that.isEmpty) {
      this
    } else if (that.sizeIs == 1) {
      SymbolBitSet {
        val (symbol, other) = that.iterator.next()
        underlying.updatedWith(symbol) {
          case Some(bits) =>
            val diff = bits diff other
            if (diff.isEmpty) None else Some(diff)
          case None => None
        }
      }
    } else {
      SymbolBitSet from {
        underlying.iterator map {
          case (symbol, bits) =>
            symbol -> {
              that.get(symbol) match {
                case Some(other) => bits diff other
                case None        => bits
              }
            }
        } filter {
          _._2.nonEmpty
        }
      }
    }

  override def toString: String = s"SymbolBitSet(${underlying.toString})"

}

object SymbolBitSet {

  val empty = new SymbolBitSet(Map.empty)

  def apply(symbol: Symbol, bitSet: BitSet): SymbolBitSet = new SymbolBitSet(Map(symbol -> bitSet))

  implicit def apply(underlying: Map[Symbol, BitSet]): SymbolBitSet = {
    new SymbolBitSet(underlying)
  }

  implicit def toUnderlying(symbolBitSet: SymbolBitSet): Map[Symbol, BitSet] = {
    symbolBitSet.underlying
  }

  def from(it: IterableOnce[(Symbol, BitSet)]): SymbolBitSet = SymbolBitSet(Map.from(it))

}
