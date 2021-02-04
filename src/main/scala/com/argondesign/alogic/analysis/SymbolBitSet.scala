////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// A data structure representing a set of (symbol, bit index) pairs in a
// [hopefully] efficient and convenient way
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.analysis

import com.argondesign.alogic.core.Symbol

import scala.collection.immutable.HashMap

final class SymbolBitSet private (private val underlying: HashMap[Symbol, BigInt]) extends AnyVal {

  def iterator: Iterator[(Symbol, BigInt)] = underlying.iterator

  def size: Int = underlying.size

  def contains(symbol: Symbol): Boolean = underlying.contains(symbol)

  def keySet: Set[Symbol] = underlying.keySet

  def getOrElse(symbol: Symbol, default: => BigInt): BigInt = underlying.getOrElse(symbol, default)

  def updatedWith(symbol: Symbol)(f: Option[BigInt] => Option[BigInt]): SymbolBitSet =
    new SymbolBitSet(underlying.updatedWith(symbol)(f))

  def collect[T](pf: PartialFunction[(Symbol, BigInt), T]): Iterable[T] = underlying.collect(pf)

  private def unionImpl(
      large: HashMap[Symbol, BigInt],
      small: HashMap[Symbol, BigInt]
    ): SymbolBitSet = {
    // Imperative for speed
    val it = small.iterator
    var result = large
    while (it.hasNext) {
      val (symbol, addBits) = it.next()
      result = result.updatedWith(symbol) {
        case Some(oldBits) => Some(oldBits | addBits)
        case None          => Some(addBits)
      }
    }
    new SymbolBitSet(result)
  }

  def union(that: SymbolBitSet): SymbolBitSet =
    if (size >= that.size) {
      unionImpl(underlying, that.underlying)
    } else {
      unionImpl(that.underlying, underlying)
    }

  private def intersectImpl(
      large: HashMap[Symbol, BigInt],
      small: HashMap[Symbol, BigInt]
    ): SymbolBitSet = {
    // Imperative for speed
    val it = small.iterator
    var result = HashMap.empty[Symbol, BigInt]
    while (it.hasNext) {
      val (symbol, smallBits) = it.next()
      result = large.get(symbol) match {
        case Some(largeBits) =>
          val bits = largeBits & smallBits
          if (bits != 0) result.updated(symbol, bits) else result
        case None => result
      }
    }
    new SymbolBitSet(result)
  }

  def intersect(that: SymbolBitSet): SymbolBitSet =
    if (size >= that.size) {
      intersectImpl(underlying, that.underlying)
    } else {
      intersectImpl(that.underlying, underlying)
    }

  def diff(that: SymbolBitSet): SymbolBitSet = {
    // Imperative for speed
    val it = that.iterator
    var result = underlying
    while (it.hasNext) {
      val (symbol, remBits) = it.next()
      result = result.updatedWith(symbol) {
        case Some(oldBits) =>
          val bits = oldBits &~ remBits
          if (bits != 0) Some(bits) else None
        case None => None
      }
    }
    new SymbolBitSet(result)
  }

  override def toString: String = iterator.mkString("SymbolBitSet(", ", ", ")")
}

object SymbolBitSet {
  val empty = new SymbolBitSet(HashMap.empty)

  def apply(pairs: (Symbol, BigInt)*): SymbolBitSet = new SymbolBitSet(HashMap(pairs: _*))

  def from(underlying: HashMap[Symbol, BigInt]): SymbolBitSet = new SymbolBitSet(underlying)

  def from(it: IterableOnce[(Symbol, BigInt)]): SymbolBitSet = new SymbolBitSet(HashMap.from(it))
}
