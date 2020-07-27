////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//
// A data structure representing a map from symbols to their value as
// expressions
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.ast.Trees.Expr
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.immutable

sealed abstract class Bindings extends Iterable[(Symbol, Expr)] {
  def get(symbol: Symbol): Option[Expr]
  def +(pair: (Symbol, Expr)): Bindings
  def ++(pairs: IterableOnce[(Symbol, Expr)]): Bindings

  override def filterNot(pred: ((Symbol, Expr)) => Boolean): Bindings = unreachable
  def removedAll(iterable: Set[Symbol]): Bindings
  def valuesIterator: Iterator[Expr] = iterator map { _._2 }

  def fastIntersect(f: Bindings => Option[IterableOnce[Bindings]]): Option[Bindings] = {
    val itOpt: Option[Iterator[FastIntersectBindings]] = {
      val fib = FastIntersectBindings(this, Map.empty, Set.empty)
      f(fib).map(_.iterator.map(_.asInstanceOf[FastIntersectBindings]))
    }

    itOpt map { it =>
      if (it.isEmpty) {
        Bindings.empty
      } else {
        it reduceLeft [FastIntersectBindings] {
          case (a: FastIntersectBindings, b: FastIntersectBindings) =>
            val FastIntersectBindings(oldA, addA, remA) = a
            val FastIntersectBindings(oldB, addB, remB) = b
            assert(oldA eq this)
            assert(oldB eq this)
            val newAdd = {
              // Intersection of 'add' Maps, using filter on the shorter
              val (addS, remS, addL, remL) = if (addA.knownSize <= addB.knownSize) {
                (addA, remA, addB, remB)
              } else {
                (addB, remB, addA, remA)
              }
              addS filter { case (k, v) => !remS(k) && (addL.get(k) contains v) && !remL(k) }
            }
            FastIntersectBindings(
              this,
              newAdd,
              remA ++ remB ++ addA.keysIterator ++ addB.keysIterator -- newAdd.keysIterator
            )
        } match {
          case FastIntersectBindings(BasicBindings(underlying), add, rem) =>
            BasicBindings((underlying.iterator concat add).filterNot {
              case (k, _) => rem(k)
            }.toMap)
          case FastIntersectBindings(FastIntersectBindings(old, add, rem), add2, rem2) =>
            FastIntersectBindings(old, add concat add2, rem -- add2.keysIterator ++ rem2)
        }
      }
    }
  }

  def contains(symbol: Symbol): Boolean = get(symbol).isDefined
}

final case class BasicBindings(underlying: immutable.Map[Symbol, Expr]) extends Bindings {

  def get(symbol: Symbol): Option[Expr] = underlying.get(symbol)

  def +(pair: (Symbol, Expr)): Bindings = BasicBindings(underlying + pair)

  def ++(pairs: IterableOnce[(Symbol, Expr)]): Bindings = BasicBindings(underlying ++ pairs)

  override def filterNot(pred: ((Symbol, Expr)) => Boolean): Bindings = BasicBindings(
    underlying filterNot pred
  )

  def removedAll(set: Set[Symbol]): Bindings =
    BasicBindings(underlying.view.filterKeys(k => !set(k)).toMap)

  def iterator: Iterator[(Symbol, Expr)] = underlying.iterator
}

final case class FastIntersectBindings(
    old: Bindings, // Baseline bindings
    add: immutable.Map[Symbol, Expr], // Added bindings
    rem: immutable.Set[Symbol] // Removed bindings (applies to 'add' as well)
  ) extends Bindings {

  def get(symbol: Symbol): Option[Expr] =
    if (rem(symbol)) {
      None
    } else {
      add.get(symbol) orElse old.get(symbol)
    }

  def +(pair: (Symbol, Expr)): Bindings =
    if (get(pair._1) contains pair._2) {
      this
    } else {
      FastIntersectBindings(old, add + pair, rem - pair._1)
    }

  def ++(pairs: IterableOnce[(Symbol, Expr)]): Bindings = {
    @tailrec
    def loop(
        it: Iterator[(Symbol, Expr)],
        add: immutable.Map[Symbol, Expr],
        rem: immutable.Set[Symbol]
      ): (immutable.Map[Symbol, Expr], immutable.Set[Symbol]) =
      if (it.hasNext) {
        val pair = it.next
        if (get(pair._1) contains pair._2) {
          loop(it, add, rem)
        } else {
          loop(it, add + pair, rem - pair._1)
        }
      } else {
        (add, rem)
      }

    val (a, r) = loop(pairs.iterator, add, rem)
    FastIntersectBindings(old, a, r)
  }

  override def filterNot(pred: ((Symbol, Expr)) => Boolean): Bindings = {
    val remFromAdd = add.iterator collect { case pair if !rem(pair._1) && pred(pair) => pair._1 }
    val remFromOld = old.iterator collect { case pair if !rem(pair._1) && pred(pair) => pair._1 }
    FastIntersectBindings(old, add, rem ++ (remFromAdd concat remFromOld))
  }

  def removedAll(set: Set[Symbol]): Bindings =
    FastIntersectBindings(old, add, rem union set)

  private lazy val storedIterable = Iterable from {
    val remFromOld = rem ++ add.keysIterator
    add.iterator.filterNot { case (k, _) => rem(k) } concat
      old.iterator.filterNot { case (k, _) => remFromOld(k) }
  }

  def iterator: Iterator[(Symbol, Expr)] = storedIterable.iterator

}

object Bindings {

  val empty: Bindings = BasicBindings(Map.empty)

  def from(pairs: IterableOnce[(Symbol, Expr)]): Bindings = BasicBindings(Map.from(pairs))

}
