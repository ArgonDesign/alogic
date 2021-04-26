////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Wrapper around an immutable.Iterable or immutable.ParIterable, controleld by
// cc.settings.parallel
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.util.BooleanOps.BooleanClassOps

import scala.collection.immutable.Iterable
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.collection.parallel.immutable.ParIterable

// An immutable.Iterable or immutable.ParIterable
final class ParOrSeqIterable[T] private (
    private val eitherIterable: Either[Iterable[T], ParIterable[T]],
    private val forceSeq: Boolean) {
  require(forceSeq |-> eitherIterable.isLeft)

  //////////////////////////////////////////////////////////////////////////////
  // Internal helpers
  //////////////////////////////////////////////////////////////////////////////

  private def wrap[U](iterable: Iterable[U]): ParOrSeqIterable[U] =
    new ParOrSeqIterable(iterable, forceSeq)
  private def wrap[U](parIterable: ParIterable[U]): ParOrSeqIterable[U] =
    new ParOrSeqIterable(parIterable, forceSeq)

  private def this(iterable: Iterable[T], forceSeq: Boolean) =
    this(Left(iterable), forceSeq)
  private def this(parIterable: ParIterable[T], forceSeq: Boolean) =
    this(if (forceSeq) Left(parIterable.seq) else Right(parIterable), forceSeq)

  //////////////////////////////////////////////////////////////////////////////
  // The public constructors
  //////////////////////////////////////////////////////////////////////////////

  def this(iterable: Iterable[T])(implicit cc: CompilerContext) =
    this(iterable, !cc.settings.parallel)
  def this(parIterable: ParIterable[T])(implicit cc: CompilerContext) =
    this(parIterable, !cc.settings.parallel)

  //////////////////////////////////////////////////////////////////////////////
  // Convert to parallel of sequential. Note asPar might not yield parallelism
  //////////////////////////////////////////////////////////////////////////////

  def asSeq: ParOrSeqIterable[T] = eitherIterable match {
    case Left(_)            => this
    case Right(parIterable) => wrap(parIterable.seq)
  }

  def asPar: ParOrSeqIterable[T] = eitherIterable match {
    case Left(iterable) => if (forceSeq) this else wrap(iterable.par)
    case Right(_)       => this
  }

  //////////////////////////////////////////////////////////////////////////////
  // The iterable like interface
  //////////////////////////////////////////////////////////////////////////////

  def iterator: Iterator[T] = eitherIterable match {
    case Left(iterable)     => iterable.iterator
    case Right(parIterable) => parIterable.iterator
  }

  def map[U](f: T => U): ParOrSeqIterable[U] = eitherIterable match {
    case Left(iterable)     => wrap(iterable.map(f))
    case Right(parIterable) => wrap(parIterable.map(f))
  }

  def flatMap[U](f: T => IterableOnce[U]): ParOrSeqIterable[U] = eitherIterable match {
    case Left(iterable)     => wrap(iterable.flatMap(f))
    case Right(parIterable) => wrap(parIterable.flatMap(f))
  }

  def filter[U](f: T => Boolean): ParOrSeqIterable[T] = eitherIterable match {
    case Left(iterable)     => wrap(iterable.filter(f))
    case Right(parIterable) => wrap(parIterable.filter(f))
  }

  def filterNot[U](f: T => Boolean): ParOrSeqIterable[T] = eitherIterable match {
    case Left(iterable)     => wrap(iterable.filterNot(f))
    case Right(parIterable) => wrap(parIterable.filterNot(f))
  }

  def foreach[U](f: T => U): Unit = eitherIterable match {
    case Left(iterable)     => iterable.foreach(f)
    case Right(parIterable) => parIterable.foreach(f)
  }

  def collect[U](pf: PartialFunction[T, U]): ParOrSeqIterable[U] = eitherIterable match {
    case Left(iterable)     => wrap(iterable.collect(pf))
    case Right(parIterable) => wrap(parIterable.collect(pf))
  }

  def exists[U](f: T => Boolean): Boolean = eitherIterable match {
    case Left(iterable)     => iterable.exists(f)
    case Right(parIterable) => parIterable.exists(f)
  }

  def groupBy[K](f: T => K): ParOrSeqMap[K, ParOrSeqIterable[T]] = eitherIterable match {
    case Left(iterable) =>
      new ParOrSeqMap(iterable.groupBy(f).map { case (k, v) => k -> wrap(v) }, forceSeq)
    case Right(parIterable) =>
      new ParOrSeqMap(parIterable.groupBy(f).map { case (k, v) => k -> wrap(v) }, forceSeq)
  }

  def tapEach[U](f: T => U): this.type = eitherIterable match {
    case Left(iterable)     => iterable.foreach(f); this
    case Right(parIterable) => parIterable.foreach(f); this
  }

  def ++(that: IterableOnce[T]): ParOrSeqIterable[T] = eitherIterable match {
    case Left(iterable)     => wrap(iterable ++ that)
    case Right(parIterable) => wrap(parIterable ++ that)
  }

  def ++(that: ParOrSeqIterable[T]): ParOrSeqIterable[T] = eitherIterable match {
    case Left(iterable)     => wrap(iterable ++ that.iterator)
    case Right(parIterable) => wrap(parIterable ++ that.iterator)
  }

  def +(item: T): ParOrSeqIterable[T] = ++(Iterator.single(item))

  def size: Int = eitherIterable.fold(_.size, _.size)

}

object ParOrSeqIterable {

  implicit class ImmutableIterableToParOrSeqIterable[T](val subject: Iterable[T]) extends AnyVal {
    def asPar(implicit cc: CompilerContext): ParOrSeqIterable[T] = new ParOrSeqIterable(subject)
  }

  def empty[T](implicit cc: CompilerContext): ParOrSeqIterable[T] =
    new ParOrSeqIterable[T](Iterable.empty[T])

}
