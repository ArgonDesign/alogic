////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Wrapper around an immutable.Map or immutable.ParMap, controlled by
// cc.settings.parallel
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.util.BooleanOps.BooleanClassOps

import scala.collection.immutable.Map
import scala.collection.parallel.immutable.ParMap
import scala.collection.parallel.CollectionConverters.ImmutableMapIsParallelizable

// An immutable.Map or immutable.ParMap
final class ParOrSeqMap[K, V] private (
    private val eitherMap: Either[Map[K, V], ParMap[K, V]],
    private val forceSeq: Boolean) {
  require(forceSeq |-> eitherMap.isLeft)

  //////////////////////////////////////////////////////////////////////////////
  // Internal helpers
  //////////////////////////////////////////////////////////////////////////////

  private def wrap[U](seqMap: Map[K, V]): ParOrSeqMap[K, V] = new ParOrSeqMap(seqMap, forceSeq)
  private def wrap[U](parMap: ParMap[K, V]): ParOrSeqMap[K, V] = new ParOrSeqMap(parMap, forceSeq)

  private[core] def this(seqMap: Map[K, V], forceSeq: Boolean) =
    this(Left(seqMap), forceSeq)
  private[core] def this(parMap: ParMap[K, V], forceSeq: Boolean) =
    this(if (forceSeq) Left(parMap.seq) else Right(parMap), forceSeq)

  //////////////////////////////////////////////////////////////////////////////
  // The public constructors
  //////////////////////////////////////////////////////////////////////////////

  def this(seqMap: Map[K, V])(implicit cc: CompilerContext) =
    this(seqMap, !cc.settings.parallel)
  def this(parMap: ParMap[K, V])(implicit cc: CompilerContext) =
    this(parMap, !cc.settings.parallel)

  //////////////////////////////////////////////////////////////////////////////
  // Convert to parallel of sequential. Note asPar might not yield parallelism
  //////////////////////////////////////////////////////////////////////////////

  def asSeq: ParOrSeqMap[K, V] = eitherMap match {
    case Left(_)       => this
    case Right(parMap) => wrap(parMap.seq)
  }

  def asPar: ParOrSeqMap[K, V] = eitherMap match {
    case Left(seqMap) => if (forceSeq) this else wrap(seqMap.par)
    case Right(_)     => this
  }

  //////////////////////////////////////////////////////////////////////////////
  // The seqMap like interface
  //////////////////////////////////////////////////////////////////////////////

  def iterator: Iterator[(K, V)] = eitherMap match {
    case Left(seqMap)  => seqMap.iterator
    case Right(parMap) => parMap.iterator
  }

  def foreach[U](f: ((K, V)) => U): Unit = eitherMap match {
    case Left(seqMap)  => seqMap.foreach(f)
    case Right(parMap) => parMap.foreach(f)
  }

}

object ParOrSeqMap {

  implicit class ImmutableMapToParOrSeqMap[K, V](val subject: Map[K, V]) extends AnyVal {
    def asPar(implicit cc: CompilerContext): ParOrSeqMap[K, V] = new ParOrSeqMap(subject)
  }

}
