////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Provide extension methods for Iterator
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.util

object IteratorOps {

  // This just allows nicer syntax: Iterator.when(cond)._
  final class IteratorWhen(private val cond: Boolean) extends AnyVal {
    // If condition is true, return an iterator yielding the given single
    // element, otherwise return an empty iterator
    def thenSingle[T](elem: => T): Iterator[T] = if (cond) Iterator.single(elem) else Iterator.empty

    // If condition is true, return the given iterator, otherwise return an
    // empty iterator
    def thenIterator[T](iterator: => Iterator[T]): Iterator[T] =
      if (cond) iterator else Iterator.empty
  }

  implicit final class IteratorObjectOps(private val unused: Iterator.type) extends AnyVal {
    def when(cond: Boolean): IteratorWhen = new IteratorWhen(cond)
  }

}
