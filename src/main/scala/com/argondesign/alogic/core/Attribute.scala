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
// A mutable attribute
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

class Attribute[T] {
  private var store: Option[T] = None

  def value: T = store.get

  def get: Option[T] = store

  def set(v: T): Unit = store = Some(v)

  def append[E](v: E)(implicit a: Attribute.Appendable[E, T]): Unit = store match {
    case Some(coll) => store = Some(a.append(coll, v))
    case None       => store = Some(a.create(v))
  }

  def clear(): Unit = store = None

  def isSet = store.isDefined

  def update(that: Attribute[T]): Unit = store = that.store
}

object Attribute {
  abstract class Appendable[E, T] {
    def create(elem: E): T
    def append(coll: T, elem: E): T
  }

  implicit def appendableList[E] = new Appendable[E, List[E]] {
    def create(elem: E) = List(elem)
    def append(coll: List[E], elem: E) = coll ::: elem :: Nil
  }
}
