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
// mutable.Stack in the Scala standard library is deprecated, but we use stacks
// quite often in traversals, so we roll our own with a minimal API.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.lib

class Stack[T] {
  private[this] var _stack: List[T] = Nil

  def push(item: T): this.type = {
    _stack = item :: _stack
    this
  }

  def pop(): this.type = {
    _stack = _stack.tail
    this
  }

  def top: T = _stack.head

  def isEmpty: Boolean = _stack.isEmpty

  def nonEmpty: Boolean = _stack.nonEmpty

  def depth: Int = _stack.length
}

object Stack {

  def apply[T](): Stack[T] = new Stack[T]

  def apply[T](item: T): Stack[T] = Stack[T]() push item
}
