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

  def top: T = _stack.head

  def isEmpty: Boolean = _stack.isEmpty

  def nonEmpty: Boolean = _stack.nonEmpty

  def push(item: T): Unit = {
    _stack = item :: _stack
  }

  def pop(): Unit = {
    _stack = _stack.tail
  }
}

object Stack {

  def apply[T](): Stack[T] = new Stack[T]
}
