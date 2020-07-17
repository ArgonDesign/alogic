////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Compiler message handling
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.core.Messages._

import scala.collection.mutable

final class MessageBuffer {

  //////////////////////////////////////////////////////////////////////////////
  // Internals
  //////////////////////////////////////////////////////////////////////////////

  private val buffer = mutable.ListBuffer[Message]() // buffer storing messages

  private var _hasError = false

  private def append(message: Message, once: Boolean): Unit =
    if (!once || !(buffer contains message)) {
      buffer append message
    }

  //////////////////////////////////////////////////////////////////////////////
  // Get messages/status
  //////////////////////////////////////////////////////////////////////////////

  def messages: List[Message] = buffer.toList

  def hasError: Boolean = _hasError

  //////////////////////////////////////////////////////////////////////////////
  // Add message to bufer
  //////////////////////////////////////////////////////////////////////////////

  def add(msg: Message): Unit = msg match {
    case _: Warning => buffer append msg
    case _: Error   => buffer append msg; _hasError = true
    case _: Note    => buffer append msg
    case _: Fatal   => buffer append msg; _hasError = true
    case _: Ice     => buffer append msg; _hasError = true
  }

  //////////////////////////////////////////////////////////////////////////////
  // Create messages, add them to buffer
  //////////////////////////////////////////////////////////////////////////////

  def warning(loc: Loc, msg: Seq[String], once: Boolean = false): Unit = synchronized {
    append(Warning(loc, msg), once)
  }

  def error(loc: Loc, msg: Seq[String], once: Boolean = false): Unit = synchronized {
    append(Error(loc, msg), once)
    _hasError = true
  }

  def note(loc: Loc, msg: Seq[String], once: Boolean = false): Unit = synchronized {
    append(Note(loc, msg), once)
  }

}

trait Messaging { self: CompilerContext =>

  //////////////////////////////////////////////////////////////////////////////
  // Get messages/status
  //////////////////////////////////////////////////////////////////////////////

  final def messages: List[Message] = messageBuffer.messages

  final def hasError: Boolean = messageBuffer.hasError

  //////////////////////////////////////////////////////////////////////////////
  // Versions without source location
  //////////////////////////////////////////////////////////////////////////////

  final def warning(msg: String*): Unit = messageBuffer.warning(Loc.unknown, msg)

  final def error(msg: String*): Unit = messageBuffer.error(Loc.unknown, msg)

  final def note(msg: String*): Unit = messageBuffer.note(Loc.unknown, msg)

  //////////////////////////////////////////////////////////////////////////////
  // Versions with a source location
  //////////////////////////////////////////////////////////////////////////////

  final def warning[T](item: T, msg: String*)(implicit ev: Locatable[T]): Unit =
    messageBuffer.warning(ev(item), msg)

  final def error[T](item: T, msg: String*)(implicit ev: Locatable[T]): Unit =
    messageBuffer.error(ev(item), msg)

  final def note[T](item: T, msg: String*)(implicit ev: Locatable[T]): Unit =
    messageBuffer.note(ev(item), msg)

  //////////////////////////////////////////////////////////////////////////////
  // Versions that discard messages identical to ones already issued
  //////////////////////////////////////////////////////////////////////////////

  final def warningOnce[T](item: T, msg: String*)(implicit loc: Locatable[T]): Unit =
    messageBuffer.warning(loc(item), msg, once = true)

  final def errorOnce[T](item: T, msg: String*)(implicit loc: Locatable[T]): Unit =
    messageBuffer.error(loc(item), msg, once = true)

  final def noteOnce[T](item: T, msg: String*)(implicit loc: Locatable[T]): Unit =
    messageBuffer.note(loc(item), msg, once = true)

  //////////////////////////////////////////////////////////////////////////////
  // Add pre-computed messages
  //////////////////////////////////////////////////////////////////////////////

  final def addMessage(message: Message): Unit = messageBuffer.add(message)
}
