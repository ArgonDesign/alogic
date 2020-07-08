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
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

// Thrown when adding a fatal message to the MessageBuffer
class FatalErrorException extends Exception

final class MessageBuffer {

  //////////////////////////////////////////////////////////////////////////////
  // Internals
  //////////////////////////////////////////////////////////////////////////////

  private val buffer = mutable.ListBuffer[Message]() // buffer storing messages

  private var errorCount = 0 // Number of Error/Fatal/ICE encountered

  //////////////////////////////////////////////////////////////////////////////
  // Get messages/status
  //////////////////////////////////////////////////////////////////////////////

  def messages: List[Message] = buffer.toList

  def hasError: Boolean = errorCount > 0

  //////////////////////////////////////////////////////////////////////////////
  // Add message to bufer
  //////////////////////////////////////////////////////////////////////////////

  def add(msg: Message): Unit = msg match {
    case _: Warning => buffer append msg
    case _: Error   => buffer append msg; errorCount += 1
    case _: Note    => buffer append msg
    case _: Fatal   => unreachable
    case _: Ice     => unreachable
  }

  //////////////////////////////////////////////////////////////////////////////
  // Create messages, add them to buffer
  //////////////////////////////////////////////////////////////////////////////

  def warning(loc: Loc, msg: Seq[String], once: Boolean = false): Unit = synchronized {
    val message = Warning(loc, msg)
    if (!once || !(buffer contains message)) {
      buffer append message
    }
  }

  def error(loc: Loc, msg: Seq[String], once: Boolean = false): Unit = synchronized {
    val message = Error(loc, msg)
    if (!once || !(buffer contains message)) {
      buffer append message
      errorCount += 1
    }
  }

  def note(loc: Loc, msg: Seq[String]): Unit = synchronized {
    buffer append Note(loc, msg)
  }

  def fatal(loc: Loc, msg: Seq[String]): Nothing = synchronized {
    buffer append Fatal(loc, msg)
    errorCount += 1
    throw new FatalErrorException
  }

  def ice(loc: Loc, msg: Seq[String]): Nothing = synchronized {
    buffer append Ice(loc, msg)
    errorCount += 1
    throw new FatalErrorException
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

  final def fatal(msg: String*): Nothing = messageBuffer.fatal(Loc.unknown, msg)

  final def ice(msg: String*): Nothing = messageBuffer.ice(Loc.unknown, msg)

  //////////////////////////////////////////////////////////////////////////////
  // Versions with a source location
  //////////////////////////////////////////////////////////////////////////////

  final def warning[T](item: T, msg: String*)(implicit ev: Locatable[T]): Unit =
    messageBuffer.warning(ev(item), msg)

  final def error[T](item: T, msg: String*)(implicit ev: Locatable[T]): Unit =
    messageBuffer.error(ev(item), msg)

  final def note[T](item: T, msg: String*)(implicit ev: Locatable[T]): Unit =
    messageBuffer.note(ev(item), msg)

  final def fatal[T](item: T, msg: String*)(implicit ev: Locatable[T]): Nothing =
    messageBuffer.fatal(ev(item), msg)

  final def ice[T](item: T, msg: String*)(implicit ev: Locatable[T]): Nothing =
    messageBuffer.ice(ev(item), msg)

  //////////////////////////////////////////////////////////////////////////////
  // Versions that discard messages identical to ones already issued
  //////////////////////////////////////////////////////////////////////////////

  final def warningOnce[T](item: T, msg: String*)(implicit loc: Locatable[T]): Unit =
    messageBuffer.warning(loc(item), msg, once = true)

  final def errorOnce[T](item: T, msg: String*)(implicit loc: Locatable[T]): Unit =
    messageBuffer.error(loc(item), msg, once = true)

  //////////////////////////////////////////////////////////////////////////////
  // Add pre-computed messages
  //////////////////////////////////////////////////////////////////////////////

  final def addMessage(message: Message): Unit = messageBuffer.add(message)

  final def addMessages(messages: IterableOnce[Message]): Unit =
    messages.iterator foreach messageBuffer.add
}
