////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.core.Messages._
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

final class MessageBuffer {

  //////////////////////////////////////////////////////////////////////////////
  // Internals
  //////////////////////////////////////////////////////////////////////////////

  private val buffer = mutable.ListBuffer[Message]() // buffer storing messages

  private var _hasError = false

  //////////////////////////////////////////////////////////////////////////////
  // Get messages/status
  //////////////////////////////////////////////////////////////////////////////

  // TODO: Sort
  def messages: List[Message] = buffer.toList.distinct flatMap {
    case message: MessageWithNotes => message +: message.notes
    case other                     => Seq(other)
  }

  def hasError: Boolean = _hasError

  //////////////////////////////////////////////////////////////////////////////
  // Add message to buffer
  //////////////////////////////////////////////////////////////////////////////

  def add(msg: Message): Unit = synchronized {
    msg match {
      case _: Warning                   => buffer.append(msg)
      case _: Error | _: Fatal | _: Ice => buffer.append(msg); _hasError = true
      case _: Note                      => unreachable // Must be attached to other message
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Create messages, add them to buffer
  //////////////////////////////////////////////////////////////////////////////

  def warning[T](item: T, msg: String*)(implicit locate: Locatable[T]): Unit =
    add(Warning(locate(item), msg))

  def error[T](item: T, msg: String*)(implicit locate: Locatable[T]): Unit =
    add(Error(locate(item), msg))
}
