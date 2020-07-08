////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Compiler message representation
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import scala.io.AnsiColor

object Messages {

  sealed trait MessageCategory
  case object WarningCategory extends MessageCategory
  case object ErrorCategory extends MessageCategory
  case object NoteCategory extends MessageCategory
  case object FatalCategory extends MessageCategory
  case object IceCategory extends MessageCategory

  def ansiColorMap: Map[MessageCategory, (String, String)] = Map(
    Messages.WarningCategory -> ((AnsiColor.MAGENTA + AnsiColor.BOLD, AnsiColor.RESET)),
    Messages.ErrorCategory -> ((AnsiColor.RED + AnsiColor.BOLD, AnsiColor.RESET)),
    Messages.NoteCategory -> ((AnsiColor.CYAN + AnsiColor.BOLD, AnsiColor.RESET)),
    Messages.FatalCategory -> ((AnsiColor.RED + AnsiColor.BOLD, AnsiColor.RESET)),
    Messages.IceCategory -> ((AnsiColor.RED + AnsiColor.BOLD, AnsiColor.RESET))
  )

  def emptyColorMap: Map[MessageCategory, (String, String)] = Map.empty

  sealed abstract class Message(val category: MessageCategory) {
    val loc: Loc
    val msg: Seq[String]

    private def render(highlightStart: String, highlightEnd: String): String = {
      val prefix = {
        val tag = category match {
          case WarningCategory => "WARNING"
          case ErrorCategory   => "ERROR"
          case NoteCategory    => "NOTE"
          case FatalCategory   => "FATAL"
          case IceCategory     => "INTERNAL COMPILER ERROR"
        }
        loc match {
          case Loc.unknown => s"$tag: "
          case _           => s"${loc.prefix}: $tag: "
        }
      }
      val lines = category match {
        case IceCategory => msg.iterator.concat(Iterator.single("Please file a bug report"))
        case _           => msg.iterator
      }
      val context = loc match {
        case Loc.unknown => ""
        case _           => loc.context(highlightStart, highlightEnd)
      }
      (lines mkString (prefix, "\n" + prefix + "... ", "\n")) + context
    }

    def render: String = render("", "")

    def render(colorMap: Map[MessageCategory, (String, String)]): String =
      colorMap.get(category) match {
        case Some((highlightStart, highlightEnd)) => render(highlightStart, highlightEnd)
        case None                                 => render("", "")
      }

  }

  // Warnings are informative messages about issues that the compiler can
  // recover from, and still produce functional output.
  case class Warning(loc: Loc, msg: Seq[String]) extends Message(WarningCategory)

  object Warning {
    def apply(msg: String*): Warning = Warning(Loc.unknown, msg)
    def apply[T](item: T, msg: String*)(implicit ev: Locatable[T]): Warning = Warning(ev(item), msg)
  }

  // Errors indicate situations where the compiler can still make forward
  // progress, but the generated output would not be functional. In this case
  // the compiler carries on trying to generate as many messages as possible,
  // but the final exit status of the program will indicate failure.
  case class Error(loc: Loc, msg: Seq[String]) extends Message(ErrorCategory)

  object Error {
    def apply(msg: String*): Error = Error(Loc.unknown, msg)
    def apply[T](item: T, msg: String*)(implicit ev: Locatable[T]): Error = Error(ev(item), msg)
  }

  // Note messages provide additional information, usually emitted right after
  // the message they augment.
  case class Note(loc: Loc, msg: Seq[String]) extends Message(NoteCategory)

  object Note {
    def apply(msg: String*): Note = Note(Loc.unknown, msg)
    def apply[T](item: T, msg: String*)(implicit ev: Locatable[T]): Note = Note(ev(item), msg)
  }

  // Fatal indicates situations where the compiler cannot make forward
  // progress. The first fatal message will cause the compiler to terminate.
  case class Fatal(loc: Loc, msg: Seq[String]) extends Message(FatalCategory)

  object Fatal {
    def apply(msg: String*): Fatal = Fatal(Loc.unknown, msg)
    def apply[T](item: T, msg: String*)(implicit ev: Locatable[T]): Fatal = Fatal(ev(item), msg)
  }

  // Internal compiler error indicates a programming error in the compiler.
  case class Ice(loc: Loc, msg: Seq[String]) extends Message(IceCategory)

  object Ice {
    def apply(msg: String*): Ice = Ice(Loc.unknown, msg)
    def apply[T](item: T, msg: String*)(implicit ev: Locatable[T]): Ice = Ice(ev(item), msg)
  }

}
