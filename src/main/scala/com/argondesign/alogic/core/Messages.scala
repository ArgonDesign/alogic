////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Compiler message representation
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.ast.Trees.Desc

import scala.io.AnsiColor

object Messages {

  sealed abstract class MessageCategory(val ord: Int)
  case object WarningCategory extends MessageCategory(0)
  case object ErrorCategory extends MessageCategory(1)
  case object NoteCategory extends MessageCategory(2)
  case object FatalCategory extends MessageCategory(3)
  case object IceCategory extends MessageCategory(4)

  implicit val messageCategoryOrdering: Ordering[MessageCategory] = {
    (x: MessageCategory, y: MessageCategory) => x.ord compare y.ord
  }

  def ansiColorMap: Map[MessageCategory, (String, String)] = Map(
    Messages.WarningCategory -> ((AnsiColor.MAGENTA + AnsiColor.BOLD, AnsiColor.RESET)),
    Messages.ErrorCategory -> ((AnsiColor.RED + AnsiColor.BOLD, AnsiColor.RESET)),
    Messages.NoteCategory -> ((AnsiColor.CYAN + AnsiColor.BOLD, AnsiColor.RESET)),
    Messages.FatalCategory -> ((AnsiColor.RED + AnsiColor.BOLD, AnsiColor.RESET)),
    Messages.IceCategory -> ((AnsiColor.RED + AnsiColor.BOLD, AnsiColor.RESET))
  )

  def emptyColorMap: Map[MessageCategory, (String, String)] = Map.empty

  sealed trait Message {
    val category: MessageCategory
    val loc: Loc
    val msg: Seq[String]

    final private def context(highlightStart: String, highlightEnd: String): String =
      loc.context(highlightStart, highlightEnd)

    final def context: String = context("", "")

    final private def render(highlightStart: String, highlightEnd: String): String = {
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
      lines.mkString(prefix, System.lineSeparator + prefix + "... ", System.lineSeparator) +
        context(highlightStart, highlightEnd)
    }

    final def render(colorMap: Map[MessageCategory, (String, String)]): String =
      colorMap.get(category) match {
        case Some((highlightStart, highlightEnd)) => render(highlightStart, highlightEnd)
        case None                                 => render("", "")
      }

  }

  //////////////////////////////////////////////////////////////////////////////
  // Note message
  //////////////////////////////////////////////////////////////////////////////

  // Note messages provide additional information, attached to another message
  case class Note(loc: Loc, msg: Seq[String]) extends Message {
    override val category: MessageCategory = NoteCategory
  }

  object Note {
    def apply(msg: String*): Note = Note(Loc.unknown, msg)
    def apply[T](item: T, msg: String*)(implicit ev: Locatable[T]): Note = Note(ev(item), msg)
    def definedHere(desc: Desc): Note = Note(desc, s"'${desc.symbol.name}' is defined here")
  }

  sealed trait MessageWithNotes extends Message {
    val notes: Seq[Note]

    final def withNote(note: Note): MessageWithNotes = this match {
      // $COVERAGE-OFF$ Trivial but not all might be used
      case m: Warning => m.copy(notes = notes :+ note)
      case m: Error   => m.copy(notes = notes :+ note)
      case m: Fatal   => m.copy(notes = notes :+ note)
    }

    final def withNotes(notes: IterableOnce[Note]): MessageWithNotes = this match {
      // $COVERAGE-OFF$ Trivial but not all might be used
      case m: Warning => m.copy(notes = this.notes concat notes)
      case m: Error   => m.copy(notes = this.notes concat notes)
      case m: Fatal   => m.copy(notes = this.notes concat notes)
      // $COVERAGE-ON$
    }

  }

  //////////////////////////////////////////////////////////////////////////////
  // Regular messages
  //////////////////////////////////////////////////////////////////////////////

  // Warnings are informative messages about issues that the compiler can
  // recover from, and still produce functional output.
  case class Warning(loc: Loc, msg: Seq[String], notes: Seq[Note] = Seq.empty)
      extends MessageWithNotes {
    override val category: MessageCategory = WarningCategory
  }

  object Warning {
    def apply(msg: String*): Warning = Warning(Loc.unknown, msg)
    def apply[T](item: T, msg: String*)(implicit ev: Locatable[T]): Warning = Warning(ev(item), msg)
  }

  // Errors indicate situations where the compiler can still make forward
  // progress, but the generated output would not be functional. In this case
  // the compiler carries on trying to generate as many messages as possible,
  // but the final exit status of the program will indicate failure.
  case class Error(loc: Loc, msg: Seq[String], notes: Seq[Note] = Seq.empty)
      extends MessageWithNotes {
    override val category: MessageCategory = ErrorCategory
  }

  object Error {
    def apply(msg: String*): Error = Error(Loc.unknown, msg)
    def apply[T](item: T, msg: String*)(implicit ev: Locatable[T]): Error = Error(ev(item), msg)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Fatal messages - these are Throwable
  //////////////////////////////////////////////////////////////////////////////

  // Fatal indicates situations where the compiler cannot make forward
  // progress. The first fatal message will cause the compiler to terminate.
  case class Fatal(loc: Loc, msg: Seq[String], notes: Seq[Note] = Seq.empty)
      extends Throwable
      with MessageWithNotes {
    override val category: MessageCategory = FatalCategory
  }

  object Fatal {
    def apply(msg: String*): Fatal = Fatal(Loc.unknown, msg)
    def apply[T](item: T, msg: String*)(implicit ev: Locatable[T]): Fatal = Fatal(ev(item), msg)
  }

  // Internal compiler error indicates a programming error in the compiler.
  case class Ice(loc: Loc, msg: Seq[String]) extends Throwable with Message {
    override val category: MessageCategory = IceCategory
  }

  object Ice {
    def apply(msg: String*): Ice = Ice(Loc.unknown, msg)
    def apply[T](item: T, msg: String*)(implicit ev: Locatable[T]): Ice = Ice(ev(item), msg)
  }

}
