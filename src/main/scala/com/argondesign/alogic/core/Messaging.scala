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

import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.util.unreachable
import org.antlr.v4.runtime.ParserRuleContext

import scala.collection.mutable
import scala.io.AnsiColor

sealed trait Message {
  val locOpt: Option[Loc]
  protected val cat: String
  protected val color: String
  val msg: Seq[String]

  lazy val loc: Loc = locOpt match {
    case Some(loc) => loc
    case None      => unreachable
  }

  private def string(colorOpt: Option[String]): String = {
    val prefix = locOpt match {
      case Some(loc) => s"${loc.prefix}: $cat: "
      case None      => s"$cat: "
    }
    val context = locOpt match {
      case Some(loc) => "\n" + loc.context(colorOpt)
      case None      => ""
    }
    (msg mkString (prefix, "\n" + prefix + "... ", "")) + context
  }

  def render: String = render(colorize = false)
  def render(colorize: Boolean): String = string(Option.when(colorize)(color))
}

// Warnings are informative messages about issues that the compiler
// can recover from, and still produce functional output.
case class Warning(msg: Seq[String], locOpt: Option[Loc]) extends Message {
  val cat: String = "WARNING"
  val color: String = AnsiColor.MAGENTA + AnsiColor.BOLD
}

// Errors indicate situations where the compiler can still make
// forward progress, but the generated output would not be functional.
// In this case the compiler carries on trying to generate as many
// messages as possible, but the final exit status of the program
// will indicate failure.
case class Error(msg: Seq[String], locOpt: Option[Loc]) extends Message {
  val cat: String = "ERROR"
  val color: String = AnsiColor.RED + AnsiColor.BOLD
}

// Fatal indicates situations where the compiler cannot make forward
// progress. The first fatal message will cause the program to exit.
case class Fatal(msg: Seq[String], locOpt: Option[Loc]) extends Message {
  val cat: String = "FATAL"
  val color: String = AnsiColor.RED + AnsiColor.BOLD
}

// Internal compiler error indicates a programming error in the compiler
// please file a bug report
case class ICE(initialMsg: Seq[String], locOpt: Option[Loc]) extends Message {
  val cat: String = "INTERNAL COMPILER ERROR"
  val msg: Seq[String] = initialMsg ++ Seq("Please file a bug report")
  val color: String = AnsiColor.CYAN + AnsiColor.BOLD
}

class FatalErrorException extends Exception

final class MessageBuffer {

  //////////////////////////////////////////////////////////////////////////////
  // Internals
  //////////////////////////////////////////////////////////////////////////////

  private val buffer = mutable.ListBuffer[Message]() // buffer storing messages

  private var errorCount = 0 // Number of Error/Fatal/ICE encountered

  private def addDistinct(message: Message): Unit = {
    if (!(buffer contains message)) {
      buffer append message
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Get messages/status
  //////////////////////////////////////////////////////////////////////////////

  def messages: List[Message] = buffer.toList

  def hasError: Boolean = errorCount > 0

  //////////////////////////////////////////////////////////////////////////////
  // Create messages, add them to buffer
  //////////////////////////////////////////////////////////////////////////////

  def warning(locOpt: Option[Loc], msg: String*): Unit = synchronized {
    addDistinct(Warning(msg, locOpt))
  }

  def error(locOpt: Option[Loc], msg: String*): Unit = synchronized {
    addDistinct(Error(msg, locOpt))
    errorCount += 1
  }

  def fatal(locOpt: Option[Loc], msg: String*): Nothing = synchronized {
    addDistinct(Fatal(msg, locOpt))
    errorCount += 1
    throw new FatalErrorException
  }

  def ice(locOpt: Option[Loc], msg: String*): Nothing = synchronized {
    addDistinct(ICE(msg, locOpt))
    errorCount += 1
    throw new FatalErrorException
  }

}

trait Messaging { self: CompilerContext =>

  //////////////////////////////////////////////////////////////////////////////
  // Util
  //////////////////////////////////////////////////////////////////////////////

  def colorOpt(ansiColor: String): Option[String] = Option.when(settings.colorize)(ansiColor)

  //////////////////////////////////////////////////////////////////////////////
  // Get messages/status
  //////////////////////////////////////////////////////////////////////////////

  final def messages: List[Message] = messageBuffer.messages

  final def hasError: Boolean = messageBuffer.hasError

  //////////////////////////////////////////////////////////////////////////////
  // Versions without source location
  //////////////////////////////////////////////////////////////////////////////

  final def warning(msg: String*): Unit = messageBuffer.warning(None, msg: _*)

  final def error(msg: String*): Unit = messageBuffer.error(None, msg: _*)

  final def fatal(msg: String*): Nothing = messageBuffer.fatal(None, msg: _*)

  final def ice(msg: String*): Nothing = messageBuffer.ice(None, msg: _*)

  //////////////////////////////////////////////////////////////////////////////
  // Versions that take a source location
  //////////////////////////////////////////////////////////////////////////////

  final def warning(loc: Loc, msg: String*): Unit = messageBuffer.warning(Some(loc), msg: _*)

  final def error(loc: Loc, msg: String*): Unit = messageBuffer.error(Some(loc), msg: _*)

  final def fatal(loc: Loc, msg: String*): Nothing = messageBuffer.fatal(Some(loc), msg: _*)

  final def ice(loc: Loc, msg: String*): Nothing = messageBuffer.ice(Some(loc), msg: _*)

  //////////////////////////////////////////////////////////////////////////////
  // Versions that take an Antlr4 token/parse tree node for location info
  //////////////////////////////////////////////////////////////////////////////

  final def warning(ctx: ParserRuleContext, msg: String*): Unit = warning(ctx.loc, msg: _*)

  final def error(ctx: ParserRuleContext, msg: String*): Unit = error(ctx.loc, msg: _*)

  final def fatal(ctx: ParserRuleContext, msg: String*): Nothing = fatal(ctx.loc, msg: _*)

  final def ice(ctx: ParserRuleContext, msg: String*): Nothing = ice(ctx.loc, msg: _*)

  //////////////////////////////////////////////////////////////////////////////
  // Versions that take anything that has a location
  //////////////////////////////////////////////////////////////////////////////

  final def warning(item: Locationed, msg: String*): Unit = warning(item.loc, msg: _*)

  final def error(item: Locationed, msg: String*): Unit = error(item.loc, msg: _*)

  final def fatal(item: Locationed, msg: String*): Nothing = fatal(item.loc, msg: _*)

  final def ice(item: Locationed, msg: String*): Nothing = ice(item.loc, msg: _*)

  //////////////////////////////////////////////////////////////////////////////
  // Versions that take a symbol
  //////////////////////////////////////////////////////////////////////////////

  final def warning(symbol: Symbol, msg: String*): Unit = warning(symbol.loc, msg: _*)

  final def error(symbol: Symbol, msg: String*): Unit = error(symbol.loc, msg: _*)

  final def fatal(symbol: Symbol, msg: String*): Nothing = fatal(symbol.loc, msg: _*)

  final def ice(symbol: Symbol, msg: String*): Nothing = ice(symbol.loc, msg: _*)
}
