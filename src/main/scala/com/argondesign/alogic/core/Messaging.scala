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

import scala.collection.mutable
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.util.unreachable
import org.antlr.v4.runtime.ParserRuleContext

import scala.io.AnsiColor

sealed abstract trait Message {
  protected val lop: Option[Loc]
  protected val cat: String
  protected val color: String
  val msg: Seq[String]

  lazy val loc: Loc = lop match {
    case Some(loc) => loc
    case None      => unreachable
  }

  def string(implicit cc: CompilerContext): String = {
    val prefix = lop match {
      case Some(loc) => s"${loc.prefix}: ${cat}: "
      case None      => s"${cat}: "
    }
    val context = lop match {
      case Some(loc) => "\n" + loc.context(color)
      case None      => ""
    }
    (msg mkString (prefix, "\n" + prefix + "... ", "")) + context
  }
}

// Warnings are informative messages about issues that the compiler
// can recover from, and still produce functional output.
case class Warning(msg: Seq[String], lop: Option[Loc] = None) extends Message {
  val cat = "WARNING"
  val color = AnsiColor.MAGENTA
}

// Errors indicate situations where the compiler can still make
// forward progress, but the generated output would not be functional.
// In this case the compiler carries on trying to generate as many
// messages as possible, but the final exit status of the program
// will indicate failure.
case class Error(msg: Seq[String], lop: Option[Loc] = None) extends Message {
  val cat = "ERROR"
  val color = AnsiColor.RED
}

// Fatal indicates situations where the compiler cannot make forward
// progress. The first fatal message will cause the program to exit.
case class Fatal(msg: Seq[String], lop: Option[Loc] = None) extends Message {
  val cat = "FATAL"
  val color = AnsiColor.RED
}

// Internal compiler error indicates a programming error in the compiler
// please file a bug report
case class ICE(initialMsg: Seq[String], lop: Option[Loc] = None) extends Message {
  val cat = "INTERNAL COMPILER ERROR"
  val msg = initialMsg ++ Seq("Please file a bug report")
  val color = AnsiColor.CYAN
}

case class FatalErrorException(cc: CompilerContext, message: Fatal) extends Exception
case class InternalCompilerErrorException(cc: CompilerContext, message: ICE) extends Exception

trait Messaging { self: CompilerContext =>

  // buffer to store messages without source location information
  final private[this] val messageBuffer = mutable.ListBuffer[Message]()

  //////////////////////////////////////////////////////////////////////////////
  // Versions without source location
  //////////////////////////////////////////////////////////////////////////////

  final def warning(msg: String*): Unit = synchronized {
    messageBuffer append Warning(msg)
  }

  final def error(msg: String*): Unit = synchronized {
    messageBuffer append Error(msg)
  }

  final def fatal(msg: String*): Nothing = synchronized {
    val message = Fatal(msg)
    messageBuffer append message
    throw FatalErrorException(this, message)
  }

  final def ice(msg: String*): Nothing = synchronized {
    val message = ICE(msg)
    messageBuffer append message
    throw InternalCompilerErrorException(this, message)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Versions that take a source location
  //////////////////////////////////////////////////////////////////////////////

  final def warning(loc: Loc, msg: String*): Unit = synchronized {
    messageBuffer append Warning(msg, Some(loc))
  }

  final def error(loc: Loc, msg: String*): Option[Loc] = synchronized {
    messageBuffer append Error(msg, Some(loc))
    Some(loc)
  }

  final def fatal(loc: Loc, msg: String*): Nothing = synchronized {
    val message = Fatal(msg, Some(loc))
    messageBuffer append message
    throw FatalErrorException(this, message)
  }

  final def ice(loc: Loc, msg: String*): Nothing = synchronized {
    val message = ICE(msg, Some(loc))
    messageBuffer append message
    throw InternalCompilerErrorException(this, message)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Get messages
  //////////////////////////////////////////////////////////////////////////////

  final def messages: List[Message] = messageBuffer.toList

  //////////////////////////////////////////////////////////////////////////////
  // Versions that take an Antlr4 token/parse tree node for location info
  //////////////////////////////////////////////////////////////////////////////

  final def warning(ctx: ParserRuleContext, msg: String*): Unit = warning(ctx.loc, msg: _*)

  final def error(ctx: ParserRuleContext, msg: String*): Option[Loc] = error(ctx.loc, msg: _*)

  final def fatal(ctx: ParserRuleContext, msg: String*): Nothing = fatal(ctx.loc, msg: _*)

  final def ice(ctx: ParserRuleContext, msg: String*): Nothing = ice(ctx.loc, msg: _*)

  //////////////////////////////////////////////////////////////////////////////
  // Versions that anything that has a location
  //////////////////////////////////////////////////////////////////////////////

  final def warning(item: Locationed, msg: String*): Unit = warning(item.loc, msg: _*)

  final def error(item: Locationed, msg: String*): Option[Loc] = error(item.loc, msg: _*)

  final def fatal(item: Locationed, msg: String*): Nothing = fatal(item.loc, msg: _*)

  final def ice(item: Locationed, msg: String*): Nothing = ice(item.loc, msg: _*)
}
