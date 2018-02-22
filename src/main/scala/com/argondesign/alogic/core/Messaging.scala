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
import com.argondesign.alogic.ast.Locationed
import com.argondesign.alogic.util.unreachable

import org.antlr.v4.runtime.ParserRuleContext

sealed abstract trait Message {
  protected val lop: Option[Loc]
  protected val cat: String
  val msg: Seq[String]

  lazy val loc: Loc = lop match {
    case Some(loc) => loc
    case None      => unreachable
  }

  override def toString = {
    val prefix = lop match {
      case Some(loc) => s"${loc}: ${cat}: "
      case None      => s"${cat}: "
    }
    msg mkString (prefix, "\n" + prefix + "... ", "")
  }
}

// Warnings are informative messages about issues that the compiler
// can recover from, and still produce functional output.
case class Warning(msg: Seq[String], lop: Option[Loc] = None) extends Message {
  val cat = "WARNING"
}

// Errors indicate situations where the compiler can still make
// forward progress, but the generated output would not be functional.
// In this case the compiler carries on trying to generate as many
// messages as possible, but the final exit status of the program
// will indicate failure.
case class Error(msg: Seq[String], lop: Option[Loc] = None) extends Message {
  val cat = "ERROR"
}

// Fatal indicates situations where the compiler cannot make forward
// progress. The first fatal message will cause the program to exit.
case class Fatal(msg: Seq[String], lop: Option[Loc] = None) extends Message {
  val cat = "FATAL"
}

// Internal compiler error indicates a programming error in the compiler
// please file a bug report
case class ICE(initialMsg: Seq[String], lop: Option[Loc] = None) extends Message {
  val cat = "INTERNAL COMPILER ERROR"
  val msg = initialMsg ++ Seq("Please file a bug report")
}

case class FatalErrorException(cc: CompilerContext) extends Exception
case class InternalCompilerErrorException(ccOpt: Option[CompilerContext]) extends RuntimeException

trait Messaging { self: CompilerContext =>

  private[this] implicit val implicitThis: CompilerContext = this

  // buffer to store messages without source location information
  private[this] val messageBuffer = mutable.ListBuffer[Message]()

  //////////////////////////////////////////////////////////////////////////////
  // Versions without source location
  //////////////////////////////////////////////////////////////////////////////

  def warning(msg: String*): Unit = synchronized {
    messageBuffer append Warning(msg)
  }

  def error(msg: String*): Unit = synchronized {
    messageBuffer append Error(msg)
  }

  def fatal(msg: String*): Nothing = synchronized {
    messageBuffer append Fatal(msg)
    throw FatalErrorException(this)
  }

  def ice(msg: String*): Nothing = synchronized {
    messageBuffer append ICE(msg)
    throw InternalCompilerErrorException(Some(this))
  }

  //////////////////////////////////////////////////////////////////////////////
  // Versions that take a source location
  //////////////////////////////////////////////////////////////////////////////

  def warning(loc: Loc, msg: String*): Unit = synchronized {
    messageBuffer append Warning(msg, Some(loc))
  }

  def error(loc: Loc, msg: String*): Unit = synchronized {
    messageBuffer append Error(msg, Some(loc))
  }

  def fatal(loc: Loc, msg: String*): Nothing = synchronized {
    messageBuffer append Fatal(msg, Some(loc))
    throw FatalErrorException(this)
  }

  def ice(loc: Loc, msg: String*): Nothing = synchronized {
    messageBuffer append ICE(msg, Some(loc))
    throw InternalCompilerErrorException(Some(this))
  }

  //////////////////////////////////////////////////////////////////////////////
  // Get messages
  //////////////////////////////////////////////////////////////////////////////

  def messages: List[Message] = messageBuffer.toList

  //////////////////////////////////////////////////////////////////////////////
  // Versions that take an Antlr4 token/parse tree node for location info
  //////////////////////////////////////////////////////////////////////////////

  def warning(ctx: ParserRuleContext, msg: String*): Unit = warning(ctx.loc, msg: _*)

  def error(ctx: ParserRuleContext, msg: String*): Unit = error(ctx.loc, msg: _*)

  def fatal(ctx: ParserRuleContext, msg: String*): Nothing = fatal(ctx.loc, msg: _*)

  def ice(ctx: ParserRuleContext, msg: String*): Nothing = ice(ctx.loc, msg: _*)

  //////////////////////////////////////////////////////////////////////////////
  // Versions that anything that has a location
  //////////////////////////////////////////////////////////////////////////////

  def warning(item: Locationed, msg: String*): Unit = warning(item.loc, msg: _*)

  def error(item: Locationed, msg: String*): Unit = error(item.loc, msg: _*)

  def fatal(item: Locationed, msg: String*): Nothing = fatal(item.loc, msg: _*)

  def ice(item: Locationed, msg: String*): Nothing = ice(item.loc, msg: _*)
  //
  //  //////////////////////////////////////////////////////////////////////////////
  //  // Versions that take an ast.Attr for location info
  //  //////////////////////////////////////////////////////////////////////////////
  //
  //  def info(attr: Attr, msg: String*): Unit = info(attr.loc, msg: _*)
  //
  //  def note(attr: Attr, msg: String*): Unit = note(attr.loc, msg: _*)
  //
  //  def warning(attr: Attr, msg: String*): Unit = warning(attr.loc, msg: _*)
  //
  //  def error(attr: Attr, msg: String*): Unit = error(attr.loc, msg: _*)
  //
  //  def fatal(attr: Attr, msg: String*): Nothing = fatal(attr.loc, msg: _*)
  //
  //  def ice(attr: Attr, msg: String*): Nothing = ice(attr.loc, msg: _*)
}
