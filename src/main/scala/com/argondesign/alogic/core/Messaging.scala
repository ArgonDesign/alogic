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

import java.io.PrintStream

import scala.collection.mutable

sealed abstract trait Message {
  val cat: String
  val msg: Seq[String]

  override def toString = {
    val prefix = s"${cat}: "
    msg mkString (prefix, "\n" + prefix + "... ", "")
  }
}

// Warnings are informative messages about issues that the compiler
// can recover from, and still produce functional output.
case class Warning(msg: Seq[String]) extends Message {
  val cat = "WARNING"
}

// Errors indicate situations where the compiler can still make
// forward progress, but the generated output would not be functional.
// In this case the compiler carries on trying to generate as many
// messages as possible, but the final exit status of the program
// will indicate failure.
case class Error(msg: Seq[String]) extends Message {
  val cat = "ERROR"
}

// Fatal indicates situations where the compiler cannot make forward
// progress. The first fatal message will cause the program to exit.
case class Fatal(msg: Seq[String]) extends Message {
  val cat = "FATAL"
}

// Internal compiler error indicates a programming error in the compiler
// please file a bug report
case class ICE(initialMsg: Seq[String]) extends Message {
  val cat = "INTERNAL COMPILER ERROR"
  val msg = initialMsg ++ Seq("Please file a bug report")
}

case class FatalErrorException(cc: CompilerContext) extends Exception
case class InternalCompilerErrorException(cc: CompilerContext) extends RuntimeException

trait Messaging { self: CompilerContext =>

  // buffer to store messages without source location information
  val globalMessages = mutable.ListBuffer[Message]()

  //////////////////////////////////////////////////////////////////////////////
  // Versions without source location
  //////////////////////////////////////////////////////////////////////////////

  def warning(msg: String*): Unit = globalMessages append Warning(msg)

  def error(msg: String*): Unit = globalMessages append Error(msg)

  def fatal(msg: String*): Nothing = {
    globalMessages append Fatal(msg)
    throw FatalErrorException(this)
  }

  def ice(msg: String*): Nothing = {
    globalMessages append ICE(msg)
    throw InternalCompilerErrorException(this)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Emit messages
  //////////////////////////////////////////////////////////////////////////////

  def emitMessages(printStream: PrintStream = Console.err): Unit = globalMessages foreach printStream.println

  //  //////////////////////////////////////////////////////////////////////////////
  //  // Versions that take a source location
  //  //////////////////////////////////////////////////////////////////////////////
  //
  //  def info(loc: Loc, msg: String*): Unit = messages append Info(Some(loc), msg)
  //
  //  def note(loc: Loc, msg: String*): Unit = messages append Note(Some(loc), msg)
  //
  //  def warning(loc: Loc, msg: String*): Unit = messages append Warning(Some(loc), msg)
  //
  //  def error(loc: Loc, msg: String*): Unit = messages append Error(Some(loc), msg)
  //
  //  def fatal(loc: Loc, msg: String*): Nothing = {
  //    messages append Fatal(Some(loc), msg)
  //    throw new FatalErrorException
  //  }
  //
  //  def ice(loc: Loc, msg: String*): Nothing = {
  //    messages append ICE(Some(loc), msg)
  //    throw new InternalCompilerErrorException
  //  }
  //
  //  //////////////////////////////////////////////////////////////////////////////
  //  // Versions that take an Antlr4 token/parse tree node for location info
  //  //////////////////////////////////////////////////////////////////////////////
  //
  //  def info(ctx: ParserRuleContext, msg: String*): Unit = info(ctx.loc, msg: _*)
  //
  //  def note(ctx: ParserRuleContext, msg: String*): Unit = note(ctx.loc, msg: _*)
  //
  //  def warning(ctx: ParserRuleContext, msg: String*): Unit = warning(ctx.loc, msg: _*)
  //
  //  def error(ctx: ParserRuleContext, msg: String*): Unit = error(ctx.loc, msg: _*)
  //
  //  def fatal(ctx: ParserRuleContext, msg: String*): Nothing = fatal(ctx.loc, msg: _*)
  //
  //  def ice(ctx: ParserRuleContext, msg: String*): Nothing = ice(ctx.loc, msg: _*)
  //
  //  //////////////////////////////////////////////////////////////////////////////
  //  // Versions that take an ast.Node for location info
  //  //////////////////////////////////////////////////////////////////////////////
  //
  //  def info(node: Node, msg: String*): Unit = info(node.loc, msg: _*)
  //
  //  def note(node: Node, msg: String*): Unit = note(node.loc, msg: _*)
  //
  //  def warning(node: Node, msg: String*): Unit = warning(node.loc, msg: _*)
  //
  //  def error(node: Node, msg: String*): Unit = error(node.loc, msg: _*)
  //
  //  def fatal(node: Node, msg: String*): Nothing = fatal(node.loc, msg: _*)
  //
  //  def ice(node: Node, msg: String*): Nothing = ice(node.loc, msg: _*)
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
