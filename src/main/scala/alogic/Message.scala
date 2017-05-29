package alogic

import org.antlr.v4.runtime.ParserRuleContext
import Antlr4Conversions._

object Message {

  // Indicate what the final exit status of the program shoud be
  var fail = false

  // Warnings are informative messages about issues that the compiler
  // can recover from, and still produce functional output.
  def warning(msg: String): Unit = {
    println(s"WARNING: $msg")
  }

  // Errors indicate situations where the compiler can still make
  // forward progress, but the generated output would not be functional.
  // In this case the compiler carries on trying to generate as many
  // messages as possible, but the final exit status of the program
  // will indicate failure.
  def error(msg: String): Unit = {
    println(s"ERROR: $msg")
    fail = true
  }

  // Fatal indicates situations where the compiler cannot make forward
  // progress. The first fatal message will cause the program to exit.
  def fatal(msg: String): Unit = {
    println(s"FATAL: $msg")
    sys exit 1
  }

  //////////////////////////////////////////////////////////////////////////////
  // Versions that take a source location
  //////////////////////////////////////////////////////////////////////////////

  def warning(loc: Loc, msg: String): Unit = {
    println(s"$loc: WARNING: $msg")
  }

  def error(loc: Loc, msg: String): Unit = {
    println(s"$loc: ERROR: $msg")
    fail = true
  }

  def fatal(loc: Loc, msg: String): Unit = {
    println(s"$loc: FATAL: $msg")
    sys exit 1
  }

  //////////////////////////////////////////////////////////////////////////////
  // Versions that take an Antlr4 token/parse tree node for location info
  //////////////////////////////////////////////////////////////////////////////

  def warning(ctx: ParserRuleContext, msg: String): Unit = warning(ctx.loc, msg)

  def error(ctx: ParserRuleContext, msg: String): Unit = error(ctx.loc, msg)

  def fatal(ctx: ParserRuleContext, msg: String): Unit = fatal(ctx.loc, msg)
}
