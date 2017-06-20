package alogic

import org.antlr.v4.runtime.ParserRuleContext
import Antlr4Conversions._

object Message {

  // Whether to output verbose messages
  var verbose = false;

  // Indicate what the final exit status of the program shoud be
  var fail = false

  private def printit(prefix: String, msg: Seq[String]): Unit = {
    println(msg mkString (prefix, "\n" + prefix, ""))
  }

  // INFO messages are only displayed when the verbose option is
  // provided, and in general describe compiler status
  def info(msg: String*): Unit = {
    if (verbose) printit("INFO: ", msg)
  }

  // NOTE messages are always displayed and in general describe
  // compiler status
  def note(msg: String*): Unit = {
    printit("NOTE: ", msg)
  }

  // Warnings are informative messages about issues that the compiler
  // can recover from, and still produce functional output.
  def warning(msg: String*): Unit = {
    printit("WARNING: ", msg)
  }

  // Errors indicate situations where the compiler can still make
  // forward progress, but the generated output would not be functional.
  // In this case the compiler carries on trying to generate as many
  // messages as possible, but the final exit status of the program
  // will indicate failure.
  def error(msg: String*): Unit = {
    printit("ERROR: ", msg)
    fail = true
  }

  // Fatal indicates situations where the compiler cannot make forward
  // progress. The first fatal message will cause the program to exit.
  def fatal(msg: String*): Nothing = {
    printit("FATAL: ", msg)
    sys exit 1
  }

  // Internal compiler error indicates a programming error in the compiler
  // please file a bug report
  def ice(msg: String*): Nothing = {
    printit("INTERNAL COMPILER ERROR: ", msg ++ Seq("Please file a bug report"))
    sys exit 1
  }

  //////////////////////////////////////////////////////////////////////////////
  // Versions that take a source location
  //////////////////////////////////////////////////////////////////////////////

  def info(loc: Loc, msg: String*): Unit = {
    printit(s"$loc: INFO: ", msg)
  }

  def warning(loc: Loc, msg: String*): Unit = {
    printit(s"$loc: WARNING: ", msg)
  }

  def error(loc: Loc, msg: String*): Unit = {
    printit(s"$loc: ERROR: ", msg)
    fail = true
  }

  def fatal(loc: Loc, msg: String*): Nothing = {
    printit(s"$loc: FATAL: ", msg)
    sys exit 1
  }

  def ice(loc: Loc, msg: String*): Nothing = {
    printit(s"$loc: INTERNAL COMPILER ERROR: ", msg ++ Seq("Please file a bug report"))
    sys exit 1
  }

  //////////////////////////////////////////////////////////////////////////////
  // Versions that take an Antlr4 token/parse tree node for location info
  //////////////////////////////////////////////////////////////////////////////
  def info(ctx: ParserRuleContext, msg: String*): Unit = info(ctx.loc, msg: _*)

  def warning(ctx: ParserRuleContext, msg: String*): Unit = warning(ctx.loc, msg: _*)

  def error(ctx: ParserRuleContext, msg: String*): Unit = error(ctx.loc, msg: _*)

  def fatal(ctx: ParserRuleContext, msg: String*): Nothing = fatal(ctx.loc, msg: _*)

  def ice(ctx: ParserRuleContext, msg: String*): Nothing = ice(ctx.loc, msg: _*)
}
