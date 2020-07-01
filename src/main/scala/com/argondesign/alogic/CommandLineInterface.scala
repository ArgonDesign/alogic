////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Command line interface entry point
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import com.argondesign.alogic.ast.Trees.Arg
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.Messages
import com.argondesign.alogic.core.Settings
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.core.SourceContext
import com.argondesign.alogic.frontend.Parser

import java.io.File

object CommandLineInterface {

  def main(args: Array[String]): Unit = {
    // $COVERAGE-OFF$ The test suite invokes run
    val (messageBuffer, colorize, returnCode) = run(args.toSeq)

    // Emit messages
    val colorMap = if (colorize) Messages.ansiColorMap else Messages.emptyColorMap
    messageBuffer.messages foreach { msg =>
      Console.err.println(msg.render(colorMap))
    }

    // Finished
    sys exit returnCode
    // $COVERAGE-ON$
  }

  def run(args: Seq[String]): (MessageBuffer, Boolean, Int) = {
    ////////////////////////////////////////////////////////////////////////////
    // Create the message buffer for this run
    ////////////////////////////////////////////////////////////////////////////

    implicit val messageBuffer: MessageBuffer = new MessageBuffer

    ////////////////////////////////////////////////////////////////////////////
    // Parse arguments and then compile
    ////////////////////////////////////////////////////////////////////////////

    Compiler.parseArgs(messageBuffer, args, None) flatMap {
      case (settings, source, params) =>
        compile(args, settings, source, params)
    } getOrElse {
      // If argument parsing failed
      (messageBuffer, false, 2)
    }
  }

  def compile(
      args: Seq[String],
      settings: Settings,
      source: Source,
      params: List[String]
    )(
      implicit
      mb: MessageBuffer
    ): Option[(MessageBuffer, Boolean, Int)] = {
    ////////////////////////////////////////////////////////////////////////
    // Setup command line location reporting
    ////////////////////////////////////////////////////////////////////////

    val cliText = args.mkString(" ")
    val cliSource = Source("command-line", cliText)

    // Location where input file parameter errors will be reported, as if
    // it was the specialization location
    val packageInstanceLoc: Loc = {
      val end = cliText.length
      val start = end - args.last.length
      val point = start + args.last.reverse.dropWhile(_ != File.separatorChar).length
      Loc(cliSource.name, 1, cliSource, start, end, point)
    }

    ////////////////////////////////////////////////////////////////////////
    // Parse the parameter assignments
    ////////////////////////////////////////////////////////////////////////

    val ps: List[Arg] = params flatMap { param =>
      val start = cliText.indexOf(s"-P $param") + 3
      assert(start >= 0)
      Parser[Arg](cliSource, SourceContext.Unknown, start, start + param.length)
    }

    Option.unless(mb.hasError) {
      (
        mb,
        settings.colorize,
        Compiler.compile(mb, settings, source, packageInstanceLoc, ps)
      )
    }
  }

}
