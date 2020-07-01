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

import java.io.File

import com.argondesign.alogic.core.MessageBuffer

object CommandLineInterface {

  def main(args: Array[String]): Unit = {
    // $COVERAGE-OFF$ The test suite invokes run
    val (messageBuffer, colorize, returnCode) = run(args)

    // Emit messages
    messageBuffer.messages foreach { msg =>
      Console.err.println(msg.render(colorize))
    }

    // Finished
    sys exit returnCode
    // $COVERAGE-ON$
  }

  def run(args: Array[String]): (MessageBuffer, Boolean, Int) = {
    ////////////////////////////////////////////////////////////////////////////
    // Create the message buffer for this run
    ////////////////////////////////////////////////////////////////////////////

    val messageBuffer = new MessageBuffer

    ////////////////////////////////////////////////////////////////////////////
    // Parse arguments and then compile
    ////////////////////////////////////////////////////////////////////////////

    Compiler.parseArgs(messageBuffer, args.toSeq) map {
      case (s, topLevels) =>
        // Add some convenience default settings for the command line interface
        lazy val cwd = new File(".").getCanonicalFile

        val settings = s.copy(
          moduleSearchDirs = if (s.moduleSearchDirs.isEmpty) List(cwd) else s.moduleSearchDirs,
          includeSearchDirs = if (s.includeSearchDirs.isEmpty) List(cwd) else s.includeSearchDirs
        )

        (messageBuffer, settings.colorize, Compiler.compile(messageBuffer, settings, topLevels))
    } getOrElse {
      // If argument parsing failed
      (messageBuffer, false, 2)
    }
  }

}
