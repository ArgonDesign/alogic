////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Compiler entry point
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.Settings

object Compiler {

  // Parse argument vector. Return compiler settings and list of requested top level entities
  def parseArgs(
      messageBuffer: MessageBuffer,
      args: Seq[String]
    ): Option[(Settings, List[String])] = {

    val options = new OptionParser(args, messageBuffer)

    Option.unless(messageBuffer.hasError) {
      val settings = Settings(
        moduleSearchDirs = options.ydir(),
        includeSearchDirs = options.incdir(),
        initialDefines = options.defs,
        srcBase = options.srcbase.toOption map { _.toPath },
        oPath = options.odir.toOption map { _.toPath },
        sep = options.sep(),
        uninitialized = options.uninitialized(),
        ensurePrefix = options.ensurePrefix(),
        outputNameMaxLength = options.outputNameMaxLength.toOption,
        header = options.header.toOption map { file =>
          val str = new String(Files.readAllBytes(file.toPath), StandardCharsets.UTF_8)
          if (str.endsWith("\n")) str else str + "\n"
        } getOrElse "",
        colorize = options.color() match {
          case "always" => true
          case "never"  => false
          case _        => options.stderrisatty.toOption contains true
        },
        dumpTrees = options.dumpTrees.toOption contains true,
        profile = options.profile.toOption contains true,
        resetStyle = options.resetStyle(),
        resetAll = !options.noResetAll(),
        genLoopLimit = options.genLoopLimit(),
        combRecLimit = options.combRecLimit(),
        assertions = !options.noAssertions(),
        stats = options.stats(),
        traceElaborate = options.traceElaborate.toOption contains true
      )

      (settings, options.topLevels())
    }
  }

  def compile(
      messageBuffer: MessageBuffer,
      settings: Settings,
      topLevels: List[String]
    ): Int = {

    ////////////////////////////////////////////////////////////////////////////
    // Create the compiler context
    ////////////////////////////////////////////////////////////////////////////

    val cc = new CompilerContext(messageBuffer, settings)

    ////////////////////////////////////////////////////////////////////////////
    // Do the work
    ////////////////////////////////////////////////////////////////////////////

    // Compile what is requested
    cc.compile(topLevels)

    // Emit profile, if required
    if (cc.settings.profile) {
      val pw = cc.getOutputWriter("profile.txt")
      cc.writeProfile(pw)
      pw.close()
    }

    // Return the exit code
    if (cc.hasError) 1 else 0
  }

}
