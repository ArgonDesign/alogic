////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Compiler entry points
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import com.argondesign.alogic.ast.Trees.Arg

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.Settings
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.util.unreachable

import java.io.File
import java.nio.file.Path

object Compiler {

  // Parse compiler arguments. Possibly adds messages to message buffer.
  // On success, returns Some(compiler settings and input file).
  def parseArgs(
      messageBuffer: MessageBuffer,
      args: Seq[String],
      sandboxPathOpt: Option[Path]
    ): Option[(Settings, File, List[String])] = {

    // Parse command line arguments
    val options = new OptionParser(
      args,
      messageBuffer,
      sandboxPathOpt.map(_.toFile.getCanonicalFile.toPath)
    )

    Option.unless(messageBuffer.hasError) {
      // Build settings based on arguments
      val settings = Settings(
        importSearchDirs = options.ydir() map { _.toAbsolutePath },
        srcBase = options.srcbase.toOption map { _.toAbsolutePath },
        oPath = options.odir.toOption map { _.toAbsolutePath },
        sep = options.sep(),
        uninitialized = options.uninitialized(),
        ensurePrefix = options.ensurePrefix(),
        outputNameMaxLength = options.outputNameMaxLength.toOption,
        header = options.header.toOption map { file =>
          val str = new String(Files.readAllBytes(file), StandardCharsets.UTF_8)
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

      // Add some defaults for convenience
      val settings2 = settings.copy(
        importSearchDirs = if (settings.importSearchDirs.isEmpty) {
          List(options.file().toAbsolutePath.getParent)
        } else {
          settings.importSearchDirs
        }
      )

      (settings2, options.file().toFile, options.param())
    }
  }

  def compile(
      messageBuffer: MessageBuffer,
      settings: Settings,
      source: File,
      loc: Loc,
      params: List[Arg]
    ): Int = {

    ////////////////////////////////////////////////////////////////////////////
    // Create the compiler context
    ////////////////////////////////////////////////////////////////////////////

    val cc = new CompilerContext(messageBuffer, settings)

    ////////////////////////////////////////////////////////////////////////////
    // Do the work
    ////////////////////////////////////////////////////////////////////////////

    // Read the input file. The OptionParser already proved the source file
    // exists and is in the sandbox, so readFile will always succeed.
    val src = Source(source, cc.readFile(source).getOrElse(unreachable))

    // Compile what is requested
    cc.compile(src, loc, params)

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
