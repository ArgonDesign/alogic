////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic compiler
// Author: Peter de Rivaz/Geza Lore
//
// DESCRIPTION:
//
// Alogic compiler entry point
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import java.io.File
import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path

import com.argondesign.alogic.ast.Trees.Decl
import com.argondesign.alogic.ast.Trees.Root
import com.argondesign.alogic.ast.Trees.Tree
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.Settings
import com.argondesign.alogic.util.unreachable

object Main {

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
    // Parse arguments
    ////////////////////////////////////////////////////////////////////////////

    val cliConf = new CLIConf(args.toSeq, messageBuffer)

    // Fail fast if arguments are invalid
    if (messageBuffer.hasError) {
      (messageBuffer, false, 2)
    } else {
      execute(messageBuffer, cliConf)
    }
  }

  private def execute(
      messageBuffer: MessageBuffer,
      cliConf: CLIConf
    ): (MessageBuffer, Boolean, Int) = {
    ////////////////////////////////////////////////////////////////////////////
    // Create the compiler settings
    ////////////////////////////////////////////////////////////////////////////

    val settings = {
      val oPath: Path = cliConf.odir().toPath
      val srcBase = cliConf.srcbase.toOption map { _.toPath }

      def outputWriterFactory(
          treeAndSuffixOrFileName: Either[(Tree, String), String]
        ): PrintWriter = {
        val oFile = treeAndSuffixOrFileName match {
          case Left((decl: Decl, suffix)) =>
            val oDir = if (decl.loc eq Loc.synthetic) {
              oPath // Emit synthetic decls to the root output directory
            } else {
              srcBase match {
                case None => oPath
                case Some(base) =>
                  val dirPath = decl.loc.source.file.toPath.toRealPath().getParent
                  assert(dirPath startsWith base)
                  val relPath = base relativize dirPath
                  oPath resolve relPath
              }
            }
            (oDir resolve (decl.symbol.name + suffix)).toFile
          case Left((root: Root, suffix)) =>
            (oPath resolve (root.loc.source.file.getName.split('.').head + suffix)).toFile
          case Right(fileName) => (oPath resolve fileName).toFile
          case _               => unreachable
        }

        if (!oFile.exists) {
          oFile.getParentFile.mkdirs()
          oFile.createNewFile()
        }

        new PrintWriter(oFile)
      }

      val defaultToCWD = cliConf.ydir().isEmpty && cliConf.incdir().isEmpty && srcBase.isEmpty
      lazy val cwd = new File(".").getCanonicalFile

      Settings(
        moduleSearchDirs = if (defaultToCWD) List(cwd) else cliConf.ydir(),
        includeSearchDirs = if (defaultToCWD) List(cwd) else cliConf.incdir(),
        initialDefines = cliConf.defs,
        outputWriterFactory = outputWriterFactory,
        sep = cliConf.sep(),
        uninitialized = cliConf.uninitialized(),
        ensurePrefix = cliConf.ensurePrefix(),
        outputNameMaxLength = cliConf.outputNameMaxLength.toOption,
        header = cliConf.header.toOption map { file =>
          val str = new String(Files.readAllBytes(file.toPath), StandardCharsets.UTF_8)
          if (str.endsWith("\n")) str else str + "\n"
        } getOrElse "",
        colorize = cliConf.color() match {
          case "always" => true
          case "never"  => false
          case _        => cliConf.stderrisatty.toOption contains true
        },
        dumpTrees = cliConf.dumpTrees.toOption contains true,
        profile = cliConf.profile.toOption contains true,
        resetStyle = cliConf.resetStyle(),
        resetAll = !cliConf.noResetAll(),
        genLoopLimit = cliConf.genLoopLimit(),
        combRecLimit = cliConf.combRecLimit(),
        assertions = !cliConf.noAssertions(),
        traceElaborate = cliConf.traceElaborate.toOption contains true
      )
    }

    ////////////////////////////////////////////////////////////////////////////
    // Create the compiler context
    ////////////////////////////////////////////////////////////////////////////

    val cc = new CompilerContext(messageBuffer, settings)

    ////////////////////////////////////////////////////////////////////////////
    // Do the work
    ////////////////////////////////////////////////////////////////////////////

    // Compile what is requested
    cc.compile(cliConf.topLevels())

    // Emit profile, if required
    if (cc.settings.profile) {
      val pw = cc.settings.outputWriterFactory(Right("profile"))
      cc.writeProfile(pw)
      pw.close()
    }

    // Return the message buffer (for testing), colourize flag (for emitting
    // messages in main), and the program exit code
    (messageBuffer, cc.settings.colorize, if (cc.hasError) 1 else 0)
  }

}
