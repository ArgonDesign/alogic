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
import java.io.Writer
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path

import com.argondesign.alogic.ast.Trees.Decl
import com.argondesign.alogic.ast.Trees.Root
import com.argondesign.alogic.ast.Trees.Tree
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Settings

object Main extends App {

  //////////////////////////////////////////////////////////////////////////////
  // Parse arguments
  //////////////////////////////////////////////////////////////////////////////

  val cliConf = new CLIConf(args.toSeq)

  //////////////////////////////////////////////////////////////////////////////
  // Create the compiler context
  //////////////////////////////////////////////////////////////////////////////

  lazy val cwd = new File(".").getCanonicalFile
  val defaultToCWD = cliConf.ydir().isEmpty && cliConf.incdir().isEmpty && cliConf.srcbase.isEmpty

  val opath = cliConf.odir().toPath
  val srcbase = cliConf.srcbase.toOption map { _.toPath }

  def outputWriterFactory(treeAndSuffixOrFileName: Either[(Tree, String), String]): Writer = {
    def oPathFor(decl: Decl, suffix: String): Path = {
      val oDir = if (decl.loc eq Loc.synthetic) {
        // Emit synthetic decls to the root output directory
        opath
      } else {
        srcbase match {
          case None => opath
          case Some(base) =>
            val dirPath = decl.loc.source.file.toPath.toRealPath().getParent
            assert(dirPath startsWith base)
            val relPath = base relativize dirPath
            opath resolve relPath
        }
      }

      oDir resolve (decl.symbol.name + suffix)
    }

    val oPath = treeAndSuffixOrFileName match {
      case Left((decl: Decl, suffix)) => oPathFor(decl, suffix)
      case Left((root: Root, suffix)) =>
        opath resolve (root.loc.source.file.getName.split('.').head + suffix)
      case Right(fileName) => opath resolve fileName
      case _               => ???
    }
    val oFile = oPath.toFile

    if (!oFile.exists) {
      oFile.getParentFile.mkdirs()
      oFile.createNewFile()
    }

    new PrintWriter(oFile)
  }

  val settings = Settings(
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
    colourize = cliConf.color() match {
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

  implicit val cc: CompilerContext = new CompilerContext(settings)

  //////////////////////////////////////////////////////////////////////////////
  // Do the work
  //////////////////////////////////////////////////////////////////////////////

  cc.compile(cliConf.topLevels())

  if (cc.settings.profile) {
    val pw = new PrintWriter((opath resolve "profile").toFile)
    cc.writeProfile(pw)
    pw.close()
  }

  sys exit (if (cc.hasError) 1 else 0)
}
