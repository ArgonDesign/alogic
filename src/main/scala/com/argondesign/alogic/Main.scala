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
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import com.argondesign.alogic.ast.Trees.Root
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FatalErrorException
import com.argondesign.alogic.core.Settings
import com.argondesign.alogic.frontend.Frontend
import com.argondesign.alogic.passes.Passes
import com.argondesign.alogic.util.unreachable

object Main extends App {

  //////////////////////////////////////////////////////////////////////////////
  // Parse arguments
  //////////////////////////////////////////////////////////////////////////////

  val cliConf = new CLIConf(args)

  //////////////////////////////////////////////////////////////////////////////
  // Create the compiler context
  //////////////////////////////////////////////////////////////////////////////

  val settings = Settings(
    odir = Some(cliConf.odir().toPath),
    srcbase = cliConf.srcbase.toOption map { _.toPath },
    sep = cliConf.sep(),
    uninitialized = cliConf.uninitialized(),
    ensurePrefix = cliConf.ensurePrefix(),
    header = cliConf.header.toOption map { file =>
      val str = new String(Files.readAllBytes(file.toPath), StandardCharsets.UTF_8)
      if (str.endsWith("\n")) str else str + "\n"
    } getOrElse "",
    colourize = cliConf.color() match {
      case "always" => true
      case "never"  => false
      case _        => cliConf.stderrisatty.toOption contains true
    },
    dumpTrees = cliConf.dumpTrees.toOption contains true
  )

  implicit val cc = new CompilerContext(settings)

  //////////////////////////////////////////////////////////////////////////////
  // Do the work
  //////////////////////////////////////////////////////////////////////////////

  try {
    val toplevels = cliConf.toplevel()

    lazy val cwd = new File(".").getCanonicalFile

    val defaultToCWD = cliConf.ydir().isEmpty && cliConf.incdir().isEmpty && cliConf.srcbase.isEmpty
    val moduleSeachDirs = if (defaultToCWD) List(cwd) else cliConf.ydir()
    val includeSeachDirs = if (defaultToCWD) List(cwd) else cliConf.incdir()

    val initalDefines = cliConf.defs.toMap

    //////////////////////////////////////////////////////////////////////////////
    // Create the front end and built the ASTs
    //////////////////////////////////////////////////////////////////////////////

    val frontEndTrees = {
      val frontend = new Frontend(moduleSeachDirs, includeSeachDirs, initalDefines)
      frontend(toplevels)
    }

    // Insert entity symbols into the global scope
    cc.addGlobalEntities {
      frontEndTrees map {
        case Root(_, entity) => entity
        case _               => unreachable
      }
    }

    //////////////////////////////////////////////////////////////////////////////
    // Compile the trees
    //////////////////////////////////////////////////////////////////////////////

    Passes(frontEndTrees)
  } catch {
    case _: FatalErrorException => ()
  } finally {
    cc.emitMessages(Console.err)
  }

  if (cc.hasError) {
    sys exit 1
  }

  sys exit 0
}
