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

import com.argondesign.alogic.ast.Trees.Entity
import com.argondesign.alogic.ast.Trees.EntityIdent
import com.argondesign.alogic.ast.Trees.EntityLowered
import com.argondesign.alogic.ast.Trees.EntityNamed
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Settings

object Main extends App {

  //////////////////////////////////////////////////////////////////////////////
  // Parse arguments
  //////////////////////////////////////////////////////////////////////////////

  val cliConf = new CLIConf(args)

  //////////////////////////////////////////////////////////////////////////////
  // Create the compiler context
  //////////////////////////////////////////////////////////////////////////////

  lazy val cwd = new File(".").getCanonicalFile
  val defaultToCWD = cliConf.ydir().isEmpty && cliConf.incdir().isEmpty && cliConf.srcbase.isEmpty

  val opath = cliConf.odir().toPath
  val srcbase = cliConf.srcbase.toOption map { _.toPath }

  def entityWriterFactory(entity: Entity, suffix: String): Writer = {
    def oPathFor(entity: Entity, suffix: String): Path = {
      val oDir = if (entity.loc eq Loc.synthetic) {
        // Emit synthetic entities to the root output directory
        opath
      } else {
        srcbase match {
          case None => opath
          case Some(base) => {
            val dirPath = entity.loc.source.file.toPath.toRealPath().getParent
            assert(dirPath startsWith base)
            val relPath = base relativize dirPath
            opath resolve relPath
          }
        }
      }

      val name = entity match {
        case entity: EntityIdent   => entity.ident.name + suffix
        case entity: EntityNamed   => entity.symbol.name + suffix
        case entity: EntityLowered => entity.symbol.name + suffix
      }

      oDir resolve name
    }

    val oPath = oPathFor(entity, suffix)
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
    initialDefines = cliConf.defs.toMap,
    entityWriterFactory = entityWriterFactory,
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
    dumpTrees = cliConf.dumpTrees.toOption contains true,
    moduleManifestPath = cliConf.moduleManifest.toOption map { _.toPath },
    resetStyle = cliConf.resetStyle()
  )

  implicit val cc = new CompilerContext(settings)

  //////////////////////////////////////////////////////////////////////////////
  // Do the work
  //////////////////////////////////////////////////////////////////////////////

  cc.compile(cliConf.toplevel())

  sys exit (if (cc.hasError) 1 else 0)
}
