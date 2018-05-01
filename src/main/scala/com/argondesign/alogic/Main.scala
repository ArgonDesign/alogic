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
import java.nio.file.Path

import com.argondesign.alogic.ast.Trees.Entity
import com.argondesign.alogic.ast.Trees.Root
import com.argondesign.alogic.ast.Trees.Sym
import com.argondesign.alogic.backend.MakeVerilog
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.core.FatalErrorException
import com.argondesign.alogic.frontend.Frontend
import com.argondesign.alogic.passes.Passes
import com.argondesign.alogic.util.unreachable

object Main extends App {

  //////////////////////////////////////////////////////////////////////////////
  // Create the compiler context
  //////////////////////////////////////////////////////////////////////////////

  implicit val cc = new CompilerContext

  //////////////////////////////////////////////////////////////////////////////
  // Parse arguments
  //////////////////////////////////////////////////////////////////////////////

  val cliConf = new CLIConf(args)

  val results = try {
    val toplevels = cliConf.toplevel()

    lazy val cwd = (new File(".")).getCanonicalFile()

    val defaultToCWD = cliConf.ydir.isEmpty && cliConf.incdir.isEmpty && cliConf.srcbase.isEmpty
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

    val trees = Passes(frontEndTrees)

    Some(trees)
  } catch {
    case _: FatalErrorException => None
  } finally {
    cc.messages map { _.string } foreach Console.err.println
  }

  val oDirBase = cliConf.odir().toPath
  val baseOpt = cliConf.srcbase.toOption map { _.toPath }

  def oPathFor(entity: Entity): Path = {
    val oDir = baseOpt match {
      case None => oDirBase
      case Some(base) => {
        val dirPath = entity.loc.source.file.toPath.toRealPath().getParent
        assert(dirPath startsWith base)
        val relPath = base relativize dirPath
        oDirBase resolve relPath
      }
    }
    val Sym(symbol) = entity.ref
    val name = symbol.denot.name.str + ".v"
    oDir resolve name
  }

  def writeEntity(entity: Entity, oPath: Path): Unit = {
    val oFile = oPath.toFile
    if (!oFile.exists) {
      oFile.getParentFile.mkdirs()
      oFile.createNewFile()
    }
    {
      val pw = new PrintWriter(oFile + ".alogic")
      pw.write(entity.toSource)
      pw.close()
    }
    {
      val pw = new PrintWriter(oFile)
      val mk = new MakeVerilog(entity)
      pw.write(mk.moduleSource)
      pw.close()
    }
  }

  if (results.isEmpty) {
    sys exit 2
  }

  if (cc.hasError) {
    sys exit 1
  }

  for (tree <- results.get) {
    val entity = tree.asInstanceOf[Entity]
    writeEntity(entity, oPathFor(entity))
  }

  sys exit 0
}
