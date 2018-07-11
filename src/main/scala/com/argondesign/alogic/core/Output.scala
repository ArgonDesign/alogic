////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Outputting facilities
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import java.io.PrintWriter
import java.nio.file.Path

import com.argondesign.alogic.ast.Trees._

trait Output { this: CompilerContext =>

  private implicit val implicitThis = this

  def oPathFor(entity: Entity, suffix: String): Path = {
    val oDir = if (entity.loc eq Loc.synthetic) {
      // Emit synthetic entities to the root output directory
      settings.odir.get
    } else {
      settings.srcbase match {
        case None => settings.odir.get
        case Some(base) => {
          val dirPath = entity.loc.source.file.toPath.toRealPath().getParent
          assert(dirPath startsWith base)
          val relPath = base relativize dirPath
          settings.odir.get resolve relPath
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

  def dumpEntity(entity: Entity, suffix: String): Unit = {
    val oPath = oPathFor(entity, suffix + ".alogic")
    val oFile = oPath.toFile

    if (!oFile.exists) {
      oFile.getParentFile.mkdirs()
      oFile.createNewFile()
    }

    val pw = new PrintWriter(oFile)
    pw.write(entity.toSource)
    pw.close()
  }

}
