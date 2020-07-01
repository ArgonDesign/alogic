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

import com.argondesign.alogic.ast.Trees.Decl
import com.argondesign.alogic.ast.Trees.Defn
import com.argondesign.alogic.ast.Trees.Root
import com.argondesign.alogic.ast.Trees.Tree
import com.argondesign.alogic.util.unreachable

trait Output { this: CompilerContext =>

  implicit private val implicitThis: CompilerContext = this

  private def getOutputWriter(
      treeAndSuffixOrFileName: Either[(Tree, String), String]
    ): PrintWriter = {
    val oPath = settings.oPath match {
      case Some(value) => value
      case None        => unreachable
    }
    val srcBase = settings.srcBase

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

  def getOutputWriter(name: String): PrintWriter =
    getOutputWriter(Right(name))

  def getOutputWriter(tree: Tree, suffix: String): PrintWriter =
    getOutputWriter(Left((tree, suffix)))

  def dump(decl: Decl, defn: Defn, suffix: String): Unit = {
    val writer = getOutputWriter(decl, suffix + ".alogic")
    writer.write(decl.toSource)
    writer.write("\n")
    writer.write(defn.toSource)
    writer.write("\n")
    writer.close()
  }

  def dump(tree: Tree, suffix: String): Unit = {
    val writer = getOutputWriter(tree, suffix + ".alogic")
    writer.write(tree.toSource)
    writer.write("\n")
    writer.close()
  }

}
