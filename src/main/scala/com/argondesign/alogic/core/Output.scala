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

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.util.unreachable

import java.io.File
import java.io.PrintWriter

trait Output { this: CompilerContext =>

  private def getOutputFile(treeAndSuffixOrFileName: Either[(Tree, String), String]): File = {
    val oPath = settings.oPath match {
      case Some(value) => value
      case None        => unreachable
    }
    val srcBase = settings.srcBase

    treeAndSuffixOrFileName match {
      case Left((tree: Tree, suffix)) =>
        val oDir = if (tree.loc eq Loc.synthetic) {
          oPath // Emit synthetic decls to the root output directory
        } else {
          srcBase match {
            case None => oPath
            case Some(base) =>
              val dirPath = tree.loc.source.file.toPath.toRealPath().getParent
              assert(dirPath startsWith base)
              val relPath = base relativize dirPath
              oPath resolve relPath
          }
        }
        val base = tree match {
          case decl: Decl        => decl.symbol.name
          case desc: DescPackage => desc.packageName
          case desc: Desc        => desc.symbol.name
          case _                 => ???
        }
        (oDir resolve (base + suffix)).toFile
      case Left(_)         => ???
      case Right(fileName) => (oPath resolve fileName).toFile
    }
  }

  def getOutputFileName(tree: Tree, suffix: String): String =
    getOutputFile(Left((tree, suffix))).getCanonicalPath

  private def getOutputWriter(
      treeAndSuffixOrFileName: Either[(Tree, String), String]
    ): PrintWriter = {
    val oFile = getOutputFile(treeAndSuffixOrFileName)

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
