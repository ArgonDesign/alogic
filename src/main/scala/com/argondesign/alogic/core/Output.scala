////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
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

    treeAndSuffixOrFileName match {
      case Left((tree, suffix)) =>
        val oDir = if (tree.loc eq Loc.synthetic) {
          oPath // Emit synthetic decls to the root output directory
        } else {
          // Directory containing the source of this tree
          val sourceDir = tree.loc.source.file.getParentFile.getCanonicalFile.toPath
          // If there is an input search dir that contains the source of this
          // symbol, put it under the same directory hierarchical in the output
          // directory, otherwise put it straight the output directory.
          settings.importSearchDirs.find(sourceDir.startsWith) match {
            case Some(searchDir) => oPath.resolve(searchDir relativize sourceDir)
            case None            => oPath
          }
        }
        val base = tree match {
          case decl: Decl        => decl.symbol.name
          case desc: DescPackage => desc.packageName
          case desc: Desc        => desc.symbol.name
          case _                 => unreachable
        }
        (oDir resolve (base + suffix)).toFile
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
