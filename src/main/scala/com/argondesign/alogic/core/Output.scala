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

import java.io.Writer

import com.argondesign.alogic.ast.Trees.Decl
import com.argondesign.alogic.ast.Trees.Defn
import com.argondesign.alogic.ast.Trees.Tree

trait Output { this: CompilerContext =>

  private implicit val implicitThis: CompilerContext = this

  def getOutputWriter(tree: Tree, suffix: String): Writer = {
    settings.outputWriterFactory(tree, suffix)
  }

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
