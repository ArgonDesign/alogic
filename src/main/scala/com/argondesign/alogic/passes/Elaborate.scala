////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Specialize parameters and process 'gen' constructs
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.specialize.Specialize
import com.argondesign.alogic.util.unreachable

import scala.util.ChainingSyntax

object Elaborate extends Pass[List[Root], List[(Decl, Defn)]] with ChainingSyntax {
  val name = "elaborate"

  override def dump(result: List[(Decl, Defn)], tag: String)(implicit cc: CompilerContext): Unit =
    result foreach {
      case (decl, defn) => cc.dump(decl, defn, "." + tag)
    }

  def process(trees: List[Root])(implicit cc: CompilerContext): List[(Decl, Defn)] = {

    val topLevelDescs = trees flatMap {
      case root: Root => root.descs filter { _.symbol.attr.topLevel.isSet }
      case _          => unreachable
    }

    val specializedTopLevelDescs = Specialize(topLevelDescs)

    specializedTopLevelDescs map { _.toList } getOrElse Nil
  }
}
