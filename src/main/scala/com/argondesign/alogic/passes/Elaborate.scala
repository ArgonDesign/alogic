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

import scala.util.ChainingSyntax

object Elaborate extends Pass[(List[Root], List[Expr]), List[(Decl, Defn)]] with ChainingSyntax {
  val name = "elaborate"

  override def dump(result: List[(Decl, Defn)], tag: String)(implicit cc: CompilerContext): Unit =
    result foreach {
      case (decl, defn) => cc.dump(decl, defn, "." + tag)
    }

  def process(input: (List[Root], List[Expr]))(implicit cc: CompilerContext): List[(Decl, Defn)] = {
    val topLevelSpecs = input._2
    val specializedTopLevelDescs = Specialize(topLevelSpecs)
    specializedTopLevelDescs map { _.toList } getOrElse Nil
  }
}
