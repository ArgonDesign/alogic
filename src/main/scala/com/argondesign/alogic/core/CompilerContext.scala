////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// The CompilerContext holds all mutable state of the compiler.
// Throughout the compiler, the CompilerContext is held in a variable called
// 'cc', which is often passed as an implicit parameter.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.ast.Trees.Tree
import com.argondesign.alogic.builtins.Builtins
import com.argondesign.alogic.core.Messages.Fatal
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Types.TypeUnknown
import com.argondesign.alogic.core.enums.ResetStyle._
import com.argondesign.alogic.passes.Passes
import com.argondesign.alogic.typer.Typer

import scala.collection.mutable

class CompilerContext(
    val messageBuffer: MessageBuffer = new MessageBuffer(),
    val settings: Settings = Settings())
    extends Messaging
    with Symbols
    with Builtins
    with Output
    with Profiling
    with StatelessTransforms {

  // Shorthand for frequently accessed settings
  val sep: String = settings.sep

  // Name of reset signal
  val rst: String = settings.resetStyle match {
    case AsyncLow | SyncLow => "rst_n"
    case _                  => "rst"
  }

  val manifest: mutable.Map[String, Any] = mutable.LinkedHashMap[String, Any]()

  val stats: mutable.Map[(String, String), Any] = mutable.Map.empty

  //////////////////////////////////////////////////////////////////////////////
  // Compile the top levels
  //////////////////////////////////////////////////////////////////////////////

  def compile(topLevels: List[String]): Unit = {
    try {
      Passes(topLevels)(cc = this)
    } catch {
      // Catch fatal messages, add them to message buffer for reporting,
      // then return normally from here.
      case message: Fatal => addMessage(message)
      case message: Ice   => addMessage(message)
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Entry point to the type checker
  //////////////////////////////////////////////////////////////////////////////

  // Type check tree
  def typeCheck(tree: Tree): Boolean = {
    if (!tree.hasTpe) {
      (tree rewrite new Typer()(this)) ensuring { _ eq tree }
    }
    assert(tree.hasTpe)
    assert(tree.tpe != TypeUnknown)
    !tree.tpe.isError
  }

}
