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
import com.argondesign.alogic.core.Types.TypeUnknown
import com.argondesign.alogic.core.enums.ResetStyle._
import com.argondesign.alogic.passes.Passes
import com.argondesign.alogic.typer.Typer

class CompilerContext(val settings: Settings = Settings())
    extends Messaging
    with LocationRemapping
    with Symbols
    with Builtins
    with Output
    with Profiling {

  // Shorthand for frequently accessed settings
  val sep: String = settings.sep

  // Name of reset signal
  val rst: String = settings.resetStyle match {
    case AsyncLow | SyncLow => "rst_n"
    case _                  => "rst"
  }

  //////////////////////////////////////////////////////////////////////////////
  // Compile the top levels
  //////////////////////////////////////////////////////////////////////////////

  def compile(topLevels: List[String]): Unit = {
    try {
      Passes(topLevels)(cc = this)
    } catch {
      case _: FatalErrorException =>
    } finally {
      emitMessages()
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Entry point to the type checker
  //////////////////////////////////////////////////////////////////////////////

  // Return Some(false) for type error, Some(true) for well typed, and None,
  // if the type cannot be determined due to unresolved type parameters
  def typeCheck(tree: Tree): Boolean = {
    if (!tree.hasTpe) {
      (tree rewrite new Typer()(this)) ensuring { _ eq tree }
    }
    assert(tree.hasTpe)
    assert(tree.tpe != TypeUnknown)
    !tree.tpe.isError
  }

}
