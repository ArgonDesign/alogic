////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
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

import com.argondesign.alogic.ast.Trees.EntityIdent
import com.argondesign.alogic.ast.Trees.Root
import com.argondesign.alogic.builtins.Builtins
import com.argondesign.alogic.frontend.Frontend
import com.argondesign.alogic.passes.Passes
import com.argondesign.alogic.util.unreachable

class CompilerContext(val settings: Settings = Settings())
    extends Messaging
    with LocationRemapping
    with Symbols
    with Builtins
    with Output {

  // Shorthand for frequently accessed settings
  val sep = settings.sep

  var postSpecialization = false

  var passNumber = 0

  def compile(toplevels: List[String]): Unit = {
    try {
      //////////////////////////////////////////////////////////////////////////////
      // Create the front end and built the ASTs
      //////////////////////////////////////////////////////////////////////////////

      val frontEndTrees = {
        val frontend = new Frontend(settings.moduleSearchDirs,
                                    settings.includeSearchDirs,
                                    settings.initialDefines)(this)
        frontend(toplevels)
      }

      // Insert entity symbols into the global scope
      addGlobalEntities {
        frontEndTrees map {
          case Root(_, entity: EntityIdent) => entity
          case _                            => unreachable
        }
      }

      //////////////////////////////////////////////////////////////////////////////
      // Compile the trees
      //////////////////////////////////////////////////////////////////////////////

      Passes(frontEndTrees)(this)
    } catch {
      case _: FatalErrorException => ()
    } finally {
      emitMessages(Console.err)
    }
  }
}
