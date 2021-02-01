////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// The final standard pass implementing code generation
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.backend

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.ParOrSeqMap.ImmutableMapToParOrSeqMap
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.passes.Pairs
import com.argondesign.alogic.passes.SimplePass
import com.argondesign.alogic.util.unreachable

import scala.collection.immutable.Map

object CodeGeneration extends SimplePass[Pairs, Unit] {
  val name = "code-generation"

  override protected def process(input: Pairs)(implicit cc: CompilerContext): Unit = {
    // Create the details objects for all entities.
    val entityDetails: Map[Symbol, EntityDetails] = Map from {
      input.iterator.map {
        case (decl: DeclEntity, defn: DefnEntity) => decl.symbol -> new EntityDetails(decl, defn)
        case _                                    => unreachable
      }
    }

    // Generate code in parallel
    entityDetails.asPar foreach {
      case (_, details) =>
        val verilog = new MakeVerilog(details).moduleSource
        val writer = cc.getOutputWriter(details.decl, ".v")
        writer.write(cc.settings.header)
        writer.write(verilog)
        writer.close()
    }
  }

}
