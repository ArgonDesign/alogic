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
// The final standard pass implementing code generation
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.backend

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.passes.PairsTransformerPass

import scala.collection.parallel.CollectionConverters._

object CodeGeneration extends PairsTransformerPass {
  val name = "code-generation"

  override protected def process(
      input: Iterable[(Decl, Defn)]
    )(
      implicit
      cc: CompilerContext
    ): Iterable[(Decl, Defn)] = {
    // Create the details objects for all entities, collect them into a map,
    // passing the resulting map as a by name argument to EntityDetails itself,
    // this way EntityDetails can use details of other entities to figure out
    // their own details.
    lazy val entityDetails: Map[Symbol, EntityDetails] = Map from {
      for ((decl: DeclEntity, defn: DefnEntity) <- input) yield {
        val details = new EntityDetails(decl, defn, entityDetails)
        decl.symbol -> details
      }
    }

    // Generate code in parallel
    entityDetails.values.par foreach { details =>
      val verilog = new MakeVerilog(details, entityDetails).moduleSource

      val writer = cc.getOutputWriter(details.decl, ".v")
      writer.write(cc.settings.header)
      writer.write(verilog)
      writer.close()
    }

    // Job done
    Nil
  }

}
