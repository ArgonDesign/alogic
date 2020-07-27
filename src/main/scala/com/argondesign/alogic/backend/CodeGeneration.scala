////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
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
    // Create the details objects for all entities.
    val entityDetails: Map[Symbol, EntityDetails] = Map from {
      for ((decl: DeclEntity, defn: DefnEntity) <- input)
        yield decl.symbol -> new EntityDetails(decl, defn)
    }

    // Generate code in parallel
    entityDetails.values.par foreach { details =>
      val verilog = new MakeVerilog(details).moduleSource
      val writer = cc.getOutputWriter(details.decl, ".v")
      writer.write(cc.settings.header)
      writer.write(verilog)
      writer.close()
    }

    // Job done
    Nil
  }

}
