////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Add entities constructed by the factories in cc into the global list
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.core.CompilerContext

import scala.collection.immutable.ListMap

object AddSyntheticEntities extends PairsTransformerPass {
  val name = "add-constructed-entities"

  override protected def process(input: Pairs)(implicit cc: CompilerContext): Pairs = {
    // Add required SRAM sizes to the manifest
    cc.manifest("sram-sizes") = List from {
      cc.sramFactory.items.map(_._1).toSeq.sorted map {
        case (depth, width) => ListMap("depth" -> depth, "width" -> width)
      }
    }
    // Add all synthetic entities
    input ++
      cc.syncRegFactory.items.map(_._2) ++
      cc.syncSliceFactory.items.map(_._2) ++
      cc.sramFactory.items.map(_._2) ++
      cc.stackFactory.items.map(_._2)
  }

}
