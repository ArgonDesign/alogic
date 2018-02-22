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
// Build a StorageType from an Antlr4 parse tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import scala.collection.JavaConverters._

import com.argondesign.alogic.antlr.AlogicParser.Storage_typeContext
import com.argondesign.alogic.antlr.AlogicParser.StorageTypeSlicesContext
import com.argondesign.alogic.antlr.AlogicParser.StorageTypeWireContext
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.StorageTypes.StorageSliceBReg
import com.argondesign.alogic.core.StorageTypes.StorageSliceBubble
import com.argondesign.alogic.core.StorageTypes.StorageSliceFReg
import com.argondesign.alogic.core.StorageTypes.StorageType
import com.argondesign.alogic.core.StorageTypes.StorageTypeSlices
import com.argondesign.alogic.core.StorageTypes.StorageTypeWire
import com.argondesign.alogic.util.unreachable

object StorageTypeBuilder extends BaseBuilder[Storage_typeContext, StorageType] {

  def apply(ctx: Storage_typeContext)(implicit cc: CompilerContext): StorageType = {
    object Visitor extends AlogicScalarVisitor[StorageType] {
      override def visitStorageTypeWire(ctx: StorageTypeWireContext) = StorageTypeWire
      override def visitStorageTypeSlices(ctx: StorageTypeSlicesContext) = {
        val kinds = ctx.slices.asScala.toList map {
          _.text match {
            case "bubble" => StorageSliceBubble
            case "freg"   => StorageSliceFReg
            case "breg"   => StorageSliceBReg
            case _        => unreachable
          }
        }
        StorageTypeSlices(kinds)
      }
    }

    Visitor(ctx)
  }

}
