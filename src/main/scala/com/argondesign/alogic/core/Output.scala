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
// Outputting facilities
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import java.io.Writer

import com.argondesign.alogic.ast.Trees._

trait Output { this: CompilerContext =>

  private implicit val implicitThis = this

  def getEntityWriter(entity: Entity, suffix: String): Writer = {
    settings.entityWriterFactory(entity, suffix)
  }

  def dumpEntity(entity: Entity, suffix: String): Unit = {
    val writer = getEntityWriter(entity, suffix + ".alogic")
    writer.write(entity.toSource)
    writer.close()
  }
}
