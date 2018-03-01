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
// Members of ast.Trees.Entity
// These are factored out into a separate file to keep ast.Trees readable
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.util.unreachable

import Trees.Entity

trait EntityOps { this: Entity =>
  private[this] var _variant: String = _ // scalastyle:ignore var.field

  def hasVariant: Boolean = _variant != null // scalastyle:ignore null

  def variant: String = if (hasVariant) _variant else unreachable

  def withVariant(variant: String): this.type = {
    if (hasVariant) {
      unreachable
    } else {
      _variant = variant
    }
    this
  }
}
