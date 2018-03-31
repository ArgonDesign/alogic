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
// Base trait for things that can have source attributes attached
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.ast.Trees.Expr
import com.argondesign.alogic.util.unreachable

trait Attributes {

  // Can only be set once, and cannot be observed as null
  private[this] var _attr: Map[String, Expr] = _ // scalastyle:ignore var.field

  def hasAttr: Boolean = _attr != null

  def attr: Map[String, Expr] = if (hasAttr) _attr else unreachable

  def withAttr(attr: Map[String, Expr]): this.type = {
    if (hasAttr) {
      unreachable
    } else {
      _attr = attr
    }
    this
  }
}
