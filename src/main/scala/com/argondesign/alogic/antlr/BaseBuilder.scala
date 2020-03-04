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
// Common functionality of AST builders
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import scala.jdk.CollectionConverters._

import com.argondesign.alogic.core.CompilerContext

trait BaseBuilder[C, T] {
  def apply(ctx: C)(implicit cc: CompilerContext): T

  def apply(ctxs: java.util.List[_ <: C])(implicit cc: CompilerContext): List[T] = List from {
    ctxs.iterator.asScala map apply
  }
}
