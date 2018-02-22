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
// Simple Antlr base visitor that yields a scalar value
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import org.antlr.v4.runtime.tree.RuleNode

class AlogicScalarVisitor[T] extends AlogicBaseVisitor[T, List[T], Option[T]] {
  override def visit[U <: RuleNode](ctxList: List[U]): List[T] = ctxList map visit

  override def visit[U <: RuleNode](ctxOpt: Option[U]): Option[T] = ctxOpt map visit
}
