////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Simple Antlr base visitor that yields a scalar value
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import org.antlr.v4.runtime.tree.RuleNode

class AlogicScalarVisitor[T] extends AlogicBaseVisitor[T, List[T]] {
  override def visit[U <: RuleNode](ctxList: List[U]): List[T] = ctxList map visit
}
