////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Simple Antlr base visitor that yields a list of values
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import org.antlr.v4.runtime.tree.RuleNode

class AlogicListVisitor[T] extends AlogicBaseVisitor[List[T], List[T], List[T]] {
  override def visit[U <: RuleNode](ctxList: List[U]): List[T] = ctxList flatMap visit

  override def visit[U <: RuleNode](ctxOpt: Option[U]): List[T] = ctxOpt match {
    case Some(ctx) => visit(ctx)
    case None      => Nil
  }
}
