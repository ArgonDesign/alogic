////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017 Argon Design Ltd. All rights reserved.
//
// Module : Scala Alogic Compiler
// Author : Peter de Rivaz/Geza Lore
//
// DESCRIPTION:
//
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic

import org.antlr.v4.runtime.tree.RuleNode

class VScalarVisitor[T] extends VBaseVisitor[T, List[T], Option[T]] {
  override def visit[U <: RuleNode](ctxList: List[U]): List[T] = ctxList map visit

  override def visit[U <: RuleNode](ctxOpt: Option[U]): Option[T] = ctxOpt map visit
}
