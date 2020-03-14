////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.specialize

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext

import scala.collection.mutable

private[specialize] object SimplifyParamBindings {
  def apply(paramBindings: ParamBindings)(implicit cc: CompilerContext): ParamBindings = {
    val transform: StatefulTreeTransformer = new StatefulTreeTransformer {
      override val typed = false

      // Number of tick children this Expr node has
      private val ticks = mutable.Stack[Int](0)

      override def enter(tree: Tree): Option[Tree] = tree match {
        // Don't try to fold solitary symbols as some of these (like polymorphic
        // built-ins are not well formed on their own. They will be folded in
        // the context they are used instead
        case _: ExprSym => Some(tree)
        case ExprUnary("'", expr) =>
          ticks push 0 // For the walk of the operand
          Some(ExprUnary("'", walk(expr).asInstanceOf[Expr]) withLoc tree.loc) tap { _ =>
            ticks.pop() // Pop push above
            ticks push (ticks.pop() + 1) // update count of ticks under parent
          }
        case _: Expr =>
          ticks push 0
          None
        case _ => None
      }

      override def transform(tree: Tree): Tree = tree match {
        case expr: Expr =>
          // Simplify it if it has no tick children, otherwise leave it alone
          if (ticks.pop() == 0) expr.simplify else tree
        case _ =>
          tree
      }
    }

    paramBindings match {
      case ParamBindingsPositional(params) =>
        ParamBindingsPositional(params map { _ rewrite transform })
      case ParamBindingsNamed(params) =>
        ParamBindingsNamed(Map from { params.view mapValues { _ rewrite transform } })
    }
  }
}
