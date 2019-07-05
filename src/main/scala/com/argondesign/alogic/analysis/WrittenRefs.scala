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
// Compute symbols that are written in expressions
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.analysis

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.util.unreachable

object WrittenRefs {

  // Given an expression, return an iterable of the ExprRefs that would be
  // written should this expression be used on the left hand side of an
  // assignment
  def apply(expr: Expr): Iterator[ExprRef] = {
    expr match {
      case expr: ExprRef            => Iterator.single(expr)
      case ExprCat(parts)           => parts.toIterator flatMap apply
      case ExprIndex(expr, _)       => apply(expr)
      case ExprSlice(expr, _, _, _) => apply(expr)
      case ExprSelect(expr, _)      => apply(expr)
      case _ => {
        println(expr)
        unreachable
      }
    }
  }

}
