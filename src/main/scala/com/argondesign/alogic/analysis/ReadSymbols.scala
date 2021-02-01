////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Compute symbols that are read in expressions
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.analysis

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.util.unreachable

object ReadSymbols {

  // Given an expression, return an Iterable of Symbols that would be read
  // if this expression is not used on the left hand side of an assignment
  def rval(expr: Expr): Iterator[Symbol] = expr collect { case ExprSym(symbol) => symbol }

  // Given an expression, return an Iterable of Symbols that would be read
  // if this expression is used on the left hand side of an assignment
  def lval(expr: Expr): Iterator[Symbol] = expr match {
    case _: ExprSym                  => Iterator.empty
    case ExprCat(parts)              => parts.iterator flatMap lval
    case ExprIndex(_, idx)           => rval(idx)
    case ExprSlice(_, lIdx, _, rIdx) => rval(lIdx) ++ rval(rIdx)
    case ExprSel(expr, _)            => rval(expr)
    case ExprSymSel(expr, _)         => rval(expr)
    case ExprDot(expr, _, Nil)       => rval(expr)
    case _                           => unreachable
  }

}
