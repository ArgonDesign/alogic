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
// Compute symbols that are read in expressions
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.analysis

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.util.unreachable

object ReadSymbols {

  // Given an expression, return an Iterable of Symbols that would be read
  // if this expression is not used on the left hand side of an assignment
  def rval(expr: Expr): Iterator[TermSymbol] = {
    expr collect { case ExprRef(symbol: TermSymbol) => symbol }
  }

  // Given an expression, return an Iterable of Symbols that would be read
  // if this expression is used on the left hand side of an assignment
  def lval(expr: Expr): Iterator[TermSymbol] = expr match {
    case _: ExprRef                  => Iterator.empty
    case ExprCat(parts)              => parts.iterator flatMap lval
    case ExprIndex(_, idx)           => rval(idx)
    case ExprSlice(_, lidx, _, ridx) => rval(lidx) ++ rval(ridx)
    case _                           => unreachable
  }

}
