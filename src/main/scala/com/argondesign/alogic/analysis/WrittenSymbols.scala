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
import com.argondesign.alogic.core.Symbols.Symbol

object WrittenSymbols {

  // Given an expression, return an iterable of symbols that would be written
  // should this expression be used on the left hand side of an assignment
  def apply(expr: Expr): Iterator[Symbol] = WrittenSyms(expr) collect {
    case ExprSym(symbol) => symbol
  }

}
