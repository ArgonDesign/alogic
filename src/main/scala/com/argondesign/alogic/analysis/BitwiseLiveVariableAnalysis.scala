////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.analysis

import com.argondesign.alogic.ast.Trees._

object BitwiseLiveVariableAnalysis extends DataFlowAnalysis[SymbolBitSet] {

  //////////////////////////////////////////////////////////////////////////////
  // Direction
  //////////////////////////////////////////////////////////////////////////////

  val directionForward: Boolean = false

  //////////////////////////////////////////////////////////////////////////////
  // Meet operator
  //////////////////////////////////////////////////////////////////////////////

  def meet(x: SymbolBitSet, y: SymbolBitSet): SymbolBitSet = x union y

  //////////////////////////////////////////////////////////////////////////////
  // Transfer functions
  //////////////////////////////////////////////////////////////////////////////

  def f(curr: SymbolBitSet, stmt: StmtIf): SymbolBitSet =
    curr union ReadSymbolBits.possiblyRVal(stmt.cond)

  def f(curr: SymbolBitSet, stmt: StmtCase): SymbolBitSet =
    curr union ReadSymbolBits.possiblyRVal(stmt.expr)

  def f(curr: SymbolBitSet, kase: CaseRegular): SymbolBitSet =
    kase.cond.foldLeft(curr) {
      case (curr, cond) => curr union ReadSymbolBits.possiblyRVal(cond)
    }

  def f(curr: SymbolBitSet, stmt: StmtAssign): SymbolBitSet =
    curr
      .diff(WrittenSymbolBits.definitely(stmt.lhs))
      .union(ReadSymbolBits.possiblyLVal(stmt.lhs))
      .union(ReadSymbolBits.possiblyRVal(stmt.rhs))

  def f(curr: SymbolBitSet, stmt: StmtDelayed): SymbolBitSet =
    curr
      .diff(WrittenSymbolBits.definitely(stmt.lhs))
      .union(ReadSymbolBits.possiblyLVal(stmt.lhs))
      .union(ReadSymbolBits.possiblyRVal(stmt.rhs))

  def f(curr: SymbolBitSet, stmt: StmtOutcall): SymbolBitSet =
    stmt.inputs.foldLeft(
      curr
        .diff(WrittenSymbolBits.definitely(stmt.output))
        .union(ReadSymbolBits.possiblyLVal(stmt.output))
        .union(ReadSymbolBits.possiblyRVal(stmt.func))
    ) {
      case (curr, input) => curr.union(ReadSymbolBits.possiblyRVal(input))
    }

  def f(curr: SymbolBitSet, stmt: StmtExpr): SymbolBitSet =
    curr union ReadSymbolBits.possiblyRVal(stmt.expr)

  //////////////////////////////////////////////////////////////////////////////
  // Boundary condition
  //////////////////////////////////////////////////////////////////////////////

  val defaultBoundaryCondition: SymbolBitSet = SymbolBitSet.empty
}
