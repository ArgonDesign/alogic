////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Liveness analysis
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.analysis

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.util.unreachable

object Liveness {

  // Perform bit-wise accurate liveness analysis of given statements. It
  // returns the 'live' and 'dead' bit sets. The 'live' map  contains bits
  // that might be read, before being assigned in the statements, and the
  // 'dead' map contains bits that are definitely assigned before being
  // read in the statements. Note that a bit that is not present in either
  // maps only means that those bits are neither read, nor assigned in the
  // given statements.
  def apply(stmts: List[Stmt]): (SymbolBitSet, SymbolBitSet) = {

    def analyse(
        cLive: SymbolBitSet,
        cDead: SymbolBitSet,
        stmts: List[Stmt]
      ): (SymbolBitSet, SymbolBitSet) = stmts match {
      case Nil => (cLive, cDead)
      case head :: tail =>
        val (nLive, nDead) = head match {
          case StmtAssign(lhs, rhs) =>
            val readRhs = ReadSymbolBits.possiblyRVal(rhs)
            val readLhs = ReadSymbolBits.possiblyLVal(lhs)
            // Everything read is born, unless it's already dead
            val born = (readRhs union readLhs) diff cDead
            val live = cLive union born
            // Everything written is killed unless it's already live
            val kill = WrittenSymbolBits.definitely(lhs) diff live
            val dead = cDead union kill
            (live, dead)

          case StmtOutcall(output, func, inputs) =>
            val readFunc = ReadSymbolBits.possiblyRVal(func)
            val readInputs =
              (inputs map ReadSymbolBits.possiblyRVal).foldLeft(SymbolBitSet.empty)(_ union _)
            val readOutput = ReadSymbolBits.possiblyLVal(output)
            // Everything read is born, unless it's already dead
            val born = (readFunc union readInputs union readOutput) diff cDead
            val live = cLive union born
            // Everything written is killed unless it's already live
            val kill = WrittenSymbolBits.definitely(output) diff live
            val dead = cDead union kill
            (live, dead)

          case StmtIf(cond, thenStmts, elseStmts) =>
            val born = ReadSymbolBits.possiblyRVal(cond) diff cDead
            val dLive = cLive union born

            val (tLive, tDead) = analyse(dLive, cDead, thenStmts)
            val (eLive, eDead) = analyse(dLive, cDead, elseStmts)

            val live = tLive union eLive
            val dead = tDead intersect eDead

            (live, dead)

          case s @ StmtCase(expr, cases) =>
            val caseReaders = cases flatMap {
              case CaseRegular(cond, _) => cond
              case _: CaseDefault       => Nil
              case _: CaseSplice        => unreachable
            }
            val readers = expr :: caseReaders
            val reads = readers map { ReadSymbolBits.possiblyRVal }
            val born = reads reduce { _ union _ } diff cDead
            val dLive = cLive union born

            val (bLive, bDead) = {
              val sets = cases map {
                case CaseRegular(_, stmt) => analyse(dLive, cDead, stmt)
                case CaseDefault(stmt)    => analyse(dLive, cDead, stmt)
                case _: CaseSplice        => unreachable
              }
              (if (s.isFullCase) sets else (dLive, cDead) :: sets).unzip
            }

            val live = bLive reduce { _ union _ }
            val dead = bDead reduce { _ intersect _ }

            (live, dead)

          case StmtWait(cond) =>
            val born = ReadSymbolBits.possiblyRVal(cond) diff cDead
            val live = cLive union born
            (live, cDead)

          case StmtExpr(expr) =>
            val born = ReadSymbolBits.possiblyRVal(expr) diff cDead
            val live = cLive union born
            (live, cDead)

          case StmtBlock(body) => analyse(cLive, cDead, body)

          case _: StmtComment => (cLive, cDead)
          case _              => unreachable
        }

        analyse(nLive, nDead, tail)
    }

    analyse(SymbolBitSet.empty, SymbolBitSet.empty, stmts)
  }

}
