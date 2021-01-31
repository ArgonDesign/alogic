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
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.util.unreachable
import com.argondesign.alogic.util.BitSetOps._

import scala.collection.immutable.BitSet

object Liveness {

  // Given an expression, return a SymbolBitSet that holds bits that are known
  // to be written, should this expression be used on the left hand side of an
  // assignment
  def killed(lval: Expr): SymbolBitSet = {

    def gather(expr: Expr): Iterator[(Symbol, BitSet)] = expr match {
      case ExprSym(symbol) =>
        Iterator single {
          symbol -> BitSet.range(0, symbol.kind.width.toInt)
        }
      case ExprIndex(ExprSym(symbol), idx) =>
        idx.valueOption.iterator map { bit =>
          symbol -> BitSet(bit.toInt)
        }
      case ExprSlice(ExprSym(symbol), lIdx, op, rIdx) =>
        lIdx.valueOption.iterator flatMap { l =>
          rIdx.valueOption.iterator map { r =>
            val (msb, lsb) = op match {
              case ":"  => (l, r)
              case "+:" => (l + r - 1, l)
              case "-:" => (l, l - r + 1)
              case _    => unreachable
            }
            symbol -> BitSet.range(lsb.toInt, msb.toInt + 1)
          }
        }
      case ExprCat(parts) => parts.iterator flatMap gather
      case other =>
        throw Ice(other, "Don't know how to extract written variables from", other.toSource)
    }

    gather(lval).foldLeft(Map.empty[Symbol, BitSet]) {
      case (acc, (symbol, bits)) =>
        acc.updatedWith(symbol) {
          case Some(prev) => Some(prev union bits)
          case None       => Some(bits)
        }
    }
  }

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
            val readRhs = ReadSymbolBits.rval(rhs)
            val readLhs = ReadSymbolBits.lval(lhs)
            // Everything read is born, unless it's already dead
            val born = (readRhs union readLhs) diff cDead
            val live = cLive union born
            // Everything written is killed unless it's already live
            val kill = killed(lhs) diff live
            val dead = cDead union kill
            (live, dead)

          case StmtOutcall(output, func, inputs) =>
            val readFunc = ReadSymbolBits.rval(func)
            val readInputs =
              (inputs map ReadSymbolBits.rval).foldLeft(SymbolBitSet.empty)(_ union _)
            val readOutput = ReadSymbolBits.lval(output)
            // Everything read is born, unless it's already dead
            val born = (readFunc union readInputs union readOutput) diff cDead
            val live = cLive union born
            // Everything written is killed unless it's already live
            val kill = killed(output) diff live
            val dead = cDead union kill
            (live, dead)

          case StmtIf(cond, thenStmts, elseStmts) =>
            val born = ReadSymbolBits.rval(cond) diff cDead
            val dLive = cLive union born

            val (tLive, tDead) = analyse(dLive, cDead, thenStmts)
            val (eLive, eDead) = analyse(dLive, cDead, elseStmts)

            val live = tLive union eLive
            val dead = tDead intersect eDead

            (live, dead)

          case StmtCase(expr, cases) =>
            val caseReaders = cases flatMap {
              case CaseRegular(cond, _) => cond
              case _: CaseDefault       => Nil
              case _: CaseSplice        => unreachable
            }
            val readers = expr :: caseReaders
            val reads = readers map { ReadSymbolBits.rval }
            val born = reads reduce { _ union _ } diff cDead
            val dLive = cLive union born

            val (bLive, bDead) = {
              val sets = cases map {
                case CaseRegular(_, stmt) => analyse(dLive, cDead, stmt)
                case CaseDefault(stmt)    => analyse(dLive, cDead, stmt)
                case _: CaseSplice        => unreachable
              }

              val explicitDefault = cases exists {
                case _: CaseDefault => true
                case _              => false
              }

              (if (explicitDefault) sets else (dLive, cDead) :: sets).unzip
            }

            val live = bLive reduce { _ union _ }
            val dead = bDead reduce { _ intersect _ }

            (live, dead)

          case StmtWait(cond) =>
            val born = ReadSymbolBits.rval(cond) diff cDead
            val live = cLive union born
            (live, cDead)

          case StmtExpr(expr) =>
            val born = ReadSymbolBits.rval(expr) diff cDead
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
