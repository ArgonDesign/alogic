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
// Liveness analysis
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.analysis

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.util.unreachable

import scala.collection.immutable.BitSet

object Liveness {

  private val buf1 = ThreadLocal.withInitial[Array[Long]](() => Array[Long](0))
  private val buf2 = ThreadLocal.withInitial[Array[Long]](() => Array[Long](0, 0))

  private def bitRange(lo: Int, hi: Int): BitSet =
    if (hi <= 64) {
      val arr = buf1.get()
      arr(0) = (-1L >>> (64 - hi)) & (-1L << lo)
      BitSet.fromBitMask(arr)
    } else if (hi <= 128) {
      val arr = buf2.get()
      if (lo >= 64) {
        arr(1) = (-1L >>> (128 - hi)) & (-1L << (lo - 64))
        arr(0) = 0
      } else {
        arr(1) = -1L >>> (128 - hi)
        arr(0) = -1L << lo
      }
      BitSet.fromBitMask(arr)
    } else {
      val qh = hi / 64
      val rh = hi % 64

      val arr = Array.fill[Long](qh + 1)(-1L)
      arr(qh) = ~(-1L << rh)

      if (lo != 0) {
        var ql = lo / 64
        val rl = lo % 64
        arr(ql) &= -1L << rl
        while (ql > 0) {
          ql -= 1
          arr(ql) = 0
        }
      }

      BitSet.fromBitMaskNoCopy(arr)
    }

  private def usedRvalPairs(
      expr: Expr
    )(
      implicit
      cc: CompilerContext
    ): Iterator[(Symbol, BitSet)] = expr flatCollect {
    case ExprSym(symbol) if symbol.kind.isPacked =>
      Iterator.single(symbol -> bitRange(0, symbol.kind.width.toInt))
    case ExprIndex(ExprSym(symbol), idx) if symbol.kind.isPacked =>
      val m = idx.value match {
        case Some(bit) => symbol -> BitSet(bit.toInt)
        case None      => symbol -> bitRange(0, symbol.kind.width.toInt)
      }
      Iterator.single(m) ++ usedRvalPairs(idx)
    case ExprSlice(ExprSym(symbol), lIdx, op, rIdx) if symbol.kind.isPacked =>
      val m = lIdx.value flatMap { l =>
        rIdx.value map { r =>
          val (msb, lsb) = op match {
            case ":"  => (l, r)
            case "+:" => (l + r - 1, l)
            case "-:" => (l, l - r + 1)
            case _    => unreachable
          }
          symbol -> bitRange(lsb.toInt, msb.toInt + 1)
        }
      } getOrElse {
        symbol -> bitRange(0, symbol.kind.width.toInt)
      }
      Iterator.single(m) ++ usedRvalPairs(lIdx) ++ usedRvalPairs(rIdx)
  }

  // Given an expression, return a SymbolBitSet that holds bits that might be
  // read if this expression is not used on the left hand side of an assignment
  def usedRv(expr: Expr)(implicit cc: CompilerContext): SymbolBitSet =
    usedRvalPairs(expr).foldLeft(Map.empty[Symbol, BitSet]) {
      case (acc, (symbol, bits)) =>
        acc.updatedWith(symbol) {
          case Some(prev) => Some(prev union bits)
          case None       => Some(bits)
        }
    }

  // Given an expression, return a SymbolBitSet that holds bits that might be
  // read if this expression is used on the left hand side of an assignment
  def usedLv(lval: Expr)(implicit cc: CompilerContext): SymbolBitSet = {
    def gather(expr: Expr): Iterator[(Symbol, BitSet)] = expr match {
      case _: ExprSym                           => Iterator.empty
      case ExprIndex(_: ExprSym, idx)           => usedRvalPairs(idx)
      case ExprSlice(_: ExprSym, lIdx, _, rIdx) => usedRvalPairs(lIdx) ++ usedRvalPairs(rIdx)
      case ExprCat(parts)                       => parts.iterator flatMap gather
      case other =>
        throw Ice(other, "Don't know how to extract read variables from lval", other.toSource)
    }

    gather(lval).foldLeft(Map.empty[Symbol, BitSet]) {
      case (acc, (symbol, bits)) =>
        acc.updatedWith(symbol) {
          case Some(prev) => Some(prev union bits)
          case None       => Some(bits)
        }
    }
  }

  // Given an expression, return a SymbolBitSet that holds bits that are known
  // to be written, should this expression be used on the left hand side of an
  // assignment
  def killed(lval: Expr)(implicit cc: CompilerContext): SymbolBitSet = {

    def gather(expr: Expr): Iterator[(Symbol, BitSet)] = expr match {
      case ExprSym(symbol) =>
        Iterator single {
          symbol -> bitRange(0, symbol.kind.width.toInt)
        }
      case ExprIndex(ExprSym(symbol), idx) =>
        idx.value.iterator map { bit =>
          symbol -> BitSet(bit.toInt)
        }
      case ExprSlice(ExprSym(symbol), lIdx, op, rIdx) =>
        lIdx.value.iterator flatMap { l =>
          rIdx.value.iterator map { r =>
            val (msb, lsb) = op match {
              case ":"  => (l, r)
              case "+:" => (l + r - 1, l)
              case "-:" => (l, l - r + 1)
              case _    => unreachable
            }
            symbol -> bitRange(lsb.toInt, msb.toInt + 1)
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
  def apply(stmts: List[Stmt])(implicit cc: CompilerContext): (SymbolBitSet, SymbolBitSet) = {

    def analyse(
        cLive: SymbolBitSet,
        cDead: SymbolBitSet,
        stmts: List[Stmt]
      ): (SymbolBitSet, SymbolBitSet) = stmts match {
      case Nil => (cLive, cDead)
      case head :: tail =>
        val (nLive, nDead) = head match {
          case StmtAssign(lhs, rhs) =>
            val readRhs = usedRv(rhs)
            val readLhs = usedLv(lhs)
            // Everything read is born, unless it's already dead
            val born = (readRhs union readLhs) diff cDead
            val live = cLive union born
            // Everything written is killed unless it's already live
            val kill = killed(lhs) diff live
            val dead = cDead union kill
            (live, dead)

          case StmtOutcall(output, func, inputs) =>
            val readFunc = usedRv(func)
            val readInputs = (inputs map usedRv).foldLeft(SymbolBitSet.empty)(_ union _)
            val readOutput = usedLv(output)
            // Everything read is born, unless it's already dead
            val born = (readFunc union readInputs union readOutput) diff cDead
            val live = cLive union born
            // Everything written is killed unless it's already live
            val kill = killed(output) diff live
            val dead = cDead union kill
            (live, dead)

          case StmtIf(cond, thenStmts, elseStmts) =>
            val born = usedRv(cond) diff cDead
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
            val reads = readers map { usedRv }
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
            val born = usedRv(cond) diff cDead
            val live = cLive union born
            (live, cDead)

          case StmtExpr(expr) =>
            val born = usedRv(expr) diff cDead
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
