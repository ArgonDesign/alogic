////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Compute symbol bits that are read in expressions
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.analysis

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.util.unreachable
import com.argondesign.alogic.util.BitSetOps._

import scala.collection.immutable.BitSet

object ReadSymbolBits {

  private def usedRvalPairs(
      expr: Expr
    ): Iterator[(Symbol, BitSet)] = expr flatCollect {
    case ExprSym(symbol) if symbol.kind.isPacked =>
      Iterator.single(symbol -> BitSet.whole(symbol))
    case ExprIndex(ExprSym(symbol), idx) if symbol.kind.isPacked =>
      val m = idx.valueOption match {
        case Some(bit) => symbol -> BitSet(bit.toInt)
        case None      => symbol -> BitSet.whole(symbol)
      }
      Iterator.single(m) ++ usedRvalPairs(idx)
    case ExprSlice(ExprSym(symbol), lIdx, op, rIdx) if symbol.kind.isPacked =>
      val m = lIdx.valueOption flatMap { l =>
        rIdx.valueOption map { r =>
          val (msb, lsb) = op match {
            case ":"  => (l, r)
            case "+:" => (l + r - 1, l)
            case "-:" => (l, l - r + 1)
            case _    => unreachable
          }
          symbol -> BitSet.range(lsb.toInt, msb.toInt + 1)
        }
      } getOrElse {
        symbol -> BitSet.whole(symbol)
      }
      Iterator.single(m) ++ usedRvalPairs(lIdx) ++ usedRvalPairs(rIdx)
  }

  // Given an expression, return a SymbolBitSet that holds bits that might be
  // read if this expression is not used on the left hand side of an assignment
  def rval(expr: Expr): SymbolBitSet =
    usedRvalPairs(expr).foldLeft(Map.empty[Symbol, BitSet]) {
      case (acc, (symbol, bits)) =>
        acc.updatedWith(symbol) {
          case Some(prev) => Some(prev union bits)
          case None       => Some(bits)
        }
    }

  // Given an expression, return a SymbolBitSet that holds bits that might be
  // read if this expression is used on the left hand side of an assignment
  def lval(lval: Expr): SymbolBitSet = {
    def gather(expr: Expr): Iterator[(Symbol, BitSet)] = expr match {
      case _: ExprSym                           => Iterator.empty
      case ExprIndex(_: ExprSym, idx)           => usedRvalPairs(idx)
      case ExprSlice(_: ExprSym, lIdx, _, rIdx) => usedRvalPairs(lIdx) ++ usedRvalPairs(rIdx)
      case ExprCat(parts)                       => parts.iterator flatMap gather
      case _                                    => unreachable
    }

    gather(lval).foldLeft(Map.empty[Symbol, BitSet]) {
      case (acc, (symbol, bits)) =>
        acc.updatedWith(symbol) {
          case Some(prev) => Some(prev union bits)
          case None       => Some(bits)
        }
    }
  }

}
