////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.analysis

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.util.unreachable
import com.argondesign.alogic.util.BitSetOps._

import scala.collection.immutable.BitSet

object WrittenSymbolBits {

  // Given an expression, return a SymbolBitSet that holds bits that are known
  // to be written, should this expression be used on the left hand side of an
  // assignment
  def definitely(lval: Expr): SymbolBitSet = {

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

}
