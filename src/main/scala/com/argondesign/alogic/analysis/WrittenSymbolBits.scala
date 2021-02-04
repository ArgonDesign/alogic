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
import com.argondesign.alogic.util.BigIntOps._
import scala.collection.immutable.HashMap

object WrittenSymbolBits {

  // Given an expression, return a SymbolBitSet that holds bits that are known
  // to be written, should this expression be used on the left hand side of an
  // assignment
  def definitely(lval: Expr): SymbolBitSet = {
    // Mutable for speed
    var result = HashMap[Symbol, BigInt]()

    def gather(expr: Expr): Unit = expr match {
      case ExprSym(symbol) =>
        result = result.updated(symbol, BigInt.mask(symbol.kind.width.toInt))
      case ExprIndex(ExprSym(symbol), idx) =>
        idx.valueOption.foreach { bit =>
          result = result.updatedWith(symbol) {
            case Some(bits) => Some(bits.setBit(bit.toInt))
            case None       => Some(BigInt.oneHot(bit.toInt))
          }
        }
      case ExprSlice(ExprSym(symbol), lIdx, op, rIdx) =>
        lIdx.valueOption foreach { ll =>
          rIdx.valueOption foreach { rr =>
            val l = ll.asInt
            val r = rr.asInt
            val newBits = op match {
              case ":"  => BigInt.range(l, r)
              case "+:" => BigInt.range(l + r - 1, l)
              case "-:" => BigInt.range(l, l - r + 1)
              case _    => unreachable
            }
            result = result.updatedWith(symbol) {
              case Some(bits) => Some(bits | newBits)
              case None       => Some(newBits)
            }
          }
        }
      case ExprCat(parts) => parts foreach gather
      case other =>
        throw Ice(other, "Don't know how to extract written variables from", other.toSource)
    }

    gather(lval)

    SymbolBitSet.from(result)
  }

}
