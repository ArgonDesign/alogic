////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.analysis

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.util.unreachable
import com.argondesign.alogic.util.BigIntOps._
import scala.collection.immutable.HashMap

object ReadSymbolBits {

  private def addPossiblyRead(
      init: HashMap[Symbol, BigInt],
      expr: Expr
    ): HashMap[Symbol, BigInt] = {
    // Mutable for speed
    var result = init
    expr visit {
      case ExprSym(symbol) =>
        if (symbol.kind.isPacked) {
          result = result.updated(symbol, BigInt.mask(symbol.kind.width.toInt))
        }
      case ExprIndex(ExprSym(symbol), idx) =>
        if (symbol.kind.isPacked) {
          idx.valueOption match {
            case Some(bit) =>
              result = result.updatedWith(symbol) {
                case Some(bits) => Some(bits.setBit(bit.asInt))
                case None       => Some(BigInt.oneHot(bit.asInt))
              }
            case None =>
              result = result.updated(symbol, BigInt.mask(symbol.kind.width.toInt))
          }
        }
        result = addPossiblyRead(result, idx)
      case ExprSlice(ExprSym(symbol), lIdx, op, rIdx) =>
        val newBits = lIdx.valueOption flatMap { ll =>
          rIdx.valueOption map { rr =>
            val l = ll.asInt
            val r = rr.asInt
            op match {
              case ":"  => BigInt.range(l, r)
              case "+:" => BigInt.range(l + r - 1, l)
              case "-:" => BigInt.range(l, l - r + 1)
              case _    => unreachable
            }
          }
        } getOrElse {
          BigInt.mask(symbol.kind.width.toInt)
        }
        result = result.updatedWith(symbol) {
          case Some(bits) => Some(bits | newBits)
          case None       => Some(newBits)
        }
        result = addPossiblyRead(result, lIdx)
        result = addPossiblyRead(result, rIdx)
    }

    result
  }

  // Given an expression, return a SymbolBitSet that holds bits that might be
  // read if this expression is not used on the left hand side of an assignment
  def possiblyRVal(expr: Expr): SymbolBitSet =
    SymbolBitSet.from(addPossiblyRead(HashMap[Symbol, BigInt](), expr))

  // Given an expression, return a SymbolBitSet that holds bits that might be
  // read if this expression is used on the left hand side of an assignment
  def possiblyLVal(lval: Expr): SymbolBitSet = {
    // Immutable for speed
    var result = HashMap[Symbol, BigInt]()

    def gather(expr: Expr): Unit = expr match {
      case _: ExprSym =>
      case ExprIndex(_: ExprSym, idx) =>
        result = addPossiblyRead(result, idx)
      case ExprSlice(_: ExprSym, lIdx, _, rIdx) =>
        result = addPossiblyRead(result, lIdx)
        result = addPossiblyRead(result, rIdx)
      case ExprCat(parts) => parts foreach gather
      case _              => unreachable
    }

    gather(lval)

    SymbolBitSet.from(result)
  }

}
