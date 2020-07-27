////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Provide extension methods for BigInt
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.util

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.ExprInt
import com.argondesign.alogic.ast.Trees.ExprNum
import com.argondesign.alogic.core.Types.Type

object BigIntOps {

  implicit final class ClassOps(private val value: BigInt) extends AnyVal {

    // Extract bit field of width 'width' starting from 'lsb' if 'signed'
    // ensure result is negative if the bit 'lsb + width - 1' is set
    def extract(lsb: Int, width: Int, signed: Boolean = false): BigInt = {
      require(lsb >= 0)
      require(width >= 1)
      val mask = (BigInt(1) << width) - 1
      val bits = (value >> lsb) & mask
      assert(bits >= 0)
      if (!signed || !bits.testBit(width - 1)) bits else (BigInt(-1) << width) | bits
    } ensuring { result =>
      !signed || (value.testBit(lsb + width - 1) == (result < 0))
    }

    def asExpr(kind: Type): Expr = {
      require(kind.isFund)
      require(kind.isNumeric || kind.isPacked)
      if (kind.isNum) {
        ExprNum(kind.isSigned, value)
      } else if (kind.isInt) {
        ExprInt(kind.isSigned, kind.width.toInt, value)
      } else { // kind.isPacked
        ExprCast(kind.asFund, ExprInt(kind.isSigned, kind.width.toInt, value))
      }
    }

  }

  implicit final class ObjectOps(private val value: BigInt.type) extends AnyVal {
    def mask(width: Int): BigInt = (BigInt(1) << width) - 1
    def mask(width: BigInt): BigInt = (BigInt(1) << width.toInt) - 1

    def oneHot(bit: Int): BigInt = BigInt(1) << bit
    def oneHot(bit: BigInt): BigInt = BigInt(1) << bit.toInt
  }

}
