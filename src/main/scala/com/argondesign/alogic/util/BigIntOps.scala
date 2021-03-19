////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
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

  implicit final class BigIntClassOps(private val value: BigInt) extends AnyVal {

    // Extract bit field of width 'width' starting from 'lsb' if 'signed'
    // ensure result is negative if the bit 'lsb + width - 1' is set
    def extract(lsb: Int, width: Int, signed: Boolean = false): BigInt = {
      require(lsb >= 0)
      require(width >= 1)
      val mask = BigInt.mask(width)
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

    def asU(width: Int): BigInt = {
      require(width >= 0)
      if (width == 0) 0 else extract(0, width)
    }

    def asI(width: Int): BigInt = {
      require(width >= 0)
      if (width == 0) 0 else extract(0, width, signed = true)
    }

    def asInt: Int = value.bigInteger.intValueExact
    def asLong: Long = value.bigInteger.longValueExact
  }

  implicit final class BigIntObjectOps(private val unused: BigInt.type) extends AnyVal {
    def mask(width: Int): BigInt = (BigInt(1) << width) - 1

    def range(msb: Int, lsb: Int): BigInt = BigInt.mask(msb - lsb + 1) << lsb

    def oneHot(width: Int): BigInt = BigInt(1) << width

    def uMin(width: Int): BigInt = 0
    def uMax(width: Int): BigInt = mask(width)

    def iMin(width: Int): BigInt = BigInt(-1) << (width - 1)
    def iMax(width: Int): BigInt = mask(width - 1)
  }

}
