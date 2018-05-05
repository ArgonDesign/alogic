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
// Provide extension methods for BigInt
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.util

import scala.language.implicitConversions

// For importing with BigIntOps._
object BigIntOps {
  implicit final class BigIntOpsImpl(val value: BigInt) extends AnyVal {

    // Extract bit field of width 'width' starting from 'lsb' if 'signed'
    // ensure result is negative if the bit 'lsb + width - 1' is set
    def extract(lsb: Int, width: Int, signed: Boolean = false): BigInt = {
      require(lsb >= 0)
      require(width >= 1)
      val mask = (1 << width) - 1
      val bits = (value >> lsb) & mask
      assert(bits >= 0)
      if (!signed || !bits.testBit(width - 1)) bits else (BigInt(-1) << width) | bits
    } ensuring { result =>
      !signed || (value.testBit(lsb + width - 1) == (result < 0))
    }
  }
}

// For mixing into classes
trait BigIntOps {
  import BigIntOps.BigIntOpsImpl
  implicit final def value2BigIntOpsImpl(value: BigInt): BigIntOpsImpl = new BigIntOpsImpl(value)

}
