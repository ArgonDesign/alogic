////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Provide extension methods for BitSet
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.util

import com.argondesign.alogic.core.Symbols.Symbol

import scala.collection.immutable
import scala.collection.immutable.BitSet

object BitSetOps {

  private val buf1 = ThreadLocal.withInitial[Array[Long]](() => Array[Long](0))
  private val buf2 = ThreadLocal.withInitial[Array[Long]](() => Array[Long](0, 0))

  implicit final class BitSetObjectOps(private val unused: immutable.BitSet.type) extends AnyVal {

    def whole(symbol: Symbol): BitSet = BitSet.range(0, symbol.kind.width.toInt)

    def range(lo: Int, hi: Int): BitSet =
      // This mess is for no reason but speed
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

  }

}
