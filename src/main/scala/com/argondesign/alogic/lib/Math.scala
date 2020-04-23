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
// Utility math functions
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.lib

import scala.annotation.tailrec

final object Math {

  def clog2(value: BigInt): Int = {
    require(value >= 0)
    @tailrec
    def loop(value: BigInt, acc: Int): Int = {
      if (value > 0) {
        loop(value >> 1, acc + 1)
      } else {
        acc
      }
    }
    loop(value - 1, 0)
  }

}
