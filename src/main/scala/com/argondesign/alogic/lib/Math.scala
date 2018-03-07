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

import scala.math.Integral.Implicits.infixIntegralOps

final object Math {

  def clog2(value: BigInt): Int = {
    require(value >= 0)
    if (value <= 1) {
      0
    } else {
      var x = value - 1
      var y = 0
      while (x > 0) {
        x = x / 2
        y += 1
      }
      y
    }
  }
}
