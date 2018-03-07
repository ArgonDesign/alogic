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
// Tests for Math
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.lib

import com.argondesign.alogic.AlogicTest

import org.scalatest.FreeSpec

final class MathSpec extends FreeSpec with AlogicTest {

  "Math" - {
    "clog2" - {
      for {
        (value, expected) <- List(
          (0, 0),
          (1, 0),
          (2, 1),
          (3, 2),
          (4, 2),
          (5, 3),
          (6, 3),
          (7, 3),
          (8, 3),
          (9, 4),
          (16, 4),
          (17, 5),
          (32, 5),
          (33, 6),
          (64, 6),
          (65, 7),
          (128, 7),
          (129, 8),
          (255, 8)
        )
      } s"clog2($value) == $expected" in {
        Math.clog2(value) shouldBe expected
      }
    }
  }

}
