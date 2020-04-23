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
// Tests for Matrix
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.lib

import com.argondesign.alogic.AlogicTest
import org.scalatest.freespec.AnyFreeSpec

final class MatrixSpec extends AnyFreeSpec with AlogicTest {

  val square = Matrix(
    List(
      List(0, 1, 2),
      List(3, 4, 5),
      List(6, 7, 8)
    )
  )

  val rect = Matrix(
    List(
      List(0, 1, 2),
      List(3, 4, 5)
    )
  )

  "Matrix" - {
    "transpose" - {
      "square" in {
        square.transpose shouldBe Matrix(
          List(
            List(0, 3, 6),
            List(1, 4, 7),
            List(2, 5, 8)
          )
        )
      }

      "rect" in {
        rect.transpose shouldBe Matrix(
          List(
            List(0, 3),
            List(1, 4),
            List(2, 5)
          )
        )
      }
    }

    "+" - {
      "square" in {
        (square + square) shouldBe Matrix(
          List(
            List(0, 2, 4),
            List(6, 8, 10),
            List(12, 14, 16)
          )
        )
      }

      "rect" in {
        (rect + rect) shouldBe Matrix(
          List(
            List(0, 2, 4),
            List(6, 8, 10)
          )
        )
      }
    }

    "*" - {
      "square" in {
        (square * square) shouldBe Matrix(
          List(
            List(15, 18, 21),
            List(42, 54, 66),
            List(69, 90, 111)
          )
        )
      }

      "rect" in {
        (rect * rect.transpose) shouldBe Matrix(
          List(
            List(5, 14),
            List(14, 50)
          )
        )
      }
    }

    "pow" - {
      "1" in {
        (square pow 1) shouldBe square
      }

      "2" in {
        (square pow 2) shouldBe (square * square)
      }

      "3" in {
        (square pow 3) shouldBe (square * square * square)
      }

      "4" in {
        (square pow 4) shouldBe (square * square * square * square)
      }

      "5" in {
        (square pow 5) shouldBe (square * square * square * square * square)
      }
    }

    "diagonal" in {
      square.diagonal shouldBe List(0, 4, 8)
    }

    "zeros" - {
      "1" in {
        Matrix.zeros[Int](1) shouldBe Matrix(
          List(
            List(0)
          )
        )
      }
      "2" in {
        Matrix.zeros[Int](2) shouldBe Matrix(
          List(
            List(0, 0),
            List(0, 0)
          )
        )
      }

      "3" in {
        Matrix.zeros[Int](3) shouldBe Matrix(
          List(
            List(0, 0, 0),
            List(0, 0, 0),
            List(0, 0, 0)
          )
        )
      }
    }

  }

}
