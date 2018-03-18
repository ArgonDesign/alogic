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
// Namer tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.typer

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees.Expr.ImplicitConversions._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.core.Types._
import org.scalatest.FreeSpec

final class CoerceWidthSpec extends FreeSpec with AlogicTest {

  implicit val cc = new CompilerContext

  val dummyLoc = Loc(Source("", ""), 0, 0, 0)

  "CoerceWidth should handle " - {
    "unsized constants" - {
      for {
        (const, width, result, msg) <- List(
          (ExprNum(true, 0), 8, ExprInt(true, 8, 0), ""),
          (ExprNum(false, 0), 8, ExprInt(false, 8, 0), ""),
          (ExprNum(true, 1), 2, ExprInt(true, 2, 1), ""),
          (ExprNum(false, 3), 2, ExprInt(false, 2, 3), ""),
          (ExprNum(true, -2), 2, ExprInt(true, 2, -2), ""),
          (ExprNum(false, 0), 2, ExprInt(false, 2, 0), ""),
          (ExprNum(true, 2), 2, ExprError(), "Value 2 is too large to fit in 2 signed bits"),
          (ExprNum(false, 4), 2, ExprError(), "Value 4 is too large to fit in 2 unsigned bits"),
          (ExprNum(true, -3), 2, ExprError(), "Value -3 is too small to fit in 2 signed bits")
        )
      } {
        s"${const} as ${width} bits" in {
          const withLoc dummyLoc
          CoerceWidth(const, width) shouldBe result
          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.last should beThe[Error](msg)
          }
        }
      }
    }

    "unary operators over unsized constants" - {
      for {
        (expr, width, result) <- List(
          (s"+(0)", 8, ExprUnary("+", ExprInt(false, 8, 0))),
          (s"-(0)", 8, ExprUnary("-", ExprInt(false, 8, 0))),
          (s"~(0)", 8, ExprUnary("~", ExprInt(false, 8, 0))),
          (s"+(0s)", 8, ExprUnary("+", ExprInt(true, 8, 0))),
          (s"-(0s)", 8, ExprUnary("-", ExprInt(true, 8, 0))),
          (s"~(0s)", 8, ExprUnary("~", ExprInt(true, 8, 0)))
        )
      } {
        val text = expr.trim.replaceAll(" +", " ")
        s"${text} as ${width} bits" in {
          CoerceWidth(text.asTree[Expr], width) shouldBe result
          cc.messages shouldBe empty
        }
      }
    }

    "binary operators over unsized constants" - {
      for {
        (expr, width, result) <- List(
          // unsigned unsigned
          ("1   * 2", 8, ExprInt(false, 8, 1) * ExprInt(false, 8, 2)),
          ("1   / 2", 8, ExprInt(false, 8, 1) / ExprInt(false, 8, 2)),
          ("1   % 2", 8, ExprInt(false, 8, 1) % ExprInt(false, 8, 2)),
          ("1   + 2", 8, ExprInt(false, 8, 1) + ExprInt(false, 8, 2)),
          ("1   - 2", 8, ExprInt(false, 8, 1) - ExprInt(false, 8, 2)),
          ("1  << 2", 8, ExprInt(false, 8, 1) << ExprNum(false, 2)),
          ("1  >> 2", 8, ExprInt(false, 8, 1) >> ExprNum(false, 2)),
          ("1 <<< 2", 8, ExprInt(false, 8, 1) <<< ExprNum(false, 2)),
          ("1 >>> 2", 8, ExprInt(false, 8, 1) >>> ExprNum(false, 2)),
          ("1   & 2", 8, ExprInt(false, 8, 1) & ExprInt(false, 8, 2)),
          ("1   ^ 2", 8, ExprInt(false, 8, 1) ^ ExprInt(false, 8, 2)),
          ("1   | 2", 8, ExprInt(false, 8, 1) | ExprInt(false, 8, 2)),
          // unsigned signed
          ("1   * 2s", 8, ExprInt(false, 8, 1) * ExprInt(true, 8, 2)),
          ("1   / 2s", 8, ExprInt(false, 8, 1) / ExprInt(true, 8, 2)),
          ("1   % 2s", 8, ExprInt(false, 8, 1) % ExprInt(true, 8, 2)),
          ("1   + 2s", 8, ExprInt(false, 8, 1) + ExprInt(true, 8, 2)),
          ("1   - 2s", 8, ExprInt(false, 8, 1) - ExprInt(true, 8, 2)),
          ("1  << 2s", 8, ExprInt(false, 8, 1) << ExprNum(true, 2)),
          ("1  >> 2s", 8, ExprInt(false, 8, 1) >> ExprNum(true, 2)),
          ("1 <<< 2s", 8, ExprInt(false, 8, 1) <<< ExprNum(true, 2)),
          ("1 >>> 2s", 8, ExprInt(false, 8, 1) >>> ExprNum(true, 2)),
          ("1   & 2s", 8, ExprInt(false, 8, 1) & ExprInt(true, 8, 2)),
          ("1   ^ 2s", 8, ExprInt(false, 8, 1) ^ ExprInt(true, 8, 2)),
          ("1   | 2s", 8, ExprInt(false, 8, 1) | ExprInt(true, 8, 2)),
          // signed unsigned
          ("1s   * 2", 8, ExprInt(true, 8, 1) * ExprInt(false, 8, 2)),
          ("1s   / 2", 8, ExprInt(true, 8, 1) / ExprInt(false, 8, 2)),
          ("1s   % 2", 8, ExprInt(true, 8, 1) % ExprInt(false, 8, 2)),
          ("1s   + 2", 8, ExprInt(true, 8, 1) + ExprInt(false, 8, 2)),
          ("1s   - 2", 8, ExprInt(true, 8, 1) - ExprInt(false, 8, 2)),
          ("1s  << 2", 8, ExprInt(true, 8, 1) << ExprNum(false, 2)),
          ("1s  >> 2", 8, ExprInt(true, 8, 1) >> ExprNum(false, 2)),
          ("1s <<< 2", 8, ExprInt(true, 8, 1) <<< ExprNum(false, 2)),
          ("1s >>> 2", 8, ExprInt(true, 8, 1) >>> ExprNum(false, 2)),
          ("1s   & 2", 8, ExprInt(true, 8, 1) & ExprInt(false, 8, 2)),
          ("1s   ^ 2", 8, ExprInt(true, 8, 1) ^ ExprInt(false, 8, 2)),
          ("1s   | 2", 8, ExprInt(true, 8, 1) | ExprInt(false, 8, 2)),
          // signed signed
          ("1s   * 2s", 8, ExprInt(true, 8, 1) * ExprInt(true, 8, 2)),
          ("1s   / 2s", 8, ExprInt(true, 8, 1) / ExprInt(true, 8, 2)),
          ("1s   % 2s", 8, ExprInt(true, 8, 1) % ExprInt(true, 8, 2)),
          ("1s   + 2s", 8, ExprInt(true, 8, 1) + ExprInt(true, 8, 2)),
          ("1s   - 2s", 8, ExprInt(true, 8, 1) - ExprInt(true, 8, 2)),
          ("1s  << 2s", 8, ExprInt(true, 8, 1) << ExprNum(true, 2)),
          ("1s  >> 2s", 8, ExprInt(true, 8, 1) >> ExprNum(true, 2)),
          ("1s <<< 2s", 8, ExprInt(true, 8, 1) <<< ExprNum(true, 2)),
          ("1s >>> 2s", 8, ExprInt(true, 8, 1) >>> ExprNum(true, 2)),
          ("1s   & 2s", 8, ExprInt(true, 8, 1) & ExprInt(true, 8, 2)),
          ("1s   ^ 2s", 8, ExprInt(true, 8, 1) ^ ExprInt(true, 8, 2)),
          ("1s   | 2s", 8, ExprInt(true, 8, 1) | ExprInt(true, 8, 2))
        )
      } {
        val text = expr.trim.replaceAll(" +", " ")
        s"${text} as ${width} bits" in {
          CoerceWidth(text.asTree[Expr], width) shouldBe result
          cc.messages shouldBe empty
        }
      }
    }

    "ternary operator over unsized constants" - {
      for {
        (expr, width, result) <- List(
          ("0 ? 1 : 2", 8, ExprTernary(Expr(0), ExprInt(false, 8, 1), ExprInt(false, 8, 2))),
          ("bool ? 1 : 2",
           8,
           ExprTernary(ExprType(TypeUInt(1)), ExprInt(false, 8, 1), ExprInt(false, 8, 2)))
        )
      } {
        val text = expr.trim.replaceAll(" +", " ")
        s"${text} as ${width} bits" in {
          CoerceWidth(text.asTree[Expr], width) shouldBe result
          cc.messages shouldBe empty
        }
      }
    }
  }
}
