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
// Tests for WrittenSymbols
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.analysis

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.passes.FoldExpr
import com.argondesign.alogic.passes.Namer
import com.argondesign.alogic.typer.Typer
import org.scalatest.FreeSpec

import scala.collection.immutable.BitSet

class LivenessSpec extends FreeSpec with AlogicTest {

  implicit val cc = new CompilerContext()

  val typer = new Typer(paramsOnly = false)
  val fold = new FoldExpr(assignTypes = true, foldRefs = true)

  val aSymbol = cc.newTermSymbol("a", Loc.synthetic, TypeUInt(Expr(4) regularize Loc.synthetic))
  val bSymbol = cc.newTermSymbol("b", Loc.synthetic, TypeUInt(Expr(8) regularize Loc.synthetic))
  val cSymbol = cc.newTermSymbol("c", Loc.synthetic, TypeUInt(Expr(256) regularize Loc.synthetic))

  val aRef = ExprRef(aSymbol)
  val bRef = ExprRef(bSymbol)
  val cRef = ExprRef(cSymbol)

  def zext(n: Int, expr: Expr) = ExprCat(List(ExprInt(false, n, 0), expr))

  val randBitCall = {
    ("@randbit()".asTree[Expr] rewrite new Namer rewrite new Typer(paramsOnly = false))
      .asInstanceOf[ExprCall]
  }

  def prep(expr: Expr): Expr = {
    expr assignLocs Loc.synthetic
    val res = (expr rewrite typer rewrite fold).asInstanceOf[Expr]
    cc.messages shouldBe empty
    res
  }

  def killed(expr: Expr) = Liveness.killed(prep(expr))

  def usedLval(expr: Expr) = Liveness.usedLv(prep(expr))

  def usedRval(expr: Expr) = Liveness.usedRv(prep(expr))

  "Liveness" - {

    "killed" - {

      "should recognize complete ref" - {
        "a" in {
          killed(aRef) shouldBe SymbolBitSet(Map(aSymbol -> BitSet(0 to 3: _*)))
        }
        "b" in {
          killed(bRef) shouldBe SymbolBitSet(Map(bSymbol -> BitSet(0 to 7: _*)))
        }
        "c" in {
          killed(cRef) shouldBe SymbolBitSet(Map(cSymbol -> BitSet(0 to 255: _*)))
        }
      }

      "should recognize constant indices of ref" - {
        "a[0]" in {
          killed(aRef index 0) shouldBe SymbolBitSet(Map(aSymbol -> BitSet(0)))
        }
        "a[3]" in {
          killed(aRef index 3) shouldBe SymbolBitSet(Map(aSymbol -> BitSet(3)))
        }
        "b[0]" in {
          killed(bRef index 0) shouldBe SymbolBitSet(Map(bSymbol -> BitSet(0)))
        }
        "b[7]" in {
          killed(bRef index 7) shouldBe SymbolBitSet(Map(bSymbol -> BitSet(7)))
        }
        "c[0]" in {
          killed(cRef index 0) shouldBe SymbolBitSet(Map(cSymbol -> BitSet(0)))
        }
        "c[63]" in {
          killed(cRef index 63) shouldBe SymbolBitSet(Map(cSymbol -> BitSet(63)))
        }
        "c[64]" in {
          killed(cRef index 64) shouldBe SymbolBitSet(Map(cSymbol -> BitSet(64)))
        }
        "c[127]" in {
          killed(cRef index 127) shouldBe SymbolBitSet(Map(cSymbol -> BitSet(127)))
        }
      }

      "should ignore variable indices of ref" - {
        "a[@randbit()]]" in {
          killed(aRef index zext(1, randBitCall)) shouldBe SymbolBitSet.empty
        }
        "b[@randbit()]]" in {
          killed(bRef index zext(2, randBitCall)) shouldBe SymbolBitSet.empty
        }
        "c[@randbit()]]" in {
          killed(cRef index zext(7, randBitCall)) shouldBe SymbolBitSet.empty
        }
      }

      "should recognize constant slices of ref" - {
        "a[1:0]" in {
          killed(aRef.slice(1, ":", 0)) shouldBe SymbolBitSet(Map(aSymbol -> BitSet(1, 0)))
        }
        "a[2+:2]" in {
          killed(aRef.slice(2, "+:", 2)) shouldBe SymbolBitSet(Map(aSymbol -> BitSet(3, 2)))
        }
        "a[2-:2]" in {
          killed(aRef.slice(2, "-:", 2)) shouldBe SymbolBitSet(Map(aSymbol -> BitSet(2, 1)))
        }
        "c[120:0]" in {
          killed(cRef.slice(120, ":", 0)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(0 to 120: _*)))
        }
        "c[32+:64]" in {
          killed(cRef.slice(32, "+:", 64)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(32 to 95: _*)))
        }
        "c[95-:64]" in {
          killed(cRef.slice(95, "-:", 64)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(32 to 95: _*)))
        }
      }

      "should ignore variable slices of ref" - {
        "a[@randbit()+:1]" in {
          killed(aRef.slice(zext(1, randBitCall), "+:", 1)) shouldBe SymbolBitSet.empty
        }
        "a[@randbit()-:1]" in {
          killed(aRef.slice(zext(1, randBitCall), "-:", 1)) shouldBe SymbolBitSet.empty
        }
      }

      "should merge concatenation assignment targets" - {
        "{b, a[3], a[1:0]}" in {
          val cat = ExprCat(List(bRef, aRef index 3, aRef.slice(1, ":", 0)))
          killed(cat) shouldBe {
            SymbolBitSet(Map(aSymbol -> BitSet(3, 1, 0), bSymbol -> BitSet(0 to 7: _*)))
          }
        }
      }
    }

    "used" - {
      "rvalues" - {

        "should recognize complete ref" - {
          "a" in { usedRval(aRef) shouldBe SymbolBitSet(Map(aSymbol -> BitSet(0 to 3: _*))) }
          "b" in { usedRval(bRef) shouldBe SymbolBitSet(Map(bSymbol -> BitSet(0 to 7: _*))) }
          "c" in {
            usedRval(cRef) shouldBe SymbolBitSet(Map(cSymbol -> BitSet(0 to 255: _*)))
          }
        }

        "should recognize constant indices of ref" - {
          "a[0]" in {
            usedRval(aRef index 0) shouldBe SymbolBitSet(Map(aSymbol -> BitSet(0)))
          }
          "a[3]" in {
            usedRval(aRef index 3) shouldBe SymbolBitSet(Map(aSymbol -> BitSet(3)))
          }
          "b[0]" in {
            usedRval(bRef index 0) shouldBe SymbolBitSet(Map(bSymbol -> BitSet(0)))
          }
          "b[7]" in {
            usedRval(bRef index 7) shouldBe SymbolBitSet(Map(bSymbol -> BitSet(7)))
          }
          "c[0]" in {
            usedRval(cRef index 0) shouldBe SymbolBitSet(Map(cSymbol -> BitSet(0)))
          }
          "c[63]" in {
            usedRval(cRef index 63) shouldBe SymbolBitSet(Map(cSymbol -> BitSet(63)))
          }
          "c[64]" in {
            usedRval(cRef index 64) shouldBe SymbolBitSet(Map(cSymbol -> BitSet(64)))
          }
          "c[127]" in {
            usedRval(cRef index 127) shouldBe SymbolBitSet(Map(cSymbol -> BitSet(127)))
          }
        }

        "should be pessimistic for variable indices of ref" - {
          "a[@randbit()]]" in {
            usedRval(aRef index zext(1, randBitCall)) shouldBe SymbolBitSet(
              Map(aSymbol -> BitSet(0 to 3: _*)))
          }
          "b[@randbit()]]" in {
            usedRval(bRef index zext(2, randBitCall)) shouldBe SymbolBitSet(
              Map(bSymbol -> BitSet(0 to 7: _*)))
          }
          "c[@randbit()]]" in {
            usedRval(cRef index zext(7, randBitCall)) shouldBe SymbolBitSet(
              Map(cSymbol -> BitSet(0 to 255: _*)))
          }
        }

        "should recognize non-constant indices of ref" - {
          "c[b]" in {
            usedRval(cRef index bRef) shouldBe SymbolBitSet(
              Map(cSymbol -> BitSet(0 to 255: _*), bSymbol -> BitSet(0 to 7: _*)))
          }
        }

        "should recognize constant slices of ref" - {
          "a[1:0]" in {
            usedRval(aRef.slice(1, ":", 0)) shouldBe SymbolBitSet(Map(aSymbol -> BitSet(1, 0)))
          }
          "a[2+:2]" in {
            usedRval(aRef.slice(2, "+:", 2)) shouldBe SymbolBitSet(Map(aSymbol -> BitSet(3, 2)))
          }
          "a[2-:2]" in {
            usedRval(aRef.slice(2, "-:", 2)) shouldBe SymbolBitSet(Map(aSymbol -> BitSet(2, 1)))
          }
          "c[120:0]" in {
            usedRval(cRef.slice(120, ":", 0)) shouldBe SymbolBitSet(
              Map(cSymbol -> BitSet(0 to 120: _*)))
          }
          "c[32+:64]" in {
            usedRval(cRef.slice(32, "+:", 64)) shouldBe SymbolBitSet(
              Map(cSymbol -> BitSet(32 to 95: _*)))
          }
          "c[95-:64]" in {
            usedRval(cRef.slice(95, "-:", 64)) shouldBe SymbolBitSet(
              Map(cSymbol -> BitSet(32 to 95: _*)))
          }
        }

        "should be pessimistic for variable slices of ref" - {
          "a[@randbit()+:1]" in {
            usedRval(aRef.slice(zext(1, randBitCall), "+:", 1)) shouldBe {
              SymbolBitSet(Map(aSymbol -> BitSet(0 to 3: _*)))
            }
          }
          "a[@randbit()-:1]" in {
            usedRval(aRef.slice(zext(1, randBitCall), "-:", 1)) shouldBe {
              SymbolBitSet(Map(aSymbol -> BitSet(0 to 3: _*)))
            }
          }
        }

        "should recognize non-constant slices of ref" - {
          "c[b +: 1]" in {
            usedRval(cRef.slice(bRef, "+:", 1)) shouldBe SymbolBitSet(
              Map(cSymbol -> BitSet(0 to 255: _*), bSymbol -> BitSet(0 to 7: _*)))
          }
          "c[1 +: b]" in {
            usedRval(cRef.slice(1, "+:", bRef)) shouldBe SymbolBitSet(
              Map(cSymbol -> BitSet(0 to 255: _*), bSymbol -> BitSet(0 to 7: _*)))
          }
        }

        "should merge concatenation assignment targets" - {
          "{b, a[3], a[1:0]}" in {
            val cat = ExprCat(List(bRef, aRef index 3, aRef.slice(1, ":", 0)))
            usedRval(cat) shouldBe {
              SymbolBitSet(Map(aSymbol -> BitSet(3, 1, 0), bSymbol -> BitSet(0 to 7: _*)))
            }
          }
        }
      }

      "lvalues" - {
        "should handle index" - {
          "c[b]" in {
            usedLval(cRef index bRef) shouldBe SymbolBitSet(Map(bSymbol -> BitSet(0 to 7: _*)))
          }
          "a[b[0]]" in {
            usedLval(aRef index zext(1, (bRef index 0))) shouldBe {
              SymbolBitSet(Map(bSymbol -> BitSet(0)))
            }
          }
        }

        "should handle slice" - {
          "c[b +: 1]" in {
            usedLval(cRef.slice(bRef, "+:", 1)) shouldBe {
              SymbolBitSet(Map(bSymbol -> BitSet(0 to 7: _*)))
            }
          }
          "a[b[2] +: 1]" in {
            usedLval(aRef.slice(zext(1, bRef index 2), "+:", 1)) shouldBe {
              SymbolBitSet(Map(bSymbol -> BitSet(2)))
            }
          }
        }

        "should handle concatenations" - {
          "{b, a[0], a[b[1]], c[b[4] + b[3] +: 3]}" in {
            val cat = ExprCat(
              List(
                bRef,
                aRef index 0,
                aRef index zext(1, bRef index 1),
                cRef.slice(zext(7, (bRef index 4) + (bRef index 3)), "+:", 3)
              ))
            usedLval(cat) shouldBe SymbolBitSet(Map(bSymbol -> BitSet(4, 3, 1)))
          }
        }
      }
    }
  }
}
