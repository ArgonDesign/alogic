////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.analysis

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.builtins.AtUnknownU
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Types._
import org.scalatest.freespec.AnyFreeSpec

import scala.collection.immutable.BitSet

class ReadSymbolBitsSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext()

  private val aSymbol = cc.newSymbol("a", Loc.synthetic) tap { _.kind = TypeUInt(4) }
  private val bSymbol = cc.newSymbol("b", Loc.synthetic) tap { _.kind = TypeUInt(8) }
  private val cSymbol = cc.newSymbol("c", Loc.synthetic) tap { _.kind = TypeUInt(256) }

  // Make sure decl/defn exists
  List(aSymbol, bSymbol, cSymbol) foreach { symbol =>
    symbol.mkDecl regularize symbol.loc
    symbol.mkDefn regularize symbol.loc
  }

  private val aRef = ExprSym(aSymbol) regularize Loc.synthetic
  private val bRef = ExprSym(bSymbol) regularize Loc.synthetic
  private val cRef = ExprSym(cSymbol) regularize Loc.synthetic

  private def zext(n: Int, expr: Expr) =
    ExprCat(List(ExprInt(false, n, 0), expr)) regularize Loc.synthetic

  private val randBitCall = ExprBuiltin(AtUnknownU, ArgP(Expr(1)) :: Nil) regularize Loc.synthetic

  private def prep(expr: Expr): Expr = {
    (expr regularize Loc.synthetic).simplify
  } tap { _ =>
    cc.messages shouldBe empty
  }

  private def possiblyLVal(expr: Expr) = ReadSymbolBits.possiblyLVal(prep(expr))

  private def possiblyRVal(expr: Expr) = ReadSymbolBits.possiblyRVal(prep(expr))

  "ReadSymbolBits" - {
    "possiblyRVal" - {
      "should recognize complete ref" - {
        "a" in {
          possiblyRVal(aRef) shouldBe SymbolBitSet(Map(aSymbol -> BitSet(0 to 3: _*)))
        }
        "b" in {
          possiblyRVal(bRef) shouldBe SymbolBitSet(Map(bSymbol -> BitSet(0 to 7: _*)))
        }
        "c" in {
          possiblyRVal(cRef) shouldBe SymbolBitSet(Map(cSymbol -> BitSet(0 to 255: _*)))
        }
      }

      "should recognize constant indices of ref" - {
        "a[0]" in {
          possiblyRVal(aRef index 0) shouldBe SymbolBitSet(Map(aSymbol -> BitSet(0)))
        }
        "a[3]" in {
          possiblyRVal(aRef index 3) shouldBe SymbolBitSet(Map(aSymbol -> BitSet(3)))
        }
        "b[0]" in {
          possiblyRVal(bRef index 0) shouldBe SymbolBitSet(Map(bSymbol -> BitSet(0)))
        }
        "b[7]" in {
          possiblyRVal(bRef index 7) shouldBe SymbolBitSet(Map(bSymbol -> BitSet(7)))
        }
        "c[0]" in {
          possiblyRVal(cRef index 0) shouldBe SymbolBitSet(Map(cSymbol -> BitSet(0)))
        }
        "c[63]" in {
          possiblyRVal(cRef index 63) shouldBe SymbolBitSet(Map(cSymbol -> BitSet(63)))
        }
        "c[64]" in {
          possiblyRVal(cRef index 64) shouldBe SymbolBitSet(Map(cSymbol -> BitSet(64)))
        }
        "c[127]" in {
          possiblyRVal(cRef index 127) shouldBe SymbolBitSet(Map(cSymbol -> BitSet(127)))
        }
      }

      "should be pessimistic for variable indices of ref" - {
        "a[@unknownu(1)]]" in {
          possiblyRVal(aRef index zext(1, randBitCall)) shouldBe SymbolBitSet(
            Map(aSymbol -> BitSet(0 to 3: _*))
          )
        }
        "b[@unknownu(1)]]" in {
          possiblyRVal(bRef index zext(2, randBitCall)) shouldBe SymbolBitSet(
            Map(bSymbol -> BitSet(0 to 7: _*))
          )
        }
        "c[@unknownu(1)]]" in {
          possiblyRVal(cRef index zext(7, randBitCall)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(0 to 255: _*))
          )
        }
      }

      "should recognize non-constant indices of ref" - {
        "c[b]" in {
          possiblyRVal(cRef index bRef) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(0 to 255: _*), bSymbol -> BitSet(0 to 7: _*))
          )
        }
      }

      "should recognize constant slices of ref" - {
        "a[1:0]" in {
          possiblyRVal(aRef.slice(1, ":", 0)) shouldBe SymbolBitSet(Map(aSymbol -> BitSet(1, 0)))
        }
        "a[2+:2]" in {
          possiblyRVal(aRef.slice(2, "+:", 2)) shouldBe SymbolBitSet(Map(aSymbol -> BitSet(3, 2)))
        }
        "a[2-:2]" in {
          possiblyRVal(aRef.slice(2, "-:", 2)) shouldBe SymbolBitSet(Map(aSymbol -> BitSet(2, 1)))
        }
        "c[120:0]" in {
          possiblyRVal(cRef.slice(120, ":", 0)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(0 to 120: _*))
          )
        }
        "c[32+:64]" in {
          possiblyRVal(cRef.slice(32, "+:", 64)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(32 to 95: _*))
          )
        }
        "c[95-:64]" in {
          possiblyRVal(cRef.slice(95, "-:", 64)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(32 to 95: _*))
          )
        }
      }

      "should be pessimistic for variable slices of ref" - {
        "a[@unknownu(1)+:1]" in {
          possiblyRVal(aRef.slice(zext(1, randBitCall), "+:", 1)) shouldBe {
            SymbolBitSet(Map(aSymbol -> BitSet(0 to 3: _*)))
          }
        }
        "a[@unknownu(1)-:1]" in {
          possiblyRVal(aRef.slice(zext(1, randBitCall), "-:", 1)) shouldBe {
            SymbolBitSet(Map(aSymbol -> BitSet(0 to 3: _*)))
          }
        }
      }

      "should recognize non-constant slices of ref" - {
        "c[b +: 1]" in {
          possiblyRVal(cRef.slice(bRef, "+:", 1)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(0 to 255: _*), bSymbol -> BitSet(0 to 7: _*))
          )
        }
      }

      "should merge concatenation assignment targets" - {
        "{b, a[3], a[1:0]}" in {
          val cat = ExprCat(List(bRef, aRef index 3, aRef.slice(1, ":", 0)))
          possiblyRVal(cat) shouldBe {
            SymbolBitSet(Map(aSymbol -> BitSet(3, 1, 0), bSymbol -> BitSet(0 to 7: _*)))
          }
        }
      }
    }

    "possiblyLVal" - {
      "should handle index" - {
        "c[b]" in {
          possiblyLVal(cRef index bRef) shouldBe SymbolBitSet(Map(bSymbol -> BitSet(0 to 7: _*)))
        }
        "a[b[0]]" in {
          possiblyLVal(aRef index zext(1, bRef index 0)) shouldBe {
            SymbolBitSet(Map(bSymbol -> BitSet(0)))
          }
        }
      }

      "should handle slice" - {
        "c[b +: 1]" in {
          possiblyLVal(cRef.slice(bRef, "+:", 1)) shouldBe {
            SymbolBitSet(Map(bSymbol -> BitSet(0 to 7: _*)))
          }
        }
        "a[b[2] +: 1]" in {
          possiblyLVal(aRef.slice(zext(1, bRef index 2), "+:", 1)) shouldBe {
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
            )
          )
          possiblyLVal(cat) shouldBe SymbolBitSet(Map(bSymbol -> BitSet(4, 3, 1)))
        }
      }
    }
  }
}
