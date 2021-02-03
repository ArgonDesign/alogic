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

class WrittenSymbolBitsSpec extends AnyFreeSpec with AlogicTest {

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

  private def definitely(expr: Expr) = WrittenSymbolBits.definitely(prep(expr))

  "WrittenSymbolBits" - {
    "definitely" - {
      "should recognize complete ref" - {
        "a" in {
          definitely(aRef) shouldBe SymbolBitSet(Map(aSymbol -> BitSet(0 to 3: _*)))
        }
        "b" in {
          definitely(bRef) shouldBe SymbolBitSet(Map(bSymbol -> BitSet(0 to 7: _*)))
        }
        "c" in {
          definitely(cRef) shouldBe SymbolBitSet(Map(cSymbol -> BitSet(0 to 255: _*)))
        }
      }

      "should recognize constant indices of ref" - {
        "a[0]" in {
          definitely(aRef index 0) shouldBe SymbolBitSet(Map(aSymbol -> BitSet(0)))
        }
        "a[3]" in {
          definitely(aRef index 3) shouldBe SymbolBitSet(Map(aSymbol -> BitSet(3)))
        }
        "b[0]" in {
          definitely(bRef index 0) shouldBe SymbolBitSet(Map(bSymbol -> BitSet(0)))
        }
        "b[7]" in {
          definitely(bRef index 7) shouldBe SymbolBitSet(Map(bSymbol -> BitSet(7)))
        }
        "c[0]" in {
          definitely(cRef index 0) shouldBe SymbolBitSet(Map(cSymbol -> BitSet(0)))
        }
        "c[63]" in {
          definitely(cRef index 63) shouldBe SymbolBitSet(Map(cSymbol -> BitSet(63)))
        }
        "c[64]" in {
          definitely(cRef index 64) shouldBe SymbolBitSet(Map(cSymbol -> BitSet(64)))
        }
        "c[127]" in {
          definitely(cRef index 127) shouldBe SymbolBitSet(Map(cSymbol -> BitSet(127)))
        }
      }

      "should ignore variable indices of ref" - {
        "a[@unknownu(1)]]" in {
          definitely(aRef index zext(1, randBitCall)) shouldBe SymbolBitSet.empty
        }
        "b[@unknownu(1)]]" in {
          definitely(bRef index zext(2, randBitCall)) shouldBe SymbolBitSet.empty
        }
        "c[@unknownu(1)]]" in {
          definitely(cRef index zext(7, randBitCall)) shouldBe SymbolBitSet.empty
        }
      }

      "should recognize constant slices of ref" - {
        "a[1:0]" in {
          definitely(aRef.slice(1, ":", 0)) shouldBe SymbolBitSet(Map(aSymbol -> BitSet(1, 0)))
        }
        "a[2+:2]" in {
          definitely(aRef.slice(2, "+:", 2)) shouldBe SymbolBitSet(Map(aSymbol -> BitSet(3, 2)))
        }
        "a[2-:2]" in {
          definitely(aRef.slice(2, "-:", 2)) shouldBe SymbolBitSet(Map(aSymbol -> BitSet(2, 1)))
        }
        "c[63:0]" in {
          definitely(cRef.slice(63, ":", 0)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(0 to 63: _*))
          )
        }
        "c[63:10]" in {
          definitely(cRef.slice(63, ":", 10)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(10 to 63: _*))
          )
        }
        "c[120:0]" in {
          definitely(cRef.slice(120, ":", 0)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(0 to 120: _*))
          )
        }
        "c[120:66]" in {
          definitely(cRef.slice(120, ":", 66)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(66 to 120: _*))
          )
        }
        "c[127:0]" in {
          definitely(cRef.slice(127, ":", 0)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(0 to 127: _*))
          )
        }
        "c[127:10]" in {
          definitely(cRef.slice(127, ":", 10)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(10 to 127: _*))
          )
        }
        "c[127:64]" in {
          definitely(cRef.slice(127, ":", 64)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(64 to 127: _*))
          )
        }
        "c[127:70]" in {
          definitely(cRef.slice(127, ":", 70)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(70 to 127: _*))
          )
        }
        "c[250:249]" in {
          definitely(cRef.slice(250, ":", 249)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(249 to 250: _*))
          )
        }
        "c[250:128]" in {
          definitely(cRef.slice(250, ":", 128)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(128 to 250: _*))
          )
        }
        "c[32+:64]" in {
          definitely(cRef.slice(32, "+:", 64)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(32 to 95: _*))
          )
        }
        "c[95-:64]" in {
          definitely(cRef.slice(95, "-:", 64)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(32 to 95: _*))
          )
        }
      }

      "should ignore variable slices of ref" - {
        "a[@unknownu(1)+:1]" in {
          definitely(aRef.slice(zext(1, randBitCall), "+:", 1)) shouldBe SymbolBitSet.empty
        }
        "a[@unknownu(1)-:1]" in {
          definitely(aRef.slice(zext(1, randBitCall), "-:", 1)) shouldBe SymbolBitSet.empty
        }
      }

      "should merge concatenation assignment targets" - {
        "{b, a[3], a[1:0]}" in {
          val cat = ExprCat(List(bRef, aRef index 3, aRef.slice(1, ":", 0)))
          definitely(cat) shouldBe {
            SymbolBitSet(Map(aSymbol -> BitSet(3, 1, 0), bSymbol -> BitSet(0 to 7: _*)))
          }
        }
      }
    }
  }
}
