////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Tests for Liveness analysis
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.analysis

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.builtins.Builtins
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.frontend.SymbolTable
import org.scalatest.freespec.AnyFreeSpec

import scala.collection.immutable.BitSet

class LivenessSpec extends AnyFreeSpec with AlogicTest {

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

  private val randBitCall = Builtins.symbolTable.get("@unknownu") pipe {
    case SymbolTable.Local(symbol) => symbol
    case _                         => fail()
  } pipe { poly =>
    val args = (ArgP(Expr(1)) regularize Loc.synthetic) :: Nil
    ExprCall(ExprSym(poly.kind.asPolyFunc.resolve(args, None).get), args)
  }

  private def prep(expr: Expr): Expr = {
    (expr regularize Loc.synthetic).simplify
  } tap { _ =>
    cc.messages shouldBe empty
  }

  private def killed(expr: Expr) = Liveness.killed(prep(expr))

  private def usedLval(expr: Expr) = Liveness.usedLv(prep(expr))

  private def usedRval(expr: Expr) = Liveness.usedRv(prep(expr))

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
        "a[@unknownu(1)]]" in {
          killed(aRef index zext(1, randBitCall)) shouldBe SymbolBitSet.empty
        }
        "b[@unknownu(1)]]" in {
          killed(bRef index zext(2, randBitCall)) shouldBe SymbolBitSet.empty
        }
        "c[@unknownu(1)]]" in {
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
        "c[63:0]" in {
          killed(cRef.slice(63, ":", 0)) shouldBe SymbolBitSet(Map(cSymbol -> BitSet(0 to 63: _*)))
        }
        "c[63:10]" in {
          killed(cRef.slice(63, ":", 10)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(10 to 63: _*))
          )
        }
        "c[120:0]" in {
          killed(cRef.slice(120, ":", 0)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(0 to 120: _*))
          )
        }
        "c[120:66]" in {
          killed(cRef.slice(120, ":", 66)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(66 to 120: _*))
          )
        }
        "c[127:0]" in {
          killed(cRef.slice(127, ":", 0)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(0 to 127: _*))
          )
        }
        "c[127:10]" in {
          killed(cRef.slice(127, ":", 10)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(10 to 127: _*))
          )
        }
        "c[127:64]" in {
          killed(cRef.slice(127, ":", 64)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(64 to 127: _*))
          )
        }
        "c[127:70]" in {
          killed(cRef.slice(127, ":", 70)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(70 to 127: _*))
          )
        }
        "c[250:249]" in {
          killed(cRef.slice(250, ":", 249)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(249 to 250: _*))
          )
        }
        "c[250:128]" in {
          killed(cRef.slice(250, ":", 128)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(128 to 250: _*))
          )
        }
        "c[32+:64]" in {
          killed(cRef.slice(32, "+:", 64)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(32 to 95: _*))
          )
        }
        "c[95-:64]" in {
          killed(cRef.slice(95, "-:", 64)) shouldBe SymbolBitSet(
            Map(cSymbol -> BitSet(32 to 95: _*))
          )
        }
      }

      "should ignore variable slices of ref" - {
        "a[@unknownu(1)+:1]" in {
          killed(aRef.slice(zext(1, randBitCall), "+:", 1)) shouldBe SymbolBitSet.empty
        }
        "a[@unknownu(1)-:1]" in {
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
          "a[@unknownu(1)]]" in {
            usedRval(aRef index zext(1, randBitCall)) shouldBe SymbolBitSet(
              Map(aSymbol -> BitSet(0 to 3: _*))
            )
          }
          "b[@unknownu(1)]]" in {
            usedRval(bRef index zext(2, randBitCall)) shouldBe SymbolBitSet(
              Map(bSymbol -> BitSet(0 to 7: _*))
            )
          }
          "c[@unknownu(1)]]" in {
            usedRval(cRef index zext(7, randBitCall)) shouldBe SymbolBitSet(
              Map(cSymbol -> BitSet(0 to 255: _*))
            )
          }
        }

        "should recognize non-constant indices of ref" - {
          "c[b]" in {
            usedRval(cRef index bRef) shouldBe SymbolBitSet(
              Map(cSymbol -> BitSet(0 to 255: _*), bSymbol -> BitSet(0 to 7: _*))
            )
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
              Map(cSymbol -> BitSet(0 to 120: _*))
            )
          }
          "c[32+:64]" in {
            usedRval(cRef.slice(32, "+:", 64)) shouldBe SymbolBitSet(
              Map(cSymbol -> BitSet(32 to 95: _*))
            )
          }
          "c[95-:64]" in {
            usedRval(cRef.slice(95, "-:", 64)) shouldBe SymbolBitSet(
              Map(cSymbol -> BitSet(32 to 95: _*))
            )
          }
        }

        "should be pessimistic for variable slices of ref" - {
          "a[@unknownu(1)+:1]" in {
            usedRval(aRef.slice(zext(1, randBitCall), "+:", 1)) shouldBe {
              SymbolBitSet(Map(aSymbol -> BitSet(0 to 3: _*)))
            }
          }
          "a[@unknownu(1)-:1]" in {
            usedRval(aRef.slice(zext(1, randBitCall), "-:", 1)) shouldBe {
              SymbolBitSet(Map(aSymbol -> BitSet(0 to 3: _*)))
            }
          }
        }

        "should recognize non-constant slices of ref" - {
          "c[b +: 1]" in {
            usedRval(cRef.slice(bRef, "+:", 1)) shouldBe SymbolBitSet(
              Map(cSymbol -> BitSet(0 to 255: _*), bSymbol -> BitSet(0 to 7: _*))
            )
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
            usedLval(aRef index zext(1, bRef index 0)) shouldBe {
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
              )
            )
            usedLval(cat) shouldBe SymbolBitSet(Map(bSymbol -> BitSet(4, 3, 1)))
          }
        }
      }
    }
  }
}
