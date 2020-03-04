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
// Expression type checking tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.passes.Elaborate
import com.argondesign.alogic.passes.Namer
import com.argondesign.alogic.passes.TypeCheck
import org.scalatest.FreeSpec

final class UnaryTickSpec extends FreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  protected def replaceUnaryTicks(text: String): Option[Thicket] = {
    transformWithPass(Namer andThen
                        Elaborate andThen
                        TypeCheck,
                      text) map { pairs =>
      Thicket {
        pairs flatMap {
          case (decl, defn) => List(decl, defn)
        }
      }
    }
  }

  def checkUnpacked(text: String): Unit = {
    replaceUnaryTicks(text)
    cc.messages.loneElement should beThe[Error](
      """Operand of unary ' operator is of non-packed type""")
  }

  def checkNarrowing(text: String): Unit = {
    replaceUnaryTicks(text)
    cc.messages.loneElement should beThe[Error](
      """Unary ' causes narrowing of width from \d+ to \d+""")
  }

  def checkInvalidContext(text: String): Unit = {
    replaceUnaryTicks(text)
    cc.messages.loneElement should beThe[Error]("Unary ' operator used in invalid context")
  }

  def check(kinds: List[Type])(text: String): Unit = {
    val tree = replaceUnaryTicks(text)
    cc.messages foreach { msg =>
      println(msg.string(cc))
    }
    cc.messages shouldBe empty
    val exprs = { tree.value collect { case e @ ExprUnary("'", _) => e } }.toList
    exprs should have length kinds.length
    for ((kind, expr) <- kinds zip exprs) {
      expr.tpe shouldBe kind
    }
  }

  "The Typer should assign the type of the unary tick operator in" - {
    "valid context" - {
      "declaration initializer" - {
        "a" in check(TypeUInt(9) :: Nil) {
          """|fsm f {
             |  in u8 a;
             |  (* unused *) u9 b = 'a;
             |}""".stripMargin
        }

        "b" in check(TypeUInt(9) :: Nil) {
          """|fsm f {
             |  in u8 a;
             |  void main() {
             |    (* unused *) u9 b = 'a;
             |    fence;
             |  }
             |}""".stripMargin
        }

        "c" in check(TypeUInt(9) :: Nil) {
          """|fsm f {
             |  in u8 a;
             |  void main() {
             |    (* unused *)u9 b = 'a + 10;
             |    fence;
             |  }
             |}""".stripMargin
        }

        "d" in check(TypeSInt(9) :: Nil) {
          """|fsm f {
             |  in i8 a;
             |  void main() {
             |    (* unused *)u9 b = 'a;
             |    fence;
             |  }
             |}""".stripMargin
        }

        "e" in check(TypeUInt(9) :: Nil) {
          """|fsm f {
             |  in i9 a;
             |  u2 c = 0;
             |  void main() {
             |    c++;
             |    (* unused *)u9 b = a + 'c;
             |    fence;
             |  }
             |}""".stripMargin
        }

        "f" in check(TypeSInt(9) :: Nil) {
          """|fsm f {
             |  in i9 a;
             |  i2 c = 0;
             |  void main() {
             |    c++;
             |    (* unused *)u9 b = a + 'c;
             |    fence;
             |  }
             |}""".stripMargin
        }

        "g" in check(TypeUInt(9) :: Nil) {
          """|struct s {
             |  u2 f;
             |  u3 g;
             |}
             |
             |(* toplevel *)
             |fsm x {
             |  in s a;
             |  void main() {
             |    (* unused *)u9 b = 'a;
             |    fence;
             |  }
             |}""".stripMargin
        }

        "h" in check(TypeUInt(40) :: Nil) {
          """|fsm f {
             |  in u10[3] a;
             |  void main() {
             |    (* unused *)u10[4] b = 'a;
             |    fence;
             |  }
             |}""".stripMargin
        }
      }

      "rhs of assignment" - {
        "a" in check(TypeUInt(9) :: Nil) {
          """|fsm f {
             |  in u8 a;
             |  out u9 b;
             |  void main() {
             |    b = 'a;
             |    fence;
             |  }
             |}""".stripMargin
        }

        "b" in check(TypeUInt(9) :: Nil) {
          """|fsm f {
             |  in u8 a;
             |  out u9 b;
             |  void main() {
             |    b = a == 0 ? 1 : 'a;
             |    fence;
             |  }
             |}""".stripMargin
        }
      }

      "rhs of operator assignment" - {
        "a" in check(TypeUInt(9) :: Nil) {
          """|fsm f {
             |  in u8 a;
             |  out u9 b;
             |  void main() {
             |    b += 'a;
             |    fence;
             |  }
             |}""".stripMargin
        }
      }

      "index" - {
        "a" in check(TypeUInt(3) :: Nil) {
          """|fsm f {
             |  in u8 a;
             |  out bool b;
             |  in u2 i;
             |  void main() {
             |    b = a['i];
             |    fence;
             |  }
             |}""".stripMargin
        }

        "b" in check(TypeSInt(3) :: Nil) {
          """|fsm f {
             |  in u8 a;
             |  out bool b;
             |  in i2 i;
             |  void main() {
             |    b = a['i + 2];
             |    fence;
             |  }
             |}""".stripMargin
        }

        "c" in check(TypeSInt(3) :: Nil) {
          """|fsm f {
             |  in u8 a;
             |  out u8 b;
             |  in i2 i;
             |  void main() {
             |    b['i + 2] = |a;
             |    fence;
             |  }
             |}""".stripMargin
        }
      }

      "slice" - {
        "a" in check(TypeUInt(3) :: Nil) {
          """|fsm f {
             |  in u8 a;
             |  out u2 b;
             |  in u2 i;
             |  void main() {
             |    b = a['i +: 2];
             |    fence;
             |  }
             |}""".stripMargin
        }

        "b" in check(TypeUInt(3) :: Nil) {
          """|fsm f {
             |  in u8 a;
             |  out bool b;
             |  const u2 i = 2;
             |  void main() {
             |    b = |a['i : 0];
             |    fence;
             |  }
             |}""".stripMargin
        }

        "c" in check(TypeUInt(4) :: Nil) {
          """|fsm f {
             |  in u8 a;
             |  out bool b;
             |  const u2 i = 2;
             |  void main() {
             |    b = |a[0 +: 'i * 2];
             |    fence;
             |  }
             |}""".stripMargin
        }

        "d" in check(TypeUInt(3) :: TypeUInt(5) :: Nil) {
          """|fsm f {
             |  in u32 a;
             |  out u8 b;
             |  in u2 i;
             |  void main() {
             |    b['i +: 2] = {2{a['i]}};
             |    fence;
             |  }
             |}""".stripMargin
        }

        "e" in check(TypeUInt(3) :: TypeUInt(4) :: Nil) {
          """|fsm f {
             |  const u2 A = 2'd2;
             |  in  u32[8]  a;
             |  out u32[2]  b;
             |  in  u2 i;
             |  void main() {
             |    b = a['i +: 'A];
             |    fence;
             |  }
             |}""".stripMargin
        }
      }

      "function argument" - {
        "a" in check(TypeUInt(20) :: Nil) {
          """|fsm f {
             |  in bool a;
             |  out sync u20 b;
             |  void main() {
             |    b.write('a);
             |    fence;
             |  }
             |}""".stripMargin
        }

        "b" in check(TypeUInt(8) :: TypeUInt(20) :: Nil) {
          """|fsm f {
             |  in bool a;
             |  in u2   b;
             |
             |  u20 store[256];
             |
             |  void main() {
             |    store.write('b, 'a);
             |    fence;
             |  }
             |}""".stripMargin
        }
      }

      "multiple" - {
        "a" in check(TypeUInt(3) :: TypeUInt(4) :: Nil) {
          """|fsm f {
             |  in u8 a;
             |  out bool b;
             |  in u2 i;
             |  out u4 j;
             |  void main() {
             |    b = a['i];
             |    j = 'i;
             |    fence;
             |  }
             |}""".stripMargin
        }
      }

      "error on narrowing" - {
        "equal" in check(TypeUInt(9) :: Nil) {
          """|fsm f {
             |  in u9 a;
             |  out u9 b;
             |  void main() {
             |    b = a == 0 ? 1 : 'a;
             |    fence;
             |  }
             |}""".stripMargin
        }

        "narrower" in checkNarrowing {
          """|fsm f {
             |  in u10 a;
             |  out u9 b;
             |  void main() {
             |    b = a == 0 ? 1 : 'a;
             |    fence;
             |  }
             |}""".stripMargin
        }
      }

      "error for un-packed operand" - {
        "uint" in checkUnpacked {
          """|fsm f {
             |  const uint N = 0;
             |  in u9 a;
             |  out bool b;
             |  void main() {
             |    b = a['N];
             |    fence;
             |  }
             |}""".stripMargin
        }
      }
    }

    "invalid context" - {
      "a" in checkInvalidContext {
        """|fsm f {
           |  const u2 P = 2;
           |  in uint('P) a;
           |}""".stripMargin
      }

      "b" in checkInvalidContext {
        """|fsm f {
           |  in u2 a;
           |  void main() {
           |    'a;
           |    fence;
           |  }
           |}""".stripMargin
      }

      "c" in checkInvalidContext {
        """|fsm f {
           |  in u2 a;
           |  out bool b;
           |  void main() {
           |    if ('a) b = 0; else b = 1;
           |    fence;
           |  }
           |}""".stripMargin
      }

      "d" in checkInvalidContext {
        """|fsm f {
           |  in u2 a;
           |  out bool b;
           |  void main() {
           |    case ('a) {
           |      3'b0: b = 1;
           |      default: b = 0;
           |    }
           |    fence;
           |  }
           |}""".stripMargin
      }

      "e" in checkInvalidContext {
        """|fsm f {
           |  in u2 a;
           |  out bool b;
           |  void main() {
           |    case (3'b0) {
           |      'a: b = 1;
           |      default: b = 0;
           |    }
           |    fence;
           |  }
           |}""".stripMargin
      }

      "f" in checkInvalidContext {
        """|fsm f {
           |  const u2 P = 2;
           |  in bool['P] a;
           |}""".stripMargin
      }

      "g" in checkInvalidContext {
        """|fsm f {
           |  const u2 P = 2;
           |  u8 a = @bits(bool['P]);
           |}""".stripMargin
      }

    }
  }
}
