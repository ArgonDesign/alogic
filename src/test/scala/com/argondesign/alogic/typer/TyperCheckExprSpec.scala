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

package com.argondesign.alogic.typer

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.passes.Elaborate
import com.argondesign.alogic.passes.Namer
import com.argondesign.alogic.passes.TypeCheck
import org.scalatest.FreeSpec

final class TyperCheckExprSpec extends FreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  private def typeCheck(text: String): Thicket = Thicket {
    transformWithPass(Namer andThen Elaborate andThen TypeCheck, text) map {
      _ flatMap {
        case (decl, defn) => List(decl, defn)
      }
    } getOrElse Nil
  }

  "The Typer should type check expressions" - {
    "unary" - {
      for {
        (expr, err) <- List(
          (s"+(N)", Nil),
          (s"-(N)", Nil),
          (s"~(N)", Nil),
          (s"!(N)", Nil),
          (s"&(N)", Nil),
          (s"|(N)", Nil),
          (s"^(N)", Nil),
          (s"+(main)", "Operand of unary operator '+' is of non-packed type" :: Nil),
          (s"-(main)", "Operand of unary operator '-' is of non-packed type" :: Nil),
          (s"~(main)", "Operand of unary operator '~' is of non-packed type" :: Nil),
          (s"!(main)", "Operand of unary operator '!' is of non-packed type" :: Nil),
          (s"&(main)", "Operand of unary operator '&' is of non-packed type" :: Nil),
          (s"|(main)", "Operand of unary operator '|' is of non-packed type" :: Nil),
          (s"^(main)", "Operand of unary operator '^' is of non-packed type" :: Nil),
          (s"+(8'd1)", Nil),
          (s"-(8'd1)", Nil),
          (s"~(8'd1)", Nil),
          (s"!(8'd1)", Nil),
          (s"+(8'sd1)", Nil),
          (s"-(8'sd1)", Nil),
          (s"~(8'sd1)", Nil),
          (s"!(8'sd1)", Nil),
          (s"&(1)", "Unary operator '&' cannot be applied to unsized integer value" :: Nil),
          (s"|(1)", "Unary operator '|' cannot be applied to unsized integer value" :: Nil),
          (s"^(1)", "Unary operator '^' cannot be applied to unsized integer value" :: Nil)
        )
      } {
        expr in {
          typeCheck {
            s"""
            |fsm a {
            |  const u8 N = 8'd2;
            |  void main() {
            |    $$display("", $expr);
            |    fence;
            |  }
            |}"""
          }
          checkSingleError(err)
        }
      }
    }

    "binary" - {
      "errors" - {
        for (op <- List("*", "/", "%", "+", "-", "&", "|", "^", ">", ">=", "<", "<=", "==", "!=")) {
          for {
            (expr, err) <- List(
              (s"8'd3 $op 8'd2", Nil),
              (s"8'd3 $op 8'sd2", Nil),
              (s"8'sd3 $op 8'd2", Nil),
              (s"8'sd3 $op 8'sd2", Nil),
              (s"8'd3 $op 2", Nil),
              (s"3 $op 8'd2", Nil),
              (s"3 $op 2", Nil),
              (
                s"7'd3 $op 8'd2",
                s"Both operands of binary '$op' must have the same width, but" ::
                  "left  hand operand is 7 bits wide, and" ::
                  "right hand operand is 8 bits wide" :: Nil
              ),
              (
                s"8'd3 $op 7'd2",
                s"Both operands of binary '$op' must have the same width, but" ::
                  "left  hand operand is 8 bits wide, and" ::
                  "right hand operand is 7 bits wide" :: Nil
              ),
              (
                s"4'sd3 $op 3'sd2",
                s"Both operands of binary '$op' must have the same width, but" ::
                  "left  hand operand is 4 bits wide, and" ::
                  "right hand operand is 3 bits wide" :: Nil
              ),
              (
                s"3'sd3 $op 4'sd2",
                s"Both operands of binary '$op' must have the same width, but" ::
                  "left  hand operand is 3 bits wide, and" ::
                  "right hand operand is 4 bits wide" :: Nil
              ),
              (s"bool $op 8'd2", s"Left hand operand of '$op' is of non-packed type" :: Nil),
              (s"8'd3 $op bool", s"Right hand operand of '$op' is of non-packed type" :: Nil)
            )
          } {
            expr in {
              typeCheck {
                s"""
                |fsm a {
                |  void main() {
                |    $$display("", $expr);
                |    fence;
                |  }
                |}"""
              }
              checkSingleError(err)
            }
          }
        }
      }

      "warnings" - {
        "shift signedness" - {
          for {
            (expr, warn) <- List(
              (s"8'd3  << 1", Nil),
              (s"8'sd3 << 8'sd2", "Logical shift used on signed left operand" :: Nil),
              (s"3     << 1", Nil),
              (s"3s    << 8'sd2", "Logical shift used on signed left operand" :: Nil),
              (s"8'd3  >> 1", Nil),
              (s"8'sd3 >> 8'sd2", "Logical shift used on signed left operand" :: Nil),
              (s"3     >> 1", Nil),
              (s"3s    >> 8'sd2", "Logical shift used on signed left operand" :: Nil),
              (s"8'd3  <<< 1", "Arithmetic shift used on unsigned left operand" :: Nil),
              (s"8'sd3 <<< 8'sd2", Nil),
              (s"3     <<< 1", "Arithmetic shift used on unsigned left operand" :: Nil),
              (s"3s    <<< 8'sd2", Nil),
              (s"8'd3  >>> 1", "Arithmetic shift used on unsigned left operand" :: Nil),
              (s"8'sd3 >>> 8'sd2", Nil),
              (s"3     >>> 1", "Arithmetic shift used on unsigned left operand" :: Nil),
              (s"3s    >>> 8'sd2", Nil)
            )
          } {
            expr in {
              typeCheck {
                s"""
                |fsm a {
                |  void main() {
                |    $$display("", $expr);
                |    fence;
                |  }
                |}"""
              }
              checkSingleWarning(warn)
            }
          }
        }

        "comparison signedness" - {
          for (op <- List(">", ">=", "<", "<=", "==", "!=")) {
            for {
              (expr, warn) <- List(
                (s"8'd3  $op 1", Nil),
                (s"8'sd3 $op 1", "Comparison between signed and unsigned operands" :: Nil),
                (s"3     $op 1s", "Comparison between unsigned and signed operands" :: Nil),
                (s"3s    $op 1s", Nil)
              )
            } {
              expr in {
                typeCheck {
                  s"""
                  |fsm a {
                  |  void main() {
                  |    $$display("", $expr);
                  |    fence;
                  |  }
                  |}"""
                }
                checkSingleWarning(warn)
              }
            }
          }
        }
      }
    }

    "ternary" - {
      for {
        (expr, err) <- List(
          ("a ? b[0][0] : c[0][0][0]", Nil),
          ("c ? b : c[0][0][0]", "Condition of '?:' is of neither numeric nor packed type" :: Nil),
          ("a ? 8'd3 : 8'd2", Nil),
          ("a ? 8'd3 : 8'sd2", Nil),
          ("a ? 8'sd3 : 8'd2", Nil),
          ("a ? 8'sd3 : 8'sd2", Nil),
          ("a ? 8'd3 : 2", Nil),
          ("a ? 3 : 8'd1", Nil),
          ("a ? 3 : 2", "Expression of unsized integer type must be compile time constant" :: Nil),
          (
            "a ? 7'd3 : 8'd2",
            s"'then' and 'else' operands of ternary '?:' must have the same width, but" ::
              "'then' operand is 7 bits wide, and" ::
              "'else' operand is 8 bits wide" :: Nil
          ),
          (
            "a ? 8'd3 : 7'd2",
            s"'then' and 'else' operands of ternary '?:' must have the same width, but" ::
              "'then' operand is 8 bits wide, and" ::
              "'else' operand is 7 bits wide" :: Nil
          ),
          (
            "a ? 4'sd3 : 3'sd2",
            s"'then' and 'else' operands of ternary '?:' must have the same width, but" ::
              "'then' operand is 4 bits wide, and" ::
              "'else' operand is 3 bits wide" :: Nil
          ),
          (
            "a ? 3'sd3 : 4'sd2",
            s"'then' and 'else' operands of ternary '?:' must have the same width, but" ::
              "'then' operand is 3 bits wide, and" ::
              "'else' operand is 4 bits wide" :: Nil
          ),
          ("a ? c : b", "'then' operand of '?:' is of non-packed type" :: Nil),
          ("a ? b : c", "'else' operand of '?:' is of non-packed type" :: Nil)
        )
      } {
        expr in {
          typeCheck {
            s"""
            |fsm f {
            |  (* unused *) out sync u2 a;
            |  (* unused *) i2[1][2] b;
            |  (* unused *) i2[1][2] c[4];
            |  void main() {
            |    $$display("", $expr);
            |    fence;
            |  }
            |}"""
          }
          checkSingleError(err)
        }
      }
    }

    "index" - {
      "term" - {
        for {
          (expr, err) <- List(
            ("a[0]", Nil),
            ("b[0]", Nil),
            ("b[0][0]", Nil),
            ("b[0][0][0]", Nil),
            ("c[0]", Nil),
            ("c[0][0]", Nil),
            ("c[0][0][0]", Nil),
            ("c[0][0][0][0]", Nil),
            ("8'd2[0]", Nil),
            ("8'd2[0][0]", Nil),
            ("8'd2[0][0][0]", Nil),
            ("main[0]", "Target is not indexable" :: Nil),
            ("a[bool]", "Index is of non-packed type" :: Nil),
            ("8'd0[2'd0]", "Index yields 2 bits, 3 bits are expected" :: Nil),
            ("8'd0[3'd0]", Nil),
            ("8'd0[4'd0]", "Index yields 4 bits, 3 bits are expected" :: Nil),
            ("7'd0[2'd0]", "Index yields 2 bits, 3 bits are expected" :: Nil),
            ("7'd0[3'd0]", Nil),
            ("7'd0[4'd0]", "Index yields 4 bits, 3 bits are expected" :: Nil),
            ("9'd0[2'd0]", "Index yields 2 bits, 4 bits are expected" :: Nil),
            ("9'd0[3'd0]", "Index yields 3 bits, 4 bits are expected" :: Nil),
            ("9'd0[4'd0]", Nil),
            ("1'd0[1'd0]", Nil),
            ("1'd0[2'd0]", "Index yields 2 bits, 1 bits are expected" :: Nil),
            ("c[2'd3][1'd0][1'd1][2'd2]", Nil),
            ("8'd0[3'sd0]", "Index must be unsigned" :: Nil)
          )
        } {
          expr in {
            typeCheck {
              s"""
              |fsm f {
              |  out sync u2 a;
              |  i3[1][2] b;
              |  i3[1][2] c[4];
              |  void main() {
              |    $$display("", $expr);
              |    fence;
              |  }
              |}"""
            }
            checkSingleError(err)
          }
        }
      }

      "type" - {
        for {
          (expr, err) <- List(
            ("u1[ 2 ]", Nil),
            ("u1[ 1 ]", Nil),
            ("u1[ 0 ]", "Size of vector must be positive (not 0)" :: Nil),
            ("u1[-1s]", "Size of vector must be positive (not -1)" :: Nil),
            ("u1[ x ]", "Size of vector must be a compile time constant" :: Nil),
            ("u1[1][ 2 ]", Nil),
            ("u1[1][ 1 ]", Nil),
            ("u1[1][ 0 ]", "Size of vector must be positive (not 0)" :: Nil),
            ("u1[1][-1s]", "Size of vector must be positive (not -1)" :: Nil),
            ("u1[1][ x ]", "Size of vector must be a compile time constant" :: Nil),
            ("int[2]", "Vector element must have a packed type" :: Nil),
            ("void[2]", "Vector element must not have 'void' type" :: Nil),
            ("s[1]", "Vector element must not have 'struct' type" :: Nil)
          )
        } {
          expr in {
            typeCheck {
              s"""
              |struct s {
              |  bool b;
              |}
              |
              |(* toplevel *)
              |fsm f {
              |  in u8 x;
              |  void main() {
              |    $expr y;
              |    fence;
              |  }
              |}"""
            }
            checkSingleError(err)
          }
        }
      }
    }

    "slice" - {
      for {
        (expr, err) <- List(
          ("a[1:0]", Nil),
          ("b[1:0]", Nil),
          ("b[0][1:0]", Nil),
          ("b[0][0][1:0]", Nil),
          ("c[1:0]", "Target is not sliceable" :: Nil),
          ("c[0][1:0]", Nil),
          ("c[0][0][1:0]", Nil),
          ("c[0][0][0][1:0]", Nil),
          ("bool[1:0]", "Target is not sliceable" :: Nil),
          ("main[1:0]", "Target is not sliceable" :: Nil),
          ("a[1:bool]", "Right index is of non-packed type" :: Nil),
          ("a[bool:0]", "Left index is of non-packed type" :: Nil),
          ("8'd0[3'd1:2'd0]", "Right index yields 2 bits, 3 bits are expected" :: Nil),
          ("8'd0[3'd1:3'd0]", Nil),
          ("8'd0[3'd1:4'd0]", "Right index yields 4 bits, 3 bits are expected" :: Nil),
          ("7'd0[3'd1:2'd0]", "Right index yields 2 bits, 3 bits are expected" :: Nil),
          ("7'd0[3'd1:3'd0]", Nil),
          ("7'd0[3'd1:4'd0]", "Right index yields 4 bits, 3 bits are expected" :: Nil),
          ("9'd0[4'd1:2'd0]", "Right index yields 2 bits, 4 bits are expected" :: Nil),
          ("9'd0[4'd1:3'd0]", "Right index yields 3 bits, 4 bits are expected" :: Nil),
          ("9'd0[4'd1:4'd0]", Nil),
          ("8'd0[2'd1:3'd0]", "Left index yields 2 bits, 3 bits are expected" :: Nil),
          ("8'd0[4'd1:3'd0]", "Left index yields 4 bits, 3 bits are expected" :: Nil),
          ("7'd0[2'd1:3'd0]", "Left index yields 2 bits, 3 bits are expected" :: Nil),
          ("7'd0[4'd1:3'd0]", "Left index yields 4 bits, 3 bits are expected" :: Nil),
          ("9'd0[2'd1:4'd0]", "Left index yields 2 bits, 4 bits are expected" :: Nil),
          ("9'd0[3'd1:4'd0]", "Left index yields 3 bits, 4 bits are expected" :: Nil),
          ("9'd0[4'sd1:4'd0]", "Left index must be unsigned" :: Nil),
          ("9'd0[4'd1:4'sd0]", "Right index must be unsigned" :: Nil),
          ("b[0:0]", Nil),
          ("b[1'd1:2'd0]", "Right index yields 2 bits, 1 bits are expected" :: Nil),
          ("b[2'd1:1'd0]", "Left index yields 2 bits, 1 bits are expected" :: Nil),
          ("b[1'd0+:1'd1]", "Right index yields 1 bits, 2 bits are expected" :: Nil)
        )
      } {
        expr in {
          typeCheck {
            s"""
            |fsm f {
            |  out sync u2 a;
            |  i3[2][2] b;
            |  i3[2][2] c[4];
            |  void main() {
            |    $$display("", $expr);
            |    fence;
            |  }
            |}"""
          }
          checkSingleError(err)
        }
      }
    }

    "select" - {
      for {
        (text, kind, msg) <- List[(String, PartialFunction[Any, Unit], String)](
          // format: off
          ("d.x", { case TypeSInt(w) if w == 8 => }, ""),
          ("e.y", { case TypeRecord(a, List(x)) if a.name == "a" && x.name == "x" => }, ""),
          ("e.y.x", { case TypeSInt(w) if w == 8 => }, ""),
          ("d.z", { case TypeError => }, "No member named 'z' in value of type 'struct a'"),
          ("e.z", { case TypeError => }, "No member named 'z' in value of type 'struct b'"),
          ("e.y.z", { case TypeError => }, "No member named 'z' in value of type 'struct a'"),
          ("f.x", { case TypeSInt(w) if w == 8 => }, ""),
          ("g.y", { case TypeRecord(a, List(x)) if a.name == "a" && x.name == "x" => }, ""),
          ("g.y.x", { case TypeSInt(w) if w == 8 => }, ""),
          ("f.valid", { case TypeUInt(w) if w == 1 => }, ""),
          ("g.valid", { case TypeUInt(w) if w == 1 => }, ""),
          ("a.x", { case TypeNone(TypeSInt(w)) if w == 8 => }, ""),
          ("b.y", { case TypeNone(TypeRecord(a, List(x))) if a.name == "a" && x.name == "x" => }, ""),
          ("b.y.x", { case TypeNone(TypeSInt(w)) if w == 8 => }, "")
          // format: on
        )
      } {
        text in {
          typeCheck {
            s"""
            |struct a {
            |  i8 x;
            |}
            |
            |struct b {
            |  a y;
            |}
            |
            |(* toplevel *)
            |fsm c {
            |  a d;
            |  b e;
            |  in sync a f;
            |  out sync b g;
            |  void main() {
            |    $$display("", @bits($text));
            |    fence;
            |  }
            |}"""
          } tap { tree =>
            if (msg.isEmpty) {
              tree getFirst {
                case e: ExprSelect => e
              } tap {
                _.tpe should matchPattern(kind)
              }
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }
        }
      }
    }

    "call" - {
      for {
        (text, kind, msg) <- List(
          // format: off
          ("a()", TypeError, "'.*' is not callable"),
          ("a.valid()", TypeError, s"'.*' is not callable"),
          ("a.valid(1'b1)", TypeError, s"'.*' is not callable"),
          ("a.write()", TypeError, "Function call expects 1 arguments, 0 given"),
          ("a.write(2'b1)", TypeVoid, ""),
          ("a.write(1'b1, 1'b1)", TypeError, "Function call expects 1 arguments, 2 given"),
          ("bar()", TypeVoid, ""),
          ("bar(1, 2, 3, 4, 5)", TypeError, "Function call expects 0 arguments, 5 given"),
          ("a.write(bar)", TypeError, "Argument 1 of function call is of non-packed type"),
          ("a.write(3'b1)", TypeError, "Argument 1 of function call yields 3 bits, 2 bits are expected"),
          ("a.write(1'b1)", TypeError, "Argument 1 of function call yields 1 bits, 2 bits are expected"),
          ("int x = @bits(a)", TypeNum(false), ""),
          ("int x = @bits(a.valid)", TypeNum(false), "")
          // format: on
        )
      } {
        text in {
          typeCheck {
            s"""
            |fsm c {
            |  out sync u2 a;
            |
            |  void bar() {
            |    fence;
            |  }
            |
            |  void main() {
            |    $text;
            |    fence;
            |  }
            |}"""
          } tap { tree =>
            if (msg.isEmpty) {
              tree getFirst {
                case e: ExprCall => e
              } tap {
                _.tpe shouldBe kind
              }
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }
        }
      }
    }

    "cat" - {
      for {
        (text, kind, msg) <- List(
          ("{1'b1, 1'b0}", TypeUInt(2), ""),
          ("{a, a}", TypeUInt(4), ""),
          ("{a, 1}", TypeError, s"Part 2 of bit concatenation is of non-packed type"),
          ("{a, bool}", TypeError, s"Part 2 of bit concatenation is of non-packed type")
        )
      } {
        text in {
          typeCheck {
            s"""
            |fsm c {
            |  out sync u2 a;
            |  void main() {
            |    $$display("", $text);
            |    fence;
            |  }
            |}"""
          } tap { tree =>
            if (msg.isEmpty) {
              tree getFirst {
                case e: ExprCat => e
              } tap {
                _.tpe shouldBe kind
              }
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }
        }
      }
    }

    "rep" - {
      for {
        (text, kind, msg) <- List(
          ("{4{1'b1}}", TypeUInt(4), ""),
          ("{4{2'b0, 1'b1}}", TypeUInt(12), ""),
          ("{4{a}}", TypeUInt(8), ""),
          ("{4{1}}", TypeError, s"Value of bit repetition is of non-packed type"),
          ("{4{bool}}", TypeError, s"Value of bit repetition is of non-packed type"),
          ("{bool{1'b1}}", TypeError, s"Count of bit repetition is of non-numeric type")
        )
      } {
        text in {
          typeCheck {
            s"""
            |fsm c {
            |  (* unused *) out sync u2 a;
            |  void main() {
            |    $$display("", $text);
            |    fence;
            |  }
            |}"""
          } tap { tree =>
            if (msg.isEmpty) {
              tree getFirst {
                case e: ExprRep => e
              } tap {
                _.tpe shouldBe kind
              }
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }
        }
      }
    }
  }
}
