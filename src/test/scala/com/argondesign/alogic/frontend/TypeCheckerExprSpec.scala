////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Expression type checking tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.core.Types._
import org.scalatest.freespec.AnyFreeSpec

final class TypeCheckerExprSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  private val fe = new Frontend

  private def elaborate(text: String): Option[Desc] =
    fe.elaborate(Source("TyperCheckerExprSpec", text)) pipe {
      case Left(ms)      => ms foreach cc.addMessage; None
      case Right(result) => Some(result)
    }

  private def typeCheck(tree: Tree): Unit =
    fe.typeCheck(tree) match {
      case Complete(_) =>
      case Unknown(_)  => fail()
      case Failure(ms) => ms foreach cc.addMessage
    }

  private def typeCheck(text: String): Tree =
    elaborate(text) tap { _ =>
      cc.messages foreach println
      cc.messages shouldBe empty
    } pipe { _.value } tap { tree =>
      typeCheck(tree)
    }

  "The Typer should type check expressions" - {
    "unary" - {
      for {
        (expr, err) <- List(
          // format: off
          ("+(N)", Nil),
          ("-(N)", Nil),
          ("~(N)", Nil),
          ("!(N)", "Operand of unary '!' operator yields 8 bits, a 1 bit value is expected" :: Nil),
          ("&(N)", Nil),
          ("|(N)", Nil),
          ("^(N)", Nil),
          ("+(main)", "Operand of unary '+' operator is of non-packed type" :: Nil),
          ("-(main)", "Operand of unary '-' operator is of non-packed type" :: Nil),
          ("~(main)", "Operand of unary '~' operator is of non-packed type" :: Nil),
          ("!(main)", "Operand of unary '!' operator is of non-packed type, a 1 bit value is expected" :: Nil),
          ("&(main)", "Operand of unary '&' operator is of non-packed type" :: Nil),
          ("|(main)", "Operand of unary '|' operator is of non-packed type" :: Nil),
          ("^(main)", "Operand of unary '^' operator is of non-packed type" :: Nil),
          ("+(8'd1)", Nil),
          ("-(8'd1)", Nil),
          ("~(8'd1)", Nil),
          ("!(8'd1)", "Operand of unary '!' operator yields 8 bits, a 1 bit value is expected" :: Nil),
          ("+(8'sd1)", Nil),
          ("-(8'sd1)", Nil),
          ("~(8'sd1)", Nil),
          ("!(8'sd1)", "Operand of unary '!' operator yields 8 bits, a 1 bit value is expected" :: Nil),
          ("&(1)", Nil),
          ("|(1)", Nil),
          ("^(1)", Nil),
          ("&(1s)", Nil),
          ("|(1s)", Nil),
          ("^(1s)", Nil),
          ("^(-1s)", "Unary operator '^' is not well defined on a negative unsized value (-1s)" :: Nil),
          ("-(0)", Nil),
          ("-(1)", "Unary operator '-' has 'uint' result type, but evaluates to a negative value" :: Nil),
          ("~(0)", "Unary operator '~' has 'uint' result type, but evaluates to a negative value" :: Nil),
          ("~(1)", "Unary operator '~' has 'uint' result type, but evaluates to a negative value" :: Nil),
          ("!true", Nil)
          // format: on
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
               |}""".stripMargin
          }
          checkSingleError(err)
        }
      }
    }

    "binary" - {
      "errors" - {
        "same width" - {
          for (
            op <- List("*", "/", "%", "+", "-", "&", "|", "^", ">", ">=", "<", "<=", "==", "!=")
          ) {
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
                     |  const u8 N = 8'd2;
                     |  void main() {
                     |    $$display("", $expr);
                     |    fence;
                     |  }
                     |}""".stripMargin
                }
                checkSingleError(err)
              }
            }
          }
        }

        "non numeric operand" - {
          for (
            (op, strictWidth) <- List(
              ("*", true),
              ("/", true),
              ("%", true),
              ("+", true),
              ("-", true),
              ("<<", false),
              ("<<<", false),
              (">>", false),
              (">>>", false),
              (">", true),
              (">=", true),
              ("<", true),
              ("<=", true),
              ("==", true),
              ("!=", true),
              ("&", true),
              ("|", true),
              ("^", true)
            )
          ) {
            val msg =
              if (strictWidth) "is of non-packed type" else "is of neither numeric nor packed type"
            for {
              (expr, err) <- List(
                (s"bool $op 1'd1", s"Left hand operand of '$op' $msg" :: Nil),
                (s"1'd1 $op bool", s"Right hand operand of '$op' $msg" :: Nil)
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
                     |}""".stripMargin
                }
                checkSingleError(err)
              }
            }
          }
        }

        "binary ' " - {
          "LHS is const" - {
            for (lhs <- List("i", "bool ", "@unknownu(1)")) {
              lhs in {
                typeCheck {
                  s"""
                     |fsm a {
                     |  in u32 i;
                     |  void main() {
                     |    $$display("", i'(2'd0));
                     |    fence;
                     |  }
                     |}""".stripMargin
                }
                checkSingleError(
                  "Left hand side operand of binary ' operator must be a compile time constant" :: Nil
                )
              }
            }
          }

          "LHS is positive" - {
            for (lhs <- List("0", "-1s")) {
              lhs in {
                typeCheck {
                  s"""
                     |fsm a {
                     |  in u32 i;
                     |  void main() {
                     |    $$display("", $lhs'(2'd0));
                     |    fence;
                     |  }
                     |}""".stripMargin
                }
                val value = if (lhs.endsWith("s")) lhs.dropRight(1) else lhs
                checkSingleError(
                  s"Left hand side operand of binary ' operator must be positive ($value is <= 0)" :: Nil
                )
              }
            }
          }

          "RHS is packed" - {
            for ((rhs, ok) <- List(("1", false), ("bool", false), ("2'd0", true))) {
              rhs in {
                typeCheck {
                  s"""
                     |fsm a {
                     |  void main() {
                     |    $$display("", 32'($rhs));
                     |    fence;
                     |  }
                     |}""".stripMargin
                }
                checkSingleError(
                  if (ok) Nil
                  else "Right hand side operand of binary ' operator is of non-packed type" :: Nil
                )
              }
            }
          }

          "narrowing" in {
            typeCheck {
              s"""
                 |fsm a {
                 |  void main() {
                 |    $$display("", 1'(2'd0));
                 |    fence;
                 |  }
                 |}""".stripMargin
            }
            checkSingleError(
              "Binary ' operator causes narrowing" :: Nil
            )
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
                   |}""".stripMargin
              }
              checkSingleWarning(warn)
            }
          }
        }

        "comparison signedness" - {
          for (op <- List(">", ">=", "<", "<=", "==", "!=")) {
            for {
              (expr, warn) <- List(
                // format: off
                (s"3     $op 1    ", Nil),
                (s"3     $op 1s   ", Nil),
                (s"3s    $op 1    ", Nil),
                (s"3s    $op 1s   ", Nil),
                (s"3     $op 8'd1 ", Nil),
                (s"3     $op 8'sd1", "Comparison between signed and unsigned operands is interpreted as a comparison between two unsigned values" :: Nil),
                (s"3s    $op 8'd1 ", "Comparison between signed and unsigned operands is interpreted as a comparison between two unsigned values" :: Nil),
                (s"3s    $op 8'sd1", Nil),
                (s"8'd3  $op 1    ", Nil),
                (s"8'd3  $op 1s   ", "Comparison between signed and unsigned operands is interpreted as a comparison between two unsigned values" :: Nil),
                (s"8'sd3 $op 1    ", "Comparison between signed and unsigned operands is interpreted as a comparison between two unsigned values" :: Nil),
                (s"8'sd3 $op 1s   ", Nil),
                (s"8'd3  $op 8'd1 ", Nil),
                (s"8'd3  $op 8'sd1", "Comparison between signed and unsigned operands is interpreted as a comparison between two unsigned values" :: Nil),
                (s"8'sd3 $op 8'd1 ", "Comparison between signed and unsigned operands is interpreted as a comparison between two unsigned values" :: Nil),
                (s"8'sd3 $op 8'sd1", Nil)
                // format: on
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
                     |}""".stripMargin
                }
                checkSingleWarning(warn)
              }
            }
          }
        }
      }

      "&& ||" - {
        for (op <- List("&&", "||")) {
          for {
            (expr, error) <- List(
              // format: off
              (s"1'd1 $op 1'd0", Nil),
              (s"1    $op 1'd1", List(s"Left hand operand of '$op' yields an unsized value, a 1 bit value is expected")),
              (s"1'd1 $op    1", List(s"Right hand operand of '$op' yields an unsized value, a 1 bit value is expected")),
              (s"bool $op 1'd1", List(s"Left hand operand of '$op' is of non-packed type, a 1 bit value is expected")),
              (s"1'd1 $op bool", List(s"Right hand operand of '$op' is of non-packed type, a 1 bit value is expected")),
              (s"2'd0 $op 1'd1", List(s"Left hand operand of '$op' yields 2 bits, a 1 bit value is expected"))
              // format: on
            )
          } expr in {
            typeCheck {
              s"""
                 |fsm a {
                 |  const u8 N = 8'd2;
                 |  void main() {
                 |    $$display("", $expr);
                 |    fence;
                 |  }
                 |}""".stripMargin
            }
            checkSingleError(error)
          }
        }
      }
    }

    "ternary" - {
      for {
        (expr, err) <- List(
          ("a ? b[0][0] : c[0][0][0]", Nil),
          (
            "c ? b : c[0][0][0]",
            "Condition of ternary '?:' is of non-packed type, a 1 bit value is expected" :: Nil
          ),
          ("a ? 8'd3 : 8'd2", Nil),
          ("a ? 8'd3 : 8'sd2", Nil),
          ("a ? 8'sd3 : 8'd2", Nil),
          ("a ? 8'sd3 : 8'sd2", Nil),
          ("a ? 8'd3 : 2", Nil),
          ("a ? 3 : 8'd1", Nil),
          (
            "a ? 3 : 2",
            "Expression of unsized integer type must be a compile time constant" :: Nil
          ),
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
          ("a ? c : b", "'then' operand of ternary '?:' is of non-packed type" :: Nil),
          ("a ? b : c", "'else' operand of ternary '?:' is of non-packed type" :: Nil),
          (
            "2'd0 ? 1'd0 : 1'd1",
            "Condition of ternary '?:' yields 2 bits, a 1 bit value is expected" :: Nil
          ),
          (
            "0 ? 1'd0 : 1'd1",
            "Condition of ternary '?:' yields an unsized value, a 1 bit value is expected" :: Nil
          )
        )
      } {
        expr in {
          typeCheck {
            s"""
               |fsm f {
               |  out sync u1 a;
               |  i2[1][2] b;
               |  i2[1][2] c[4];
               |  void main() {
               |    $$display("", $expr);
               |    fence;
               |  }
               |}""".stripMargin
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
            ("8'd0[2'd0]", "Index yields 2 bits, a 3 bit value is expected" :: Nil),
            ("8'd0[3'd0]", Nil),
            ("8'd0[4'd0]", "Index yields 4 bits, a 3 bit value is expected" :: Nil),
            ("7'd0[2'd0]", "Index yields 2 bits, a 3 bit value is expected" :: Nil),
            ("7'd0[3'd0]", Nil),
            ("7'd0[4'd0]", "Index yields 4 bits, a 3 bit value is expected" :: Nil),
            ("9'd0[2'd0]", "Index yields 2 bits, a 4 bit value is expected" :: Nil),
            ("9'd0[3'd0]", "Index yields 3 bits, a 4 bit value is expected" :: Nil),
            ("9'd0[4'd0]", Nil),
            ("1'd0[1'd0]", Nil),
            ("1'd0[2'd0]", "Index yields 2 bits, a 1 bit value is expected" :: Nil),
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
                 |}""".stripMargin
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
            ("u1[ 0 ]", "Size of vector must be positive (0 is <= 0)" :: Nil),
            ("u1[-1s]", "Size of vector must be positive (-1 is <= 0)" :: Nil),
            ("u1[ x ]", "Size of vector must be a compile time constant" :: Nil),
            ("u1[1][ 2 ]", Nil),
            ("u1[1][ 1 ]", Nil),
            ("u1[1][ 0 ]", "Size of vector must be positive (0 is <= 0)" :: Nil),
            ("u1[1][-1s]", "Size of vector must be positive (-1 is <= 0)" :: Nil),
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
                 |fsm f {
                 |  in u8 x;
                 |  void main() {
                 |    $expr y;
                 |    fence;
                 |  }
                 |}""".stripMargin
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
          ("a[1:bool]", "Right index of ':' slice is of non-packed type" :: Nil),
          ("a[bool:0]", "Left index of ':' slice is of non-packed type" :: Nil),
          (
            "8'd0[3'd1:2'd0]",
            "Right index of ':' slice yields 2 bits, a 3 bit value is expected" :: Nil
          ),
          ("8'd0[3'd1:3'd0]", Nil),
          (
            "8'd0[3'd1:4'd0]",
            "Right index of ':' slice yields 4 bits, a 3 bit value is expected" :: Nil
          ),
          (
            "7'd0[3'd1:2'd0]",
            "Right index of ':' slice yields 2 bits, a 3 bit value is expected" :: Nil
          ),
          ("7'd0[3'd1:3'd0]", Nil),
          (
            "7'd0[3'd1:4'd0]",
            "Right index of ':' slice yields 4 bits, a 3 bit value is expected" :: Nil
          ),
          (
            "9'd0[4'd1:2'd0]",
            "Right index of ':' slice yields 2 bits, a 4 bit value is expected" :: Nil
          ),
          (
            "9'd0[4'd1:3'd0]",
            "Right index of ':' slice yields 3 bits, a 4 bit value is expected" :: Nil
          ),
          ("9'd0[4'd1:4'd0]", Nil),
          (
            "8'd0[2'd1:3'd0]",
            "Left index of ':' slice yields 2 bits, a 3 bit value is expected" :: Nil
          ),
          (
            "8'd0[4'd1:3'd0]",
            "Left index of ':' slice yields 4 bits, a 3 bit value is expected" :: Nil
          ),
          (
            "7'd0[2'd1:3'd0]",
            "Left index of ':' slice yields 2 bits, a 3 bit value is expected" :: Nil
          ),
          (
            "7'd0[4'd1:3'd0]",
            "Left index of ':' slice yields 4 bits, a 3 bit value is expected" :: Nil
          ),
          (
            "9'd0[2'd1:4'd0]",
            "Left index of ':' slice yields 2 bits, a 4 bit value is expected" :: Nil
          ),
          (
            "9'd0[3'd1:4'd0]",
            "Left index of ':' slice yields 3 bits, a 4 bit value is expected" :: Nil
          ),
          ("9'd0[4'sd1:4'd0]", "Left index of ':' slice must be unsigned" :: Nil),
          ("9'd0[4'd1:4'sd0]", "Right index of ':' slice must be unsigned" :: Nil),
          ("b[0:0]", Nil),
          (
            "b[1'd1:2'd0]",
            "Right index of ':' slice yields 2 bits, a 1 bit value is expected" :: Nil
          ),
          (
            "b[2'd1:1'd0]",
            "Left index of ':' slice yields 2 bits, a 1 bit value is expected" :: Nil
          ),
          (
            "b[1'd0+:1'd1]",
            "Right index of '+:' slice yields 1 bit, a 2 bit value is expected" :: Nil
          )
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
               |}""".stripMargin
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
               |fsm c {
               |  a d;
               |  b e;
               |  in sync a f;
               |  out sync b g;
               |  void main() {
               |    $$display("", @bits($text));
               |    fence;
               |  }
               |}""".stripMargin
          } tap { tree =>
            if (msg.isEmpty) {
              tree getFirst {
                case e: ExprDot => e
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
          ("a()", TypeError, "Expression is not callable"),
          ("a.valid()", TypeError, s"Expression is not callable"),
          ("a.valid(1'b1)", TypeError, s"Expression is not callable"),
          ("a.write()", TypeError, "Function call expects 1 arguments, 0 given"),
          ("a.write(2'b1)", TypeVoid, ""),
          ("a.write(2'b1, 1'b1)", TypeError, "Function call expects 1 arguments, 2 given"),
          ("bar()", TypeVoid, ""),
          ("bar(1, 2, 3, 4, 5)", TypeError, "Function call expects 0 arguments, 5 given"),
          ("a.write(bar)", TypeError, "Argument 1 of function call is of non-packed type, a 2 bit value is expected"),
          ("a.write(3'b1)", TypeError, "Argument 1 of function call yields 3 bits, a 2 bit value is expected"),
          ("a.write(1'b1)", TypeError, "Argument 1 of function call yields 1 bit, a 2 bit value is expected"),
          ("const uint x = @bits(a)", TypeNum(false), ""),
          ("const uint x = @bits(a.valid)", TypeNum(false), ""),
          ("s.f()", TypeError, "Attempting to call non-static method via type")
          // format: on
        )
      } {
        text in {
          typeCheck {
            s"""
               |fsm c {
               |  out sync u2 a;
               |
               |  struct s {
               |    void f() {}
               |  }
               |
               |  void bar() {
               |    fence;
               |  }
               |
               |  void main() {
               |    $text;
               |    fence;
               |  }
               |}""".stripMargin
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
               |}""".stripMargin
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
          ("{4{1}}", TypeError, s"Replicated value is of non-packed type"),
          ("{4{bool}}", TypeError, s"Replicated value is of non-packed type"),
          ("{bool{1'b1}}", TypeError, s"Replication count is of non-numeric type")
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
               |}""".stripMargin
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
