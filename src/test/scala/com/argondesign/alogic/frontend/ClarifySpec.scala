////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Tests for the Clarify transform
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.ast.Trees.Expr.ImplicitConversions._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Messages.Warning
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.core.Types._
import org.scalatest.freespec.AnyFreeSpec

final class ClarifySpec extends AnyFreeSpec with AlogicTest {

  implicit private val cc: CompilerContext = new CompilerContext

  implicit private val fe: Frontend = new Frontend

  private def elaborate(text: String): Option[Desc] =
    fe.elaborate(Source("TyperCheckerExprSpec", text)) pipe {
      case Left(ms)      => ms foreach cc.addMessage; None
      case Right(result) => Some(result)
    }

  private def typeCheck(tree: Tree): Unit =
    fe.typeCheck(tree) match {
      case Complete(_) =>
      case Unknown(_)  => fail
      case Failure(ms) => ms foreach cc.addMessage
    }

  private def typeCheck(text: String): Tree =
    elaborate(text) tap { _ =>
      cc.messages foreach println
      cc.messages shouldBe empty
    } pipe { _.value } tap { tree =>
      typeCheck(tree)
    }

  private def clarify(text: String): Tree = Clarify(typeCheck(text))

  "Clarify should" - {
    "automatically insert casts" - {
      "to infer sizes of unsized literals in" - {
        "binary operator operands" - {
          for (
            op <- List("*", "/", "%", "+", "-", "&", "|", "^", ">", ">=", "<", "<=", "==", "!=")
          ) {
            for {
              (text, res) <- List(
                (s"8'd3 $op 2", Some(Right(ExprCast(TypeUInt(8), 2)))),
                (s"2 $op 8'd3", Some(Left(ExprCast(TypeUInt(8), 2)))),
                (s"8'd3 $op -2s", Some(Right(ExprCast(TypeSInt(8), ExprNum(true, -2))))),
                (s"-2s $op 8'd3", Some(Left(ExprCast(TypeSInt(8), ExprNum(true, -2))))),
                (s"7'sd3 $op 2", Some(Right(ExprCast(TypeUInt(7), 2)))),
                (s"2 $op 7'sd3", Some(Left(ExprCast(TypeUInt(7), 2)))),
                (s"7'sd3 $op -2s", Some(Right(ExprCast(TypeSInt(7), ExprNum(true, -2))))),
                (s"-2s $op 7'sd3", Some(Left(ExprCast(TypeSInt(7), ExprNum(true, -2))))),
                (s"4 $op 2 ", None)
              )
            } {
              text in {
                clarify {
                  s"""
                     |struct s {
                     |  void function() {
                     |    $$display("", $text);
                     |  }
                     |}""".stripMargin
                } getFirst {
                  case expr: ExprBinary => expr
                } tap { expr =>
                  res match {
                    case Some(Left(l))  => expr.lhs shouldBe l
                    case Some(Right(r)) => expr.rhs shouldBe r
                    case None           =>
                  }
                }
                cc.messages filter {
                  !_.isInstanceOf[Warning]
                } shouldBe empty
              }
            }
          }
        }

        "ternary operator operands" - {
          for {
            (text, res) <- List(
              ("0 ? 0 : 2'd1", Some(Left(ExprCast(TypeUInt(2), 0)))),
              ("0 ? 2'd1 : 0", Some(Right(ExprCast(TypeUInt(2), 0)))),
              ("0 ? 0 : 3'sd1", Some(Left(ExprCast(TypeUInt(3), 0)))),
              ("0 ? 3'sd1 : 0", Some(Right(ExprCast(TypeUInt(3), 0)))),
              ("0 ? 0s : 2'd1", Some(Left(ExprCast(TypeSInt(2), ExprNum(true, 0))))),
              ("0 ? 2'd1 : 0s", Some(Right(ExprCast(TypeSInt(2), ExprNum(true, 0))))),
              ("0 ? 0s : 3'sd1", Some(Left(ExprCast(TypeSInt(3), ExprNum(true, 0))))),
              ("0 ? 3'sd1 : 0s", Some(Right(ExprCast(TypeSInt(3), ExprNum(true, 0))))),
              ("0 ? 1 : 0", None)
            )
          } {
            text in {
              clarify {
                s"""
                   |struct s {
                   |  void function() {
                   |    $$display("", $text);
                   |  }
                   |}""".stripMargin
              } getFirst {
                case expr: ExprCond => expr
              } tap { expr =>
                res match {
                  case Some(Left(l))  => expr.thenExpr shouldBe l
                  case Some(Right(r)) => expr.elseExpr shouldBe r
                  case None           =>
                }
              }
              cc.messages shouldBe empty
            }
          }
        }

        "index expressions" - {
          def check(expr: Expr, expected: List[Expr]): Unit = expr match {
            case ExprIndex(e, i) =>
              expected match {
                case v :: vs =>
                  i shouldBe v
                  if (vs.nonEmpty) check(e, vs)
                case _ => fail()
              }
            case _ => fail()
          }

          for {
            (index, res) <- List(
              // format: off
              ("u8 a; u1 b = a[0]", List(ExprCast(TypeUInt(3), 0))),
              ("u9 a; u1 b = a[0]", List(ExprCast(TypeUInt(4), 0))),
              ("u32[8] a; u32 b = a[0]", List(ExprCast(TypeUInt(3), 0))),
              ("u33[9] a; u33 b = a[0]", List(ExprCast(TypeUInt(4), 0))),
              ("u32[8] a; u1 b = a[0][2]", List(ExprCast(TypeUInt(5), 2), ExprCast(TypeUInt(3), 0))),
              ("u33[9] a; u1 b = a[0][2]", List(ExprCast(TypeUInt(6), 2), ExprCast(TypeUInt(4), 0)))
              // format: on
            )
          } {
            index in {
              clarify {
                s"""
                   |struct s {
                   |  void function () {
                   |    $index;
                   |  }
                   |}""".stripMargin
              } getFirst {
                case DescVar(_, _, _, Some(i)) => i
              } tap {
                check(_, res)
              }
              cc.messages shouldBe empty
            }
          }
        }

        "slice expressions" - {
          def check(expr: Expr, expected: List[(Expr, Expr)]): Unit = expr match {
            case ExprSlice(e, l, _, r) =>
              expected match {
                case (vl, vr) :: vs =>
                  l shouldBe vl
                  r shouldBe vr
                  if (vs.nonEmpty) check(e, vs)
                case _ => fail()
              }
            case _ => fail()
          }

          for {
            (slice, res) <- List(
              // format: off
              ("u8 a; u2 b = a[1:0]", List((ExprCast(TypeUInt(3), 1), ExprCast(TypeUInt(3), 0)))),
              ("u9 a; u2 b = a[1:0]", List((ExprCast(TypeUInt(4), 1), ExprCast(TypeUInt(4), 0)))),
              ("u8 a; u1 b = a[2+:1]", List((ExprCast(TypeUInt(3), 2), ExprCast(TypeUInt(4), 1)))),
              ("u9 a; u1 b = a[2+:1]", List((ExprCast(TypeUInt(4), 2), ExprCast(TypeUInt(4), 1)))),
              ("u8 a; u1 b = a[2-:1]", List((ExprCast(TypeUInt(3), 2), ExprCast(TypeUInt(4), 1)))),
              ("u9 a; u1 b = a[2-:1]", List((ExprCast(TypeUInt(4), 2), ExprCast(TypeUInt(4), 1)))),
              ("u32[8] a; u32[2] b = a[1:0]", List((ExprCast(TypeUInt(3), 1), ExprCast(TypeUInt(3), 0)))),
              ("u33[9] a; u33[2] b = a[1:0]", List((ExprCast(TypeUInt(4), 1), ExprCast(TypeUInt(4), 0)))),
              ("u32[8] a; u32[1] b = a[2+:1]", List((ExprCast(TypeUInt(3), 2), ExprCast(TypeUInt(4), 1)))),
              ("u33[9] a; u33[1] b = a[2+:1]", List((ExprCast(TypeUInt(4), 2), ExprCast(TypeUInt(4), 1)))),
              ("u32[8] a; u32[1] b = a[2-:1]", List((ExprCast(TypeUInt(3), 2), ExprCast(TypeUInt(4), 1)))),
              ("u33[9] a; u33[1] b = a[2-:1]", List((ExprCast(TypeUInt(4), 2), ExprCast(TypeUInt(4), 1)))),
              // format: on
            )
          } {
            slice in {
              clarify {
                s"""
                   |struct s {
                   |  void function() {
                   |    $slice;
                   |  }
                   |}""".stripMargin
              } getFirst {
                case DescVar(_, _, _, Some(i)) => i
              } tap {
                check(_, res)
              }
              cc.messages shouldBe empty
            }
          }
        }

        "initializer expressions" - {
          for {
            (decl, pattern) <- List[(String, PartialFunction[Any, Unit])](
              // format: off
              ("i8 a = 2s", {
                case ExprCast(TypeSInt(w), ExprNum(true, v)) if v == 2 && w == 8 =>
              }),
              ("u8 a = 2s", {
                case ExprCast(TypeUInt(w), ExprNum(true, v)) if v == 2 && w == 8 =>
              }),
              ("i7 a = 2", {
                case ExprCast(TypeSInt(w), ExprNum(false, v)) if v == 2 && w == 7 =>
              }),
              ("u7 a = 2", {
                case ExprCast(TypeUInt(w), ExprNum(false, v)) if v == 2 && w == 7 =>
              }),
              ("out i8 a = 2s", {
                case ExprCast(TypeSInt(w), ExprNum(true, v)) if v == 2 && w == 8 =>
              }),
              ("out u8 a = 2s", {
                case ExprCast(TypeUInt(w), ExprNum(true, v)) if v == 2 && w == 8 =>
              }),
              ("out i7 a = 2", {
                case ExprCast(TypeSInt(w), ExprNum(false, v)) if v == 2 && w == 7 =>
              }),
              ("out u7 a = 2", {
                case ExprCast(TypeUInt(w), ExprNum(false, v)) if v == 2 && w == 7 =>
              }),
              // const/static initializers get evaluted during elaboration, so we have exact values
              ("const i8 a = 2s", {
                case ExprInt(true, 8, v) if v == 2 =>
              }),
              ("const u8 a = 2s", {
                case ExprInt(false, 8, v) if v == 2 =>
              }),
              ("const i7 a = 2", {
                case ExprInt(true, 7, v) if v == 2 =>
              }),
              ("const u7 a = 2", {
                case ExprInt(false, 7, v) if v == 2 =>
              }),
              ("const int  a = 2s", {
                case ExprNum(true, v) if v == 2 =>
              }),
              ("const uint a = 2s", {
                case ExprNum(false, v) if v == 2 =>
              }),
              ("const int  a = 2", {
                case ExprNum(true, v) if v == 2 =>
              }),
              ("const uint a = 2", {
                case ExprNum(false, v) if v == 2 =>
              }),
              ("static i8 a = 2s", {
                case ExprInt(true, 8, v) if v == 2 =>
              }),
              ("static u8 a = 2s", {
                case ExprInt(false, 8, v) if v == 2 =>
              }),
              ("static i7 a = 2", {
                case ExprInt(true, 7, v) if v == 2 =>
              }),
              ("static u7 a = 2", {
                case ExprInt(false, 7, v) if v == 2 =>
              })
              // format: on
            )
          } {
            s"$decl" in {
              clarify {
                s"""
                   |fsm s {
                   |  $decl;
                   |}""".stripMargin
              } getFirst {
                case DescVar(_, _, _, Some(i))       => i
                case DescConst(_, _, _, i)           => i
                case DescStatic(_, _, _, Some(i))    => i
                case DescOut(_, _, _, _, _, Some(i)) => i
              } tap {
                _ should matchPattern(pattern)
              }
              cc.messages shouldBe empty
            }
          }
        }

        "right hand sides of assignments" - {
          for {
            (assign, res) <- List(
              ("i8 a = 0; a = 2s", ExprCast(TypeSInt(8), ExprNum(true, 2))),
              ("u8 a = 0; a = 2s", ExprCast(TypeSInt(8), ExprNum(true, 2))),
              ("i7 a = 0; a = 2", ExprCast(TypeUInt(7), ExprNum(false, 2))),
              ("u7 a = 0; a = 2", ExprCast(TypeUInt(7), ExprNum(false, 2))),
              ("i8 a = 0; a += 2s", ExprCast(TypeSInt(8), ExprNum(true, 2))),
              ("u8 a = 0; a += 2s", ExprCast(TypeSInt(8), ExprNum(true, 2))),
              ("i7 a = 0; a += 2", ExprCast(TypeUInt(7), ExprNum(false, 2))),
              ("u7 a = 0; a += 2", ExprCast(TypeUInt(7), ExprNum(false, 2)))
            )
          } {
            assign in {
              clarify {
                s"""
                   |struct s {
                   |  void function() {
                   |    $assign;
                   |  }
                   |}""".stripMargin
              } getFirst {
                case StmtAssign(_, rhs)    => rhs
                case StmtUpdate(_, _, rhs) => rhs
              } tap {
                _ shouldBe res
              }
              cc.messages shouldBe empty
            }
          }
        }

        "function argument expressions" - {
          for {
            (call, res) <- List(
              // format: off
              ("a.write(0s)", ExprCast(TypeSInt(1), ExprNum(true, 0))),
              ("a.write(1u)", ExprCast(TypeUInt(1), ExprNum(false, 1))),
              ("b.write(2s)", ExprCast(TypeSInt(10), ExprNum(true, 2))),
              ("b.write(3u)", ExprCast(TypeUInt(10), ExprNum(false, 3))),
              ("c.write(4s)", ExprCast(TypeSInt(20), ExprNum(true, 4))),
              ("c.write(5u)", ExprCast(TypeUInt(20), ExprNum(false, 5)))
              // format: on
            )
          } {
            call in {
              clarify {
                s"""
                   |fsm f {
                   |  out sync bool a;
                   |  out sync u10 b;
                   |  out sync i20 c;
                   |  void main() {
                   |    $call;
                   |    fence;
                   |  }
                   |}""".stripMargin
              } getFirst {
                case ExprCall(_, List(ArgP(expr))) => expr
              } tap {
                _ shouldBe res
              }
              cc.messages shouldBe empty
            }
          }
        }

        "function return expressions" - {
          for {
            (kind, value, res) <- List(
              ("i8", "2s", ExprCast(TypeSInt(8), ExprNum(true, 2))),
              ("u8", "2s", ExprCast(TypeSInt(8), ExprNum(true, 2))),
              ("i7", "2", ExprCast(TypeUInt(7), ExprNum(false, 2))),
              ("u7", "2", ExprCast(TypeUInt(7), ExprNum(false, 2)))
            )
          } {
            s"return $kind $value" in {
              clarify {
                s"""
                   |struct s {
                   |  $kind f() {
                   |    return $value;
                   |  }
                   |}""".stripMargin
              } getFirst {
                case StmtReturn(_, Some(expr)) => expr
              } tap {
                _ shouldBe res
              }
              cc.messages shouldBe empty
            }
          }
        }
      }
    }

    "replace calls to polymorphic builtins" - {

      "inside slice" in {
        clarify {
          """fsm f {
            |  out wire bool o;
            |  void main() {
            |    o = 4'b1111[@zx(2, 1'd1) +: 1];
            |    fence;
            |  }
            |}""".stripMargin
        } getFirst {
          case ExprCall(tgt, _) => tgt
        } tap {
          _.tpe should not be a[TypePolyFunc]
        }

      }
    }
  }
}
