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

package com.argondesign.alogic.passes

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.ast.Trees.Expr.ImplicitConversions._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Messages.Warning
import org.scalatest.freespec.AnyFreeSpec

final class AddCastsSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  private def addCasts(text: String): Thicket = Thicket {
    transformWithPass(
      Namer andThen
        Elaborate andThen
        TypeCheck andThen
        ResolvePolyFunc andThen
        AddCasts,
      text
    ) map {
      _ flatMap {
        case (decl, defn) => List(decl, defn)
      }
    } getOrElse Nil
  }

  "AddCasts should automatically insert casts" - {
    "to infer sizes of unsized literals in" - {
      "binary operator operands" - {
        for (op <- List("*", "/", "%", "+", "-", "&", "|", "^", ">", ">=", "<", "<=", "==", "!=")) {
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
              addCasts {
                s"""
                |struct s {
                |  void function() {
                |    $$display("", $text);
                |  }
                |}"""
              } getFirst {
                case expr: ExprBinary => expr
              } tap { expr =>
                res match {
                  case Some(Left(l))  => expr.lhs shouldBe l
                  case Some(Right(r)) => expr.rhs shouldBe r
                  case None           =>
                }
              }
              cc.messages filter { !_.isInstanceOf[Warning] } shouldBe empty
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
            addCasts {
              s"""
              |struct s {
              |  void function() {
              |    $$display("", $text);
              |  }
              |}"""
            } getFirst {
              case expr: ExprTernary => expr
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
            addCasts {
              s"""
              |struct s {
              |  void function () {
              |    $index;
              |  }
              |}"""
            } getFirst {
              case DefnVar(_, Some(i)) => i
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
            addCasts {
              s"""
              |struct s {
              |  void function() {
              |    $slice;
              |  }
              |}"""
            } getFirst {
              case DefnVar(_, Some(i)) => i
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
            ("i8 a = 2s", { case ExprCast(TypeSInt(w), ExprNum(true, v)) if v == 2 && w == 8 => }),
            ("u8 a = 2s", { case ExprCast(TypeSInt(w), ExprNum(true, v)) if v == 2 && w == 8 => }),
            ("i7 a = 2", { case ExprCast(TypeUInt(w), ExprNum(false, v)) if v == 2 && w == 7 => }),
            ("u7 a = 2", { case ExprCast(TypeUInt(w), ExprNum(false, v)) if v == 2 && w == 7 => }),
            ("int  a = 2s", { case ExprNum(true, v) if v == 2 => }),
            ("uint a = 2s", { case ExprCall(ExprSym(Symbol("$unsigned")), List(ArgP(ExprNum(true, v)))) if v == 2 => }),
            ("int  a = 2", { case ExprCall(ExprSym(Symbol("$signed")), List(ArgP(ExprNum(false, v)))) if v == 2 => }),
            ("uint a = 2", { case ExprNum(false, v) if v == 2 => })
            // format: on
          )
        } {
          decl in {
            addCasts {
              s"""
              |struct s {
              |  void function() {
              |    $decl;
              |  }
              |}"""
            } getFirst {
              case DefnVar(_, Some(i)) => i
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
            addCasts {
              s"""
              |struct s {
              |  void function() {
              |    $assign;
              |  }
              |}"""
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
            addCasts {
              s"""
               |fsm f {
               |  out sync bool a;
               |  out sync u10 b;
               |  out sync i20 c;
               |  void main() {
               |    $call;
               |    fence;
               |  }
               |}"""
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
            addCasts {
              s"""
              |struct s {
              |  $kind f() {
              |    return $value;
              |  }
              |}"""
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
}
