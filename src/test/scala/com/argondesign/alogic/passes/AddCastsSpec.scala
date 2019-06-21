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
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees.Expr.ImplicitConversions._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.typer.Typer
import org.scalatest.FreeSpec

final class AddCastsSpec extends FreeSpec with AlogicTest {

  implicit val cc = new CompilerContext
  cc.postSpecialization = true

  val namer = new Namer
  val typer = new Typer
  val addImplicitCasts = new AddCasts

  def xform(tree: Tree) = {
    tree match {
      case Root(_, entity: EntityIdent) => cc.addGlobalEntity(entity)
      case entity: EntityIdent          => cc.addGlobalEntity(entity)
      case _                            =>
    }
    val node = tree rewrite namer match {
      case Root(_, entity) => entity
      case other           => other
    }
    node rewrite typer rewrite addImplicitCasts
  }

  "AddImplicitCasts should automatically insert casts" - {
    "to infer sizes of unsized literals in" - {
      "binary operator operands" - {
        for (op <- List("*", "/", "%", "+", "-", "&", "|", "^", ">", ">=", "<", "<=", "==", "!=")) {
          for {
            (text, res) <- List(
              (s"8'd3 ${op} 2", Some(Right(ExprCast(TypeUInt(8), 2)))),
              (s"2 ${op} 8'd3", Some(Left(ExprCast(TypeUInt(8), 2)))),
              (s"8'd3 ${op} -2s", Some(Right(ExprCast(TypeSInt(8), ExprNum(true, -2))))),
              (s"-2s ${op} 8'd3", Some(Left(ExprCast(TypeSInt(8), ExprNum(true, -2))))),
              (s"7'sd3 ${op} 2", Some(Right(ExprCast(TypeUInt(7), 2)))),
              (s"2 ${op} 7'sd3", Some(Left(ExprCast(TypeUInt(7), 2)))),
              (s"7'sd3 ${op} -2s", Some(Right(ExprCast(TypeSInt(7), ExprNum(true, -2))))),
              (s"-2s ${op} 7'sd3", Some(Left(ExprCast(TypeSInt(7), ExprNum(true, -2))))),
              (s"4 ${op} 2 ", None)
            )
          } {
            text in {
              val expr = xform(text.asTree[Expr])
              val result @ ExprBinary(lhs, _, rhs) = expr
              res match {
                case Some(Left(l))  => lhs shouldBe l
                case Some(Right(r)) => rhs shouldBe r
                case None           => result shouldBe expr
              }
              cc.messages shouldBe empty
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
            val expr = xform(text.asTree[Expr])
            val result @ ExprTernary(_, lhs, rhs) = expr

            res match {
              case Some(Left(l))  => lhs shouldBe l
              case Some(Right(r)) => rhs shouldBe r
              case None           => result shouldBe expr
            }
            cc.messages shouldBe empty
          }
        }
      }

      "index expressions" - {
        def check(expr: Expr, expected: List[Expr]): Unit = {
          val ExprIndex(e, i) = expr
          val v :: vs = expected
          i shouldBe v
          if (vs.nonEmpty) check(e, vs)
        }

        for {
          (index, res) <- List(
            // format: off
            ("u8 a; (* unused *) u1 b = a[0]", List(ExprCast(TypeUInt(3), 0))),
            ("u9 a; (* unused *) u1 b = a[0]", List(ExprCast(TypeUInt(4), 0))),
            ("u32[8] a; (* unused *) u32 b = a[0]", List(ExprCast(TypeUInt(3), 0))),
            ("u33[9] a; (* unused *) u33 b = a[0]", List(ExprCast(TypeUInt(4), 0))),
            ("u32[8] a; (* unused *) u1 b = a[0][2]", List(ExprCast(TypeUInt(5), 2), ExprCast(TypeUInt(3), 0))),
            ("u33[9] a; (* unused *) u1 b = a[0][2]", List(ExprCast(TypeUInt(6), 2), ExprCast(TypeUInt(4), 0)))
            // format: on
          )
        } {
          index in {
            val entity = s"""|fsm f {
                             |  void main() {
                             |    ${index};
                             |    fence;
                             |  }
                             |}""".stripMargin.asTree[Entity]
            val tree = xform(entity)
            val expr = tree getFirst { case Decl(_, Some(i)) => i }
            check(expr, res)
            cc.messages shouldBe empty
          }
        }
      }

      "slice expressions" - {
        def check(expr: Expr, expected: List[(Expr, Expr)]): Unit = {
          val ExprSlice(e, l, _, r) = expr
          val (vl, vr) :: vs = expected
          l shouldBe vl
          r shouldBe vr
          if (vs.nonEmpty) check(e, vs)
        }

        for {
          (slice, res) <- List(
            // format: off
            ("u8 a; (* unused *) u2 b = a[1:0]", List((ExprCast(TypeUInt(3), 1), ExprCast(TypeUInt(3), 0)))),
            ("u9 a; (* unused *) u2 b = a[1:0]", List((ExprCast(TypeUInt(4), 1), ExprCast(TypeUInt(4), 0)))),
            ("u8 a; (* unused *) u1 b = a[2+:1]", List((ExprCast(TypeUInt(3), 2), ExprCast(TypeUInt(3), 1)))),
            ("u9 a; (* unused *) u1 b = a[2+:1]", List((ExprCast(TypeUInt(4), 2), ExprCast(TypeUInt(4), 1)))),
            ("u8 a; (* unused *) u1 b = a[2-:1]", List((ExprCast(TypeUInt(3), 2), ExprCast(TypeUInt(3), 1)))),
            ("u9 a; (* unused *) u1 b = a[2-:1]", List((ExprCast(TypeUInt(4), 2), ExprCast(TypeUInt(4), 1)))),
            ("u32[8] a; (* unused *) u2 b = a[1:0]", List((ExprCast(TypeUInt(3), 1), ExprCast(TypeUInt(3), 0)))),
            ("u33[9] a; (* unused *) u2 b = a[1:0]", List((ExprCast(TypeUInt(4), 1), ExprCast(TypeUInt(4), 0)))),
            ("u32[8] a; (* unused *) u1 b = a[2+:1]", List((ExprCast(TypeUInt(3), 2), ExprCast(TypeUInt(3), 1)))),
            ("u33[9] a; (* unused *) u1 b = a[2+:1]", List((ExprCast(TypeUInt(4), 2), ExprCast(TypeUInt(4), 1)))),
            ("u32[8] a; (* unused *) u1 b = a[2-:1]", List((ExprCast(TypeUInt(3), 2), ExprCast(TypeUInt(3), 1)))),
            ("u33[9] a; (* unused *) u1 b = a[2-:1]", List((ExprCast(TypeUInt(4), 2), ExprCast(TypeUInt(4), 1)))),
            // format: on
          )
        } {
          slice in {
            val entity = s"""|fsm f {
                             |  void main() {
                             |    ${slice};
                             |    fence;
                             |  }
                             |}""".stripMargin.asTree[Entity]
            val tree = xform(entity)
            val expr = tree getFirst { case Decl(_, Some(i)) => i }
            check(expr, res)
            cc.messages shouldBe empty
          }
        }
      }

      "initializer expressions" - {
        for {
          (decl, res) <- List(
            ("(* unused *) i8 a = 2s", ExprCast(TypeSInt(8), ExprNum(true, 2))),
            ("(* unused *) u8 a = 2s", ExprCast(TypeSInt(8), ExprNum(true, 2))),
            ("(* unused *) i7 a = 2", ExprCast(TypeUInt(7), ExprNum(false, 2))),
            ("(* unused *) u7 a = 2", ExprCast(TypeUInt(7), ExprNum(false, 2))),
            ("(* unused *) param int a = '8'sd2", ExprCast(TypeNum(true), ExprInt(true, 8, 2))),
            ("(* unused *) param uint a = '8'sd2", ExprCast(TypeNum(true), ExprInt(true, 8, 2))),
            ("(* unused *) param int a = '8'd2", ExprCast(TypeNum(false), ExprInt(false, 8, 2))),
            ("(* unused *) param uint a = '8'd2", ExprCast(TypeNum(false), ExprInt(false, 8, 2))),
            ("(* unused *) const int a = '8'sd2", ExprCast(TypeNum(true), ExprInt(true, 8, 2))),
            ("(* unused *) const uint a = '8'sd2", ExprCast(TypeNum(true), ExprInt(true, 8, 2))),
            ("(* unused *) const int a = '8'd2", ExprCast(TypeNum(false), ExprInt(false, 8, 2))),
            ("(* unused *) const uint a = '8'd2", ExprCast(TypeNum(false), ExprInt(false, 8, 2)))
          )
        } {
          decl in {
            val entity = s"""|fsm f {
                             |  void main() {
                             |    ${decl}; 
                             |    fence;
                             |  }
                             |}""".stripMargin.asTree[Entity]
            val tree = xform(entity)
            val init = tree getFirst { case Decl(_, Some(i)) => i }
            cc.messages foreach println
            init shouldBe res
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
            val entity = s"""|fsm f {
                             |  void main() {
                             |    ${assign};
                             |    fence;
                             |  }
                             |}""".stripMargin.asTree[Entity]
            val tree = xform(entity)
            val rhs = tree getFirst {
              case StmtAssign(_, rhs)    => rhs
              case StmtUpdate(_, _, rhs) => rhs
            }
            rhs shouldBe res
            cc.messages shouldBe empty
          }
        }
      }

      // TODO: function arguments
    }
  }
}
