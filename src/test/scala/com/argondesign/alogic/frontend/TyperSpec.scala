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

package com.argondesign.alogic.frontend

import java.util.regex.Pattern

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees.Expr.ImplicitConversions._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.core.Warning
import com.argondesign.alogic.transform.ConstantFold
import org.scalatest.FreeSpec

final class TyperSpec extends FreeSpec with AlogicTest {

  implicit val cc = new CompilerContext
  val namer = new Namer
  val desugar = new Desugar
  val constantFold = new ConstantFold
  val typer = new Typer

  def xform(tree: Tree) = {
    tree match {
      case root: Root     => cc.addGlobalEntity(root.entity)
      case entity: Entity => cc.addGlobalEntity(entity)
      case _              =>
    }
    tree rewrite namer rewrite desugar rewrite constantFold rewrite typer
  }

  "The Typer should" - {
    "infer sizes of unsized literals for" - {
      "binary operator operands" - {
        for (op <- List("*", "/", "%", "+", "-", "&", "|", "^")) {
          val qop = Pattern.quote(op)
          for {
            (expr, resultWidth, msg) <- List(
              (s"8'd3 ${op} 2", Some(8), ""),
              (s"2 ${op} 8'd3", Some(8), ""),
              (s"8'd3 ${op} 2 ${op} 4", Some(8), ""),
              (s"4 ${op} 2 ${op} 8'd3", Some(8), ""),
              (s"{N{1'b1}} ${op} 2", None, s"Cannot infer width of right hand operand of '${qop}'"),
              (s"2 ${op} {N{1'b1}}", None, s"Cannot infer width of left hand operand of '${qop}'"),
              (s"bool ${op} 2", None, s"'${qop}' expects packed value on the left hand side"),
              (s"2 ${op} bool", None, s"'${qop}' expects packed value on the right hand side")
            )
          } {
            val text = expr.trim.replaceAll(" +", " ")
            text in {
              val tree = xform(text.asTree[Expr])
              if (resultWidth.isDefined) {
                cc.messages foreach println
                tree.tpe.width.value.value shouldBe resultWidth.value
                cc.messages shouldBe empty
              } else {
                tree shouldBe ExprError()
                cc.messages.last should beThe[Error](msg)
              }
            }
          }
        }
      }
    }

    "type check" - {
      "binary operators" - {
        for (op <- List("*", "/", "%", "+", "-", "&", "|", "^")) {
          val qop = Pattern.quote(op)
          for {
            (expr, pattern, msg) <- List[(String, PartialFunction[Any, Unit], String)](
              (s"8'd3 ${op} 8'd2", { case ExprBinary(_, `op`, _) => }, ""),
              (s"bool ${op} 8'd2", { case _: ExprError           => },
               s"'${qop}' expects packed value on the left hand side"),
              (s"8'd3 ${op} bool", { case _: ExprError => },
               s"'${qop}' expects packed value on the right hand side")
            )
          } {
            val text = expr.trim.replaceAll(" +", " ")
            text in {
              xform(text.asTree[Expr]) should matchPattern(pattern)
              if (msg.isEmpty) {
                cc.messages shouldBe empty
              } else {
                cc.messages.last should beThe[Error](msg)
              }
            }
          }
        }
      }

      "select" - {
        for {
          (text, kind, msg) <- List(
            ("d.x", TypeSInt(8), ""),
            ("e.y", TypeStruct(List("x"), List(TypeSInt(8))), ""),
            ("e.y.x", TypeSInt(8), ""),
            ("d.z", TypeError, "No field named 'z' in '.*'"),
            ("e.z", TypeError, "No field named 'z' in '.*'"),
            ("e.y.z", TypeError, "No field named 'z' in '.*'"),
            ("f.x", TypeSInt(8), ""),
            ("g.y", TypeStruct(List("x"), List(TypeSInt(8))), ""),
            ("g.y.x", TypeSInt(8), ""),
            ("f.valid", TypeCombFunc(Nil, TypeUInt(1)), ""),
            ("g.valid", TypeCombFunc(Nil, TypeUInt(1)), ""),
//            ("@bits(d.x)", TypeSInt(8), ""),
//            ("@bits(e.y)", TypeStruct(List("x"), List(TypeSInt(8))), ""),
//            ("@bits(e.y.x)", TypeSInt(8), ""),
//            ("@bits(f.x)", TypeSInt(8), ""),
//            ("@bits(g.y)", TypeStruct(List("x"), List(TypeSInt(8))), ""),
//            ("@bits(g.y.x)", TypeSInt(8), ""),
//            ("@bits(a.x)", TypeType(TypeSInt(8)), ""),
//            ("@bits(b.y)", TypeType(TypeStruct(List("x"), List(TypeSInt(8)))), ""),
//            ("@bits(b.y.x)", TypeType(TypeSInt(8)), "")
          )
        } {
          text in {
            val root = s"""|struct a {
                           |  i8 x;
                           |};
                           |
                           |struct b {
                           |  a y;
                           |};
                           |
                           |fsm c {
                           |  a d;
                           |  b e;
                           |  in sync a f;
                           |  out sync b g;
                           |  void main() {
                           |    d; e; f; g;
                           |    ${text};
                           |  }
                           |}""".stripMargin.asTree[Root]
            val tree = xform(root)
            if (msg.isEmpty) {
              val expr = (tree collectFirst { case e: ExprSelect => e }).value
              expr.tpe shouldBe kind
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }
        }
      }
    }

    "warn mismatching operand widths" - {
      "binary operators" - {
        for (op <- List("*", "/", "%", "+", "-", "&", "|", "^")) {
          val qop = Pattern.quote(op)
          val text = s"8'd1 ${op} 7'd0"
          text in {
            xform(text.asTree[Expr]) should matchPattern {
              case ExprBinary(_, `op`, _) =>
            }
            cc.messages.loneElement should beThe[Warning](
              s"'${qop}' expects both operands to have the same width, but",
              "left  operand is 8 bits wide, and",
              "right operand is 7 bits wide"
            )
          }

        }
      }
    }

  }
}
