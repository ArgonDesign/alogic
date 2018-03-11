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
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.core.Warning
import org.scalatest.FreeSpec

final class TyperSpec extends FreeSpec with AlogicTest {

  implicit val cc = new CompilerContext
  val namer = new Namer
  val desugar = new Desugar
  val typer = new Typer

  def xform(tree: Tree) = {
    tree match {
      case root: Root     => cc.addGlobalEntity(root.entity)
      case entity: Entity => cc.addGlobalEntity(entity)
      case _              =>
    }
    tree rewrite namer rewrite desugar rewrite typer
  }

  "The Typer should" - {
    "infer sizes of unsized literals for" - {
      "binary operator operands" - {
        for (op <- List("*", "/", "%", "+", "-", "&", "|", "^", "~^")) {
          val qop = Pattern.quote(op)
          for {
            (expr, result, msg) <- List(
              (s"8'd3 ${op} 2", ExprBinary(ExprInt(false, 8, 3), op, ExprInt(true, 8, 2)), ""),
              (s"2 ${op} 8'd3", ExprBinary(ExprInt(true, 8, 2), op, ExprInt(false, 8, 3)), ""),
              (s"{N{1'b1}} ${op} 2",
               ExprError(),
               s"Cannot infer width of right hand operand of '${qop}'"),
              (s"2 ${op} {N{1'b1}}",
               ExprError(),
               s"Cannot infer width of left hand operand of '${qop}'"),
              (s"bool ${op} 2",
               ExprError(),
               s"'${qop}' expects packed value on the left hand side"),
              (s"2 ${op} bool",
               ExprError(),
               s"'${qop}' expects packed value on the right hand side")
            )
          } {
            val text = expr.trim.replaceAll(" +", " ")
            text in {
              xform(text.asTree[Expr]) shouldBe result
              if (msg.isEmpty) {
                cc.messages shouldBe empty
              } else {
                cc.messages.last should beThe[Error](msg)
              }
            }
          }
        }
      }
    }

    "type check" - {
      "binary operators" - {
        for (op <- List("*", "/", "%", "+", "-", "&", "|", "^", "~^")) {
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
    }

    "warn mismatching operand widths" - {
      "binary operators" - {
        for (op <- List("*", "/", "%", "+", "-", "&", "|", "^", "~^")) {
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
