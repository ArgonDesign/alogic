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

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.Expr._
import com.argondesign.alogic.ast.Trees.Expr.ImplicitConversions._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.core.Types._

import org.scalatest.FreeSpec

import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.core.Warning
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.StorageTypes.StorageTypeReg

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
        for {
          (expr, result, msg) <- List(
            ("8'd3 * 2", ExprInt(false, 8, 3) * ExprInt(true, 8, 2), ""),
            ("2 * 8'd3", ExprInt(true, 8, 2) * ExprInt(false, 8, 3), ""),
            ("8'd3 / 2", ExprInt(false, 8, 3) / ExprInt(true, 8, 2), ""),
            ("2 / 8'd3", ExprInt(true, 8, 2) / ExprInt(false, 8, 3), ""),
            ("8'd3 % 2", ExprInt(false, 8, 3) % ExprInt(true, 8, 2), ""),
            ("2 % 8'd3", ExprInt(true, 8, 2) % ExprInt(false, 8, 3), ""),
            ("8'd3 + 2", ExprInt(false, 8, 3) + ExprInt(true, 8, 2), ""),
            ("2 + 8'd3", ExprInt(true, 8, 2) + ExprInt(false, 8, 3), ""),
            ("8'd3 - 2", ExprInt(false, 8, 3) - ExprInt(true, 8, 2), ""),
            ("2 - 8'd3", ExprInt(true, 8, 2) - ExprInt(false, 8, 3), ""),
            ("8'd3 & 2", ExprInt(false, 8, 3) & ExprInt(true, 8, 2), ""),
            ("2 & 8'd3", ExprInt(true, 8, 2) & ExprInt(false, 8, 3), ""),
            ("8'd3 | 2", ExprInt(false, 8, 3) | ExprInt(true, 8, 2), ""),
            ("2 | 8'd3", ExprInt(true, 8, 2) | ExprInt(false, 8, 3), ""),
            ("8'd3 ^ 2", ExprInt(false, 8, 3) ^ ExprInt(true, 8, 2), ""),
            ("2 ^ 8'd3", ExprInt(true, 8, 2) ^ ExprInt(false, 8, 3), ""),
            ("8'd3 ~^ 2", ExprInt(false, 8, 3) ~^ ExprInt(true, 8, 2), ""),
            ("2 ~^ 8'd3", ExprInt(true, 8, 2) ~^ ExprInt(false, 8, 3), "")
          )
        } {
          val e = expr.trim.replaceAll(" +", " ")
          e in {
            xform(e.asTree[Expr]) shouldBe result
            if (msg.isEmpty) {
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }
        }

      }

      //      "where possible" - {
      //        "binary ops" - {
      //          def check(str: String, eTree: Tree, eType: Type) = {
      //            str in {
      //              val tree = xform(str.asTree[Expr])
      //              tree shouldBe eTree
      //              tree.tpe shouldBe eType
      //              cc.messages shouldBe empty
      //            }
      //          }
      //
      //          for (op <- List("+", "-", "*", "/", "%", "&", "|", "^", "~^")) {
      //            check(s"8'd3 ${op} 2", ))
      //            check(s"2 ${op} 8'd3", ExprBinary(ExprInt(true, 8, 2), op, ExprInt(false, 8, 3)), TypeUInt(8))
      //          }
      //
      //          for (op <- List(">", ">=", "<", "<=", "==", "!=")) {
      //            check(s"8'd3 ${op} 2", ExprBinary(ExprInt(false, 8, 3), op, ExprInt(true, 8, 2)), TypeUInt(1))
      //            check(s"2 ${op} 8'd3", ExprBinary(ExprInt(true, 8, 2), op, ExprInt(false, 8, 3)), TypeUInt(1))
      //          }
      //
      //          for (op <- List("||", "&&")) {
      //            check(s"8'd3 ${op} 2", ExprBinary(ExprInt(false, 8, 3), op, ExprInt(true, 3, 2)), TypeUInt(1))
      //            check(s"2 ${op} 8'd3", ExprBinary(ExprInt(true, 3, 2), op, ExprInt(false, 2, 3)), TypeUInt(1))
      //            check(s"8'd3 ${op} 'd2", ExprBinary(ExprInt(false, 8, 3), op, ExprInt(false, 2, 2)), TypeUInt(1))
      //            check(s"'d2 ${op} 8'd3", ExprBinary(ExprInt(false, 2, 2), op, ExprInt(false, 8, 3)), TypeUInt(1))
      //          }
      //
      //          for (op <- List("<<", ">>", "<<<", ">>>")) {
      //            check(s"8'd3 ${op} 2", ExprBinary(ExprInt(false, 8, 3), op, ExprInt(true, 3, 2)), TypeUInt(8))
      //            check(s"8'd3 ${op} 'd2", ExprBinary(ExprInt(false, 8, 3), op, ExprInt(false, 2, 2)), TypeUInt(8))
      //          }
      //        }
      //      }
      //      "error where not possible" - {
      //        def check(str: String) = {
      //          str in {
      //            val tree = xform(str.asTree[Expr])
      //            tree shouldBe ExprError()
      //            cc.messages.loneElement should beThe[Error]("Cannot infer width of usized constant .*")
      //          }
      //        }
      //
      //        for (op <- List("<<", ">>", "<<<", ">>>")) {
      //          check(s"2 ${op} 8'd3")
      //          check(s"'d2 ${op} 8'd3")
      //        }
      //      }
    }

  }
}
