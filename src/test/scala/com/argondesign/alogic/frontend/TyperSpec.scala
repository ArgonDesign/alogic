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
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.Names._

import org.scalatest.FreeSpec

import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.core.Warning
import com.argondesign.alogic.core.Symbols.ErrorSymbol
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.StorageTypes.StorageTypeReg
import com.argondesign.alogic.frontend.Namer

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
    "assign correct types to" - {
      "identifiers" - {
        for {
          (name, decl, kind) <- List(
            ("bool", "bool a;", TypeUInt(1)),
            ("u8", "u8 a;", TypeUInt(8)),
            ("i1", "i1 a;", TypeSInt(1)),
            ("i8", "i8 a;", TypeSInt(8)),
            ("struct", "s a;", TypeStruct(List("b", "c"), List(TypeUInt(1), TypeSInt(8)))),
            ("typedef", "t a;", TypeUInt(4)),
            ("uint(2,8)", "uint(2,8) a;", TypeVector(TypeUInt(8), 2)),
            ("u8[2]", "u8 a[2];", TypeArray(TypeUInt(8), 2)),
            ("u8[2][4]", "u8 a[2][4];", TypeArray(TypeArray(TypeUInt(8), 4), 2)),
            ("param u8 ", "param u8 a = 8'd2;", TypeUInt(8)),
            ("const u8 ", "const u8 a = 8'd2;", TypeUInt(8)),
            ("pipeline u8 ", "pipeline u8 a;", TypeUInt(8)),
            ("in u8 ", "in u8 a;", TypeIn(TypeUInt(8), FlowControlTypeNone)),
            ("out u8 ", "out u8 a;", TypeOut(TypeUInt(8), FlowControlTypeNone, StorageTypeReg)),
            ("function", "void a() {}", TypeFunc(Nil, TypeVoid))
          )
        } {
          name in {
            val root = s"""|typedef u4 t;
                           |
                           |struct s {
                           |  bool b;
                           |  i8 c;
                           |};
                           |
                           |fsm thing {
                           |  ${decl}
                           |  void main() {
                           |    a;
                           |  }
                           |}""".stripMargin.asTree[Root]
            val tree = xform(root)

            inside(tree) {
              case entity: Entity => {
                val Some(main) = entity.functions collectFirst {
                  case func @ Function(Sym(sym), _) if sym.denot.name.str == "main" => func
                }
                inside(main) {
                  case Function(_, List(StmtExpr(expr))) =>
                    expr should matchPattern { case ExprRef(Sym(_)) => }
                    expr.tpe shouldBe kind
                }
              }
            }
          }
        }
      }
    }

  }
}
