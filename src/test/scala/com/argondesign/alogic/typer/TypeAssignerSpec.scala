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

package com.argondesign.alogic.typer

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees.Expr.ImplicitConversions.int2ExprNum
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Symbols.ErrorSymbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.passes.Namer
import org.scalatest.FreeSpec

final class TypeAssignerSpec extends FreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext
  lazy val namer = new Namer

  private def xform(tree: Tree) = {
    tree match {
      case root: Root     => cc.addGlobalEntity(root.entity)
      case entity: Entity => cc.addGlobalEntity(entity)
      case _              =>
    }
    tree rewrite namer
  }

  "The TypeAssigner should assign correct types to" - {
    "expressions" - {
      "names" - {
        "terms" - {
          for {
            (name, decl, kind) <- List(
              ("bool", "bool a;", TypeUInt(1)),
              ("u8", "u8 a;", TypeUInt(8)),
              ("i1", "i1 a;", TypeSInt(1)),
              ("i8", "i8 a;", TypeSInt(8)),
              ("struct", "s a;", TypeStruct("s", List("b", "c"), List(TypeUInt(1), TypeSInt(8)))),
              ("typedef", "t a;", TypeUInt(4)),
              ("u8[2]", "u8[2] a;", TypeVector(TypeUInt(8), 2)),
              ("memory u8[2]", "u8 a[2];", TypeArray(TypeUInt(8), 2)),
              ("param u8", " param u8 a = 8'd2;", TypeUInt(8)),
              ("const u8", " const u8 a = 8'd2;", TypeUInt(8)),
              ("pipeline u8", "pipeline u8 a;", TypeUInt(8)),
              ("in u8", "in u8 a;", TypeIn(TypeUInt(8), FlowControlTypeNone)),
              ("out u8",
               "out u8 a;",
               TypeOut(TypeUInt(8), FlowControlTypeNone, StorageTypeDefault)),
              ("function", "void a() {}", TypeCtrlFunc(Nil, TypeVoid))
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
                case Root(_, entity) => {
                  val Some(main) = entity.functions collectFirst {
                    case func @ Function(Sym(sym), _) if sym.name == "main" =>
                      func
                  }
                  inside(main) {
                    case Function(_, List(StmtExpr(expr))) =>
                      expr should matchPattern { case ExprRef(_) => }
                      TypeAssigner(expr).tpe shouldBe kind
                  }
                }
              }
            }
          }
        }

        "types" - {
          for {
            (expr, kind) <- List(
              ("bool", TypeUInt(1)),
              ("u2", TypeUInt(2)),
              ("uint(3)", TypeUInt(3)),
              ("uint(3)[6]", TypeVector(TypeUInt(3), 6)),
              ("i2", TypeSInt(2)),
              ("int(3)", TypeSInt(3)),
              ("int(3)[6]", TypeVector(TypeSInt(3), 6)),
              ("void", TypeVoid),
              ("t /* typedef */", TypeUInt(4)),
              ("s /* struct */", TypeStruct("s", List("b", "c"), List(TypeUInt(1), TypeSInt(8))))
            )
          } {
            expr in {
              val root = s"""|typedef u4 t;
                             |
                             |struct s {
                             |  bool b;
                             |  i8 c;
                             |};
                             |
                             |fsm thing {
                             |  void main() {
                             |    @bits(${expr});
                             |  }
                             |}""".stripMargin.asTree[Root]

              val tree = xform(root)

              val result = (tree collectFirst {
                case Function(_, List(StmtExpr(e))) => e
              }).value
              val ExprCall(_, List(arg)) = result

              TypeAssigner(arg).tpe shouldBe TypeType(kind)
            }
          }
        }

      }

      "unary operators" - {
        for {
          (expr, kind) <- List(
            ("+(32'd1)", TypeUInt(32)),
            ("-(32'd1)", TypeUInt(32)),
            ("~(32'd1)", TypeUInt(32)),
            ("!(32'd1)", TypeUInt(1)),
            ("&(32'd1)", TypeUInt(1)),
            ("|(32'd1)", TypeUInt(1)),
            ("^(32'd1)", TypeUInt(1)),
            ("+(32'sd1)", TypeSInt(32)),
            ("-(32'sd1)", TypeSInt(32)),
            ("~(32'sd1)", TypeSInt(32)),
            ("!(32'sd1)", TypeUInt(1)),
            ("&(32'sd1)", TypeUInt(1)),
            ("|(32'sd1)", TypeUInt(1)),
            ("^(32'sd1)", TypeUInt(1)),
            // unsized
            ("+('d1)", TypeNum(false)),
            ("-('d1)", TypeNum(false)),
            ("~('d1)", TypeNum(false)),
            ("!('d1)", TypeUInt(1)),
            ("&('d1)", TypeUInt(1)),
            ("|('d1)", TypeUInt(1)),
            ("^('d1)", TypeUInt(1)),
            ("+('sd1)", TypeNum(true)),
            ("-('sd1)", TypeNum(true)),
            ("~('sd1)", TypeNum(true)),
            ("!('sd1)", TypeUInt(1)),
            ("&('sd1)", TypeUInt(1)),
            ("|('sd1)", TypeUInt(1)),
            ("^('sd1)", TypeUInt(1))
          )
        } {
          val text = expr.trim.replaceAll(" +", " ")
          text in {
            val expr @ ExprUnary(_, operand) = text.asTree[Expr]
            TypeAssigner(operand)
            TypeAssigner(expr).tpe shouldBe kind
            cc.messages shouldBe empty
          }
        }
      }

      "binary operators" - {
        for {
          (expr, kind) <- List(
            // unsigned unsigned
            ("32'd1 *   32'd1", TypeUInt(32)),
            ("32'd1 /   32'd1", TypeUInt(32)),
            ("32'd1 %   32'd1", TypeUInt(32)),
            ("32'd1 +   32'd1", TypeUInt(32)),
            ("32'd1 -   32'd1", TypeUInt(32)),
            ("32'd1 <<  32'd1", TypeUInt(32)),
            ("32'd1 >>  32'd1", TypeUInt(32)),
            ("32'd1 >>> 32'd1", TypeUInt(32)),
            ("32'd1 <<< 32'd1", TypeUInt(32)),
            ("32'd1 >   32'd1", TypeUInt(1)),
            ("32'd1 >=  32'd1", TypeUInt(1)),
            ("32'd1 <   32'd1", TypeUInt(1)),
            ("32'd1 <=  32'd1", TypeUInt(1)),
            ("32'd1 ==  32'd1", TypeUInt(1)),
            ("32'd1 !=  32'd1", TypeUInt(1)),
            ("32'd1 &   32'd1", TypeUInt(32)),
            ("32'd1 ^   32'd1", TypeUInt(32)),
            ("32'd1 |   32'd1", TypeUInt(32)),
            ("32'd1 &&  32'd1", TypeUInt(1)),
            ("32'd1 ||  32'd1", TypeUInt(1)),
            // signed unsigned
            ("32'd1 *   32'sd1", TypeUInt(32)),
            ("32'd1 /   32'sd1", TypeUInt(32)),
            ("32'd1 %   32'sd1", TypeUInt(32)),
            ("32'd1 +   32'sd1", TypeUInt(32)),
            ("32'd1 -   32'sd1", TypeUInt(32)),
            ("32'd1 <<  32'sd1", TypeUInt(32)),
            ("32'd1 >>  32'sd1", TypeUInt(32)),
            ("32'd1 >>> 32'sd1", TypeUInt(32)),
            ("32'd1 <<< 32'sd1", TypeUInt(32)),
            ("32'd1 >   32'sd1", TypeUInt(1)),
            ("32'd1 >=  32'sd1", TypeUInt(1)),
            ("32'd1 <   32'sd1", TypeUInt(1)),
            ("32'd1 <=  32'sd1", TypeUInt(1)),
            ("32'd1 ==  32'sd1", TypeUInt(1)),
            ("32'd1 !=  32'sd1", TypeUInt(1)),
            ("32'd1 &   32'sd1", TypeUInt(32)),
            ("32'd1 ^   32'sd1", TypeUInt(32)),
            ("32'd1 |   32'sd1", TypeUInt(32)),
            ("32'd1 &&  32'sd1", TypeUInt(1)),
            ("32'd1 ||  32'sd1", TypeUInt(1)),
            // signed unsigned
            ("32'sd1 *   32'd1", TypeUInt(32)),
            ("32'sd1 /   32'd1", TypeUInt(32)),
            ("32'sd1 %   32'd1", TypeUInt(32)),
            ("32'sd1 +   32'd1", TypeUInt(32)),
            ("32'sd1 -   32'd1", TypeUInt(32)),
            ("32'sd1 <<  32'd1", TypeSInt(32)),
            ("32'sd1 >>  32'd1", TypeSInt(32)),
            ("32'sd1 >>> 32'd1", TypeSInt(32)),
            ("32'sd1 <<< 32'd1", TypeSInt(32)),
            ("32'sd1 >   32'd1", TypeUInt(1)),
            ("32'sd1 >=  32'd1", TypeUInt(1)),
            ("32'sd1 <   32'd1", TypeUInt(1)),
            ("32'sd1 <=  32'd1", TypeUInt(1)),
            ("32'sd1 ==  32'd1", TypeUInt(1)),
            ("32'sd1 !=  32'd1", TypeUInt(1)),
            ("32'sd1 &   32'd1", TypeUInt(32)),
            ("32'sd1 ^   32'd1", TypeUInt(32)),
            ("32'sd1 |   32'd1", TypeUInt(32)),
            ("32'sd1 &&  32'd1", TypeUInt(1)),
            ("32'sd1 ||  32'd1", TypeUInt(1)),
            // signed signed
            ("32'sd1 *   32'sd1", TypeSInt(32)),
            ("32'sd1 /   32'sd1", TypeSInt(32)),
            ("32'sd1 %   32'sd1", TypeSInt(32)),
            ("32'sd1 +   32'sd1", TypeSInt(32)),
            ("32'sd1 -   32'sd1", TypeSInt(32)),
            ("32'sd1 <<  32'sd1", TypeSInt(32)),
            ("32'sd1 >>  32'sd1", TypeSInt(32)),
            ("32'sd1 >>> 32'sd1", TypeSInt(32)),
            ("32'sd1 <<< 32'sd1", TypeSInt(32)),
            ("32'sd1 >   32'sd1", TypeUInt(1)),
            ("32'sd1 >=  32'sd1", TypeUInt(1)),
            ("32'sd1 <   32'sd1", TypeUInt(1)),
            ("32'sd1 <=  32'sd1", TypeUInt(1)),
            ("32'sd1 ==  32'sd1", TypeUInt(1)),
            ("32'sd1 !=  32'sd1", TypeUInt(1)),
            ("32'sd1 &   32'sd1", TypeSInt(32)),
            ("32'sd1 ^   32'sd1", TypeSInt(32)),
            ("32'sd1 |   32'sd1", TypeSInt(32)),
            ("32'sd1 &&  32'sd1", TypeUInt(1)),
            ("32'sd1 ||  32'sd1", TypeUInt(1)),
            // Shifts
            ("3'd1 <<  32'd1", TypeUInt(3)),
            ("3'd1 >>  32'd1", TypeUInt(3)),
            ("3'd1 >>> 32'd1", TypeUInt(3)),
            ("3'd1 <<< 32'd1", TypeUInt(3)),
            ("3'd1 <<  32'sd1", TypeUInt(3)),
            ("3'd1 >>  32'sd1", TypeUInt(3)),
            ("3'd1 >>> 32'sd1", TypeUInt(3)),
            ("3'd1 <<< 32'sd1", TypeUInt(3)),
            ("3'sd1 <<  32'd1", TypeSInt(3)),
            ("3'sd1 >>  32'd1", TypeSInt(3)),
            ("3'sd1 >>> 32'd1", TypeSInt(3)),
            ("3'sd1 <<< 32'd1", TypeSInt(3)),
            ("3'sd1 <<  32'sd1", TypeSInt(3)),
            ("3'sd1 >>  32'sd1", TypeSInt(3)),
            ("3'sd1 >>> 32'sd1", TypeSInt(3)),
            ("3'sd1 <<< 32'sd1", TypeSInt(3)),
            // Widening
            (" 4'd1 *   6'd1", TypeUInt(6)),
            (" 4'd1 /   6'd1", TypeUInt(6)),
            (" 4'd1 %   6'd1", TypeUInt(6)),
            (" 4'd1 +   6'd1", TypeUInt(6)),
            (" 4'd1 -   6'd1", TypeUInt(6)),
            (" 4'd1 &   6'd1", TypeUInt(6)),
            (" 4'd1 ^   6'd1", TypeUInt(6)),
            (" 4'd1 |   6'd1", TypeUInt(6)),
            (" 4'd1 *  6'sd1", TypeUInt(6)),
            (" 4'd1 /  6'sd1", TypeUInt(6)),
            (" 4'd1 %  6'sd1", TypeUInt(6)),
            (" 4'd1 +  6'sd1", TypeUInt(6)),
            (" 4'd1 -  6'sd1", TypeUInt(6)),
            (" 4'd1 &  6'sd1", TypeUInt(6)),
            (" 4'd1 ^  6'sd1", TypeUInt(6)),
            (" 4'd1 |  6'sd1", TypeUInt(6)),
            ("4'sd1 *   6'd1", TypeUInt(6)),
            ("4'sd1 /   6'd1", TypeUInt(6)),
            ("4'sd1 %   6'd1", TypeUInt(6)),
            ("4'sd1 +   6'd1", TypeUInt(6)),
            ("4'sd1 -   6'd1", TypeUInt(6)),
            ("4'sd1 &   6'd1", TypeUInt(6)),
            ("4'sd1 ^   6'd1", TypeUInt(6)),
            ("4'sd1 |   6'd1", TypeUInt(6)),
            ("4'sd1 *  6'sd1", TypeSInt(6)),
            ("4'sd1 /  6'sd1", TypeSInt(6)),
            ("4'sd1 %  6'sd1", TypeSInt(6)),
            ("4'sd1 +  6'sd1", TypeSInt(6)),
            ("4'sd1 -  6'sd1", TypeSInt(6)),
            ("4'sd1 &  6'sd1", TypeSInt(6)),
            ("4'sd1 ^  6'sd1", TypeSInt(6)),
            ("4'sd1 |  6'sd1", TypeSInt(6)),
            // unsigned unsigned - unsized
            ("'d1 *   'd1", TypeNum(false)),
            ("'d1 /   'd1", TypeNum(false)),
            ("'d1 %   'd1", TypeNum(false)),
            ("'d1 +   'd1", TypeNum(false)),
            ("'d1 -   'd1", TypeNum(false)),
            ("'d1 &   'd1", TypeNum(false)),
            ("'d1 ^   'd1", TypeNum(false)),
            ("'d1 |   'd1", TypeNum(false)),
            ("'d1 <<  'd1", TypeNum(false)),
            ("'d1 >>  'd1", TypeNum(false)),
            ("'d1 >>> 'd1", TypeNum(false)),
            ("'d1 <<< 'd1", TypeNum(false)),
            ("'d1 >   'd1", TypeUInt(1)),
            ("'d1 >=  'd1", TypeUInt(1)),
            ("'d1 <   'd1", TypeUInt(1)),
            ("'d1 <=  'd1", TypeUInt(1)),
            ("'d1 ==  'd1", TypeUInt(1)),
            ("'d1 !=  'd1", TypeUInt(1)),
            ("'d1 &&  'd1", TypeUInt(1)),
            ("'d1 ||  'd1", TypeUInt(1)),
            // unsigned signed - unsized
            ("'d1 *   'sd1", TypeNum(false)),
            ("'d1 /   'sd1", TypeNum(false)),
            ("'d1 %   'sd1", TypeNum(false)),
            ("'d1 +   'sd1", TypeNum(false)),
            ("'d1 -   'sd1", TypeNum(false)),
            ("'d1 &   'sd1", TypeNum(false)),
            ("'d1 ^   'sd1", TypeNum(false)),
            ("'d1 |   'sd1", TypeNum(false)),
            ("'d1 <<  'sd1", TypeNum(false)),
            ("'d1 >>  'sd1", TypeNum(false)),
            ("'d1 >>> 'sd1", TypeNum(false)),
            ("'d1 <<< 'sd1", TypeNum(false)),
            ("'d1 >   'sd1", TypeUInt(1)),
            ("'d1 >=  'sd1", TypeUInt(1)),
            ("'d1 <   'sd1", TypeUInt(1)),
            ("'d1 <=  'sd1", TypeUInt(1)),
            ("'d1 ==  'sd1", TypeUInt(1)),
            ("'d1 !=  'sd1", TypeUInt(1)),
            ("'d1 &&  'sd1", TypeUInt(1)),
            ("'d1 ||  'sd1", TypeUInt(1)),
            // signed unsigned - unsized
            ("'sd1 *   'd1", TypeNum(false)),
            ("'sd1 /   'd1", TypeNum(false)),
            ("'sd1 %   'd1", TypeNum(false)),
            ("'sd1 +   'd1", TypeNum(false)),
            ("'sd1 -   'd1", TypeNum(false)),
            ("'sd1 &   'd1", TypeNum(false)),
            ("'sd1 ^   'd1", TypeNum(false)),
            ("'sd1 |   'd1", TypeNum(false)),
            ("'sd1 <<  'd1", TypeNum(true)),
            ("'sd1 >>  'd1", TypeNum(true)),
            ("'sd1 >>> 'd1", TypeNum(true)),
            ("'sd1 <<< 'd1", TypeNum(true)),
            ("'sd1 >   'd1", TypeUInt(1)),
            ("'sd1 >=  'd1", TypeUInt(1)),
            ("'sd1 <   'd1", TypeUInt(1)),
            ("'sd1 <=  'd1", TypeUInt(1)),
            ("'sd1 ==  'd1", TypeUInt(1)),
            ("'sd1 !=  'd1", TypeUInt(1)),
            ("'sd1 &&  'd1", TypeUInt(1)),
            ("'sd1 ||  'd1", TypeUInt(1)),
            // signed signed - unsized
            ("'sd1 *   'sd1", TypeNum(true)),
            ("'sd1 /   'sd1", TypeNum(true)),
            ("'sd1 %   'sd1", TypeNum(true)),
            ("'sd1 +   'sd1", TypeNum(true)),
            ("'sd1 -   'sd1", TypeNum(true)),
            ("'sd1 &   'sd1", TypeNum(true)),
            ("'sd1 ^   'sd1", TypeNum(true)),
            ("'sd1 |   'sd1", TypeNum(true)),
            ("'sd1 <<  'sd1", TypeNum(true)),
            ("'sd1 >>  'sd1", TypeNum(true)),
            ("'sd1 >>> 'sd1", TypeNum(true)),
            ("'sd1 <<< 'sd1", TypeNum(true)),
            ("'sd1 >   'sd1", TypeUInt(1)),
            ("'sd1 >=  'sd1", TypeUInt(1)),
            ("'sd1 <   'sd1", TypeUInt(1)),
            ("'sd1 <=  'sd1", TypeUInt(1)),
            ("'sd1 ==  'sd1", TypeUInt(1)),
            ("'sd1 !=  'sd1", TypeUInt(1)),
            ("'sd1 &&  'sd1", TypeUInt(1)),
            ("'sd1 ||  'sd1", TypeUInt(1))
          )
        } {
          val text = expr.trim.replaceAll(" +", " ")
          text in {
            val expr @ ExprBinary(lhs, _, rhs) = text.asTree[Expr]
            TypeAssigner(lhs)
            TypeAssigner(rhs)
            TypeAssigner(expr).tpe shouldBe kind
            cc.messages shouldBe empty
          }
        }
      }

      "ternary operator" - {
        for {
          (expr, kind) <- List(
            ("a[1:0]?  5'd2 :  5'd3", TypeUInt(5)),
            ("a[1:0]?  5'd2 : 5'sd3", TypeUInt(5)),
            ("a[1:0]? 5'sd2 :  5'd3", TypeUInt(5)),
            ("a[1:0]? 5'sd2 : 5'sd3", TypeSInt(5)),
            ("a[1:0]?  4'd2 :  2'd1", TypeUInt(4)),
            ("a[1:0]?  4'd2 : 2'sd1", TypeUInt(4)),
            ("a[1:0]? 4'sd2 :  2'd1", TypeUInt(4)),
            ("a[1:0]? 4'sd2 : 2'sd1", TypeSInt(4)),
            ("a[1:0]?  2'd1 :  3'd3", TypeUInt(3)),
            ("a[1:0]?  2'd1 : 3'sd3", TypeUInt(3)),
            ("a[1:0]? 2'sd1 :  3'd3", TypeUInt(3)),
            ("a[1:0]? 2'sd1 : 3'sd3", TypeSInt(3)),
            // Unsized
            ("a[1:0]?  'd2 :  'd3", TypeNum(false)),
            ("a[1:0]?  'd2 : 'sd3", TypeNum(false)),
            ("a[1:0]? 'sd2 :  'd3", TypeNum(false)),
            ("a[1:0]? 'sd2 : 'sd3", TypeNum(true))
          )
        } {
          val text = expr.trim.replaceAll(" +", " ")
          text in {
            val expr @ ExprTernary(cond, thenExpr, elseExpr) = text.asTree[Expr]
            TypeAssigner(cond)
            TypeAssigner(thenExpr)
            TypeAssigner(elseExpr)
            TypeAssigner(expr).tpe shouldBe kind
            cc.messages shouldBe empty
          }
        }
      }

      "cat" - {
        for {
          (expr, kind) <- List(
            ("{  3'd0,  4'd0 }", TypeUInt(7)),
            ("{  3'd0, 4'sd0 }", TypeUInt(7)),
            ("{ 3'sd0,  4'd0 }", TypeUInt(7)),
            ("{ 3'sd0, 4'sd0 }", TypeUInt(7))
          )
        } {
          val text = expr.trim.replaceAll(" +", " ")
          text in {
            val expr @ ExprCat(parts) = text.asTree[Expr]
            parts foreach {
              TypeAssigner(_)
            }
            TypeAssigner(expr).tpe shouldBe kind
            cc.messages shouldBe empty
          }
        }
      }

      "rep" - {
        for {
          (expr, kind) <- List(
            ("{1{4'd0}}", TypeUInt(4)),
            ("{2{4'd0}}", TypeUInt(8)),
            ("{3{4'd0}}", TypeUInt(12)),
            ("{1{4'sd0}}", TypeUInt(4)),
            ("{2{4'sd0}}", TypeUInt(8)),
            ("{3{4'sd0}}", TypeUInt(12))
          )
        } {
          val text = expr.trim.replaceAll(" +", " ")
          text in {
            val expr @ ExprRep(cound, operand) = text.asTree[Expr]
            TypeAssigner(operand)
            TypeAssigner(expr).tpe shouldBe kind
            cc.messages shouldBe empty
          }
        }
      }

      "index" - {
        for {
          (expr, kind) <- List(
            ("a[0]", TypeUInt(1)),
            ("a[1]", TypeUInt(1)),
            ("b[0]", TypeSInt(7)),
            ("b[1]", TypeSInt(7)),
            ("b[0][0]", TypeUInt(1)),
            ("b[0][1]", TypeUInt(1)),
            ("c[0]", TypeVector(TypeSInt(7), 2)),
            ("c[1]", TypeVector(TypeSInt(7), 2)),
            ("c[0][0]", TypeSInt(7)),
            ("c[0][1]", TypeSInt(7)),
            ("c[0][0][0]", TypeUInt(1)),
            ("c[0][0][1]", TypeUInt(1)),
            ("d[0]", TypeSInt(7)),
            ("d[1]", TypeSInt(7)),
            ("d[0][0]", TypeUInt(1)),
            ("d[0][1]", TypeUInt(1)),
            ("e[0]", TypeSInt(7)),
            ("e[1]", TypeSInt(7)),
            ("e[0][0]", TypeUInt(1)),
            ("e[0][1]", TypeUInt(1)),
            ("f[0]", TypeVector(TypeVector(TypeSInt(7), 2), 4)),
            ("f[1]", TypeVector(TypeVector(TypeSInt(7), 2), 4)),
            ("f[0][0]", TypeVector(TypeSInt(7), 2)),
            ("f[0][1]", TypeVector(TypeSInt(7), 2)),
            ("f[0][0][0]", TypeSInt(7)),
            ("f[0][0][1]", TypeSInt(7)),
            ("f[0][0][0][0]", TypeUInt(1)),
            ("f[0][0][0][1]", TypeUInt(1)),
            ("g[0]", TypeUInt(1)),
            ("g[1]", TypeUInt(1)),
            ("1[0]", TypeUInt(1)),
            ("1[1]", TypeUInt(1))
          )
        } {
          val text = expr.trim.replaceAll(" +", " ")
          text in {
            val block = s"""|{
                            |  (* unused *) i7 a;
                            |  (* unused *) i7[2] b;
                            |  (* unused *) i7[4][2] c;
                            |  (* unused *) i7 d[2];
                            |  (* unused *) i7 e[2];
                            |  (* unused *) i7[4][2] f[3];
                            |  (* unused *) in i7 g;
                            |
                            |  ${text};
                            |}""".stripMargin.asTree[Stmt]

            val tree = xform(block)

            tree.postOrderIterator collect { case expr: Expr => expr } foreach {
              TypeAssigner(_)
            }

            inside(tree) {
              case StmtBlock(stmts) =>
                inside(stmts.last) {
                  case StmtExpr(e) =>
                    e.tpe shouldBe kind
                }
            }
            cc.messages shouldBe empty
          }
        }
      }

      "slice" - {
        for {
          (expr, kind) <- List(
            ("a[3:2]", TypeUInt(2)),
            ("a[0:0]", TypeUInt(1)),
            ("a[4+:3]", TypeUInt(3)),
            ("a[4-:3]", TypeUInt(3)),
            ("1[3:0]", TypeUInt(4)),
            ("1[3+:2]", TypeUInt(2)),
            ("1[3-:2]", TypeUInt(2))
          )
        } {
          val text = expr.trim.replaceAll(" +", " ")
          text in {
            val expr = text.asTree[Expr]
            TypeAssigner(expr).tpe shouldBe kind
            cc.messages shouldBe empty
          }
        }
      }

      "select" - {
        for {
          (expr, kind) <- List(
            ("a.a", TypeSInt(6)),
            ("a.b", TypeStruct("s", List("a", "b"), List(TypeUInt(1), TypeUInt(8)))),
            ("a.b.a", TypeUInt(1)),
            ("a.b.b", TypeUInt(8)),
            ("pi0.valid", TypeUInt(1)),
            ("pi0.read", TypeCombFunc(Nil, TypeSInt(8))),
            ("pi0.wait", TypeCombFunc(Nil, TypeVoid)),
            ("pi1.valid", TypeUInt(1)),
            ("pi1.read", TypeCombFunc(Nil, TypeVoid)),
            ("pi1.wait", TypeCombFunc(Nil, TypeVoid)),
            ("po0.valid", TypeUInt(1)),
            ("po0.write", TypeCombFunc(List(TypeSInt(8)), TypeVoid)),
            ("po0.flush", TypeCombFunc(Nil, TypeVoid)),
            ("po0.full", TypeUInt(1)),
            ("po0.empty", TypeUInt(1)),
            ("po1.valid", TypeUInt(1)),
            ("po1.write", TypeCombFunc(Nil, TypeVoid)),
            ("po1.flush", TypeCombFunc(Nil, TypeVoid)),
            ("po1.full", TypeUInt(1)),
            ("po1.empty", TypeUInt(1))
          )
        } {
          val text = expr.trim.replaceAll(" +", " ")
          text in {
            val block = s"""|struct s {
                            |  bool a;
                            |  u8   b;
                            |};
                            |
                            |struct t {
                            |  i6  a;
                            |  s   b;
                            |};
                            |
                            |fsm a {
                            |  in  sync ready i8   pi0;
                            |  in  sync ready void pi1;
                            |  out sync ready i8   po0;
                            |  out sync ready void po1;
                            |
                            |  t a;
                            |
                            |  void main() {
                            |    pi0; pi1; po0; po1; a; // Suppress unused warnings
                            |
                            |    ${text};
                            |  }
                            |}""".stripMargin.asTree[Root]

            val tree = xform(block)

            tree.postOrderIterator collect {
              case expr: ExprRef    => expr
              case expr: ExprSelect => expr
            } foreach {
              TypeAssigner(_)
            }

            val expr = (tree collectFirst { case expr: ExprSelect => expr }).value

            expr.tpe shouldBe kind

            cc.messages shouldBe empty
          }
        }
      }

      "select from type" in {
        val tree = """|struct a {
                      | i8 b;
                      |};
                      |
                      |fsm c {
                      |  void main() {
                      |    @bits(a.b);
                      |  }
                      |}""".stripMargin.asTree[Root]
        val expr = (xform(tree) collectFirst { case e: ExprSelect => e }).value

        expr.postOrderIterator collect {
          case e: Expr => e
        } foreach {
          TypeAssigner(_)
        }

        expr.tpe shouldBe TypeType(TypeSInt(8))

        cc.messages shouldBe empty
      }

      "sized integer literals" - {
        for {
          (expr, kind) <- List(
            ("1'b0", TypeUInt(1)),
            ("2'd1", TypeUInt(2)),
            ("3'sd2", TypeSInt(3)),
            ("4'sd3", TypeSInt(4))
          )
        } {
          val text = expr.trim.replaceAll(" +", " ")
          text in {
            val expr = text.asTree[Expr]
            TypeAssigner(expr).tpe shouldBe kind
            cc.messages shouldBe empty
          }
        }
      }

      "unsized integer literals" - {
        for {
          (expr, kind) <- List(
            ("0", TypeNum(false)),
            ("1", TypeNum(false)),
            ("'sd0", TypeNum(true)),
            ("'sd1", TypeNum(true))
          )
        } {
          val text = expr.trim.replaceAll(" +", " ")
          text in {
            val expr = text.asTree[Expr]
            TypeAssigner(expr).tpe shouldBe kind
            cc.messages shouldBe empty
          }
        }
      }

      "string literals" - {
        for {
          (expr, kind) <- List(
            ("\"abc\"", TypeStr),
            ("\"\"", TypeStr)
          )
        } {
          val text = expr.trim.replaceAll(" +", " ")
          text in {
            val expr = text.asTree[Expr]
            TypeAssigner(expr).tpe shouldBe kind
            cc.messages shouldBe empty
          }
        }
      }

      "call" - {
        for {
          (expr, kind) <- List(
            ("pi0.read", TypeSInt(8)),
            ("pi0.wait", TypeVoid),
            ("pi1.read", TypeVoid),
            ("pi1.wait", TypeVoid),
            ("po0.write", TypeVoid),
            ("po0.flush", TypeVoid),
            ("po1.write", TypeVoid),
            ("po1.flush", TypeVoid)
          )
        } {
          val text = expr.trim.replaceAll(" +", " ")
          text in {
            val block = s"""|struct s {
                            |  bool a;
                            |  u8   b;
                            |};
                            |
                            |struct t {
                            |  i6  a;
                            |  s   b;
                            |};
                            |
                            |fsm a {
                            |  in  sync ready i8   pi0;
                            |  in  sync ready void pi1;
                            |  out sync ready i8   po0;
                            |  out sync ready void po1;
                            |
                            |  t a;
                            |
                            |  void main() {
                            |    pi0; pi1; po0; po1; a; // Suppress unused warnings
                            |
                            |    ${text}();
                            |  }
                            |}""".stripMargin.asTree[Root]

            val tree = xform(block)

            tree.postOrderIterator collect {
              case expr: ExprRef    => expr
              case expr: ExprSelect => expr
              case expr: ExprCall   => expr
            } foreach {
              TypeAssigner(_)
            }

            val expr = (tree collectFirst { case expr: ExprCall => expr }).value

            expr.tpe shouldBe kind

            cc.messages shouldBe empty
          }
        }
      }
    }

    "statements" - {
      "unambiguous comb statements" - {
        for {
          (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
            ("a = a + 1;", { case _: StmtAssign         => }),
            ("a++;", { case _: StmtPost                 => }),
            ("a += 1;", { case _: StmtUpdate            => }),
            ("bool c;", { case _: StmtDecl              => }),
            ("read;", { case _: StmtRead                => }),
            ("write;", { case _: StmtWrite              => }),
            ("$(\"foo\");", { case _: StmtDollarComment => })
          )
        } {
          text in {
            val tree = s"""|{
                           |  bool a;
                           |  a;
                           |
                           |  ${text}
                           |}""".stripMargin.asTree[Stmt]

            val StmtBlock(stmts) = xform(tree)

            inside(stmts.last) {
              case stmt: Stmt =>
                stmt should matchPattern(pattern)
                TypeAssigner(stmt).tpe shouldBe TypeCombStmt
            }

          }
        }
      }

      "unambiguous crtl statements" - {
        for {
          (text, pattern) <- List[(String, PartialFunction[Any, Unit])](
            ("goto a;", { case _: StmtGoto                                    => }),
            ("return;", { case _: StmtReturn                                  => }),
            ("fence;", { case _: StmtFence                                    => }),
            ("break;", { case _: StmtBreak                                    => }),
            ("for(;;) {}", { case _: StmtFor                                  => }),
            ("do {} while(1);", { case _: StmtDo                              => }),
            ("while (1) {}", { case _: StmtWhile                              => }),
            ("loop {}", { case _: StmtLoop                                    => }),
            ("let (bool b = 1) for(;;) {}", { case StmtLet(_, _: StmtFor)     => }),
            ("let (bool b = 1) do {} while(1);", { case StmtLet(_, _: StmtDo) => }),
            ("let (bool b = 1) while (1) {}", { case StmtLet(_, _: StmtWhile) => }),
            ("let (bool b = 1) loop {}", { case StmtLet(_, _: StmtLoop)       => })
          )
        } {
          text in {
            val tree = s"""|{
                           |  bool a;
                           |  a;
                           |
                           |  ${text}
                           |}""".stripMargin.asTree[Stmt]

            val StmtBlock(stmts) = xform(tree)

            inside(stmts.last) {
              case stmt: Stmt =>
                stmt should matchPattern(pattern)
                stmt.postOrderIterator foreach { case tree: Tree => TypeAssigner(tree) }
                stmt.tpe shouldBe TypeCtrlStmt
            }
          }
        }
      }

      "context dependent statements" - {
        for {
          (text, pattern, kind) <- List[(String, PartialFunction[Any, Unit], Type)](
            ("if(a) read;", { case StmtIf(_, _, None)                  => }, TypeCombStmt),
            ("if(a) read; else write;", { case StmtIf(_, _, Some(_))   => }, TypeCombStmt),
            ("if(a) fence;", { case StmtIf(_, _, None)                 => }, TypeCtrlStmt),
            ("if(a) fence; else return;", { case StmtIf(_, _, Some(_)) => }, TypeCtrlStmt),
            ("case(a) {a: read;}", { case _: StmtCase                  => }, TypeCombStmt),
            ("case(a) {default: read;}", { case _: StmtCase            => }, TypeCombStmt),
            ("case(a) {a: fence;}", { case _: StmtCase                 => }, TypeCtrlStmt),
            ("case(a) {default: fence;}", { case _: StmtCase           => }, TypeCtrlStmt),
            ("case(a) {default: {read; fence;}}", { case _: StmtCase   => }, TypeCtrlStmt),
            ("a;", { case StmtExpr(_: ExprRef)                         => }, TypeCombStmt),
            ("a + a;", { case StmtExpr(_: ExprBinary)                  => }, TypeCombStmt),
            ("a.read();", { case StmtExpr(_: ExprCall)                 => }, TypeCombStmt),
            ("main();", { case StmtExpr(_: ExprCall)                   => }, TypeCtrlStmt),
            ("{ }", { case _: StmtBlock                                => }, TypeCombStmt),
            ("{ a; fence; }", { case _: StmtBlock                      => }, TypeCtrlStmt),
            ("{ a; a; }", { case _: StmtBlock                          => }, TypeCombStmt)
          )
        } {
          text in {
            val entity = s"""|fsm a {
                             |  in sync bool a;
                             |
                             |  void main() {
                             |    a;
                             |    ${text}
                             |  }
                             |}""".stripMargin.asTree[Entity]

            val tree = xform(entity)

            val stmt = (tree collectFirst { case Function(_, stmts) => stmts.last }).value

            stmt.postOrderIterator collect {
              case node: Stmt       => node
              case node: CaseClause => node
              case node: Expr       => node
            } foreach {
              TypeAssigner(_)
            }

            inside(stmt) {
              case stmt: Stmt =>
                stmt should matchPattern(pattern)
                stmt.tpe shouldBe kind
            }

          }
        }
      }
    }

    "entity contents" - {
      for {
        (name, text, pattern) <- List[(String, String, PartialFunction[Any, Tree])](
          ("entity", "fsm e {}", { case c: Entity => c }),
          ("decl", "param bool e = true;", {
            case c @ Decl(symbol, _) if symbol.kind.isInstanceOf[TypeParam] => c
          }),
          ("instance", "d = new a();", { case c: Instance   => c }),
          ("connect", "b -> c;", { case c: Connect          => c }),
          ("function", "void main() {}", { case c: Function => c })
        )
      } {
        name in {
          val entity = s"""|network a {
                           |  in bool b;
                           |  out bool c;
                           |  ${text}
                           |}""".stripMargin.asTree[Entity]

          val contents = xform(entity).children collect pattern

          TypeAssigner(contents.toList.loneElement).tpe shouldBe TypeMisc
        }
      }

      "state" in {
        TypeAssigner(State(ExprRef(ErrorSymbol), Nil)).tpe shouldBe TypeMisc
      }
    }

    "Sym" in {
      val symbol = cc.newTermSymbol("foo", Loc.synthetic, TypeUInt(4))
      TypeAssigner(Sym(symbol)).tpe shouldBe TypeUInt(4)
    }
  }
}
