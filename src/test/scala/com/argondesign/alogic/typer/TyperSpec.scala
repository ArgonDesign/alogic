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

import java.util.regex.Pattern

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees.Expr.ImplicitConversions._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.core.Warning
import com.argondesign.alogic.lib.TreeLike
import com.argondesign.alogic.passes.Namer
import org.scalatest.FreeSpec

final class TyperSpec extends FreeSpec with AlogicTest {

  implicit val cc = new CompilerContext
  cc.postSpecialization = true

  val namer = new Namer
  val typer = new Typer

  def xform(tree: Tree) = {
    tree match {
      case Root(_, entity: EntityIdent) => cc.addGlobalEntity(entity)
      case entity: EntityIdent          => cc.addGlobalEntity(entity)
      case _                            =>
    }
    tree rewrite namer rewrite typer
  }

  "The Typer should" - {
    "infer sizes of unsized literals for" ignore {
      "non-reducing binary operator operands" - {
        for (op <- List("*", "/", "%", "+", "-", "&", "|", "^")) {
          val qop = Pattern.quote(op)
          for {
            (expr, resultWidth, msg) <- List(
              (s"8'd3 ${op} 2", Some(8), ""),
              (s"2 ${op} 8'd3", Some(8), ""),
              (s"8'd3 ${op} -'sd2", Some(8), ""),
              (s"-'sd2 ${op} 8'd3", Some(8), ""),
              (s"8'd3 ${op} (N ? 2 : 3)", Some(8), ""),
              (s"(N ? 2 : 3) ${op} 8'd3", Some(8), ""),
              (s"8'd3 ${op} 2 ${op} 4", Some(8), ""),
              (s"4 ${op} 2 ${op} 8'd3", Some(8), ""),
              (s"{N{1'b1}} ${op} 2", None, s"Cannot infer width of right hand operand of '${qop}'"),
              (s"2 ${op} {N{1'b1}}", None, s"Cannot infer width of left hand operand of '${qop}'")
            )
          } {
            val text = expr.trim.replaceAll(" +", " ")
            text in {
              val root = s"""|fsm a {
                             |  param u4 N = 8'd2;
                             |  void main() {
                             |    ${text};
                             |    N;
                             |    fence;
                             |  }
                             |}""".stripMargin.asTree[Root]
              val tree = xform(root)
              if (resultWidth.isDefined) {
                val expr = (tree collectFirst { case StmtExpr(expr) => expr }).value
                lazy val visitor: PartialFunction[TreeLike, Unit] = {
                  case ExprTernary(_, t, e) => {
                    t visit visitor
                    e visit visitor
                  }
                  case node: Expr => {
                    node.tpe.width shouldBe resultWidth.value
                    node.children foreach { _ visit visitor }
                  }
                }
                expr visit visitor
                cc.messages shouldBe empty
              } else {
                cc.messages.last should beThe[Error](msg)
              }
            }
          }
        }
      }

      "reducing binary operator operands" - {
        for (op <- List(">", ">=", "<", "<=", "==", "!=")) {
          val qop = Pattern.quote(op)
          for {
            (expr, resultWidth, msg) <- List(
              (s"8'd3 ${op} 2", Some(1), ""),
              (s"2 ${op} 8'd3", Some(1), ""),
              (s"8'd3 ${op} -'sd2", Some(1), ""),
              (s"-'sd2 ${op} 8'd3", Some(1), ""),
              (s"8'd3 ${op} (N ? 2 : 3)", Some(1), ""),
              (s"(N ? 2 : 3) ${op} 8'd3", Some(1), ""),
              (s"{N{1'b1}} ${op} 2", None, s"Cannot infer width of right hand operand of '${qop}'"),
              (s"2 ${op} {N{1'b1}}", None, s"Cannot infer width of left hand operand of '${qop}'")
            )
          } {
            val text = expr.trim.replaceAll(" +", " ")
            text in {
              val root = s"""|fsm a {
                             |  param u8 N = 8'd2;
                             |  void main() {
                             |    ${text};
                             |    N;
                             |    fence;
                             |  }
                             |}""".stripMargin.asTree[Root]
              val tree = xform(root)
              if (resultWidth.isDefined) {
                cc.messages foreach println
                val expr = (tree collectFirst { case StmtExpr(expr) => expr }).value
                expr.tpe.width shouldBe resultWidth.value
                lazy val visitor: PartialFunction[TreeLike, Unit] = {
                  case ExprTernary(_, t, e) => {
                    t visit visitor
                    e visit visitor
                  }
                  case node: Expr => {
                    node.tpe.width shouldBe 8
                    node.children foreach { _ visit visitor }
                  }
                }
                expr.children foreach { _ visit visitor }

                cc.messages shouldBe empty
              } else {
                cc.messages.last should beThe[Error](msg)
              }
            }
          }
        }
      }

      "ternary operator operands" - {
        for {
          (expr, resultWidth, msg) <- List(
            (s"N ? 0 : 2'd1", Some(2), ""),
            (s"N ? 2'd1 : 0", Some(2), ""),
            (s"0 ? 0 : 2'd1", Some(2), ""),
            (s"0 ? 2'd1 : 0", Some(2), ""),
            (s"1 ? 0 : 2'd1", Some(2), ""),
            (s"1 ? 2'd1 : 0", Some(2), ""),
            (s"N ? 0 + 1 : 2'd1", Some(2), ""),
            (s"N ? 2'd1 : 0 + 1", Some(2), ""),
            (s"0 ? 0 + 1 : 2'd1", Some(2), ""),
            (s"0 ? 2'd1 : 0 + 1", Some(2), ""),
            (s"1 ? 0 + 1 : 2'd1", Some(2), ""),
            (s"1 ? 2'd1 : 0 + 1", Some(2), ""),
            (s"N ? 0 : {N{1'b1}}", None, "Cannot infer width of 'then' operand of '?:'"),
            (s"0 ? 0 : {N{1'b1}}", None, "Cannot infer width of 'then' operand of '?:'"),
            (s"1 ? 0 : {N{1'b1}}", None, "Cannot infer width of 'then' operand of '?:'"),
            (s"N ? {N{1'b1}} : 0", None, "Cannot infer width of 'else' operand of '?:'"),
            (s"0 ? {N{1'b1}} : 0", None, "Cannot infer width of 'else' operand of '?:'"),
            (s"1 ? {N{1'b1}} : 0", None, "Cannot infer width of 'else' operand of '?:'")
          )
        } {
          val text = expr.trim.replaceAll(" +", " ")
          text in {
            val root = s"""|fsm a {
                           |  param u8 N = 8'd2;
                           |  void main() {
                           |    ${text};
                           |    N;
                           |    fence;
                           |  }
                           |}""".stripMargin.asTree[Root]
            val tree = xform(root)
            if (resultWidth.isDefined) {
              cc.messages foreach println
              val expr = (tree collectFirst { case StmtExpr(expr) => expr }).value
              expr.tpe.width shouldBe resultWidth.value
              cc.messages shouldBe empty
            } else {
              cc.messages.last should beThe[Error](Pattern.quote(msg))
            }
          }
        }
      }

      "initializer expressions" - {
        for {
          (decl, expr, msg) <- List(
            ("i8 a = 'sd2", ExprInt(true, 8, 2), ""),
            ("u8 a = 'sd2", ExprInt(true, 8, 2), ""),
            ("i8 a = 2", ExprInt(false, 8, 2), ""),
            ("u8 a = 2", ExprInt(false, 8, 2), ""),
            ("int(N) a = 2", ExprError(), "Cannot infer width of initializer")
          )
        } {
          val text = decl.trim.replaceAll(" +", " ")
          text in {
            val entity = s"""|fsm a {
                             |  param u8 N = 8'd2;
                             |  ${decl};
                             |  void main() { a; N; fence; }
                             |}""".stripMargin.asTree[Entity]
            val tree = xform(entity)
            if (msg.isEmpty) {
              val decls = tree collect { case d: Decl => d }
              val decl = decls.toList(1)
              val Some(init) = decl.init
              init shouldBe expr
              cc.messages shouldBe empty
            } else {
              cc.messages.last should beThe[Error](msg)
            }
          }
        }
      }

      "right hand sides of assignments" - {
        for {
          (assign, expr, msg) <- List(
            ("i8 a; a = 'sd2", ExprInt(true, 8, 2), ""),
            ("u8 a; a = 'sd2", ExprInt(true, 8, 2), ""),
            ("i8 a; a = 2", ExprInt(false, 8, 2), ""),
            ("u8 a; a = 2", ExprInt(false, 8, 2), ""),
            ("int(N) a; a = 2", ExprError(), "Cannot infer width of right hand side of assignment")
          )
        } {
          val text = assign.trim.replaceAll(" +", " ")
          text in {
            val entity = s"""|fsm a {
                             |  param u8 N = 8'd2;
                             |  void main() {
                             |    ${text};
                             |    N;
                             |    fence;
                             |  }
                             |}""".stripMargin.asTree[Entity]
            val tree = xform(entity)
            if (msg.isEmpty) {
              val stmt = (tree collectFirst { case a: StmtAssign => a }).value
              stmt.rhs shouldBe expr
              cc.messages shouldBe empty
            } else {
              cc.messages.last should beThe[Error](msg)
            }
          }
        }
      }
    }

    "type check" - {
      "unary operators" - {
        for {
          (expr, kind, msg) <- List(
            (s"+(N)", TypeUInt(8), ""),
            (s"-(N)", TypeUInt(8), ""),
            (s"~(N)", TypeUInt(8), ""),
            (s"!(N)", TypeUInt(1), ""),
            (s"&(N)", TypeUInt(1), ""),
            (s"|(N)", TypeUInt(1), ""),
            (s"^(N)", TypeUInt(1), ""),
            (s"+(main)", TypeError, "Operand of unary '+' is of non-packed type"),
            (s"-(main)", TypeError, "Operand of unary '-' is of non-packed type"),
            (s"~(main)", TypeError, "Operand of unary '~' is of non-packed type"),
            (s"!(main)", TypeError, "Operand of unary '!' is of non-packed type"),
            (s"&(main)", TypeError, "Operand of unary '&' is of non-packed type"),
            (s"|(main)", TypeError, "Operand of unary '|' is of non-packed type"),
            (s"^(main)", TypeError, "Operand of unary '^' is of non-packed type")
          )
        } {
          val text = expr.trim.replaceAll(" +", " ")
          text in {
            val root = s"""|fsm a {
                           |  (* unused *) param u8 N = 8'd2;
                           |  void main() {
                           |    $$display("", ${text});
                           |    fence;
                           |  }
                           |}""".stripMargin.asTree[Root]
            val tree = xform(root)
            if (msg.isEmpty) {
              val expr = (tree collectFirst { case e: ExprUnary => e }).value
              expr.tpe shouldBe kind
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](Pattern.quote(msg))
            }
          }
        }
      }

      "binary operators" - {
        for (op <- List("*",
                        "/",
                        "%",
                        "+",
                        "-",
                        "&",
                        "|",
                        "^",
                        ">",
                        ">=",
                        "<",
                        "<=",
                        "==",
                        "!=",
                        "<<",
                        ">>",
                        "<<<",
                        ">>>")) {
          val qop = Pattern.quote(op)
          for {
            (expr, pattern, msg) <- List[(String, PartialFunction[Any, Unit], String)](
              (s"8'd3 ${op} 8'd2", { case ExprBinary(_, `op`, _) => }, ""),
              (s"bool ${op} 8'd2", { case _: ExprError           => },
               s"Left hand operand of '${qop}' is of non-packed type"),
              (s"8'd3 ${op} bool", { case _: ExprError => },
               s"Right hand operand of '${qop}' is of non-packed type")
            )
          } {
            val text = expr.trim.replaceAll(" +", " ")
            text in {
              val root = s"""|fsm a {
                             |  (* unused *) param u8 N = 8'd2;
                             |  void main() {
                             |    $$display("", ${text});
                             |    fence;
                             |  }
                             |}""".stripMargin.asTree[Root]
              xform(root)
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
            ("e.y", TypeStruct("a", List("x"), List(TypeSInt(8))), ""),
            ("e.y.x", TypeSInt(8), ""),
            ("d.z", TypeError, "No field named 'z' in '.*'"),
            ("e.z", TypeError, "No field named 'z' in '.*'"),
            ("e.y.z", TypeError, "No field named 'z' in '.*'"),
            ("f.x", TypeSInt(8), ""),
            ("g.y", TypeStruct("a", List("x"), List(TypeSInt(8))), ""),
            ("g.y.x", TypeSInt(8), ""),
            ("f.valid", TypeUInt(1), ""),
            ("g.valid", TypeUInt(1), ""),
            ("@bits(d.x)", TypeSInt(8), ""),
            ("@bits(e.y)", TypeStruct("a", List("x"), List(TypeSInt(8))), ""),
            ("@bits(e.y.x)", TypeSInt(8), ""),
            ("@bits(f.x)", TypeSInt(8), ""),
            ("@bits(g.y)", TypeStruct("a", List("x"), List(TypeSInt(8))), ""),
            ("@bits(g.y.x)", TypeSInt(8), ""),
            ("@bits(a.x)", TypeType(TypeSInt(8)), ""),
            ("@bits(b.y)", TypeType(TypeStruct("a", List("x"), List(TypeSInt(8)))), ""),
            ("@bits(b.y.x)", TypeType(TypeSInt(8)), "")
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
                           |  (* unused *) a d;
                           |  (* unused *) b e;
                           |  (* unused *) in sync a f;
                           |  (* unused *) out sync b g;
                           |  void main() {
                           |    $$display("", ${text});
                           |    fence;
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

      "call" - {
        for {
          (text, kind, msg) <- List(
            ("a()", TypeError, "'.*' is not callable"),
            ("a.valid()", TypeError, s"'.*' is not callable"),
            ("a.valid(1'b1)", TypeError, s"'.*' is not callable"),
            ("a.write()", TypeError, "Too few arguments to function call, expected 1, have 0"),
            ("a.write(2'b1)", TypeVoid, ""),
            ("a.write(1'b1, 1'b1)",
             TypeError,
             "Too many arguments to function call, expected 1, have 2"),
            ("bar()", TypeVoid, ""),
            ("bar(1, 2, 3, 4, 5)",
             TypeError,
             "Too many arguments to function call, expected 0, have 5"),
            ("a.write(bar)", TypeError, "Parameter 1 to function call is of non-packed type"),
            ("a.write(3'b1)",
             TypeError,
             "Width 3 of parameter 1 passed to function call is greater than expected width 2"),
            ("a.write(1'b1)",
             TypeError,
             "Width 1 of parameter 1 passed to function call is less than expected width 2"),
            ("@bits(a)", TypeNum(false), ""),
            ("@bits(a.valid)", TypeNum(false), "")
          )
        } {
          text in {
            val root = s"""|fsm c {
                           |  (* unused *) out sync u2 a;
                           |
                           |  (* unused *)
                           |  void bar() {
                           |    fence;
                           |  }
                           |
                           |  void main() {
                           |    ${text};
                           |    fence;
                           |  }
                           |}""".stripMargin.asTree[Root]
            val tree = xform(root)
            if (msg.isEmpty) {
              val expr = (tree collectFirst { case e: ExprCall => e }).value
              expr.tpe shouldBe kind
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }
        }
      }

      "cat" - {
        for {
          (text, kind, msg) <- List(
            ("{1'b1, 1'b0}", TypeUInt(2), ""),
            ("{a, a}", TypeUInt(4), ""),
//            ("{a, 1}", TypeError, s"Part 2 of bit concatenation is of non-packed type"),
            ("{a, bool}", TypeError, s"Part 2 of bit concatenation is of non-packed type")
          )
        } {
          text in {
            val root = s"""|fsm c {
                           |  (* unused *) out sync u2 a;
                           |  void main() {
                           |    $$display("", ${text});
                           |    fence;
                           |  }
                           |}""".stripMargin.asTree[Root]
            val tree = xform(root)
            if (msg.isEmpty) {
              val expr = (tree collectFirst { case e: ExprCat => e }).value
              expr.tpe shouldBe kind
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
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
//            ("{4{1}}", TypeError, s"Value of bit repetition is of non-packed type"),
            ("{4{bool}}", TypeError, s"Value of bit repetition is of non-packed type"),
            ("{bool{1'b1}}", TypeError, s"Count of bit repetition is of non-numeric type")
          )
        } {
          text in {
            val root = s"""|fsm c {
                           |  (* unused *) out sync u2 a;
                           |  void main() {
                           |    $$display("", ${text});
                           |    fence;
                           |  }
                           |}""".stripMargin.asTree[Root]
            val tree = xform(root)
            if (msg.isEmpty) {
              val expr = (tree collectFirst { case e: ExprRep => e }).value
              expr.tpe shouldBe kind
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }
        }
      }

      "index" - {
        for {
          (text, msg) <- List(
            ("a[0]", ""),
            ("b[0]", ""),
            ("b[0][0]", ""),
            ("b[0][0][0]", ""),
            ("c[0]", ""),
            ("c[0][0]", ""),
            ("c[0][0][0]", ""),
            ("c[0][0][0][0]", ""),
            ("main[0]", "Target of index is neither a packed value, nor a memory"),
            ("a[bool]", "Index is of non-numeric type"),
            ("a[b[0]]", "Index is of non-numeric type")
          )
        } {
          text in {
            val root = s"""|fsm f {
                           |  (* unused *) out sync u2 a;
                           |  (* unused *) i3[1][2] b;
                           |  (* unused *) i3[1][2] c[4];
                           |  void main() {
                           |    ${text};
                           |    fence;
                           |  }
                           |}""".stripMargin.asTree[Root]
            xform(root)
            if (msg.isEmpty) {
              cc.messages collect { case error: Error => error } shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }
        }
      }

      "slice" - {
        for {
          (text, msg) <- List(
            ("a[1:0]", ""),
            ("b[1:0]", ""),
            ("b[0][1:0]", ""),
            ("b[0][0][1:0]", ""),
            ("c[1:0]", "Target of slice is of non-packed type"),
            ("c[0][1:0]", ""),
            ("c[0][0][1:0]", ""),
            ("c[0][0][0][1:0]", ""),
            ("bool[1:0]", "Target of slice is of non-packed type"),
            ("main[1:0]", "Target of slice is of non-packed type"),
            ("a[1:bool]", "Right index of slice is of non-numeric type"),
            ("a[1:b[0]]", "Right index of slice is of non-numeric type"),
            ("a[bool:0]", "Left index of slice is of non-numeric type"),
            ("a[b[0]:0]", "Left index of slice is of non-numeric type")
          )
        } {
          text in {
            val root = s"""|fsm f {
                           |  (* unused *) out sync u2 a;
                           |  (* unused *) i3[1][2] b;
                           |  (* unused *) i3[1][2] c[4];
                           |  void main() {
                           |    $$display("", ${text});
                           |    fence;
                           |  }
                           |}""".stripMargin.asTree[Root]
            xform(root)
            if (msg.isEmpty) {
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }
        }
      }

      "ternary" - {
        for {
          (text, msg) <- List(
            ("a ? b[0][0] : c[0][0][0]", ""),
            ("c ? b : c[0][0][0]", "Condition of '?:' is of neither numeric nor packed type"),
            ("a ? c : b", "'then' operand of '?:' is of non-packed type"),
            ("a ? b : c", "'else' operand of '?:' is of non-packed type")
          )
        } {
          text in {
            val root = s"""|fsm f {
                           |  (* unused *) out sync u2 a;
                           |  (* unused *) i2[1][2] b;
                           |  (* unused *) i2[1][2] c[4];
                           |  void main() {
                           |    $$display("", ${text});
                           |    fence;
                           |  }
                           |}""".stripMargin.asTree[Root]
            xform(root)
            if (msg.isEmpty) {
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](Pattern.quote(msg))
            }
          }
        }
      }

      "blocks" - {
        for {
          (stmt, msg) <- List(
            ("{ $display(); }", ""),
            ("{ fence; }", ""),
            ("{ $display(); fence; }", ""),
            ("{ $display(); fence; $display(); fence; }", ""),
            ("{ fence; $display();}",
             "Block must contain only combinatorial statements, or end with a control statement")
          )
        } {
          stmt in {
            val result = xform(stmt.asTree[Stmt])
            if (msg.isEmpty) {
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
              result shouldBe StmtError()
            }
          }
        }
      }

      "statements" - {
        for {
          (stmt, msg) <- List(
            ("if (1) $display();", ""),
            ("if (1) $display(); else $display();", ""),
            ("if (1) fence;", ""),
            ("if (1) fence; else fence;", ""),
            ("if (1) fence; else $display();",
             "Either both or neither branches of if-else must be control statements"),
            ("if (1) $display(); else fence;",
             "Either both or neither branches of if-else must be control statements"),
            ("case(1) { 1: $display(); }", ""),
            ("case(1) { 1: fence; }", ""),
            ("case(1) { default: $display(); }", ""),
            ("case(1) { default: fence; }", ""),
            ("case(1) { 1: $display(); 2: $display(); }", ""),
            ("case(1) { 1: fence; 2: fence; }", ""),
            ("case(1) { 1: $display(); default: $display(); }", ""),
            ("case(1) { 1: fence; default: fence; }", ""),
            ("case(1) { 1: $display(); 2: fence; }",
             "Either all or no cases of a case statement must be control statements"),
            ("case(1) { 1: fence; 2: $display(); }",
             "Either all or no cases of a case statement must be control statements"),
            ("case(1) { 1: $display(); default: fence; }",
             "Either all or no cases of a case statement must be control statements"),
            ("case(1) { 1: fence; default: $display(); }",
             "Either all or no cases of a case statement must be control statements"),
            ("loop { fence; }", ""),
            ("loop { $display(); }", "Body of 'loop' must be a control statement"),
            ("loop { fence; $display(); }", "Body of 'loop' must end in a control statement")
          )
        } {
          stmt in {
            val result = xform(stmt.asTree[Stmt])
            if (msg.isEmpty) {
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
              result shouldBe StmtError()
            }
          }
        }
      }

      "function bodies" - {
        for {
          (func, msg) <- List(
            ("void main () { fence; }", ""),
            ("void main () { $display(); }", "Body of function must end in a control statement"),
            ("void main () { fence; $display(); }",
             "Body of function must end in a control statement")
          )
        } {
          func in {
            val tree = s"""|fsm a {
                           | ${func}
                           |}""".stripMargin.asTree[Entity]
            xform(tree)
            if (msg.isEmpty) {
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }
        }
      }

      "fence blocks bodies" - {
        for {
          (fb, msg) <- List(
            ("fence { $display(); }", ""),
            ("fence { $display(); fence; }",
             "'fence' block must contain only combinatorial statements")
          )
        } {
          fb in {
            val tree = s"""|fsm a {
                           | ${fb}
                           |}""".stripMargin.asTree[Entity]
            xform(tree)
            if (msg.isEmpty) {
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }
        }
      }

      "port select" - {
        for {
          (expr, msg) <- List(
            ("a.b", ""),
            ("a.c", "No port named 'c' in 'a' of type 'instance a'"),
            ("a.N", "No port named 'N' in 'a' of type 'instance a'")
          )
        } {
          expr in {
            val tree = s"""|network n {
                           |  (* unused *) in bool p;
                           |  (* unused *) new fsm a {
                           |    (* unused *) param u8 N = 8'd2;
                           |    (* unused *) in bool b;
                           |  }
                           |  p -> ${expr};
                           |}""".stripMargin.asTree[Entity]
            xform(tree)
            if (msg.isEmpty) {
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }
        }
      }

      "declaration initializers" - {
        for {
          (decl, msg) <- List(
//            ("i8 a = 2", ""),
            ("i8 a = 8'd2", ""),
            ("i8 a = bool", "Initializer expression is of non-packed type"),
//            ("i8 a = 9'd2", "Initializer expression yields 9 bits, 8 bits are expected"),
//            ("i8 a = 7'd2", "Initializer expression yields 7 bits, 8 bits are expected")
          )
        } {
          decl in {
            val tree = s"{ (* unused *) ${decl}; }".asTree[Stmt]
            xform(tree)
            if (msg.isEmpty) {
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }

        }
      }

      "assignments" - {
        for {
          (assignment, msg) <- List(
//            ("a = 2", ""),
            ("a = 8'd2", ""),
            ("a = bool", "Right hand side of assignment is of non-packed type"),
            ("bool = 8'd2", "Left hand side of assignment is of non-packed type"),
            ("a += 8'd2", ""),
            ("a += bool", "Right hand side of assignment is of non-packed type"),
            ("bool += 8'd2", "Left hand side of assignment is of non-packed type"),
//            ("a = 9'd2", "Right hand side of assignment yields 9 bits, 8 bits are expected"),
//            ("a = 7'd2", "Right hand side of assignment yields 7 bits, 8 bits are expected")
          )
        } {
          assignment in {
            val tree = s"""|fsm x {
                           |  void main() {
                           |    (* unused *) i8 a;
                           |    ${assignment};
                           |    fence;
                           |  }
                           |}""".stripMargin.asTree[Entity]
            xform(tree)
            if (msg.isEmpty) {
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }

        }
      }

      "postfix increment/decrement" - {
        for (op <- List("++", "--")) {
          for {
            (assignment, msg) <- List(
              (s"a${op}", ""),
              (s"bool${op}", s"Target of postfix '${op}' is of non-packed type")
            )
          } {
            assignment in {
              val tree = s"""|fsm x {
                             |  void main() {
                             |    (* unused *) i8 a;
                             |    ${assignment};
                             |    fence;
                             |  }
                             |}""".stripMargin.asTree[Entity]
              xform(tree)
              if (msg.isEmpty) {
                cc.messages shouldBe empty
              } else {
                cc.messages.loneElement should beThe[Error](Pattern.quote(msg))
              }
            }
          }
        }
      }

      {
        val iPortMsg = "Input port cannot be modified"
        val oPortMsg =
          Pattern.quote("Output port with flow control can only be modified using .write()")
        val paramMsg = "Parameter cannot be modified"
        val constMsg = "Constant cannot be modified"
        val memoryMsg = Pattern.quote("Memory can only be modified using .write()")

        "assignments to illegal lhs" - {
          for (op <- List("=", "+=")) {
            for {
              (assignment, msg) <- List(
                (s"a ${op} 8'd0", iPortMsg),
                (s"b ${op} 8'd0", ""),
                (s"c ${op} 8'd0", iPortMsg),
                (s"d ${op} 8'd0", oPortMsg),
                (s"e ${op} 8'd0", iPortMsg),
                (s"f ${op} 8'd0", oPortMsg),
                (s"g ${op} 8'd0", paramMsg),
                (s"h ${op} 8'd0", constMsg),
                (s"a[0] ${op} 1'b0", iPortMsg),
                (s"b[0] ${op} 1'b0", ""),
                (s"c[0] ${op} 1'b0", iPortMsg),
                (s"d[0] ${op} 1'b0", oPortMsg),
                (s"e[0] ${op} 1'b0", iPortMsg),
                (s"f[0] ${op} 1'b0", oPortMsg),
                (s"g[0] ${op} 1'b0", paramMsg),
                (s"h[0] ${op} 1'b0", constMsg),
                (s"i[0] ${op} 4'b0", memoryMsg),
                (s"a[1:0] ${op} 2'b0", iPortMsg),
                (s"b[1:0] ${op} 2'b0", ""),
                (s"c[1:0] ${op} 2'b0", iPortMsg),
                (s"d[1:0] ${op} 2'b0", oPortMsg),
                (s"e[1:0] ${op} 2'b0", iPortMsg),
                (s"f[1:0] ${op} 2'b0", oPortMsg),
                (s"g[1:0] ${op} 2'b0", paramMsg),
                (s"h[1:0] ${op} 2'b0", constMsg),
                (s"i[0][1:0] ${op} 2'b0", memoryMsg),
                (s"{b[1], a[0]} ${op} 2'b0", iPortMsg),
                (s"{b[1], b[0]} ${op} 2'b0", ""),
                (s"{b[1], c[0]} ${op} 2'b0", iPortMsg),
                (s"{b[1], d[0]} ${op} 2'b0", oPortMsg),
                (s"{b[1], e[0]} ${op} 2'b0", iPortMsg),
                (s"{b[1], f[0]} ${op} 2'b0", oPortMsg),
                (s"{b[1], g[0]} ${op} 2'b0", paramMsg),
                (s"{b[1], h[0]} ${op} 2'b0", constMsg),
                (s"{b[1], i[0]} ${op} 9'b0", memoryMsg)
              )
            } {
              assignment in {
                val tree = s"""|fsm x {
                               |  (* unused *) in i8 a;
                               |  (* unused *) out i8 b;
                               |  (* unused *) in sync i8 c;
                               |  (* unused *) out sync i8 d;
                               |  (* unused *) in sync ready i8 e;
                               |  (* unused *) out sync ready i8 f;
                               |  (* unused *) param i8 g = 8'd2;
                               |  (* unused *) const i8 h = 8'd2;
                               |  (* unused *) u8 i[4];
                               |
                               |  void main() {
                               |    ${assignment};
                               |    fence;
                               |  }
                               |}""".stripMargin.asTree[Entity]
                xform(tree)
                if (msg.isEmpty) {
                  cc.messages shouldBe empty
                } else {
                  cc.messages.loneElement should beThe[Error](msg)
                }
              }

            }
          }
        }

        "postfix increment/decrement of illegal target" - {
          for (op <- List("++", "--")) {
            for {
              (assignment, msg) <- List(
                (s"a${op}", iPortMsg),
                (s"b${op}", ""),
                (s"c${op}", iPortMsg),
                (s"d${op}", oPortMsg),
                (s"e${op}", iPortMsg),
                (s"f${op}", oPortMsg),
                (s"g${op}", paramMsg),
                (s"h${op}", constMsg),
                (s"a[0]${op}", iPortMsg),
                (s"b[0]${op}", ""),
                (s"c[0]${op}", iPortMsg),
                (s"d[0]${op}", oPortMsg),
                (s"e[0]${op}", iPortMsg),
                (s"f[0]${op}", oPortMsg),
                (s"g[0]${op}", paramMsg),
                (s"h[0]${op}", constMsg),
                (s"i[0]${op}", memoryMsg),
                (s"a[1:0]${op}", iPortMsg),
                (s"b[1:0]${op}", ""),
                (s"c[1:0]${op}", iPortMsg),
                (s"d[1:0]${op}", oPortMsg),
                (s"e[1:0]${op}", iPortMsg),
                (s"f[1:0]${op}", oPortMsg),
                (s"g[1:0]${op}", paramMsg),
                (s"h[1:0]${op}", constMsg),
                (s"i[0][1:0]${op}", memoryMsg),
                (s"{b[1], a[0]}${op}", iPortMsg),
                (s"{b[1], b[0]}${op}", ""),
                (s"{b[1], c[0]}${op}", iPortMsg),
                (s"{b[1], d[0]}${op}", oPortMsg),
                (s"{b[1], e[0]}${op}", iPortMsg),
                (s"{b[1], f[0]}${op}", oPortMsg),
                (s"{b[1], g[0]}${op}", paramMsg),
                (s"{b[1], h[0]}${op}", constMsg),
                (s"{b[1], i[0]}${op}", memoryMsg)
              )
            } {
              assignment in {
                val tree = s"""|fsm x {
                               |  (* unused *) in i8 a;
                               |  (* unused *) out i8 b;
                               |  (* unused *) in sync i8 c;
                               |  (* unused *) out sync i8 d;
                               |  (* unused *) in sync ready i8 e;
                               |  (* unused *) out sync ready i8 f;
                               |  (* unused *) param i8 g = 8'd2;
                               |  (* unused *) const i8 h = 8'd2;
                               |  (* unused *) u8 i[4];
                               |
                               |  void main() {
                               |    ${assignment};
                               |    fence;
                               |  }
                               |}""".stripMargin.asTree[Entity]
                xform(tree)
                if (msg.isEmpty) {
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

    "error for signals with non-positive width" - {
      for {
        (decl, width) <- List(
          ("uint(-'sd1) a", -1),
          ("uint(-'sd1) a = 0", -1),
          ("uint(0) a", 0),
          ("uint(0) a = 0", 0)
        )
      } {
        decl in {
          val tree = s"""|fsm tmp {
                         |  (* unused *) ${decl};
                         |}""".stripMargin.asTree[Entity]
          xform(tree)
          cc.messages.loneElement should beThe[Error](s"Signal 'a' has declared width ${width}")
        }
      }
    }

    "warn mismatching operand widths where applicable" - {
      "binary operators" ignore {
        for {
          (op, warn) <- List(
            ("*", true),
            ("/", true),
            ("%", true),
            ("+", true),
            ("-", true),
            ("&", true),
            ("|", true),
            ("^", true),
            (">", true),
            (">=", true),
            ("<", true),
            ("<=", true),
            ("==", true),
            ("!=", true),
            ("<<", false),
            (">>", false),
            ("<<<", false),
            (">>>", false),
            ("&&", false),
            ("||", false)
          )
        } {
          val qop = Pattern.quote(op)
          val text = s"8'd1 ${op} 7'd0"
          text in {
            xform(text.asTree[Expr]) should matchPattern {
              case ExprBinary(_, `op`, _) =>
            }
            if (warn) {
              cc.messages.loneElement should beThe[Warning](
                s"'${qop}' expects both operands to have the same width, but",
                "left  operand is 8 bits wide, and",
                "right operand is 7 bits wide"
              )
            } else {
              cc.messages shouldBe empty
            }
          }
        }
      }

      "ternary operator" in {
        val tree = "0 ? 2'd0 : 3'd1".asTree[Expr]
        xform(tree)
        cc.messages.loneElement should beThe[Warning](
          s"'\\?:' expects both the 'then' and 'else' operands to have the same width, but",
          "'then' operand is 2 bits wide, and",
          "'else' operand is 3 bits wide"
        )
      }
    }

  }
}
