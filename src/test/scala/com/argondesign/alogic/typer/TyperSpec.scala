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
import com.argondesign.alogic.passes.Desugar
import com.argondesign.alogic.passes.Namer
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
      "non-reducing binary operator operands" - {
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
              val root = s"""|fsm a {
                             |  param u8 N = 2;
                             |  void main() {
                             |    ${text};
                             |    N;
                             |    fence;
                             |  }
                             |}""".stripMargin.asTree[Root]
              val tree = xform(root)
              if (resultWidth.isDefined) {
                val expr = (tree collectFirst { case StmtExpr(expr) => expr }).value
                expr.tpe.width.value.value shouldBe resultWidth.value
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
              (s"8'd3 ${op} 2 ${op} 4", Some(1), ""),
              (s"4 ${op} 2 ${op} 8'd3", Some(1), ""),
              (s"{N{1'b1}} ${op} 2", None, s"Cannot infer width of right hand operand of '${qop}'"),
              (s"2 ${op} {N{1'b1}}", None, s"Cannot infer width of left hand operand of '${qop}'"),
              (s"bool ${op} 2", None, s"'${qop}' expects packed value on the left hand side"),
              (s"2 ${op} bool", None, s"'${qop}' expects packed value on the right hand side")
            )
          } {
            val text = expr.trim.replaceAll(" +", " ")
            text in {
              val root = s"""|fsm a {
                             |  param u8 N = 2;
                             |  void main() {
                             |    ${text};
                             |    N;
                             |    fence;
                             |  }
                             |}""".stripMargin.asTree[Root]
              val tree = xform(root)
              if (resultWidth.isDefined) {
                val expr = (tree collectFirst { case StmtExpr(expr) => expr }).value
                expr.tpe.width.value.value shouldBe resultWidth.value
                cc.messages shouldBe empty
              } else {
                cc.messages.last should beThe[Error](msg)
              }
            }
          }
        }
      }

      "initializer expressions" - {
        for {
          (decl, expr, msg) <- List(
            ("i8 a = 2", ExprInt(true, 8, 2), ""),
            ("u8 a = 2", ExprInt(false, 8, 2), ""),
            ("i8 a = 'd2", ExprInt(true, 8, 2), ""),
            ("u8 a = 'd2", ExprInt(false, 8, 2), ""),
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
    }

    "type check" - {
      "unary operators" - {
        for {
          (expr, kind, msg) <- List(
            (s"+N", TypeUInt(8), ""),
            (s"-N", TypeUInt(8), ""),
            (s"~N", TypeUInt(8), ""),
            (s"!N", TypeUInt(1), ""),
            (s"&N", TypeUInt(1), ""),
            (s"|N", TypeUInt(1), ""),
            (s"^N", TypeUInt(1), ""),
            (s"+main", TypeError, "Operand of unary '+' is of non-packed type"),
            (s"-main", TypeError, "Operand of unary '-' is of non-packed type"),
            (s"~main", TypeError, "Operand of unary '~' is of non-packed type"),
            (s"!main", TypeError, "Operand of unary '!' is of non-packed type"),
            (s"&main", TypeError, "Operand of unary '&' is of non-packed type"),
            (s"|main", TypeError, "Operand of unary '|' is of non-packed type"),
            (s"^main", TypeError, "Operand of unary '^' is of non-packed type")
          )
        } {
          val text = expr.trim.replaceAll(" +", " ")
          text in {
            val root = s"""|fsm a {
                           |  param u8 N = 2;
                           |  void main() {
                           |    ${text};
                           |    N;
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
                             |  param u8 N = 2;
                             |  void main() {
                             |    ${text};
                             |    N;
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
            ("f.valid", TypeCombFunc(Nil, TypeUInt(1)), ""),
            ("g.valid", TypeCombFunc(Nil, TypeUInt(1)), ""),
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
                           |  a d;
                           |  b e;
                           |  in sync a f;
                           |  out sync b g;
                           |  void main() {
                           |    d; e; f; g;
                           |    ${text};
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
            ("a.valid()", TypeUInt(1), ""),
            ("a()", TypeError, "'.*' is not callable"),
            ("a.valid(1'b1)",
             TypeError,
             s"Too many arguments to function call, expected 0, have 1"),
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
            ("@bits(a.valid)",
             TypeError,
             "Builtin function '@bits' cannot be applied to arguments 'a.valid' of type '.*'")
          )
        } {
          text in {
            val root = s"""|fsm c {
                           |  out sync u2 a;
                           |
                           |  void bar() {
                           |    fence;
                           |  }
                           |
                           |  void main() {
                           |    bar; a;
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
            ("{a, 1}", TypeError, s"Part 2 of bit concatenation is of non-packed type"),
            ("{a, bool}", TypeError, s"Part 2 of bit concatenation is of non-packed type")
          )
        } {
          text in {
            val root = s"""|fsm c {
                           |  out sync u2 a;
                           |  void main() {
                           |    a;
                           |    ${text};
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
            ("{4{a}}", TypeUInt(8), ""),
            ("{4{1}}", TypeError, s"Value of bit repetition is of non-packed type"),
            ("{4{bool}}", TypeError, s"Value of bit repetition is of non-packed type"),
            ("{bool{1'b1}}", TypeError, s"Count of bit repetition is of non-numeric type")
          )
        } {
          text in {
            val root = s"""|fsm c {
                           |  out sync u2 a;
                           |  void main() {
                           |    a;
                           |    ${text};
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
            ("c[0][0][0][0][0]", ""),
            ("c[0][0][0][0][0][0]", ""),
            ("bool[0]", "Target of index is neither a packed value, nor an array"),
            ("main[0]", "Target of index is neither a packed value, nor an array"),
            ("a[bool]", "Index is of non-numeric type"),
            ("a[b[0]]", "Index is of non-numeric type")
          )
        } {
          text in {
            val root = s"""|fsm f {
                           |  out sync u2 a;
                           |  int(1, 2, 3) b;
                           |  int(1, 2, 3) c[4][5][6];
                           |  void main() {
                           |    a; b; c;
                           |    ${text};
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

      "slice" - {
        for {
          (text, msg) <- List(
            ("a[1:0]", ""),
            ("b[1:0]", ""),
            ("b[0][1:0]", ""),
            ("b[0][0][1:0]", ""),
            ("c[1:0]", "Target of slice is of non-packed type"),
            ("c[0][1:0]", "Target of slice is of non-packed type"),
            ("c[0][0][1:0]", "Target of slice is of non-packed type"),
            ("c[0][0][0][1:0]", ""),
            ("c[0][0][0][0][1:0]", ""),
            ("c[0][0][0][0][0][1:0]", ""),
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
                           |  out sync u2 a;
                           |  int(1, 2, 3) b;
                           |  int(1, 2, 3) c[4][5][6];
                           |  void main() {
                           |    a; b; c;
                           |    ${text};
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
            ("a ? b : c[0][0][0]", ""),
            ("c[0] ? b : c[0][0][0]", "Condition of ternary operator ?: is of non-packed type"),
            ("a ? c[0] : b", "True part of ternary operator ?: is of non-packed type"),
            ("a ? b : c[0]", "False part of ternary operator ?: is of non-packed type")
          )
        } {
          text in {
            val root = s"""|fsm f {
                           |  out sync u2 a;
                           |  int(1, 2, 3) b;
                           |  int(1, 2, 3) c[4][5][6];
                           |  void main() {
                           |    a; b; c;
                           |    ${text};
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
            ("{ 0; }", ""),
            ("{ fence; }", ""),
            ("{ 0; fence; }", ""),
            ("{ 0; fence; 0; fence; }", ""),
            ("{ fence; 0;}",
             "Block must contain only combinatorial statements, or end with a control statement")
          )
        } {
          stmt in {
            xform(stmt.asTree[Stmt])
            if (msg.isEmpty) {
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }
        }
      }

      "statements" - {
        for {
          (stmt, msg) <- List(
            ("if (1) 0;", ""),
            ("if (1) 0; else 1;", ""),
            ("if (1) fence;", ""),
            ("if (1) fence; else fence;", ""),
            ("if (1) fence; else 1;",
             "Either both or neither branches of if-else must be control statements"),
            ("if (1) 0; else fence;",
             "Either both or neither branches of if-else must be control statements"),
            ("case(1) { 1: 0; }", ""),
            ("case(1) { 1: fence; }", ""),
            ("case(1) { default: 0; }", ""),
            ("case(1) { default: fence; }", ""),
            ("case(1) { 1: 0; 2: 0; }", ""),
            ("case(1) { 1: fence; 2: fence; }", ""),
            ("case(1) { 1: 0; default: 0; }", ""),
            ("case(1) { 1: fence; default: fence; }", ""),
            ("case(1) { 1: 0; 2: fence; }",
             "Either all or no cases of a case statement must be control statements"),
            ("case(1) { 1: fence; 2: 0; }",
             "Either all or no cases of a case statement must be control statements"),
            ("case(1) { 1: 0; default: fence; }",
             "Either all or no cases of a case statement must be control statements"),
            ("case(1) { 1: fence; default: 0; }",
             "Either all or no cases of a case statement must be control statements"),
            ("loop { fence; }", ""),
            ("loop { 0; }", "Body of 'loop' must be a control statement"),
            ("loop { fence; 0; }", "Body of 'loop' must end in a control statement")
          )
        } {
          stmt in {
            xform(stmt.asTree[Stmt])
            if (msg.isEmpty) {
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }
        }
      }

      "function bodies" - {
        for {
          (func, msg) <- List(
            ("void main () { fence; }", ""),
            ("void main () { 0; }", "Body of function must end in a control statement"),
            ("void main () { fence; 0; }", "Body of function must end in a control statement")
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
            ("fence { 0; }", ""),
            ("fence { 0; fence; }", "'fence' block must contain only combinatorial statements"),
            ("fence { 0; fence; 0; }", "'fence' block must contain only combinatorial statements")
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
            ("a.c", "No port named 'c' in 'a' of type 'entity a'"),
            ("a.N", "No port named 'N' in 'a' of type 'entity a'")
          )
        } {
          expr in {
            val tree = s"""|network n {
                           |  in bool p;
                           |  new fsm a {
                           |    param u8 N = 2;
                           |    in bool b;
                           |    void main() {
                           |      b;
                           |      N;
                           |      fence;
                           |    }
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
            ("i8 a = 2", ""),
            ("i8 a = 8'd2", ""),
            ("i8 a = bool", "Initializer expression is of non-packed type"),
            ("i8 a = 9'd2", "Initializer expression yields 9 bits, but 8 bits are expected"),
            ("i8 a = 7'd2", "Initializer expression yields 7 bits, but 8 bits are expected")
          )
        } {
          decl in {
            val tree = s"{ ${decl}; a; }".asTree[Stmt]
            xform(tree)
            if (msg.isEmpty) {
              cc.messages shouldBe empty
            } else {
              cc.messages.loneElement should beThe[Error](msg)
            }
          }

        }
      }
//      "port" - {
//        "directions" - {
//          for {
//            (expr, msg) <- List(
//              ("op -> op", "Output port of current entity used as driver"),
//              ("op -> ip", "Output port of current entity used as driver"),
//              ("ip -> op", ""),
//              ("ip -> ip", "Driving input port of current entity"),
//              //
//              ("op -> x.ox", "Output port of current entity used as driver"),
//              ("op -> x.ix", "Output port of current entity used as driver"),
//              ("ip -> x.ox", "Driving output port of child entity"),
//              ("ip -> x.ix", ""),
//              //
//              ("x.ox -> op", ""),
//              ("x.ox -> ip", "Driving input port of current entity"),
//              ("x.ix -> op", "Input port of child entity used as driver"),
//              ("x.ix -> ip", "Input port of child entity used as driver"),
//              //
//              ("x.ox -> x.ox", "Driving output port of child entity"),
//              ("x.ox -> x.ix", ""),
//              ("x.ix -> x.ox", "Input port of child entity used as driver"),
//              ("x.ix -> x.ix", "Input port of child entity used as driver")
//            )
//          } {
//            expr in {
//              val tree = s"""|network p {
//                             |  in bool ip;
//                             |  out bool op;
//                             |
//                             |  new fsm x {
//                             |    in bool ix;
//                             |    out bool ox;
//                             |
//                             |    void main() {
//                             |      ix;
//                             |      ox;
//                             |      fence;
//                             |    }
//                             |  }
//                             |
//                             |  fence {
//                             |    ip;
//                             |    op;
//                             |    x;
//                             |  }
//                             |
//                             |  ${expr};
//                             |}""".stripMargin.asTree[Entity]
//              xform(tree)
//              if (msg.isEmpty) {
//                cc.messages shouldBe empty
//              } else {
//                cc.messages.loneElement should beThe[Error](msg)
//              }
//            }
//          }
//        }
//      }
    }

    "warn mismatching operand widths where applicable" - {
      "binary operators" - {
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
    }

  }
}
