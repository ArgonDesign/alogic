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
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.passes.Namer
import org.scalatest.FreeSpec

final class TyperCheckExprSpec extends FreeSpec with AlogicTest {

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
    val node = tree rewrite namer match {
      case Root(_, entity) => entity
      case other           => other
    }
    node rewrite typer
  }

  def checkError(tree: Tree, err: List[String]) = {
    val errors = cc.messages.filter { _.isInstanceOf[Error] }
    if (err.isEmpty) {
      errors shouldBe empty
    } else {
      errors.loneElement should beThe[Error]((err map Pattern.quote): _*)
    }
  }

  "The Typer should type check expressions" - {
    "unary" - {
      for {
        (expr, err) <- List(
          (s"+(N)", Nil),
          (s"-(N)", Nil),
          (s"~(N)", Nil),
          (s"!(N)", Nil),
          (s"&(N)", Nil),
          (s"|(N)", Nil),
          (s"^(N)", Nil),
          (s"+(main)", "Operand of unary operator '+' is of non-packed type" :: Nil),
          (s"-(main)", "Operand of unary operator '-' is of non-packed type" :: Nil),
          (s"~(main)", "Operand of unary operator '~' is of non-packed type" :: Nil),
          (s"!(main)", "Operand of unary operator '!' is of non-packed type" :: Nil),
          (s"&(main)", "Operand of unary operator '&' is of non-packed type" :: Nil),
          (s"|(main)", "Operand of unary operator '|' is of non-packed type" :: Nil),
          (s"^(main)", "Operand of unary operator '^' is of non-packed type" :: Nil),
          (s"+(8'd1)", Nil),
          (s"-(8'd1)", Nil),
          (s"~(8'd1)", Nil),
          (s"!(8'd1)", Nil),
          (s"+(8'sd1)", Nil),
          (s"-(8'sd1)", Nil),
          (s"~(8'sd1)", Nil),
          (s"!(8'sd1)", Nil),
          (s"&(1)", "Unary operator '&' cannot be applied to value of type num" :: Nil),
          (s"|(1)", "Unary operator '|' cannot be applied to value of type num" :: Nil),
          (s"^(1)", "Unary operator '^' cannot be applied to value of type num" :: Nil)
        )
      } {
        expr in {
          val root = s"""|fsm a {
                         |  (* unused *) param u8 N = 8'd2;
                         |  void main() {
                         |    $$display("", ${expr});
                         |    fence;
                         |  }
                         |}""".stripMargin.asTree[Root]
          checkError(xform(root), err)
        }
      }
    }

    "binary" - {
      for (op <- List("*", "/", "%", "+", "-", "&", "|", "^", ">", ">=", "<", "<=", "==", "!=")) {
        for {
          (expr, err) <- List(
            (s"8'd3 ${op} 8'd2", Nil),
            (s"8'd3 ${op} 8'sd2", Nil),
            (s"8'sd3 ${op} 8'd2", Nil),
            (s"8'sd3 ${op} 8'sd2", Nil),
            (s"8'd3 ${op} 2", Nil),
            (s"3 ${op} 8'd2", Nil),
            (s"3 ${op} 2", Nil),
            (s"7'd3 ${op} 8'd2",
             s"Both operands of binary '${op}' must have the same width, but" ::
               "left  hand operand is 7 bits wide, and" ::
               "right hand operand is 8 bits wide" :: Nil),
            (s"8'd3 ${op} 7'd2",
             s"Both operands of binary '${op}' must have the same width, but" ::
               "left  hand operand is 8 bits wide, and" ::
               "right hand operand is 7 bits wide" :: Nil),
            (s"4'sd3 ${op} 3'sd2",
             s"Both operands of binary '${op}' must have the same width, but" ::
               "left  hand operand is 4 bits wide, and" ::
               "right hand operand is 3 bits wide" :: Nil),
            (s"3'sd3 ${op} 4'sd2",
             s"Both operands of binary '${op}' must have the same width, but" ::
               "left  hand operand is 3 bits wide, and" ::
               "right hand operand is 4 bits wide" :: Nil),
            (s"bool ${op} 8'd2", s"Left hand operand of '${op}' is of non-packed type" :: Nil),
            (s"8'd3 ${op} bool", s"Right hand operand of '${op}' is of non-packed type" :: Nil)
          )
        } {
          expr in {
            val root = s"""|fsm a {
                           |  void main() {
                           |    $$display("", ${expr});
                           |    fence;
                           |  }
                           |}""".stripMargin.asTree[Root]
            checkError(xform(root), err)
          }
        }
      }
    }

    "ternary" - {
      for {
        (expr, err) <- List(
          ("a ? b[0][0] : c[0][0][0]", Nil),
          ("c ? b : c[0][0][0]", "Condition of '?:' is of neither numeric nor packed type" :: Nil),
          ("a ? 8'd3 : 8'd2", Nil),
          ("a ? 8'd3 : 8'sd2", Nil),
          ("a ? 8'sd3 : 8'd2", Nil),
          ("a ? 8'sd3 : 8'sd2", Nil),
          ("a ? 8'd3 : 2", Nil),
          ("a ? 3 : 8'd1", Nil),
          ("a ? 3 : 2", Nil),
          ("a ? 7'd3 : 8'd2",
           s"'then' and 'else' operands of ternary '?:' must have the same width, but" ::
             "'then' operand is 7 bits wide, and" ::
             "'else' operand is 8 bits wide" :: Nil),
          ("a ? 8'd3 : 7'd2",
           s"'then' and 'else' operands of ternary '?:' must have the same width, but" ::
             "'then' operand is 8 bits wide, and" ::
             "'else' operand is 7 bits wide" :: Nil),
          ("a ? 4'sd3 : 3'sd2",
           s"'then' and 'else' operands of ternary '?:' must have the same width, but" ::
             "'then' operand is 4 bits wide, and" ::
             "'else' operand is 3 bits wide" :: Nil),
          ("a ? 3'sd3 : 4'sd2",
           s"'then' and 'else' operands of ternary '?:' must have the same width, but" ::
             "'then' operand is 3 bits wide, and" ::
             "'else' operand is 4 bits wide" :: Nil),
          ("a ? c : b", "'then' operand of '?:' is of non-packed type" :: Nil),
          ("a ? b : c", "'else' operand of '?:' is of non-packed type" :: Nil)
        )
      } {
        expr in {
          val root = s"""|fsm f {
                         |  (* unused *) out sync u2 a;
                         |  (* unused *) i2[1][2] b;
                         |  (* unused *) i2[1][2] c[4];
                         |  void main() {
                         |    $$display("", ${expr});
                         |    fence;
                         |  }
                         |}""".stripMargin.asTree[Root]
          checkError(xform(root), err)
        }
      }
    }

    "index" - {
      for {
        (expr, err) <- List(
          ("a[0]", Nil),
          ("b[0]", Nil),
          ("b[0][0]", Nil),
          ("b[0][0][0]", Nil),
          ("c[0]", Nil),
          ("c[0][0]", Nil),
          ("c[0][0][0]", Nil),
          ("c[0][0][0][0]", Nil),
          ("8'd2[0]", Nil),
          ("8'd2[0][0]", Nil),
          ("8'd2[0][0][0]", Nil),
          ("main[0]", "Target is not indexable" :: Nil),
          ("a[bool]", "Index is of non-packed type" :: Nil),
          ("8'd0[2'd0]", "Index yields 2 bits, 3 bits are expected" :: Nil),
          ("8'd0[3'd0]", Nil),
          ("8'd0[4'd0]", "Index yields 4 bits, 3 bits are expected" :: Nil),
          ("7'd0[2'd0]", "Index yields 2 bits, 3 bits are expected" :: Nil),
          ("7'd0[3'd0]", Nil),
          ("7'd0[4'd0]", "Index yields 4 bits, 3 bits are expected" :: Nil),
          ("9'd0[2'd0]", "Index yields 2 bits, 4 bits are expected" :: Nil),
          ("9'd0[3'd0]", "Index yields 3 bits, 4 bits are expected" :: Nil),
          ("9'd0[4'd0]", Nil),
          ("1'd0[1'd0]", Nil),
          ("1'd0[2'd0]", "Index yields 2 bits, 1 bits are expected" :: Nil),
          ("c[2'd3][1'd0][1'd1][2'd2]", Nil),
          ("8'd0[3'sd0]", "Index must be unsigned" :: Nil),
        )
      } {
        expr in {
          val root = s"""|fsm f {
                         |  (* unused *) out sync u2 a;
                         |  (* unused *) i3[1][2] b;
                         |  (* unused *) i3[1][2] c[4];
                         |  void main() {
                         |    $$display("", ${expr});
                         |    fence;
                         |  }
                         |}""".stripMargin.asTree[Root]
          checkError(xform(root), err)
        }
      }
    }

    "slice" - {
      for {
        (expr, err) <- List(
          ("a[1:0]", Nil),
          ("b[1:0]", Nil),
          ("b[0][1:0]", Nil),
          ("b[0][0][1:0]", Nil),
          ("c[1:0]", "Target is not sliceable" :: Nil),
          ("c[0][1:0]", Nil),
          ("c[0][0][1:0]", Nil),
          ("c[0][0][0][1:0]", Nil),
          ("bool[1:0]", "Target is not sliceable" :: Nil),
          ("main[1:0]", "Target is not sliceable" :: Nil),
          ("a[1:bool]", "Right index is of non-packed type" :: Nil),
          ("a[bool:0]", "Left index is of non-packed type" :: Nil),
          ("8'd0[3'd1:2'd0]", "Right index yields 2 bits, 3 bits are expected" :: Nil),
          ("8'd0[3'd1:3'd0]", Nil),
          ("8'd0[3'd1:4'd0]", "Right index yields 4 bits, 3 bits are expected" :: Nil),
          ("7'd0[3'd1:2'd0]", "Right index yields 2 bits, 3 bits are expected" :: Nil),
          ("7'd0[3'd1:3'd0]", Nil),
          ("7'd0[3'd1:4'd0]", "Right index yields 4 bits, 3 bits are expected" :: Nil),
          ("9'd0[4'd1:2'd0]", "Right index yields 2 bits, 4 bits are expected" :: Nil),
          ("9'd0[4'd1:3'd0]", "Right index yields 3 bits, 4 bits are expected" :: Nil),
          ("9'd0[4'd1:4'd0]", Nil),
          ("8'd0[2'd1:3'd0]", "Left index yields 2 bits, 3 bits are expected" :: Nil),
          ("8'd0[4'd1:3'd0]", "Left index yields 4 bits, 3 bits are expected" :: Nil),
          ("7'd0[2'd1:3'd0]", "Left index yields 2 bits, 3 bits are expected" :: Nil),
          ("7'd0[4'd1:3'd0]", "Left index yields 4 bits, 3 bits are expected" :: Nil),
          ("9'd0[2'd1:4'd0]", "Left index yields 2 bits, 4 bits are expected" :: Nil),
          ("9'd0[3'd1:4'd0]", "Left index yields 3 bits, 4 bits are expected" :: Nil),
          ("9'd0[4'sd1:4'd0]", "Left index must be unsigned" :: Nil),
          ("9'd0[4'd1:4'sd0]", "Right index must be unsigned" :: Nil)
        )
      } {
        expr in {
          val root = s"""|fsm f {
                         |  (* unused *) out sync u2 a;
                         |  (* unused *) i3[1][2] b;
                         |  (* unused *) i3[1][2] c[4];
                         |  void main() {
                         |    $$display("", ${expr});
                         |    fence;
                         |  }
                         |}""".stripMargin.asTree[Root]
          checkError(xform(root), err)
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
  }
}
