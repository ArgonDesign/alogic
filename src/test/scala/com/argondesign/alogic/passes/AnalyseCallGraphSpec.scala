////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.Messages.Warning
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FuncVariant
import com.argondesign.alogic.core.SymbolAttributes
import com.argondesign.alogic.core.Symbols.Symbol
import org.scalatest.freespec.AnyFreeSpec

final class AnalyseCallGraphSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  private def analyseCallGraph(text: String): Thicket = Thicket {
    transformWithPass(
      FrontendPass andThen
        DropPackageAndParametrizedDescs andThen
        DescToDeclDefn andThen
        Fold andThen
        LowerLoops andThen
        AnalyseCallGraph,
      text
    ) map {
      _.toList flatMap { case (decl, defn) => List(decl, defn) }
    } getOrElse Nil
  }

  "AnalyseCallGraph should" - {
    "error for directly recursive functions without reclimit attribute" - {
      "simple" in {
        analyseCallGraph {
          s"""fsm a {
             |  void main() { foo(); }
             |  void foo() { foo(); }
             |}""".stripMargin
        }
        cc.messages.loneElement should beThe[Error](
          "Recursive function 'foo' requires 'reclimit' attribute"
        )
      }

      "even if stacklimit of enclosed entity is provided" in {
        analyseCallGraph {
          s"""(* stacklimit = 2 *) fsm a {
             |  void main() { foo(); }
             |  void foo() { foo(); }
             |}""".stripMargin
        }
        cc.messages.loneElement should beThe[Error](
          "Recursive function 'foo' requires 'reclimit' attribute"
        )
      }
    }

    "do not error for directly recursive functions with reclimit attribute" in {
      analyseCallGraph {
        s"""fsm a {
           |  void main() { foo(); }
           |  (* reclimit = 2 *) void foo() { foo(); }
           |}""".stripMargin
      }
      cc.messages shouldBe empty
    }

    "do not error for functions that goto themselves without reclimit attribute" in {
      analyseCallGraph {
        s"""fsm a {
           |  void main() { foo(); }
           |  void foo() { goto foo(); }
           |}""".stripMargin
      }
      cc.messages shouldBe empty
    }

    "error for indirectly recursive functions without reclimit attribute" - {
      "2 cycle" in {
        analyseCallGraph {
          s"""fsm a {
             |  void main() { foo(); }
             |  void foo() { bar(); }
             |  void bar() { foo(); }
             |}""".stripMargin
        }
        cc.messages should have length 2
        cc.messages(0) should beThe[Error](
          "Indirectly recursive function 'foo' requires 'reclimit' attribute"
        )
        cc.messages(1) should beThe[Error](
          "Indirectly recursive function 'bar' requires 'reclimit' attribute"
        )
      }

      "3 cycle" in {
        analyseCallGraph {
          s"""fsm a {
             |  void main() { foo(); }
             |  void foo() { bar(); }
             |  void bar() { main(); }
             |}""".stripMargin
        }
        cc.messages should have length 3
        cc.messages(0) should beThe[Error](
          "Indirectly recursive function 'main' requires 'reclimit' attribute"
        )
        cc.messages(1) should beThe[Error](
          "Indirectly recursive function 'foo' requires 'reclimit' attribute"
        )
        cc.messages(2) should beThe[Error](
          "Indirectly recursive function 'bar' requires 'reclimit' attribute"
        )
      }
    }

    "do not error for indirectly recursive functions with reclimit attribute" - {
      "2 cycle" in {
        analyseCallGraph {
          s"""fsm a {
             |  void main() { foo(); }
             |  (* reclimit = 2 *) void foo() { bar(); }
             |  (* reclimit = 2 *) void bar() { foo(); }
             |}""".stripMargin
        }
        cc.messages shouldBe empty
      }

      "3 cycle" in {
        analyseCallGraph {
          s"""fsm a {
             |  (* reclimit = 2 *) void main() { foo(); }
             |  (* reclimit = 2 *) void foo() { bar(); }
             |  (* reclimit = 2 *) void bar() { main(); }
             |}""".stripMargin
        }
        cc.messages shouldBe empty
      }
    }

    "do not error for recursion through gotos only without reclimit attributes" - {
      "direct" in {
        analyseCallGraph {
          s"""fsm a {
             |  void main() { goto main(); }
             |}""".stripMargin
        }
        cc.messages shouldBe empty
      }

      "2 cycle" in {
        analyseCallGraph {
          s"""fsm a {
             |  void main() { goto foo(); }
             |  void foo() { goto main(); }
             |}""".stripMargin
        }
        cc.messages shouldBe empty
      }

      "3 cycle" in {
        analyseCallGraph {
          s"""fsm a {
             |  void main() { goto foo(); }
             |  void foo() { goto bar(); }
             |  void bar() { goto main(); }
             |}""".stripMargin
        }
        cc.messages shouldBe empty
      }
    }

    "error for non-computable reclimit attribute" in {
      analyseCallGraph {
        s"""fsm a {
           |  (* reclimit = @unknownu(1) *) void main() { main(); }
           |}""".stripMargin
      }
      cc.messages.loneElement should beThe[Error](
        "'reclimit' attribute must be a compile time constant"
      )
    }

    "error for reclimit 1" in {
      analyseCallGraph {
        s"""fsm a {
           |  (* reclimit = 1 *) void main() { main(); }
           |}""".stripMargin
      }
      cc.messages.loneElement should beThe[Error](
        "Recursive function 'main' has 'reclimit' attribute equal to 1"
      )
    }

    "warn for ignored reclimit attribute" - {
      "non-recursive" in {
        analyseCallGraph {
          s"""fsm a {
             |  void main() { foo(); }
             |  (* reclimit = 2 *) void foo() { fence; }
             |}""".stripMargin
        }
        cc.messages.loneElement should beThe[Warning](
          "'reclimit' attribute ignored on function 'foo'"
        )
      }

      "tail recursion only" in {
        analyseCallGraph {
          s"""fsm a {
             |  void main() { foo(); }
             |  (* reclimit = 2 *) void foo() { goto foo(); }
             |}""".stripMargin
        }
        cc.messages.loneElement should beThe[Warning](
          "'reclimit' attribute ignored on function 'foo'"
        )
      }

      "recursion through gotos only" - {
        "direct" in {
          analyseCallGraph {
            s"""fsm a {
               |  (* reclimit = 2 *) void main() { goto main(); }
               |}""".stripMargin
          }
          cc.messages.loneElement should beThe[Warning](
            "'reclimit' attribute ignored on function 'main'"
          )
        }

        "2 cycle" in {
          analyseCallGraph {
            s"""fsm a {
               |  (* reclimit = 2 *) void main() { goto foo(); }
               |  (* reclimit = 2 *) void foo() { goto main(); }
               |}""".stripMargin
          }
          cc.messages should have length 2
          cc.messages(0) should beThe[Warning](
            "'reclimit' attribute ignored on function 'main'"
          )
          cc.messages(1) should beThe[Warning](
            "'reclimit' attribute ignored on function 'foo'"
          )
        }

        "3 cycle" in {
          analyseCallGraph {
            s"""fsm a {
               |  (* reclimit = 2 *) void main() { goto foo(); }
               |  (* reclimit = 2 *) void foo() { goto bar(); }
               |  (* reclimit = 2 *) void bar() { goto main(); }
               |}""".stripMargin
          }
          cc.messages should have length 3
          cc.messages(0) should beThe[Warning](
            "'reclimit' attribute ignored on function 'main'"
          )
          cc.messages(1) should beThe[Warning](
            "'reclimit' attribute ignored on function 'foo'"
          )
          cc.messages(2) should beThe[Warning](
            "'reclimit' attribute ignored on function 'bar'"
          )
        }
      }

      "even if stacklimit is provided" in {
        analyseCallGraph {
          s"""(* stacklimit = 1 *) fsm a {
             |  (* reclimit = 2 *) void main() { foo(); main(); }
             |  (* reclimit = 2 *) void foo() { fence; }
             |}""".stripMargin
        }
        cc.messages.loneElement should beThe[Warning](
          "'reclimit' attribute ignored on function 'foo'"
        )
      }

    }

    "error for non-computable stacklimit attribute" in {
      analyseCallGraph {
        s"""(* stacklimit = @unknownu(1) *) fsm a {
           |   (*reclimit = 2 *) void main() { main(); }
           |}""".stripMargin
      }
      cc.messages.loneElement should beThe[Error](
        "'stacklimit' attribute must be a compile time constant"
      )
    }

    "error for stacklimit 0" in {
      analyseCallGraph {
        s"""(* stacklimit = 0 *) fsm a {
           |   (*reclimit = 2 *) void main() { main(); }
           |}""".stripMargin
      }
      cc.messages.loneElement should beThe[Error](
        "Entity 'a' has 'stacklimit' attribute equal to 0"
      )
    }

    "warn for ignored stacklimit attribute " - {
      "non-recursive fsm" in {
        analyseCallGraph {
          s"""(* stacklimit = 2 *) fsm a {
             |  void main() { foo(); }
             |  void foo() { fence; }
             |}""".stripMargin
        }
        cc.messages.loneElement should beThe[Warning](
          "'stacklimit' attribute ignored on entity 'a'"
        )
      }

      "no functions" in {
        analyseCallGraph {
          s"""(* stacklimit = 2 *) network a {
             |}""".stripMargin
        }
        cc.messages.loneElement should beThe[Warning](
          "'stacklimit' attribute ignored on entity 'a'"
        )
      }
    }

    def checkAttr[A](
        symbol: Symbol,
        getAttr: SymbolAttributes => A,
        golden: Map[String, A]
      ): Unit = {
      golden.get(symbol.name) foreach { expected =>
        getAttr(symbol.attr) shouldBe expected
      }
    }

    def checkFunctionAttrs(
        defnEntity: DefnEntity,
        goldenPushStackOnCall: Map[String, Boolean],
        goldenPopStackOnReturn: Map[String, Boolean],
        goldenStaticReturnPoint: Map[String, Option[String]]
      ): Unit =
      defnEntity.functions foreach {
        case DefnFunc(symbol, _, _)
            if symbol.decl.asInstanceOf[DeclFunc].variant == FuncVariant.Ctrl =>
          checkAttr(symbol, _.pushStackOnCall.value, goldenPushStackOnCall)
          checkAttr(symbol, _.popStackOnReturn.value, goldenPopStackOnReturn)
          checkAttr(symbol, _.staticReturnPoint.value map { _.name }, goldenStaticReturnPoint)
        case _ => fail
      }

    def checkStack(defnEnt: DefnEntity, expected: Option[BigInt]): Unit = {
      val rsSymbol = defnEnt.collectFirst {
        case Defn(symbol) if symbol.attr.returnStack.isSet => symbol
      }
      rsSymbol map { _.kind.asStack.size.intValue } shouldBe expected
    }

    "doesn't assign return stack " - {
      "if call graph is tree and only one call to each" in {
        val result = analyseCallGraph {
          """fsm fsm_e {
            |  void main() { a(); b(); c(); }
            |  void a() { e(); f(); return; }
            |  void b() { goto d(); }
            |  void c() { return; }
            |  void d() { goto g(); }
            |  void e() { return; }
            |  void f() { return; }
            |  void g() { return; }
            |}""".stripMargin
        }
        val defnEntity = result getFirst {
          case entity: DefnEntity => entity
        }

        val goldenPushStackOnCall = Map from {
          List("a", "b", "c", "e", "f") map { _ -> false }
        }
        val goldenPopStackOnReturn = Map from {
          List("a", "c", "e", "f", "g") map { _ -> false }
        }
        val goldenStaticReturnPoint = Map(
          "a" -> Some("a"),
          "c" -> Some("c"),
          "e" -> Some("e"),
          "f" -> Some("f"),
          "g" -> Some("b")
        )

        checkStack(defnEntity, None)
        checkFunctionAttrs(
          defnEntity,
          goldenPushStackOnCall,
          goldenPopStackOnReturn,
          goldenStaticReturnPoint
        )
      }

      "if all returns have static return point" in {
        val result = analyseCallGraph {
          """fsm fsm_e {
            |  in bool pi;
            |  void main() { a(); }
            |  void a() { if (pi) { goto b(); } else { goto c(); } }
            |  void b() { return; }
            |  void c() { d(); return; }
            |  void d() { return; }
            |}""".stripMargin
        }
        val defnEntity = result getFirst {
          case entity: DefnEntity => entity
        }

        val goldenPushStackOnCall = Map from {
          List("a", "d") map { _ -> false }
        }
        val goldenPopStackOnReturn = Map from {
          List("b", "c", "d") map { _ -> false }
        }
        val goldenStaticReturnPoint = Map(
          "b" -> Some("a"),
          "c" -> Some("a"),
          "d" -> Some("d")
        )

        checkStack(defnEntity, None)
        checkFunctionAttrs(
          defnEntity,
          goldenPushStackOnCall,
          goldenPopStackOnReturn,
          goldenStaticReturnPoint
        )
      }
    }

    "assigns return stack " - {

      "when return points are not static" in {
        val result = analyseCallGraph {
          """fsm fsm_e {
            |  void main() { a(); b(); }
            |  void a() { goto c(); }
            |  void b() { goto c(); }
            |  void c() { return; }
            |}""".stripMargin
        }
        val defnEntity = result getFirst {
          case entity: DefnEntity => entity
        }

        val goldenPushStackOnCall = Map(
          "a" -> true,
          "b" -> true
        )
        val goldenPopStackOnReturn = Map(
          "c" -> true
        )
        val goldenStaticReturnPoint = Map(
          "c" -> None
        )

        checkStack(defnEntity, Some(1))
        checkFunctionAttrs(
          defnEntity,
          goldenPushStackOnCall,
          goldenPopStackOnReturn,
          goldenStaticReturnPoint
        )
      }

      "only for calls which need it " - {

        "1" in {
          val result = analyseCallGraph {
            """fsm fsm_e {
              |  void main() { a(); a(); b(); }
              |  void a() { return; }
              |  void b() { return; }
              |}""".stripMargin
          }
          val defnEntity = result getFirst {
            case entity: DefnEntity => entity
          }

          val goldenPushStackOnCall = Map(
            "a" -> true,
            "b" -> false
          )
          val goldenPopStackOnReturn = Map(
            "a" -> true,
            "b" -> false
          )
          val goldenStaticReturnPoint = Map(
            "a" -> None,
            "b" -> Some("b")
          )

          checkStack(defnEntity, Some(1))
          checkFunctionAttrs(
            defnEntity,
            goldenPushStackOnCall,
            goldenPopStackOnReturn,
            goldenStaticReturnPoint
          )
        }

        "2" in {
          val result = analyseCallGraph {
            """fsm fsm_e {
              |  void main() { a(); b(); }
              |  void a() { c(); return; }
              |  void b() { c(); return; }
              |  void c() { return; }
              |}""".stripMargin
          }
          val defnEntity = result getFirst {
            case entity: DefnEntity => entity
          }

          val goldenPushStackOnCall = Map(
            "a" -> false,
            "b" -> false,
            "c" -> true
          )
          val goldenPopStackOnReturn = Map(
            "a" -> false,
            "b" -> false,
            "c" -> true
          )
          val goldenStaticReturnPoint = Map(
            "a" -> Some("a"),
            "b" -> Some("b"),
            "c" -> None
          )

          checkStack(defnEntity, Some(1))
          checkFunctionAttrs(
            defnEntity,
            goldenPushStackOnCall,
            goldenPopStackOnReturn,
            goldenStaticReturnPoint
          )
        }

      }

      "with appropriate depth" in {
        // Only need to use the stack when we call b or d, so stack depth should be 2
        val result = analyseCallGraph {
          """fsm fsm_e {
            |  void main() { a(); }
            |  void a() { b(); b(); return; }
            |  void b() { goto b2(); }
            |  void b2() { c(); return; }
            |  void c() { d(); d(); return; }
            |  void d() { goto d2(); }
            |  void d2() { e(); return; }
            |  void e() { return; }
            |}""".stripMargin
        }
        val defnEntity = result getFirst {
          case entity: DefnEntity => entity
        }

        val goldenPushStackOnCall = Map(
          "a" -> false,
          "b" -> true,
          "c" -> false,
          "d" -> true,
          "e" -> false
        )
        val goldenPopStackOnReturn = Map(
          "a" -> false,
          "b2" -> true,
          "c" -> false,
          "d2" -> true,
          "e" -> false
        )
        val goldenStaticReturnPoint = Map(
          "a" -> Some("a"),
          "b2" -> None,
          "c" -> Some("c"),
          "d2" -> None,
          "e" -> Some("e")
        )

        checkStack(defnEntity, Some(2))
        checkFunctionAttrs(
          defnEntity,
          goldenPushStackOnCall,
          goldenPopStackOnReturn,
          goldenStaticReturnPoint
        )
      }

      "correctly when function return point is static but still needs to pop" in {
        // Function b can either return to call site of a or call site of b.
        // Therefore calls to both a and b must push to stack.
        // However returns from c always go back to call site of a.
        // So c has a static return point yet must still pop the stack.
        val result = analyseCallGraph {
          """fsm fsm_e {
            |  in bool pi;
            |  void main() { a(); b(); }
            |  void a() {
            |    if (pi) { goto c(); } else { goto b(); }
            |  }
            |  void b() { return; }
            |  void c() { return; }
            |}""".stripMargin
        }
        val defnEntity = result getFirst {
          case entity: DefnEntity => entity
        }

        val goldenPushStackOnCall = Map(
          "a" -> true,
          "b" -> true
        )
        val goldenPopStackOnReturn = Map(
          "b" -> true,
          "c" -> true
        )
        val goldenStaticReturnPoint = Map(
          "b" -> None,
          "c" -> Some("a")
        )

        checkStack(defnEntity, Some(1))
        checkFunctionAttrs(
          defnEntity,
          goldenPushStackOnCall,
          goldenPopStackOnReturn,
          goldenStaticReturnPoint
        )
      }

      "with correct depth when longest path doesn't start at main" in {
        // Calls to b, c and d require pushes but not a.
        val result = analyseCallGraph {
          """fsm fsm_e {
            |  void main() { a(); }
            |  void a() { b(); b(); return; }
            |  void b() { c(); c(); return; }
            |  void c() { d(); d(); return; }
            |  void d() { return; }
            |}""".stripMargin
        }
        val defnEntity = result getFirst {
          case entity: DefnEntity => entity
        }

        val goldenPushStackOnCall = Map(
          "a" -> false,
          "b" -> true,
          "c" -> true,
          "d" -> true
        )
        val goldenPopStackOnReturn = Map(
          "a" -> false,
          "b" -> true,
          "c" -> true,
          "d" -> true
        )
        val goldenStaticReturnPoint = Map(
          "a" -> Some("a"),
          "b" -> None,
          "c" -> None,
          "d" -> None
        )

        checkStack(defnEntity, Some(3))
        checkFunctionAttrs(
          defnEntity,
          goldenPushStackOnCall,
          goldenPopStackOnReturn,
          goldenStaticReturnPoint
        )
      }
    }

    "handles returns to main by " - {
      "returning to top of main if return called" in {
        val result = analyseCallGraph {
          """fsm fsm_e {
            |  void main() { return; }
            |}""".stripMargin
        }
        val defnEntity = result getFirst {
          case entity: DefnEntity => entity
        }

        val goldenPushStackOnCall = Map[String, Boolean]()
        val goldenPopStackOnReturn = Map(
          "main" -> false
        )
        val goldenStaticReturnPoint = Map(
          "main" -> Some("main")
        )

        checkStack(defnEntity, None)
        checkFunctionAttrs(
          defnEntity,
          goldenPushStackOnCall,
          goldenPopStackOnReturn,
          goldenStaticReturnPoint
        )
      }

      "returning to top of main after goto in main function" in {
        val result = analyseCallGraph {
          """fsm fsm_e {
            |  void main() { goto a(); }
            |  void a() { return; }
            |}""".stripMargin
        }
        val defnEntity = result getFirst {
          case entity: DefnEntity => entity
        }

        val goldenPushStackOnCall = Map[String, Boolean]()
        val goldenPopStackOnReturn = Map(
          "a" -> false
        )
        val goldenStaticReturnPoint = Map(
          "a" -> Some("main")
        )

        checkStack(defnEntity, None)
        checkFunctionAttrs(
          defnEntity,
          goldenPushStackOnCall,
          goldenPopStackOnReturn,
          goldenStaticReturnPoint
        )
      }

      "refer to stack if main calls itself" in {
        // The return statement in the body of main could go to either of the two states.
        val result = analyseCallGraph {
          """fsm fsm_e {
            |  (* reclimit = 2 *) void main() { main(); return; }
            |}""".stripMargin
        }
        val defnEntity = result getFirst {
          case entity: DefnEntity => entity
        }

        val goldenPushStackOnCall = Map[String, Boolean](
          "main" -> true
        )
        val goldenPopStackOnReturn = Map(
          "main" -> true
        )
        val goldenStaticReturnPoint = Map(
          "main" -> None
        )

        // Note this needs only 1 stack entry as first call is spontaneous
        checkStack(defnEntity, Some(1))
        checkFunctionAttrs(
          defnEntity,
          goldenPushStackOnCall,
          goldenPopStackOnReturn,
          goldenStaticReturnPoint
        )
      }

      "refer to stack if main returns but is called elsewhere" in {
        // The return statement in the body of a will always return to the same point.
        // The return statement in the body of main will either go to a or to the start of main.
        val result = analyseCallGraph {
          """fsm fsm_e {
            |  (* reclimit = 3 *) void main() { a(); return; }
            |  (* reclimit = 3 *) void a() { main(); return; }
            |}""".stripMargin
        }
        val defnEntity = result getFirst {
          case entity: DefnEntity => entity
        }

        val goldenPushStackOnCall = Map[String, Boolean](
          "main" -> true,
          "a" -> false
        )
        val goldenPopStackOnReturn = Map(
          "main" -> true,
          "a" -> false
        )
        val goldenStaticReturnPoint = Map(
          "main" -> None,
          "a" -> Some("a")
        )

        // Note this needs only 2 stack entries as first call is spontaneous
        checkStack(defnEntity, Some(2))
        checkFunctionAttrs(
          defnEntity,
          goldenPushStackOnCall,
          goldenPopStackOnReturn,
          goldenStaticReturnPoint
        )
      }
    }
  }
}
