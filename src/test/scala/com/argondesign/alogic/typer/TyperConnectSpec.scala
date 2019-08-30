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
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.passes.Checker
import com.argondesign.alogic.passes.Namer
import com.argondesign.alogic.util.unreachable
import org.scalatest.FreeSpec

final class TyperConnectSpec extends FreeSpec with AlogicTest {

  implicit val cc = new CompilerContext

  def xform(trees: Tree*): Unit = {
    val entities = trees map {
      _ match {
        case Root(_, entity: Entity) => entity
        case entity: Entity          => entity
        case _                       => unreachable
      }
    }

    cc.addGlobalEntities(entities)

    trees map {
      _ rewrite new Checker
    } map {
      _ rewrite new Namer
    } map {
      _ rewrite new Typer(externalRefs = false)
    } foreach {
      _ rewrite new Typer(externalRefs = true)
    }
  }

  val treeA = s"""|fsm a_entity {
                  |  (* unused *) param           bool P = true;
                  |  (* unused *) out             bool fcn;
                  |  (* unused *) out             bool fcnb;
                  |  (* unused *) out             u2   fcn2;
                  |  (* unused *) out sync        bool fcv;
                  |  (* unused *) out sync ready  bool fcr;
                  |  (* unused *) out sync accept bool fca;
                  |}""".stripMargin.asTree[Entity]
  val treeB = s"""|fsm b_entity {
                  |  (* unused *) param           bool P = true;
                  |  (* unused *) in              bool fcn;
                  |  (* unused *) in              bool fcnb;
                  |  (* unused *) in              u2   fcn2;
                  |  (* unused *) in sync         bool fcv;
                  |  (* unused *) in sync         u2   fcv2;
                  |  (* unused *) in sync ready   bool fcr;
                  |  (* unused *) in sync accept  bool fca;
                  |}""".stripMargin.asTree[Entity]

  "The Typer should check Connect usage" - {

    "reject invalid port references on left hand side of ->" - {
      for {
        conn <- List(
          "pi2.read() -> po2",
          "-pi2 -> po2",
          "pi2 + pi2b -> po2",
          "(pi2==0) ? pi2 : pi2b -> po2",
          "{2{pi4[pi2]}} -> po2",
          "@zx(2, pi4[pi2]) -> po2",
          "$signed(2*pi2) -> po2",
          "$display() -> po2",
          "$finish() -> po2",
          "@randbit() -> po2",
          "1 -> po2",
          """ "hello" -> po2""",
          "pi4[pi2] -> po1",
          "pi4[pi2+:2] -> po2",
          "{pi4[pi2], pi4[pi2]} -> po2"
        )
      } {
        conn in {
          val tree = s"""|network p {
                         |  (* unused *) in u2 pi2;
                         |  (* unused *) in u2 pi2b;
                         |  (* unused *) in u4 pi4;
                         |  (* unused *) out u1 po1;
                         |  (* unused *) out u2 po2;
                         |  ${conn};
                         |}""".stripMargin.asTree[Entity]
          xform(tree)
          cc.messages.loneElement should beThe[Error](
            "Invalid port reference on left hand side of '->'",
            "Only expressions which are purely wiring are permitted"
          )
        }
      }
    }

    "reject invalid port references on right hand side of ->" - {
      for {
        conn <- List(
          "pi2 -> po2sr.flush()",
          "pi2 -> -po2",
          "pi2 -> po2 + po2b",
          "pi2 -> (po2==0) ? po2 : po2b",
          "pi2 -> {2{po4[po2]}}",
          "pi2 -> @zx(2, po4[po2])",
          "pi2 -> $signed(2*po2)",
          "pi2 -> 1",
          """pi2 -> "hello" """,
          "pi1 -> po4[po2]",
          "pi2 -> po4[po2+:2]",
          "pi2 -> {po4[po2], po4[po2]}"
        )
      } {
        conn in {
          val tree = s"""|network p {
                         |  (* unused *) in u1 pi1;
                         |  (* unused *) in u2 pi2;
                         |  (* unused *) out u2 po2;
                         |  (* unused *) out sync ready u2 po2sr;
                         |  (* unused *) out u2 po2b;
                         |  (* unused *) out u4 po4;
                         |  ${conn};
                         |}""".stripMargin.asTree[Entity]
          xform(tree)
          cc.messages.loneElement should beThe[Error](
            "Invalid port reference on right hand side of '->'",
            "Only expressions which are purely wiring are permitted"
          )
        }
      }
    }

    "accept valid port references on left hand side of ->" - {
      for {
        conn <- List(
          "pi2 -> po2",
          "{2{pi1}} -> po2",
          "@msb(pi2) -> po1",
          "@zx(2, pi4[2]) -> po2",
          "@sx(2, pi4[2]) -> po2",
          "@ex(pi2[0], 4, pi4[2]) -> po4",
          "$signed(pi2) -> po2",
          "$unsigned(pi2) -> po2",
          "2'd2 -> po2",
          "P -> po2",
          "pi4[2] -> po1",
          "pi4[$clog2(2)] -> po1",
          "pi4[@bits(po4)-1] -> po1",
          "pi4[2:1] -> po2",
          "{pi2,pi2} -> po4",
          "si4.a -> po4",
          "si4.a.b -> po4"
        )
      } {
        conn in {
          val tree = s"""|struct bar {
                         |  u4 b;
                         |}
                         |struct foo {
                         |  bar a;
                         |}
                         |network p {
                         |  param u2 P = 2'd2;
                         |  (* unused *) in u1 pi1;
                         |  (* unused *) in u2 pi2;
                         |  (* unused *) in u4 pi4;
                         |  (* unused *) in foo si4;
                         |  (* unused *) out u1 po1;
                         |  (* unused *) out u2 po2;
                         |  (* unused *) out u4 po4;
                         |  ${conn};
                         |}""".stripMargin.asTree[Root]
          xform(tree)
          cc.messages shouldBe empty
        }
      }
    }

    "accept valid port references on right hand side of ->" - {
      for {
        conn <- List(
          "pi2 -> po2",
          "pi1 -> po4[2]",
          "pi2 -> po4[2:1]",
          "pi4 -> {po2,po2}",
          "pi4 -> so4.a",
          "pi4 -> so4.a.b"
        )
      } {
        conn in {
          val tree = s"""|struct bar {
                         |  u4 b;
                         |}
                         |struct foo {
                         |  bar a;
                         |}
                         |network p {
                         |  (* unused *) in u1 pi1;
                         |  (* unused *) in u2 pi2;
                         |  (* unused *) in u4 pi4;
                         |  (* unused *) out u2 po2;
                         |  (* unused *) out u4 po4;
                         |  (* unused *) out foo so4;
                         |  ${conn};
                         |}""".stripMargin.asTree[Root]
          xform(tree)
          cc.messages shouldBe empty
        }
      }
    }

    "widths matching" - {
      for {
        (conn, widths) <- List(
          ("pi0 -> po0", Nil),
          ("pi1 -> po1", Nil),
          ("pi2 -> po2", Nil),
          ("pi3 -> po3", Nil),
          ("{pi1,pi2} -> po3", Nil),
          ("@zx(3, pi1) -> po3", Nil),
          ("pi3[1:0] -> po2", Nil),
          ("si4.a -> po2", Nil),
          ("pi0 -> po1", List(0, 1)),
          ("pi0 -> po2", List(0, 2)),
          ("pi0 -> po3", List(0, 3)),
          ("pi1 -> po0", List(1, 0)),
          ("pi1 -> po2", List(1, 2)),
          ("pi1 -> po3", List(1, 3)),
          ("@sx(4, pi2) -> po1", List(4, 1)),
          ("pi3[1] -> po3", List(1, 3)),
          ("@msb(pi3) -> po3", List(1, 3)),
          ("pi2 -> po0", List(2, 0)),
          ("pi2 -> po1", List(2, 1)),
          ("pi2 -> po3", List(2, 3)),
          ("pi3 -> po0", List(3, 0)),
          ("pi3 -> po1", List(3, 1)),
          ("pi3 -> po2", List(3, 2)),
          ("pi0 -> po0, po1, po2, po3", List(0, 1, 2, 3)),
          ("pi1 -> po0, po1, po2, po3", List(1, 0, 2, 3)),
          ("pi2 -> po0, po1, po2, po3", List(2, 0, 1, 3)),
          ("pi3 -> po0, po1, po2, po3", List(3, 0, 1, 2))
        )
      } {
        conn in {
          val tree = s"""|struct foo {
                         |  u2 a;
                         |  u2 b;
                         |}
                         |network p {
                         |  (* unused *) in void pi0;
                         |  (* unused *) out void po0;
                         |  (* unused *) in u1 pi1;
                         |  (* unused *) out u1 po1;
                         |  (* unused *) in u2 pi2;
                         |  (* unused *) out u2 po2;
                         |  (* unused *) in u3 pi3;
                         |  (* unused *) out u3 po3;
                         |  (* unused *) in foo si4;
                         |  ${conn};
                         |}""".stripMargin.asTree[Root]
          xform(tree)
          widths match {
            case Nil =>
              cc.messages shouldBe empty
            case lw :: rws =>
              cc.messages.length shouldBe rws.length
              for ((msg, rw) <- cc.messages zip rws) {
                msg should beThe[Error](s"Port widths do not match: ${lw} -> ${rw}")
              }
          }
        }
      }
    }

    "nested entity pipeline port usage" - {
      for {
        (conn, msg) <- List(
          ("pipea -> pipeb", ""),
          ("pipea -> pipec", ""),
          ("pipeb -> pipea", ""),
          ("pipeb -> pipec", ""),
          ("pipec -> pipea", ""),
          ("pipec -> pipeb", ""),
          ("pipea -> po1", "Cannot connect pipeline port to non-pipeline port"),
          ("pi1 -> pipea", "Cannot connect non-pipeline port to pipeline port")
        )
      } {
        conn in {
          val tree = s"""|network p {
                         |  (* unused *) in u1 pi1;
                         |  (* unused *) out u1 po1;
                         |  (* unused *) new fsm pipea {}
                         |  (* unused *) new fsm pipeb {}
                         |  (* unused *) new fsm pipec {}
                         |  ${conn};
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

    "connections have drivers on left" - {
      for {
        (conn, msg) <- List(
          ("a -> b", ""),
          ("a.fcn -> b.fcn", ""),
          ("a.fcv -> b.fcv", ""),
          ("a.fcr -> b.fcr", ""),
          ("a.fca -> b.fca", ""),
          ("{a.fcn, a.fcn} -> b.fcn2", ""),
          ("b.fcn -> b.fcn", "Left hand side of '->' contains an input to instance 'b'"),
          ("b.fcv -> b.fcv", "Left hand side of '->' contains an input to instance 'b'"),
          ("b.fcr -> b.fcr", "Left hand side of '->' contains an input to instance 'b'"),
          ("b.fca -> b.fca", "Left hand side of '->' contains an input to instance 'b'"),
          ("{b.fcn, b.fcnb} -> b.fcn2", "Left hand side of '->' contains an input to instance 'b'"),
          ("ifcn -> b.fcn", ""),
          ("ifcv -> b.fcv", ""),
          ("ifcr -> b.fcr", ""),
          ("ifca -> b.fca", ""),
          ("{ifcn, ifcn} -> b.fcn2", ""),
          ("ofcn -> b.fcn", "Left hand side of '->' contains an output from enclosing entity"),
          ("ofcv -> b.fcv", "Left hand side of '->' contains an output from enclosing entity"),
          ("ofcr -> b.fcr", "Left hand side of '->' contains an output from enclosing entity"),
          ("ofca -> b.fca", "Left hand side of '->' contains an output from enclosing entity"),
          ("{ofcn, ofcn} -> b.fcn2",
           "Left hand side of '->' contains an output from enclosing entity"),
          ("P -> b.fcn", ""),
          ("isn.a -> b.fcn", ""),
          ("a_entity -> ofcn", "Left hand side of '->' contains non-port type: entity a_entity")
        )
      } {
        conn in {
          val treeC =
            s"""|struct bar {
                |  bool a;
                |}
                |network  foo {
                |  (* unused *) param bool P = true;
                |  (* unused *) in              bool ifcn;
                |  (* unused *) in              bar  isn;
                |  (* unused *) in sync         bool ifcv;
                |  (* unused *) in sync ready   bool ifcr;
                |  (* unused *) in sync accept  bool ifca;
                |  (* unused *) out             bool ofcn;
                |  (* unused *) out sync        bool ofcv;
                |  (* unused *) out sync ready  bool ofcr;
                |  (* unused *) out sync accept bool ofca;
                |  (* unused *) a = new a_entity();
                |  (* unused *) b = new b_entity();
                |  ${conn};
                |}""".stripMargin.asTree[Root]

          xform(treeA, treeB, treeC)

          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](msg)
          }
        }
      }
    }

    "connections have sinks on right" - {
      for {
        (conn, msg) <- List(
          ("a -> b", ""),
          ("a.fcn -> b.fcn", ""),
          ("a.fcv -> b.fcv", ""),
          ("a.fcr -> b.fcr", ""),
          ("a.fca -> b.fca", ""),
          ("a.fcn2 -> {b.fcn, b.fcnb}", ""),
          ("a.fcn -> a.fcn", "Right hand side of '->' contains an output from instance 'a'"),
          ("a.fcv -> a.fcv", "Right hand side of '->' contains an output from instance 'a'"),
          ("a.fcr -> a.fcr", "Right hand side of '->' contains an output from instance 'a'"),
          ("a.fca -> a.fca", "Right hand side of '->' contains an output from instance 'a'"),
          ("a.fcn2 -> {a.fcn, a.fcnb}",
           "Right hand side of '->' contains an output from instance 'a'"),
          ("a.fcn -> ifcn", "Right hand side of '->' contains an input to enclosing entity"),
          ("a.fcv -> ifcv", "Right hand side of '->' contains an input to enclosing entity"),
          ("a.fcr -> ifcr", "Right hand side of '->' contains an input to enclosing entity"),
          ("a.fca -> ifca", "Right hand side of '->' contains an input to enclosing entity"),
          ("a.fcn2 -> {ifcn,ifcnb}",
           "Right hand side of '->' contains an input to enclosing entity"),
          ("a.fcn -> ofcn", ""),
          ("a.fcv -> ofcv", ""),
          ("a.fcr -> ofcr", ""),
          ("a.fca -> ofca", ""),
          ("a.fcn2 -> {ofcn,ofcnb}", ""),
          ("a.fcn -> P", "Right hand side of '->' contains non-port type: param u1"),
          ("a.fcn -> osn.a", ""),
          ("ifcn -> b_entity", "Right hand side of '->' contains non-port type: entity b_entity")
        )
      } {
        conn in {
          val treeC =
            s"""|struct bar {
                |  bool a;
                |}
                |network  foo {
                |  (* unused *) param bool P = true;
                |  (* unused *) in              bool ifcn;
                |  (* unused *) in              bool ifcnb;
                |  (* unused *) in sync         bool ifcv;
                |  (* unused *) in sync ready   bool ifcr;
                |  (* unused *) in sync accept  bool ifca;
                |  (* unused *) out             bar  osn;
                |  (* unused *) out             bool ofcn;
                |  (* unused *) out             bool ofcnb;
                |  (* unused *) out sync        bool ofcv;
                |  (* unused *) out sync ready  bool ofcr;
                |  (* unused *) out sync accept bool ofca;
                |  (* unused *) a = new a_entity();
                |  (* unused *) b = new b_entity();
                |  ${conn};
                |}""".stripMargin.asTree[Root]

          xform(treeA, treeB, treeC)

          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](msg)
          }
        }
      }
    }

    "non-trivial connect expressions have no flow control" - {
      def mkMsg(side: String) =
        s"Port with flow control found in non-trivial expression on ${side} hand side of '->'"

      for {
        (connect, msgs) <- List(
          ("{a.fcn, a.fcv} -> b.fcn2", List(mkMsg("left"))),
          ("{a.fcn, a.fcr} -> b.fcn2", List(mkMsg("left"))),
          ("{a.fcv, a.fcv} -> b.fcn2", List(mkMsg("left"), mkMsg("left"))),
          ("$signed(a.fcv) -> b.fcv", List(mkMsg("left"))),
          ("a.fcn2 -> {b.fcn, b.fcv}", List(mkMsg("right"))),
          ("a.fcn2 -> {b.fcr, b.fcr}", List(mkMsg("right"), mkMsg("right"))),
          ("is.a -> b.fcn", List(mkMsg("left"))),
        )
      } {
        connect in {
          val treeC =
            s"""|struct bar {
                |  bool a;
                |}
                |network  foo {
                |  (* unused *) in sync bar is;
                |  (* unused *) a = new a_entity();
                |  (* unused *) b = new b_entity();
                |  ${connect};
                |}""".stripMargin.asTree[Root]

          xform(treeA, treeB, treeC)

          cc.messages.length shouldBe msgs.length
          for ((msg, expected) <- cc.messages zip msgs) {
            msg should beThe[Error](expected)
          }

        }
      }
    }

    "ports have compatible flow control" - {

      def mkMsg(l: String, r: String) = s"Ports '${l}' and '${r}' have incompatible flow control"

      for {
        (connect, msg) <- List(
          ("a.fcn -> b.fcn", Nil),
          ("{a.fcn, a.fcn} -> b.fcn2", Nil),
          ("a.fcn -> b.fcv", List(mkMsg("a.fcn", "b.fcv"), "none -> sync")),
          ("a.fcn -> b.fcr", List(mkMsg("a.fcn", "b.fcr"), "none -> sync ready")),
          ("a.fcn -> b.fca", List(mkMsg("a.fcn", "b.fca"), "none -> sync accept")),
          ("a.fcv -> b.fcn", List(mkMsg("a.fcv", "b.fcn"), "sync -> none")),
          ("{a.fcn, a.fcn} -> b.fcv2", List(mkMsg("{a.fcn, a.fcn}", "b.fcv2"), "none -> sync")),
          ("is.a -> b.fcv2", List(mkMsg("is.a", "b.fcv2"), "none -> sync")),
          ("a.fcv -> b.fcv", Nil),
          ("a.fcv -> b.fcr", List(mkMsg("a.fcv", "b.fcr"), "sync -> sync ready")),
          ("a.fcv -> b.fca", List(mkMsg("a.fcv", "b.fca"), "sync -> sync accept")),
          ("a.fcr -> b.fcn", List(mkMsg("a.fcr", "b.fcn"), "sync ready -> none")),
          ("a.fcr -> b.fcv", List(mkMsg("a.fcr", "b.fcv"), "sync ready -> sync")),
          ("a.fcr -> b.fcr", Nil),
          ("a.fcr -> b.fca", List(mkMsg("a.fcr", "b.fca"), "sync ready -> sync accept")),
          ("a.fca -> b.fcn", List(mkMsg("a.fca", "b.fcn"), "sync accept -> none")),
          ("a.fca -> b.fcv", List(mkMsg("a.fca", "b.fcv"), "sync accept -> sync")),
          ("a.fca -> b.fcr", List(mkMsg("a.fca", "b.fcr"), "sync accept -> sync ready")),
          ("a.fca -> b.fca", Nil)
        )
      } {
        connect in {
          val treeC =
            s"""|struct bar {
                |  u2 a;
                |}
                |network  foo {
                |  (* unused *) in bar is;
                |  (* unused *) a = new a_entity();
                |  (* unused *) b = new b_entity();
                |  ${connect};
                |}""".stripMargin.asTree[Root]

          xform(treeA, treeB, treeC)

          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](msg map Pattern.quote: _*)
          }
        }
      }
    }

    "multiple right hand sides are valid" - {
      for {
        (conn, msg) <- List(
          ("a.fcn -> b.fcn, ofcn", ""),
          ("a.fcn2 -> {b.fcn, b.fcnb}, ofcn2", ""),
          ("a.fcn2 -> os.a, ofcn2", ""),
          ("a.fcv -> b.fcv, ofcv", ""),
          ("a.fcr -> b.fcr, ofcr",
           "Port with 'sync ready' flow control cannot have multiple sinks"),
          ("a.fca -> b.fca, ofca",
           "Port with 'sync accept' flow control cannot have multiple sinks")
        )
      } {
        conn in {
          val treeC =
            s"""|struct bar {
                |  u2 a;
                |}
                |network  foo {
                |  (* unused *) out             bool ofcn;
                |  (* unused *) out             u2   ofcn2;
                |  (* unused *) out             bar  os;
                |  (* unused *) out sync        bool ofcv;
                |  (* unused *) out sync ready  bool ofcr;
                |  (* unused *) out sync accept bool ofca;
                |  (* unused *) a = new a_entity();
                |  (* unused *) b = new b_entity();
                |  ${conn};
                |}""".stripMargin.asTree[Root]

          xform(treeA, treeB, treeC)

          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](msg)
          }
        }
      }
    }

    "error for storage specifier on ports driven by '->'" - {
      for {
        (fc, st, ok) <- List(
          ("", "", true),
          ("", "wire", false),
          ("sync", "", true),
          ("sync", "wire", false),
          ("sync ready", "", true),
          ("sync ready", "fslice", false),
          ("sync ready", "bslice", false),
          ("sync ready", "bubble", false),
          ("sync ready", "bubble bubble", false),
          ("sync accept", "", true),
          ("sync accept", "wire", false)
        )
      } {

        s"'${fc}' with '${st}'" in {
          val tree = s"""|network a {
                         |   in  ${fc}       bool pi;
                         |   out ${fc} ${st} bool po;
                         |   pi -> po;
                         |}""".stripMargin.asTree[Entity]
          xform(tree)
          if (ok) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](
              "Port driven by '->' must not specify output storage",
              "'->' is at:.*"
            )
          }
        }
      }
    }

    "error for initializer expression on port driven by '->'" - {
      for {
        (init, ok) <- List(
          ("/* noinit */", true),
          ("= false", false)
        )
      } {
        init in {
          val tree = s"""|network a {
                         |   in  bool pi;
                         |   out bool po ${init};
                         |   pi -> po;
                         |}""".stripMargin.asTree[Entity]
          xform(tree)
          if (ok) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](
              "Port driven by '->' must not have an initializer",
              "'->' is at:.*"
            )
          }
        }
      }
    }

    "no error for const on RHS of '->'" - {
      for {
        init <- List(
          ("const uint A = 2;"),
          ("const u2   A = 2'd2;")
        )
      } {
        init in {
          val tree = s"""|network a {
                         |   ${init}
                         |   in  bool pi;
                         |   out u4   po;
                         |   pi -> po[A];
                         |}""".stripMargin.asTree[Entity]
          xform(tree)
          cc.messages shouldBe empty
        }
      }
    }

    "port select" - {
      for {
        (expr, msg) <- List(
          ("a.b", ""),
          ("a.c.x", ""),
          ("a.d", "No port named 'd' on instance 'a' of entity 'a'"),
          ("a.d.x", "No port named 'd' on instance 'a' of entity 'a'"),
          ("a.N", "No port named 'N' on instance 'a' of entity 'a'")
        )
      } {
        expr in {
          val tree = s"""|struct bar {
                         |  bool x;
                         |}
                         |network n {
                         |  (* unused *) in bool p;
                         |  (* unused *) new fsm a {
                         |    (* unused *) param u8 N = 8'd2;
                         |    (* unused *) in bool b;
                         |    (* unused *) in bar  c;
                         |  }
                         |  p -> ${expr};
                         |}""".stripMargin.asTree[Root]
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
