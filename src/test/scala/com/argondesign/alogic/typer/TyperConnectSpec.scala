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
        case root: Root     => root.entity
        case entity: Entity => entity
        case _              => unreachable
      }
    }

    cc.addGlobalEntities(entities)

    trees map {
      _ rewrite new Checker
    } map {
      _ rewrite new Namer
    } foreach {
      _ rewrite new Typer
    }
  }

  val treeA = s"""|fsm a {
                  |  (* unused *) param           bool P = true;
                  |  (* unused *) out             bool fcn;
                  |  (* unused *) out sync        bool fcv;
                  |  (* unused *) out sync ready  bool fcr;
                  |  (* unused *) out sync accept bool fca;
                  |}""".stripMargin.asTree[Entity]
  val treeB = s"""|fsm b {
                  |  (* unused *) param           bool P = true;
                  |  (* unused *) in              bool fcn;
                  |  (* unused *) in sync         bool fcv;
                  |  (* unused *) in sync ready   bool fcr;
                  |  (* unused *) in sync accept  bool fca;
                  |}""".stripMargin.asTree[Entity]

  "The Typer should check Connect usage" - {

    "widths matching" - {
      for {
        (expr, widths) <- List(
          ("pi0 -> po0", Nil),
          ("pi1 -> po1", Nil),
          ("pi2 -> po2", Nil),
          ("pi3 -> po3", Nil),
          ("pi0 -> po1", List(0, 1)),
          ("pi0 -> po2", List(0, 2)),
          ("pi0 -> po3", List(0, 3)),
          ("pi1 -> po0", List(1, 0)),
          ("pi1 -> po2", List(1, 2)),
          ("pi1 -> po3", List(1, 3)),
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
        expr in {
          val tree = s"""|network p {
                         |  (* unused *) in void pi0;
                         |  (* unused *) out void po0;
                         |  (* unused *) in u1 pi1;
                         |  (* unused *) out u1 po1;
                         |  (* unused *) in u2 pi2;
                         |  (* unused *) out u2 po2;
                         |  (* unused *) in u3 pi3;
                         |  (* unused *) out u3 po3;
                         |  ${expr};
                         |}""".stripMargin.asTree[Entity]
          xform(tree)
          if (widths.isEmpty) {
            cc.messages shouldBe empty
          } else {
            val lw :: rws = widths
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
        (expr, msg) <- List(
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
        expr in {
          val tree = s"""|network p {
                         |  (* unused *) in u1 pi1;
                         |  (* unused *) out u1 po1;
                         |  (* unused *) new fsm pipea {}
                         |  (* unused *) new fsm pipeb {}
                         |  (* unused *) new fsm pipec {}
                         |  ${expr};
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
          ("b.fcn -> b.fcn", "Left hand side of '->' is an input to instance 'b'"),
          ("b.fcv -> b.fcv", "Left hand side of '->' is an input to instance 'b'"),
          ("b.fcr -> b.fcr", "Left hand side of '->' is an input to instance 'b'"),
          ("b.fca -> b.fca", "Left hand side of '->' is an input to instance 'b'"),
          ("ifcn -> b.fcn", ""),
          ("ifcv -> b.fcv", ""),
          ("ifcr -> b.fcr", ""),
          ("ifca -> b.fca", ""),
          ("ofcn -> b.fcn", "Left hand side of '->' is an output from enclosing entity"),
          ("ofcv -> b.fcv", "Left hand side of '->' is an output from enclosing entity"),
          ("ofcr -> b.fcr", "Left hand side of '->' is an output from enclosing entity"),
          ("ofca -> b.fca", "Left hand side of '->' is an output from enclosing entity"),
          ("P -> b.fcn", "Left hand side of '->' is of non-port type: param u1")
        )
      } {
        conn in {
          val treeC =
            s"""|network  foo {
                |  (* unused *) param bool P = true;
                |  (* unused *) in              bool ifcn;
                |  (* unused *) in sync         bool ifcv;
                |  (* unused *) in sync ready   bool ifcr;
                |  (* unused *) in sync accept  bool ifca;
                |  (* unused *) out             bool ofcn;
                |  (* unused *) out sync        bool ofcv;
                |  (* unused *) out sync ready  bool ofcr;
                |  (* unused *) out sync accept bool ofca;
                |  (* unused *) a = new a();
                |  (* unused *) b = new b();
                |  ${conn};
                |}""".stripMargin.asTree[Entity]

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
          ("a.fcn -> a.fcn", "Right hand side of '->' is an output from instance 'a'"),
          ("a.fcv -> a.fcv", "Right hand side of '->' is an output from instance 'a'"),
          ("a.fcr -> a.fcr", "Right hand side of '->' is an output from instance 'a'"),
          ("a.fca -> a.fca", "Right hand side of '->' is an output from instance 'a'"),
          ("a.fcn -> ifcn", "Right hand side of '->' is an input to enclosing entity"),
          ("a.fcv -> ifcv", "Right hand side of '->' is an input to enclosing entity"),
          ("a.fcr -> ifcr", "Right hand side of '->' is an input to enclosing entity"),
          ("a.fca -> ifca", "Right hand side of '->' is an input to enclosing entity"),
          ("a.fcn -> ofcn", ""),
          ("a.fcv -> ofcv", ""),
          ("a.fcr -> ofcr", ""),
          ("a.fca -> ofca", ""),
          ("a.fcn -> P", "Right hand side of '->' is of non-port type: param u1")
        )
      } {
        conn in {
          val treeC =
            s"""|network  foo {
                |  (* unused *) param bool P = true;
                |  (* unused *) in              bool ifcn;
                |  (* unused *) in sync         bool ifcv;
                |  (* unused *) in sync ready   bool ifcr;
                |  (* unused *) in sync accept  bool ifca;
                |  (* unused *) out             bool ofcn;
                |  (* unused *) out sync        bool ofcv;
                |  (* unused *) out sync ready  bool ofcr;
                |  (* unused *) out sync accept bool ofca;
                |  (* unused *) a = new a();
                |  (* unused *) b = new b();
                |  ${conn};
                |}""".stripMargin.asTree[Entity]

          xform(treeA, treeB, treeC)

          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](msg)
          }
        }
      }
    }

    "ports have compatible flow control" - {

      def mkMsg(l: String, r: String) = s"Ports '${l}' and '${r}' have incompatible flow control"

      for {
        (connect, msg) <- List(
          ("a.fcn -> b.fcn", Nil),
          ("a.fcn -> b.fcv", List(mkMsg("a.fcn", "b.fcv"), "none -> sync")),
          ("a.fcn -> b.fcr", List(mkMsg("a.fcn", "b.fcr"), "none -> sync ready")),
          ("a.fcn -> b.fca", List(mkMsg("a.fcn", "b.fca"), "none -> sync accept")),
          ("a.fcv -> b.fcn", List(mkMsg("a.fcv", "b.fcn"), "sync -> none")),
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
            s"""|network  foo {
                |  (* unused *) a = new a();
                |  (* unused *) b = new b();
                |  ${connect};
                |}""".stripMargin.asTree[Entity]

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
          ("a.fcv -> b.fcv, ofcv", ""),
          ("a.fcr -> b.fcr, ofcr",
           "Port with 'sync ready' flow control cannot have multiple sinks"),
          ("a.fca -> b.fca, ofca",
           "Port with 'sync accept' flow control cannot have multiple sinks")
        )
      } {
        conn in {
          val treeC =
            s"""|network  foo {
                |  (* unused *) out             bool ofcn;
                |  (* unused *) out sync        bool ofcv;
                |  (* unused *) out sync ready  bool ofcr;
                |  (* unused *) out sync accept bool ofca;
                |  (* unused *) a = new a();
                |  (* unused *) b = new b();
                |  ${conn};
                |}""".stripMargin.asTree[Entity]

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

  }
}
