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
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.Messages.Note
import com.argondesign.alogic.passes.Elaborate
import com.argondesign.alogic.passes.Namer
import com.argondesign.alogic.passes.TypeCheck
import org.scalatest.freespec.AnyFreeSpec

final class TyperConnectSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  private def typeCheck(text: String): Thicket = Thicket {
    transformWithPass(Namer andThen Elaborate andThen TypeCheck, text) map {
      _ flatMap {
        case (decl, defn) =>
          List(decl, defn)
      }
    } getOrElse Nil
  }

  private val fsmA = s"""fsm a_entity {
                        |  out             bool fcn;
                        |  out             bool fcnb;
                        |  out             u2   fcn2;
                        |  out sync        bool fcv;
                        |  out sync ready  bool fcr;
                        |}""".stripMargin
  private val fsmB = s"""fsm b_entity {
                        |  in              bool fcn;
                        |  in              bool fcnb;
                        |  in              u2   fcn2;
                        |  in sync         bool fcv;
                        |  in sync         u2   fcv2;
                        |  in sync ready   bool fcr;
                        |}""".stripMargin

  "The Typer should check Connect usage" - {

    "reject invalid port references on left hand side of ->" - {
      for {
        (conn, messages) <- List[(String, List[String])](
          // format: off
          ("pi2.read() -> po2", "Left hand side of '->' yields active logic" :: Nil),
          ("-pi2 -> po2", "Left hand side of '->' yields active logic" :: Nil),
          ("pi2 + pi2b -> po2", "Left hand side of '->' yields active logic" :: Nil),
          ("(pi2==0) ? pi2 : pi2b -> po2", "Left hand side of '->' yields active logic" :: Nil),
          ("{2{pi4[pi2]}} -> po2", "Left hand side of '->' yields active logic" :: Nil),
          ("@zx(2, pi4[pi2]) -> po2", "Left hand side of '->' yields active logic" :: Nil),
          ("$signed(2*pi2) -> po2", "Left hand side of '->' yields active logic" :: Nil),
          ("$display() -> po2", "Left hand side of '->' yields active logic" :: Nil),
          ("$finish() -> po2", "Left hand side of '->' yields active logic" :: Nil),
          ("@randbit() -> po2", "Left hand side of '->' yields active logic" :: Nil),
          ("1 -> po2", "Expression on left hand side of '->' is not a valid source." :: Nil),
          (""" "hello" -> po2""", "Expression on left hand side of '->' is not a valid source." :: Nil),
          ("2'd1 << pi2 -> po2", "Left hand side of '->' yields active logic" :: Nil),
          ("2'sd1 <<< pi2 -> po2", "Left hand side of '->' yields active logic" :: Nil),
          ("2'd1 >> pi2 -> po2", "Left hand side of '->' yields active logic" :: Nil),
          ("2'sd1 >>> pi2 -> po2", "Left hand side of '->' yields active logic" :: Nil),
          ("(pi2+1) << 1 -> po2", "Left hand side of '->' yields active logic" :: Nil),
          ("$signed(pi2+1) <<< 1 -> po2", "Left hand side of '->' yields active logic" :: Nil),
          ("(pi2+1) >> 1 -> po2", "Left hand side of '->' yields active logic" :: Nil),
          ("$signed(pi2+1) >>> 1 -> po2", "Left hand side of '->' yields active logic" :: Nil),
          ("pi4[pi2] -> po1", "Left hand side of '->' yields active logic" :: Nil),
          ("pi4[pi2+:2] -> po2", "Left hand side of '->' yields active logic" :: Nil),
          ("{pi4[pi2], pi4[pi2]} -> po2",
            "Left hand side of '->' yields active logic" ::
              "Left hand side of '->' yields active logic" ::
              Nil),
          ("bar.a -> po1", "Expression on left hand side of '->' is not a valid source." :: Nil)
          // format: on
        )
      } {
        conn in {
          typeCheck {
            s"""
            |struct bar {
            |  bool a;
            |}
            |
            |(*toplevel*)
            |network p {
            |  in u2 pi2;
            |  in u2 pi2b;
            |  in u4 pi4;
            |  out u1 po1;
            |  out u2 po2;
            |  $conn;
            |}"""
          }
          cc.messages should have length messages.length
          for ((message, expected) <- cc.messages zip messages) {
            message should beThe[Error](expected)
          }
        }
      }
    }

    "reject invalid port references on right hand side of ->" - {
      for {
        (conn, messages) <- List[(String, List[String])](
          // format: off
          ("pi2 -> -po2", "Right hand side of '->' is not a valid assignment target" :: Nil),
          ("pi2 -> po2 + po2b", "Right hand side of '->' is not a valid assignment target" :: Nil),
          ("pi2 -> (po2==0) ? po2 : po2b", "Right hand side of '->' is not a valid assignment target" :: Nil),
          ("pi2 -> {2{po4[po2]}}", "Right hand side of '->' is not a valid assignment target" :: Nil),
          ("pi2 -> @zx(2, po4[po2])", "Right hand side of '->' is not a valid assignment target" :: Nil),
          ("pi2 -> $signed(2*po2)", "Right hand side of '->' is not a valid assignment target" :: Nil),
          ("pi2 -> 1", "Right hand side of '->' is not a valid assignment target" :: Nil),
          ("""pi2 -> "hello" """, "Right hand side of '->' is not a valid assignment target" :: Nil),
          ("pi1 -> po4[po2]", "Right hand side of '->' yields active logic" :: Nil),
          ("pi2 -> po4[po2+:2]", "Right hand side of '->' yields active logic" :: Nil),
          ("pi2 -> {po4[po2], po4[po2]}",
            "Right hand side of '->' yields active logic" ::
              "Right hand side of '->' yields active logic" :: 
              Nil),
          // format: on
        )
      } {
        conn in {
          typeCheck {
            s"""
            |network p {
            |  in u1 pi1;
            |  in u2 pi2;
            |  out u2 po2;
            |  out sync ready u2 po2sr;
            |  out u2 po2b;
            |  out u4 po4;
            |  $conn;
            |}"""
          }
          cc.messages should have length messages.length
          for ((message, expected) <- cc.messages zip messages) {
            message should beThe[Error](expected)
          }
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
          "pi2 << 1 -> po2",
          "$signed(pi2) <<< 1 -> po2",
          "pi2 >> 1 -> po2",
          "$signed(pi2) >>> 1 -> po2",
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
          typeCheck {
            s"""
            |struct bar {
            |  u4 b;
            |}
            |struct foo {
            |  bar a;
            |}
            |(* toplevel *)
            |network p {
            |  const u2 P = 2'd2;
            |  in u1 pi1;
            |  in u2 pi2;
            |  in u4 pi4;
            |  in foo si4;
            |  out u1 po1;
            |  out u2 po2;
            |  out u4 po4;
            |  $conn;
            |}"""
          }
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
          typeCheck {
            s"""
            |struct bar {
            |  u4 b;
            |}
            |struct foo {
            |  bar a;
            |}
            |(* toplevel *)
            |network p {
            |  in u1 pi1;
            |  in u2 pi2;
            |  in u4 pi4;
            |  out u2 po2;
            |  out u4 po4;
            |  out foo so4;
            |  $conn;
            |}"""
          }
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
          typeCheck {
            s"""
            |struct foo {
            |  u2 a;
            |  u2 b;
            |}
            |(* toplevel *)
            |network p {
            |  in void pi0;
            |  out void po0;
            |  in u1 pi1;
            |  out u1 po1;
            |  in u2 pi2;
            |  out u2 po2;
            |  in u3 pi3;
            |  out u3 po3;
            |  in foo si4;
            |  $conn;
            |}"""
          }
          widths match {
            case Nil =>
              cc.messages shouldBe empty
            case lw :: rws =>
              cc.messages.length shouldBe rws.length
              for ((msg, rw) <- cc.messages zip rws) {
                msg should beThe[Error](s"Connected ports have mismatched widths: $lw -> $rw")
              }
          }
        }
      }
    }

    "nested entity pipeline port usage" - {
      for {
        (conn, messages) <- List[(String, List[List[String]])](
          ("pipea     -> pipeb", Nil),
          ("pipea     -> pipeb.in", Nil),
          ("pipea.out -> pipeb", Nil),
          ("pipea.out -> pipeb.in", Nil),
          ("pipea -> pipec", Nil),
          (
            "pipeb -> pipea",
            List(
              "Expression on right hand side of '->' is not a valid sink." ::
                "'pipea' has no cardinal input port." :: Nil
            )
          ),
          ("pipeb -> pipec", Nil),
          (
            "pipec -> pipea",
            List(
              "Expression on left hand side of '->' is not a valid source." ::
                "'pipec' has no cardinal output port." :: Nil,
              "Expression on right hand side of '->' is not a valid sink." ::
                "'pipea' has no cardinal input port." :: Nil
            )
          ),
          (
            "pipec -> pipeb",
            List(
              "Expression on left hand side of '->' is not a valid source." ::
                "'pipec' has no cardinal output port." :: Nil
            )
          ),
          ("pipea -> po1", List("Cannot connect pipeline port to non-pipeline port" :: Nil)),
          ("pi1 -> pipec", List("Cannot connect non-pipeline port to pipeline port" :: Nil)),
          (
            "pipec.in -> pipeb",
            List(
              "Expression on left hand side of '->' is not a valid source." ::
                "It is an input of the referenced instance." :: Nil
            )
          ),
          (
            "pipeb -> pipea.out",
            List(
              "Expression on right hand side of '->' is not a valid sink." ::
                "It is an output of the referenced instance." :: Nil
            )
          )
        )
      } {
        conn in {
          typeCheck {
            s"""
            |network p {
            |  in u1 pi1;
            |  out u1 po1;
            |  new fsm pipea {
            |    out pipeline;
            |  }
            |  new fsm pipeb {
            |    in pipeline;
            |    out pipeline;
            |  }
            |  new fsm pipec {
            |    in pipeline;
            |  }
            |  $conn;
            |}"""
          }
          cc.messages should have length messages.length
          for ((message, expected) <- cc.messages zip messages) {
            message should beThe[Error](expected: _*)
          }
        }
      }
    }

    "connections have drivers on left" - {
      for {
        (conn, messages) <- List[(String, List[(Boolean, List[String])])](
          // format: off
          ("a -> b.fcn",
            List(
              (true, "Expression on left hand side of '->' is not a valid source." ::
                "'a' has no cardinal output port." :: Nil),
              (false, "'a' is an instance of:" :: Nil)
            )),
          ("ac -> ofcn", Nil),
          ("a.fcn -> b.fcn", Nil),
          ("a.fcv -> b.fcv", Nil),
          ("a.fcr -> b.fcr", Nil),
          ("{a.fcn, a.fcn} -> b.fcn2", Nil),
          ("b.fcn -> b.fcn",
            List(
              (true, "Expression on left hand side of '->' is not a valid source." ::
                "It is an input of the referenced instance." :: Nil)
            )),
          ("b.fcv -> b.fcv",
            List(
              (true, "Expression on left hand side of '->' is not a valid source." ::
                "It is an input of the referenced instance." :: Nil)
            )),
          ("b.fcr -> b.fcr",
            List(
              (true, "Expression on left hand side of '->' is not a valid source." ::
                "It is an input of the referenced instance." :: Nil)
            )),
          ("{b.fcn, b.fcnb} -> b.fcn2",
            List(
              (true, "Expression on left hand side of '->' is not a valid source." ::
                "It is an input of the referenced instance." :: Nil),
              (true, "Expression on left hand side of '->' is not a valid source." ::
                "It is an input of the referenced instance." :: Nil)
            )),
          ("ifcn -> b.fcn", Nil),
          ("ifcv -> b.fcv", Nil),
          ("ifcr -> b.fcr", Nil),
          ("{ifcn, ifcn} -> b.fcn2", Nil),
          ("ofcn -> b.fcn",
            List(
              (true, "Expression on left hand side of '->' is not a valid source." ::
                "'ofcn' is an output of an enclosing entity." :: Nil)
            )),
          ("ofcv -> b.fcv",
            List(
              (true, "Expression on left hand side of '->' is not a valid source." ::
                "'ofcv' is an output of an enclosing entity." :: Nil)
            )),
          ("ofcr -> b.fcr",
            List(
              (true, "Expression on left hand side of '->' is not a valid source." ::
                "'ofcr' is an output of an enclosing entity." :: Nil)
            )),
          ("{ofcn, ofcn} -> b.fcn2",
            List(
              (true, "Expression on left hand side of '->' is not a valid source." ::
                "'ofcn' is an output of an enclosing entity." :: Nil),
              (true, "Expression on left hand side of '->' is not a valid source." ::
                "'ofcn' is an output of an enclosing entity." :: Nil)
            )),
          ("P -> b.fcn", Nil),
          ("isn.a -> b.fcn", Nil),
          ("a_entity -> ofcn",
            List(
              (true, "Expression on left hand side of '->' is not a valid source." :: Nil)
            ))
          // format: on
        )
      } {
        conn in {
          typeCheck {
            s"""
            |$fsmA
            |
            |$fsmB
            |
            |struct bar {
            |  bool a;
            |}
            |
            |(* toplevel *)
            |network  foo {
            |  const bool P = true;
            |  in              bool ifcn;
            |  in              bar  isn;
            |  in sync         bool ifcv;
            |  in sync ready   bool ifcr;
            |  out             bool ofcn;
            |  out sync        bool ofcv;
            |  out sync ready  bool ofcr;
            |  a = new a_entity;
            |  b = new b_entity;
            |
            |  new fsm ac {
            |    out bool;
            |  }
            |
            |  $conn;
            |}"""
          }
          if (messages.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages should have length messages.length
            for ((message, (isError, expected)) <- cc.messages zip messages) {
              if (isError) {
                message should beThe[Error](expected: _*)
              } else {
                message should beThe[Note](expected: _*)
              }
            }
          }
        }
      }
    }

    "connections have sinks on right" - {
      for {
        (conn, messages) <- List[(String, List[(Boolean, List[String])])](
          // format: off
          ("a.fcn -> b",
            List(
              (true, "Expression on right hand side of '->' is not a valid sink." ::
                "'b' has no cardinal input port." :: Nil),
              (false, "'b' is an instance of:" :: Nil),
            )),
          ("ifcn -> ac", Nil),
          ("a.fcn -> b.fcn", Nil),
          ("a.fcv -> b.fcv", Nil),
          ("a.fcr -> b.fcr", Nil),
          ("a.fcn2 -> {b.fcn, b.fcnb}", Nil),
          ("a.fcn -> a.fcn",
            List(
              (true, "Expression on right hand side of '->' is not a valid sink." ::
                "It is an output of the referenced instance." :: Nil)
            )),
          ("a.fcv -> a.fcv",
            List(
              (true, "Expression on right hand side of '->' is not a valid sink." ::
                "It is an output of the referenced instance." :: Nil)
            )),
          ("a.fcr -> a.fcr",
            List(
              (true, "Expression on right hand side of '->' is not a valid sink." ::
                "It is an output of the referenced instance." :: Nil)
            )),
          ("a.fcn2 -> {a.fcn, a.fcnb}",
            List(
              (true, "Expression on right hand side of '->' is not a valid sink." ::
                "It is an output of the referenced instance." :: Nil),
              (true, "Expression on right hand side of '->' is not a valid sink." ::
                "It is an output of the referenced instance." :: Nil)
            )),
          ("a.fcn -> ifcn",
            List(
              (true, "Expression on right hand side of '->' is not a valid sink." ::
                "'ifcn' is an input of an enclosing entity." :: Nil)
            )),
          ("a.fcv -> ifcv",
            List(
              (true, "Expression on right hand side of '->' is not a valid sink." ::
                "'ifcv' is an input of an enclosing entity." :: Nil)
            )),
          ("a.fcr -> ifcr",
            List(
              (true, "Expression on right hand side of '->' is not a valid sink." ::
                "'ifcr' is an input of an enclosing entity." :: Nil)
            )),
          ("a.fcn2 -> {ifcn,ifcnb}",
            List(
              (true, "Expression on right hand side of '->' is not a valid sink." ::
                "'ifcn' is an input of an enclosing entity." :: Nil),
              (true, "Expression on right hand side of '->' is not a valid sink." ::
                "'ifcnb' is an input of an enclosing entity." :: Nil)
            )),
          ("a.fcn -> ofcn", Nil),
          ("a.fcv -> ofcv", Nil),
          ("a.fcr -> ofcr", Nil),
          ("a.fcn2 -> {ofcn,ofcnb}", Nil),
          ("a.fcn -> P", List((true, "Expression on right hand side of '->' is not a valid sink." :: Nil))),
          ("a.fcn -> osn.a", Nil),
          ("ifcn -> b_entity", List((true, "Expression on right hand side of '->' is not a valid sink." :: Nil))),
          ("ifcn -> bar.a", List((true, "Expression on right hand side of '->' is not a valid sink." :: Nil)))
          // format: off
        )
      } {
        conn in {
          typeCheck {
            s"""
            |$fsmA
            |
            |$fsmB
            |
            |struct bar {
            |  bool a;
            |}
            |
            |(* toplevel *)
            |network  foo {
            |  const bool P = true;
            |  in              bool ifcn;
            |  in              bool ifcnb;
            |  in sync         bool ifcv;
            |  in sync ready   bool ifcr;
            |  out             bar  osn;
            |  out             bool ofcn;
            |  out             bool ofcnb;
            |  out sync        bool ofcv;
            |  out sync ready  bool ofcr;
            |  a = new a_entity;
            |  b = new b_entity;
            |
            |  new fsm ac {
            |    in bool;
            |  }
            |
            |  $conn;
            |}"""
          }
          if (messages.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages should have length messages.length
            for ((message, (isError, expected)) <- cc.messages zip messages) {
              if (isError) {
                message should beThe[Error](expected: _*)
              } else {
                message should beThe[Note](expected: _*)
              }
            }
          }
        }
      }
    }

    "complex connect expressions have no flow control" - {
      val msg ="Port with flow control cannot be connected in a complex expression"

      for {
        (connect, msgs) <- List(
          ("{a.fcn, a.fcv} -> b.fcn2", List(msg)),
          ("{a.fcn, a.fcr} -> b.fcn2", List(msg)),
          ("{a.fcv, a.fcv} -> b.fcn2", List(msg, msg)),
          ("$signed(a.fcv) -> b.fcv", List(msg)),
          ("a.fcn2 -> {b.fcn, b.fcv}", List(msg)),
          ("a.fcn2 -> {b.fcr, b.fcr}", List(msg, msg)),
          ("is.a -> b.fcn", List(msg))
        )
      } {
        connect in {
          typeCheck {
            s"""
            |$fsmA
            |
            |$fsmB
            |
            |struct bar {
            |  bool a;
            |}
            |
            |(* toplevel *)
            |network  foo {
            |  in sync bar is;
            |  a = new a_entity;
            |  b = new b_entity;
            |  $connect;
            |}"""
          }
          cc.messages.length shouldBe msgs.length
          for ((msg, expected) <- cc.messages zip msgs) {
            msg should beThe[Error](expected)
          }
        }
      }
    }

    "ports have compatible flow control" - {
      for {
        // format: off
        (connect, msg) <- List(
          ("a.fcn -> b.fcn", Nil),
          ("{a.fcn, a.fcn} -> b.fcn2", Nil),
          ("a.fcn -> b.fcv", List("Ports have incompatible flow control", "'none' -> 'sync'")),
          ("a.fcn -> b.fcr", List("Ports have incompatible flow control", "'none' -> 'sync ready'")),
          ("a.fcv -> b.fcn", List("Ports have incompatible flow control", "'sync' -> 'none'")),
          ("{a.fcn, a.fcn} -> b.fcv2", List("Ports have incompatible flow control", "'none' -> 'sync'")),
          ("is.a -> b.fcv2", List("Ports have incompatible flow control", "'none' -> 'sync'")),
          ("a.fcv -> b.fcv", Nil),
          ("a.fcv -> b.fcr", List("Ports have incompatible flow control", "'sync' -> 'sync ready'")),
          ("a.fcr -> b.fcn", List("Ports have incompatible flow control", "'sync ready' -> 'none'")),
          ("a.fcr -> b.fcv", List("Ports have incompatible flow control", "'sync ready' -> 'sync'")),
          ("a.fcr -> b.fcr", Nil)
        )
        // format: on
      } {
        connect in {
          typeCheck {
            s"""
            |$fsmA
            |
            |$fsmB
            |
            |struct bar {
            |  u2 a;
            |}
            |
            |(* toplevel *)
            |network  foo {
            |  in bar is;
            |  a = new a_entity;
            |  b = new b_entity;
            |  $connect;
            |}"""
          }
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
          // format: off
          ("a.fcn -> b.fcn, ofcn", ""),
          ("a.fcn2 -> {b.fcn, b.fcnb}, ofcn2", ""),
          ("a.fcn2 -> os.a, ofcn2", ""),
          ("a.fcv -> b.fcv, ofcv", ""),
          ("a.fcr -> b.fcr, ofcr", "Port with 'sync ready' flow control cannot have multiple sinks")
          // format: on
        )
      } {
        conn in {
          typeCheck {
            s"""
            |$fsmA
            |
            |$fsmB
            |
            |struct bar {
            |  u2 a;
            |}
            |
            |(* toplevel *)
            |network  foo {
            |  out             bool ofcn;
            |  out             u2   ofcn2;
            |  out             bar  os;
            |  out sync        bool ofcv;
            |  out sync ready  bool ofcr;
            |  a = new a_entity;
            |  b = new b_entity;
            |  $conn;
            |}"""
          }
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
          ("sync ready", "bubble bubble", false)
        )
      } {

        s"'$fc' with '$st'" in {
          typeCheck {
            s"""
            |network a {
            |   in  $fc       bool pi;
            |   out $fc $st bool po;
            |   pi -> po;
            |}"""
          }
          if (ok) {
            cc.messages shouldBe empty
          } else {
            cc.messages should have length 2
            cc.messages(0) should beThe[Error](
              "Output port driven by '->' cannot have a storage specifier"
            )
            cc.messages(1) should beThe[Note](
              "'.*' is driven here"
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
          typeCheck {
            s"""
            |network a {
            |   in  bool pi;
            |   out bool po $init;
            |   pi -> po;
            |}"""
          }
          if (ok) {
            cc.messages shouldBe empty
          } else {
            cc.messages should have length 2
            cc.messages(0) should beThe[Error](
              "Output port driven by '->' cannot have an initializer"
            )
            cc.messages(1) should beThe[Note](
              "'.*' is driven here"
            )
          }
        }
      }
    }

    "no error for const on RHS of '->'" - {
      for {
        init <- List(
          "const uint A = 2;",
          "const u2   A = 2'd2;"
        )
      } {
        init in {
          typeCheck {
            s"""
            |network a {
            |   $init
            |   in  bool pi;
            |   out u4   po;
            |   pi -> po[A];
            |}"""
          }
          cc.messages shouldBe empty
        }
      }
    }

    "port select" - {
      for {
        (expr, msg) <- List(
          ("a.b", ""),
          ("a.c.x", ""),
          ("a.d", "No port named 'd' on instance 'a'"),
          ("a.d.x", "No port named 'd' on instance 'a'"),
          ("a.N", "No port named 'N' on instance 'a'")
        )
      } {
        expr in {
          typeCheck {
            s"""
            |struct bar {
            |  bool x;
            |}
            |
            |(* toplevel *)
            |network n {
            |  in bool p;
            |  new fsm a {
            |    const u8 N = 8'd2;
            |    in bool b;
            |    in bar  c;
            |  }
            |  p -> $expr;
            |}"""
          }
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
