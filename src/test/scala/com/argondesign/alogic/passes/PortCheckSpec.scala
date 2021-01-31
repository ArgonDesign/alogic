////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.Messages.Note
import com.argondesign.alogic.core.Messages.Warning
import org.scalatest.freespec.AnyFreeSpec

final class PortCheckSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  protected def portCheck(text: String): Unit = transformWithPass(
    FrontendPass andThen
      DropPackageAndParametrizedDescs andThen
      DescToDeclDefn andThen
      Desugar andThen
      Fold andThen
      PortCheck,
    text
  )

  "PortCheck should" - {

    "error for invalid storage specifiers" - {
      for {
        kind <- List("bool", "pipeline")
        (decl, msg) <- List(
          // format: off
          (s"$kind",                          ""),
          (s"wire $kind",                     ""),
          (s"sync $kind",                     ""),
          (s"sync wire $kind",                ""),
          (s"sync ready $kind",               ""),
          (s"sync ready fslice $kind",        ""),
          (s"sync ready bslice $kind",        ""),
          (s"sync ready bubble $kind",        ""),
          (s"sync ready bubble bubble $kind", ""),
          (s"sync ready wire $kind",          "'sync ready' port cannot use 'wire' storage specifier"),
          // format: on
        )
      } {
        decl in {
          portCheck {
            s"""
               |network n {
               |  fsm a {
               |    out $decl po;
               |  }
               |}""".stripMargin
          }
          val messages = cc.messages.filterNot(_.isInstanceOf[Warning])
          if (msg.isEmpty) {
            messages shouldBe empty
          } else {
            messages.loneElement should beThe[Error](msg)
          }
        }
      }
    }

    "error for multiple sinks for sync ready driver" - {
      for {
        (conn, badConnects) <- List(
          ("pi_n -> po_n_a;", None),
          ("pi_n -> po_n_b;", None),
          ("n.po_n -> po_n_a;", None),
          ("n.po_n -> po_n_b;", None),
          ("pi_n -> po_n_a; pi_n -> po_n_b;", Some(2)),
          ("n.po_n -> po_n_a; n.po_n -> po_n_b;", Some(2)),
          ("pi_n -> po_n_a; pi_n -> po_n_b; pi_n -> po_n_c;", Some(3)),
          ("pi_n -> po_n_a; pi_n -> po_n_b; pi_n -> po_n_c; pi_n -> po_n_d;", Some(4)),
          ("pi_y -> po_y_a;", None),
          ("pi_y -> po_y_b;", None),
          ("n.po_y -> po_y_a;", None),
          ("n.po_y -> po_y_b;", None),
          ("pi_y -> po_y_a; pi_y -> po_y_b;", None),
          ("n.po_y -> po_y_a; n.po_y -> po_y_b;", None),
          ("p0.o -> p1.i;", None),
          ("p0.o -> p1.i; p0.o -> p2.i;", Some(2))
        )
      } {
        conn in {
          portCheck {
            s"""
               |network a {
               |  in sync ready bool pi_n;
               |  out sync ready bool po_n_a;
               |  out sync ready bool po_n_b;
               |  out sync ready bool po_n_c;
               |  out sync ready bool po_n_d;
               |  in sync bool pi_y;
               |  out sync bool po_y_a;
               |  out sync bool po_y_b;
               |
               |  new fsm n {
               |    out sync ready bool po_n;
               |    out sync bool po_y;
               |  }
               |
               |  new fsm p0 {
               |    out sync ready pipeline o;
               |  }
               |  new fsm p1 {
               |    in  sync ready pipeline i;
               |  }
                  new fsm p2 {
               |    in  sync ready pipeline i;
               |  }
               |
               |  $conn
               |}""".stripMargin
          }
          val messages = cc.messages.filterNot(_.isInstanceOf[Warning])
          badConnects match {
            case None => messages shouldBe empty
            case Some(count) =>
              messages should have length count + 1
              messages(0) should beThe[Error](
                "Port '.*' with 'sync ready' flow control has multiple sinks"
              )
              for (i <- 1 to count) {
                messages(i) should beThe[Note](
                  s"The $i(st|nd|rd|th) sink is here"
                )
              }
          }
        }
      }
    }

    "error for multiple drivers for simple sink" - {
      for {
        (conn, badConnects) <- List(
          ("pia -> po;", None),
          ("pia -> n.pi;", None),
          ("pib -> po;", None),
          ("pib -> n.pi;", None),
          ("pia -> po, n.pi;", None),
          ("pib -> po, n.pi;", None),
          ("pia -> po, po;", Some(2)),
          ("pia -> n.pi, n.pi;", Some(2)),
          ("pia -> po; pib -> n.pi;", None),
          ("pia -> po; pib -> po;", Some(2)),
          ("pia -> n.pi; pib -> n.pi;", Some(2))
        )
      } {
        conn in {
          portCheck {
            s"""
               |network a {
               |  in bool pia;
               |  in bool pib;
               |  out bool po;
               |
               |  new fsm n {
               |    in bool pi;
               |    out bool npo;
               |    fence { npo = pi; }
               |  }
               |
               |  $conn
               |}""".stripMargin
          }
          val messages = cc.messages.filterNot(_.isInstanceOf[Warning])
          badConnects match {
            case None => messages shouldBe empty
            case Some(count) =>
              messages should have length count + 1
              messages(0) should beThe[Error](
                "Port '.*' has multiple drivers"
              )
              for (i <- 1 to count) {
                messages(i) should beThe[Note](
                  s"The $i(st|nd|rd|th) driver is here"
                )
              }
          }
        }
      }
    }

    "not error for multiple drivers for complex sinks" in {
      portCheck {
        s"""
           |network p {
           |  in  u1 pi;
           |  out u2 po;
           |  pi -> po[0]; pi -> po[0];
           |}""".stripMargin
      }
      cc.messages shouldBe empty
    }

    "not error when pipeline connects overlap with port selections" in {
      portCheck {
        s"""
           |network p {
           |
           |  in  u8 pi;
           |  out u8 po;
           |
           |  pipeline u8 x;
           |
           |  new fsm stage_0 {
           |    in u1 i;
           |    out sync ready pipeline;
           |    void main() {
           |      x = pi + 'i;
           |      out.write();
           |      fence;
           |    }
           |  }
           |
           |  new fsm stage_1 {
           |    in sync ready pipeline;
           |    out u1 o;
           |    void main() {
           |      in.read();
           |      po = x;
           |      o = x[0];
           |      fence;
           |    }
           |  }
           |
           |  stage_0 -> stage_1;
           |  stage_1.o -> stage_0.i;
           |}""".stripMargin
      }
      cc.messages shouldBe empty
    }

  }
}
