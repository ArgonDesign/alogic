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
// PortCheckB tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import java.util.regex.Pattern

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import org.scalatest.FreeSpec

final class PortCheckBSpec extends FreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  protected def portCheckB(text: String): Unit = transformWithPass(
    Namer andThen
      Elaborate andThen
      TypeCheck andThen
      ReplaceUnaryTicks andThen
      ResolvePolyFunc andThen
      AddCasts andThen
      Desugar andThen
      InlineUnsizedConst andThen
      FoldTypeAliases andThen
      FoldExpr andThen
      PortCheckB,
    text
  )

  "PortCheckB should" - {

    "error for multiple drivers for sink" - {
      for {
        (conn, msg) <- List(
          ("pia -> po;", ""),
          ("pia -> n.pi;", ""),
          ("pib -> po;", ""),
          ("pib -> n.pi;", ""),
          ("pia -> po, n.pi;", ""),
          ("pib -> po, n.pi;", ""),
          ("pia -> po, po;", "po"),
          ("pia -> n.pi, n.pi;", "n.pi"),
          ("pia -> po; pib -> n.pi;", ""),
          ("pia -> po; pib -> po;", "po"),
          ("pia -> n.pi; pib -> n.pi;", "n.pi")
        )
      } {
        conn in {
          portCheckB {
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
            |}"""
          }
          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](
              Pattern.quote(s"'${msg}' has multiple drivers"),
              "Other '->' is at: .*"
            )
          }
        }
      }
    }

    "error for multiple drivers for complex sinks" - {
      for {
        (conn, msg) <- List(
          ("pi[0] -> po[0];", ""),
          ("pi[3:0] -> po; pi[4] -> po[0];", "po[0]"),
          ("pi[0] -> po[0],po[0];", "po[0]"),
          ("pi[2:0] -> po[2:0],po[3:1];", "po[2:1]"),
          ("pi[0] -> po[0],po[1];", ""),
          ("pi[1:0] -> po[1:0],po[3:2];", ""),
          ("pi[1:0] -> po[1:0],po[2:1];", "po[1]"),
          ("pi[1:0] -> po[1:0]; pi[2] -> po[2'd1];", "po[1]"),
          ("pi[1:0] -> po[1:0]; pi[2] -> po[2'd2];", ""),
          ("pi[1:0] -> pov[0]; pi[2:1] -> pov[1];", ""),
          ("pi[1:0] -> pov[0]; pi[3:2] -> pov[0];", "pov[0]"),
          ("pi[1:0] -> pov[0]; pi[3:2] -> pov[1];", ""),
          ("pi[0] -> pov[0][0]; pi[1] -> pov[0][1];", ""),
          ("pi[0] -> pov[0][0]; pi[1] -> pov[0][0];", "pov[1'd0][0]"),
          ("pi[0] -> pov[1'd0][0]; pi[1:0] -> pov[0][1:0];", "pov[1'd0][0]"),
          ("pi[1:0] -> pov[1'd0]; pi[1] -> pov[0][0];", "pov[1'd0][0]"),
          ("pi[2+:4] -> pos.x; pi[0 +: 6] -> pos;", "pos.x"),
          ("pi[0+:2] -> pos.y.z; pi[2+:2] -> pos.y;", "pos.y.z"),
          ("pi[0+:2] -> pos.x[0]; pi[2] -> pos.x[0][1];", "pos.x[1'd0][1]"),
          ("pi[2+:2] -> n.pin.y; pi[0+:2] -> n.pin.y.z;", "n.pin.y.z"),
          ("pi[0+:6] -> n.pin; pi[0+:2] -> n.pin.y.z;", "n.pin.y.z"),
          ("pi[0] -> pov[1][0], pov[A][0];", "pov[1'd1][0]"),
          ("pi[0] -> pov[0][1], pov[0][A];", "pov[1'd0][1]"),
          ("pi[0] -> po[0]; pi[1] -> po[1]; pi[2] -> po[1];", "po[1]")
        )
      } {
        conn in {
          portCheckB {
            s"""
            |network a {
            |  struct ss {
            |    u2 z;
            |  }
            |  struct s {
            |    u2[2] x;
            |    ss    y;
            |  }
            |  const uint A = 1;
            |  in u8 pi;
            |  out u4 po;
            |  out u2[2] pov;
            |  out s pos;
            |
            |  new fsm n {
            |    in  s pin;
            |    out s pon;
            |    fence { pon = pin; }
            |  }
            |
            |  $conn
            |}"""
          }
          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](
              Pattern.quote(s"'${msg}' has multiple drivers"),
              "Other '->' is at: .*"
            )
          }
        }
      }
    }

    "not error when pipeline connects overlap with port selections" - {
      for {
        conn <- List(
          "stage_0 -> stage_1; stage_1.o -> stage_0.i;"
        )
      } {
        conn in {
          portCheckB {
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
            |    void main() {
            |      x = pi + 'i;
            |      write; fence;
            |    }
            |  }
            |
            |  new fsm stage_1 {
            |    out u1 o;
            |    void main() {
            |      read;
            |      po = x;
            |      o = x[0];
            |      fence;
            |    }
            |  }
            |
            |  $conn
            |}"""
          }
          cc.messages shouldBe empty
        }
      }
    }

  }
}
