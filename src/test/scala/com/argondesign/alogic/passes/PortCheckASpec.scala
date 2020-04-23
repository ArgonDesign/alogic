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
// PortCheckA tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import java.util.regex.Pattern

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import org.scalatest.FreeSpec

final class PortCheckASpec extends FreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  protected def portCheckA(text: String): Unit =
    transformWithPass(
      Namer andThen
        Elaborate andThen
        TypeCheck andThen
        PortCheckA,
      text
    )

  "PortCheckA should" - {

    "error for multiple sinks for sync ready driver" - {
      for {
        (conn, good) <- List(
          ("pi_n -> po_n_a;", true),
          ("pi_n -> po_n_b;", true),
          ("n.po_n -> po_n_a;", true),
          ("n.po_n -> po_n_b;", true),
          ("pi_n -> po_n_a; pi_n -> po_n_b;", false),
          ("n.po_n -> po_n_a; n.po_n -> po_n_b;", false),
          ("pi_y -> po_y_a;", true),
          ("pi_y -> po_y_b;", true),
          ("n.po_y -> po_y_a;", true),
          ("n.po_y -> po_y_b;", true),
          ("pi_y -> po_y_a; pi_y -> po_y_b;", true),
          ("n.po_y -> po_y_a; n.po_y -> po_y_b;", true)
        )
      } {
        conn in {
          portCheckA {
            s"""
            |network a {
            |  (* unused *) in sync ready bool pi_n;
            |  (* unused *) out sync ready bool po_n_a;
            |  (* unused *) out sync ready bool po_n_b;
            |  (* unused *) in sync bool pi_y;
            |  (* unused *) out sync bool po_y_a;
            |  (* unused *) out sync bool po_y_b;
            |
            |  (* unused *) new fsm n {
            |    (* unused *) out sync ready bool po_n;
            |    (* unused *) out sync bool po_y;
            |  }
            |
            |  $conn
            |}"""
          }
          if (good) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](
              Pattern.quote(s"Port with 'sync ready' flow control has multiple sinks"),
              "Previous '->' is at: .*"
            )
          }
        }
      }
    }

    "error for invalid storage specifiers" - {
      for {
        (fc, st, msg) <- List(
          ("", "", ""),
          ("", "wire", ""),
          ("sync", "", ""),
          ("sync", "wire", ""),
          ("sync ready", "", ""),
          ("sync ready", "fslice", ""),
          ("sync ready", "bslice", ""),
          ("sync ready", "bubble", ""),
          ("sync ready", "bubble bubble", ""),
          ("sync ready", "wire", "'sync ready' port cannot use 'wire' storage specifier"),
          ("sync accept", "", "'sync accept' port must use 'wire' storage specifier"),
          ("sync accept", "wire", "")
        )
      } {

        s"'$fc' with '$st'" in {
          portCheckA {
            s"""|fsm a {
             |   (* unused *) out $fc $st bool po;
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
