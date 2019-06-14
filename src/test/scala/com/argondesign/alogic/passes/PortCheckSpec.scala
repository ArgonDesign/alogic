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
// PortCheck tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import java.util.regex.Pattern

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.typer.Typer
import com.argondesign.alogic.util.unreachable
import org.scalatest.FreeSpec

final class PortCheckSpec extends FreeSpec with AlogicTest {

  implicit val cc = new CompilerContext

  def xform(trees: Tree*): Unit = {
    val entities = trees map {
      _ match {
        case Root(_, entity: EntityIdent) => entity
        case entity: EntityIdent          => entity
        case _                            => unreachable
      }
    }

    cc.addGlobalEntities(entities)

    trees map {
      _ rewrite new Checker
    } map {
      _ rewrite new Namer
    } map {
      _ rewrite new Desugar
    } map {
      _ rewrite new Typer
    } foreach {
      _ rewrite new PortCheck
    }
  }

  "PortCheck should" - {

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
          val tree = s"""|network a {
                         |  (* unused *) in bool pia;
                         |  (* unused *) in bool pib;
                         |  (* unused *) out bool po;
                         |
                         |  (* unused *) new fsm n {
                         |    (* unused *) in bool pi;
                         |  }
                         |
                         |  ${conn}
                         |}""".stripMargin.asTree[Entity]
          xform(tree)
          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](
              Pattern.quote(s"'${msg}' has multiple drivers"),
              "Previous '->' is at: .*"
            )
          }
        }
      }
    }

    "error for multiple sinks for sync ready driver" - {
      for {
        (conn, msg) <- List(
          ("pi_n -> po_n_a;", ""),
          ("pi_n -> po_n_b;", ""),
          ("n.po_n -> po_n_a;", ""),
          ("n.po_n -> po_n_b;", ""),
          ("pi_n -> po_n_a; pi_n -> po_n_b;", "pi_n"),
          ("n.po_n -> po_n_a; n.po_n -> po_n_b;", "n.po_n"),
          ("pi_y -> po_y_a;", ""),
          ("pi_y -> po_y_b;", ""),
          ("n.po_y -> po_y_a;", ""),
          ("n.po_y -> po_y_b;", ""),
          ("pi_y -> po_y_a; pi_y -> po_y_b;", ""),
          ("n.po_y -> po_y_a; n.po_y -> po_y_b;", "")
        )
      } {
        conn in {
          val tree = s"""|network a {
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
                         |  ${conn}
                         |}""".stripMargin.asTree[Entity]
          xform(tree)
          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](
              Pattern.quote(s"'${msg}' has multiple sinks"),
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

        s"'${fc}' with '${st}'" in {
          val tree = s"""|fsm a {
                         |   (* unused *) out ${fc} ${st} bool po;
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
