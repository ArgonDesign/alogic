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

    "error for source appearing on multiple left hand sides" - {
      for {
        (conn, msg) <- List(
          ("pia -> po, n.pin;", ""),
          ("pib -> po, n.pin;", ""),
          ("k.pok -> po, n.pin;", ""),
          ("pia -> po; pib -> n.pin;", ""),
          ("pia -> po; pia -> n.pin;", "pia"),
          ("pib -> po; pib -> n.pin;", "pib"),
          ("k.pok -> po; k.pok -> n.pin;", "k.pok")
        )
      } {
        conn in {
          val tree = s"""|network a {
                         |  (* unused *) in bool pia;
                         |  (* unused *) in bool pib;
                         |  (* unused *) out bool po;
                         |
                         |  (* unused *) new fsm n {
                         |    (* unused *) in bool pin;
                         |  }
                         |
                         |  (* unused *) new fsm k {
                         |    (* unused *) out bool pok;
                         |  }
                         |
                         |  ${conn}
                         |}""".stripMargin.asTree[Entity]
          xform(tree)
          if (msg.isEmpty) {
            cc.messages shouldBe empty
          } else {
            cc.messages.loneElement should beThe[Error](
              Pattern.quote(s"'${msg}' appears on the left of more than one '->'"),
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
