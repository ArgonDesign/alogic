////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Further port usage checks
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees.Expr.InstancePortSel
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeReady
import com.argondesign.alogic.core.StorageTypes.StorageTypeWire
import com.argondesign.alogic.core.Types.TypeIn
import com.argondesign.alogic.core.Types.TypeOut

final class PortCheck(implicit cc: CompilerContext) extends StatelessTreeTransformer {

  // Note this pass runs after connects have already been type checked and
  // normalized. This means that basic flow control compatibility has already
  // been proven, complex expressions can only contain ports without flow
  // control, cardinal ports are explicitly selected, and there is only ever
  // one right hand side in a connection.

  private def ordinal(cardinal: Int): String = {
    require(cardinal > 0)
    cardinal % 100 match {
      case 11 | 12 | 13 => s"${cardinal}th"
      case rem =>
        rem % 10 match {
          case 1 => s"${cardinal}st"
          case 2 => s"${cardinal}nd"
          case 3 => s"${cardinal}rd"
          case _ => s"${cardinal}th"
        }
    }
  }

  // Check ports that can only have a single sink indeed only have a single sink
  private def checkMultipleSinks(connects: List[EntConnect]): Unit = {
    connects.groupBy(_.lhs).iterator filter {
      case (_, _ :: Nil) => false // Single sink
      case (lhs, _) =>
        lhs.tpe match {
          case TypeIn(_, FlowControlTypeReady)     => true
          case TypeOut(_, FlowControlTypeReady, _) => true
          case _                                   => false
        }
    } foreach {
      case (_, conns) =>
        val sorted = conns.sortBy(_.loc)
        cc.error(sorted.head.lhs, "Port with 'sync ready' flow control has multiple sinks")
        sorted.iterator.zipWithIndex foreach {
          case (conn, idx) => cc.note(conn, s"The ${ordinal(idx + 1)} connection is here")
        }
    }
  }

  private def checkMultipleDriversSimple(connects: List[EntConnect]): Unit = {
    connects.groupBy(_.rhs.head).iterator filter {
      case (_, _ :: Nil)              => false // Single source
      case (_: ExprSym, _)            => true
      case (InstancePortSel(_, _), _) => true
      case _                          => false // Complex expression
    } foreach {
      case (_, conns) =>
        val sorted = conns.sortBy(_.loc)
        cc.error(sorted.head.rhs.head, "Port has multiple drivers")
        sorted.iterator.zipWithIndex foreach {
          case (conn, idx) => cc.note(conn, s"The ${ordinal(idx + 1)} connection is here")
        }
    }
  }

  override def skip(tree: Tree): Boolean = tree match {
    case _: DeclEntity | _: DeclOut => false
    case _                          => true
  }

  override def enter(tree: Tree): Option[Tree] = tree match {
    //////////////////////////////////////////////////////////////////////////
    // Check output storage specifiers
    //////////////////////////////////////////////////////////////////////////

    case DeclOut(_, _, FlowControlTypeReady, StorageTypeWire) =>
      cc.error(tree, "'sync ready' port cannot use 'wire' storage specifier")
      Some(tree)

    //////////////////////////////////////////////////////////////////////////
    // Check multiple driver/sink
    //////////////////////////////////////////////////////////////////////////

    case decl: DeclEntity =>
      val connects = decl.symbol.defn.asInstanceOf[DefnEntity].connects
      checkMultipleSinks(connects)
      checkMultipleDriversSimple(connects)
      // TODO: Add back bitwise multiple driver analysis for complex port expressions
      None

    case _ => None
  }

}

object PortCheck extends PairTransformerPass {
  val name = "port-check"
  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) =
    (cc.portCheck(decl), defn)
}
