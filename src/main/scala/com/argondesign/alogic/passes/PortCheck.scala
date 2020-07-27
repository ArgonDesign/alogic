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
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.Expr.InstancePortSel
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeReady
import com.argondesign.alogic.core.StorageTypes.StorageTypeWire
import com.argondesign.alogic.core.Types.TypeIn
import com.argondesign.alogic.core.Types.TypeOut
import com.argondesign.alogic.core.Types.TypePipeIn
import com.argondesign.alogic.core.Types.TypePipeOut
import com.argondesign.alogic.util.Ordinal
import com.argondesign.alogic.util.unreachable

final class PortCheck(implicit cc: CompilerContext) extends StatelessTreeTransformer {

  // Note this pass runs after connects have already been type checked and
  // normalized. This means that basic flow control compatibility has already
  // been proven, complex expressions can only contain ports without flow
  // control, cardinal ports are explicitly selected, and there is only ever
  // one right hand side in a connection.

  // Check ports that can only have a single sink indeed only have a single sink
  private def checkMultipleSinks(assigns: List[EntAssign]): Unit = {
    assigns.groupBy(_.rhs).iterator filter {
      case (_, _ :: Nil) => false // Single sink
      case (rhs, _) =>
        rhs.tpe match {
          case TypeIn(_, FlowControlTypeReady)      => true
          case TypeOut(_, FlowControlTypeReady, _)  => true
          case TypePipeIn(FlowControlTypeReady)     => true
          case TypePipeOut(FlowControlTypeReady, _) => true
          case _                                    => false
        }
    } foreach {
      case (expr, conns) =>
        val what = expr match {
          case ExprSym(symbol)       => symbol.name
          case InstancePortSel(i, p) => s"${i.name}.${p.name}"
          case _                     => unreachable
        }
        val where = expr match {
          case ExprSym(symbol)       => symbol.loc
          case InstancePortSel(i, _) => i.loc
          case _                     => unreachable
        }
        cc.error(where, s"Port '$what' with 'sync ready' flow control has multiple sinks")
        conns.sortBy(_.loc).iterator.zipWithIndex foreach {
          case (conn, idx) => cc.note(conn, s"The ${Ordinal(idx + 1)} sink is here")
        }
    }
  }

  private def checkMultipleDriversSimple(connects: List[EntAssign]): Unit = {
    connects.groupBy(_.lhs).iterator filter {
      case (_, _ :: Nil)              => false // Single source
      case (_: ExprSym, _)            => true
      case (InstancePortSel(_, _), _) => true
      case _                          => false // Complex expression
    } foreach {
      case (expr, conns) =>
        val what = expr match {
          case ExprSym(symbol)       => symbol.name
          case InstancePortSel(i, p) => s"${i.name}.${p.name}"
          case _                     => unreachable
        }
        val where = expr match {
          case ExprSym(symbol)       => symbol.loc
          case InstancePortSel(i, _) => i.loc
          case _                     => unreachable
        }
        cc.error(where, s"Port '$what' has multiple drivers")
        conns.sortBy(_.loc).iterator.zipWithIndex foreach {
          case (conn, idx) => cc.note(conn, s"The ${Ordinal(idx + 1)} driver is here")
        }
    }
  }

  override def skip(tree: Tree): Boolean = tree match {
    case _: DeclEntity | _: DeclOut | _: DeclPipeOut => false
    case _                                           => true
  }

  override def enter(tree: Tree): Option[Tree] = tree match {
    //////////////////////////////////////////////////////////////////////////
    // Check output storage specifiers
    //////////////////////////////////////////////////////////////////////////

    case DeclOut(_, _, FlowControlTypeReady, StorageTypeWire) =>
      cc.error(tree, "'sync ready' port cannot use 'wire' storage specifier")
      Some(tree)

    case DeclPipeOut(_, FlowControlTypeReady, StorageTypeWire) =>
      cc.error(tree, "'sync ready' port cannot use 'wire' storage specifier")
      Some(tree)

    //////////////////////////////////////////////////////////////////////////
    // Check multiple driver/sink
    //////////////////////////////////////////////////////////////////////////

    case decl: DeclEntity =>
      val assigns = decl.symbol.defn.asInstanceOf[DefnEntity].assigns
      checkMultipleSinks(assigns)
      checkMultipleDriversSimple(assigns)
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
