////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
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
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.Messages.Note
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

//  // Given an expression, extract sub-expressions that require active logic
//  // to implement. If the returned Iterator is empty, then the given
//  // expression can be implemented purely as wiring.
//  def activeLogic(expr: Expr): Iterator[Expr] = {
//    return Iterator.empty
//    // TODO: Move this whole thing out of the type checker into a later pass
//    //       ... because uses _.isKnown which we cannothandle inthe frontend
//    expr match {
//      // TODO: indices must be known in connect expressions. Check and remove them
//      case ExprCall(tgt, args) =>
//        tgt match {
//          case ExprSym(Symbol("$signed" | "$unsigned" | "@zx" | "@sx" | "@ex")) =>
//            args.iterator flatMap { arg => activeLogic(arg.expr) }
//          case _ => Iterator.single(expr)
//        }
//      case _: ExprBuiltin => ???
//      case _: ExprUnary   => Iterator.single(expr)
//      case ExprBinary(lhs, op, rhs) =>
//        op match {
//          case "<<" | "<<<" | ">>" | ">>>" =>
//            if (rhs.isKnown) activeLogic(lhs) else Iterator.single(expr)
//          case _ => Iterator.single(expr)
//        }
//      case _: ExprCond      => Iterator.single(expr)
//      case ExprRep(_, expr) => activeLogic(expr)
//      case ExprCat(parts)   => parts.iterator flatMap activeLogic
//      case ExprIndex(tgt, idx) =>
//        if (idx.isKnown) activeLogic(tgt) else Iterator.single(expr)
//      case ExprSlice(tgt, lIdx, op, _) =>
//        if (op == ":" || lIdx.isKnown) activeLogic(tgt) else Iterator.single(expr)
//      case ExprDot(tgt, _, _)                                              => activeLogic(tgt)
//      case _: ExprSym | _: ExprType | _: ExprInt | _: ExprNum | _: ExprStr => Iterator.empty
//      case _: ExprSel | _: ExprSymSel | _: ExprIdent | _: ExprOld | _: ExprThis | _: ExprCast =>
//        unreachable
//    }
//  }

  // Check ports that can only have a single sink indeed only have a single sink
  private def checkMultipleSinks(assigns: List[EntAssign]): Unit = {
    assigns.filterNot(_.lhs.tpe.isSnoop).groupBy(_.rhs).iterator filter {
      case (_, _ :: Nil) => false // Single sink
      case (rhs, _) =>
        rhs.tpe match {
          case TypeIn(_, FlowControlTypeReady)         => true
          case TypeOut(_, FlowControlTypeReady, _)     => true
          case TypePipeIn(_, FlowControlTypeReady)     => true
          case TypePipeOut(_, FlowControlTypeReady, _) => true
          case _                                       => false
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
        cc.addMessage {
          Error(
            where,
            s"Port '$what' with 'sync ready' flow control has multiple sinks"
          ) withNotes {
            conns.sortBy(_.loc).iterator.zipWithIndex.map {
              case (conn, idx) => Note(conn, s"The ${Ordinal(idx + 1)} sink is here")
            }
          }
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
        cc.addMessage {
          Error(where, s"Port '$what' has multiple drivers") withNotes {
            conns.sortBy(_.loc).iterator.zipWithIndex.map {
              case (conn, idx) => Note(conn, s"The ${Ordinal(idx + 1)} driver is here")
            }
          }
        }
    }
  }

  override def enter(tree: Tree): Option[Tree] = tree match {
    //////////////////////////////////////////////////////////////////////////
    // Check output storage specifiers
    //////////////////////////////////////////////////////////////////////////

    case DeclOut(_, _, FlowControlTypeReady, StorageTypeWire) =>
      cc.error(tree, "'sync ready' port cannot use 'wire' storage specifier")
      Some(tree)

    case DeclPipeOut(_, _, FlowControlTypeReady, StorageTypeWire) =>
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

    //
    case _ => Some(tree)
  }

}

object PortCheck extends PairTransformerPass {
  val name = "port-check"
  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) =
    (cc.portCheck(decl), defn)
}
