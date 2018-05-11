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
// Type checking for '->' connections
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.typer

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.Expr.InstancePortRef
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.FlowControlTypes._

import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.PartialMatch._
import com.argondesign.alogic.util.unreachable

object ConnectChecks {

  private def flowControlType(expr: Expr): FlowControlType = {
    val portSymbol = expr match {
      case ExprRef(symbol)            => symbol
      case InstancePortRef(_, symbol) => symbol
      case _                          => unreachable
    }
    portSymbol.kind match {
      case TypeIn(_, fct)     => fct
      case TypeOut(_, fct, _) => fct
      case _: TypeInstance    => FlowControlTypeReady
      case _                  => unreachable
    }
  }

  private def fctToSource(fct: FlowControlType): String = fct match {
    case FlowControlTypeNone   => "none"
    case FlowControlTypeValid  => "sync"
    case FlowControlTypeReady  => "sync ready"
    case FlowControlTypeAccept => "sync accept"
  }

  private def lhsIsLegal(lhs: Expr)(implicit cc: CompilerContext): Boolean = {
    lhs match {
      case ExprRef(symbol) if symbol.kind.isInstance => true
      case other => {
        val opt = other partialMatch {
          case ExprRef(symbol) if symbol.kind.isOut => {
            cc.error(lhs, "Left hand side of '->' is an output from enclosing entity")
          }
          case ExprRef(symbol) if !symbol.kind.isIn => {
            cc.error(lhs, s"Left hand side of '->' is of non-port type: ${symbol.kind.toSource}")
          }
          case InstancePortRef(iSymbol, symbol) if symbol.kind.isIn => {
            cc.error(lhs, s"Left hand side of '->' is an input to instance '${iSymbol.name}'")
          }
        }
        opt.isEmpty
      }
    }
  }

  private def rhsIsLegal(rhs: Expr)(implicit cc: CompilerContext): Boolean = {
    rhs match {
      case ExprRef(symbol) if symbol.kind.isInstance => true
      case other => {
        val opt = other partialMatch {
          case ExprRef(symbol) if symbol.kind.isIn => {
            cc.error(rhs, "Right hand side of '->' is an input to enclosing entity")
          }
          case ExprRef(symbol) if !symbol.kind.isOut => {
            cc.error(rhs, s"Right hand side of '->' is of non-port type: ${symbol.kind.toSource}")
          }
          case InstancePortRef(iSymbol, symbol) if symbol.kind.isOut => {
            cc.error(rhs, s"Right hand side of '->' is an output from instance '${iSymbol.name}'")
          }
        }
        opt.isEmpty
      }
    }
  }

  private def compatibleType(
      loc: Loc,
      lhs: Expr,
      rhs: Expr
  )(implicit cc: CompilerContext): Boolean = {
    (lhs.tpe, rhs.tpe) match {
      case (_: TypeInstance, _: TypeInstance) => true
      case (_: TypeInstance, _) => {
        cc.error(rhs, "Cannot connect pipeline port to non-pipeline port")
        false
      }
      case (_, _: TypeInstance) => {
        cc.error(lhs, "Cannot connect non-pipeline port to pipeline port")
        false
      }
      case _ => {
        val opt = for {
          lhsWidth <- lhs.tpe.width.value
          rhsWidth <- rhs.tpe.width.value
          if lhsWidth != rhsWidth
        } yield {
          cc.error(
            loc.copy(point = rhs.loc.start),
            s"Port widths do not match: ${lhsWidth} -> ${rhsWidth}"
          )
        }
        opt.isEmpty
      }
    }
  }

  private def compatibleFlowControl(
      lhs: Expr,
      rhs: Expr
  )(implicit cc: CompilerContext): Boolean = {

    val fctl = flowControlType(lhs)
    val fctr = flowControlType(rhs)
    if (fctr != fctl) {
      cc.error(
        rhs,
        s"Ports '${lhs.toSource}' and '${rhs.toSource}' have incompatible flow control",
        s"${fctToSource(fctl)} -> ${fctToSource(fctr)}"
      )
      false
    } else {
      true
    }
  }

  private def sinkCountLegal(
      loc: Loc,
      lhs: Expr,
      rhss: List[Expr]
  )(implicit cc: CompilerContext): Boolean = {
    rhss.lengthCompare(1) == 0 || {
      flowControlType(lhs) match {
        case FlowControlTypeNone  => true
        case FlowControlTypeValid => true
        case other => {
          cc.error(
            loc,
            s"Port with '${fctToSource(other)}' flow control cannot have multiple sinks"
          )
          false
        }
      }
    }
  }

  // Return true if this is a well formed and typed Connect instance
  def apply(conn: Connect)(implicit cc: CompilerContext): Boolean = {
    // TODO: error on connect same thing on multiple lhss
    // TODO: error on connect same thing on multiple rhss

    val Connect(lhs, rhss) = conn

    // Check left hand side is legal
    val lhsOk = lhsIsLegal(lhs)

    // Check right hand side is legal
    val rhssOk = rhss map { rhsIsLegal } reduce { _ && _ }

    // Check types are compatible
    lazy val typeOk = rhss map { compatibleType(conn.loc, lhs, _) } reduce { _ && _ }

    // Check flow control is compatible
    lazy val flowControlOk = rhss map { compatibleFlowControl(lhs, _) } reduce { _ && _ }

    // Check number of right hand sides is legal
    lazy val sinkCountOK = sinkCountLegal(conn.loc, lhs, rhss)

    lhsOk && rhssOk && typeOk && flowControlOk && sinkCountOK
  }
}
