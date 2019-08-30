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
import com.argondesign.alogic.core.StorageTypes.StorageTypeDefault
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable

import scala.language.postfixOps
import scala.util.chaining._

object ConnectChecks {

  private def flowControlType(expr: Expr, side: String)(
      implicit cc: CompilerContext): Option[FlowControlType] = {

    val simpleExpr = expr match {
      case ExprSym(symbol)       => true
      case InstancePortRef(_, _) => true
      case _                     => false
    }

    def portTypes(expr: Expr): List[(Type, Expr)] = expr match {
      case e @ ExprSym(sym)            => List((sym.kind, e))
      case e @ InstancePortRef(_, sym) => List((sym.kind, e))
      case ExprSelect(expr, _, _)      => portTypes(expr)
      case ExprIndex(expr, _)          => portTypes(expr)
      case ExprSlice(expr, _, _, _)    => portTypes(expr)
      case ExprCat(parts)              => parts flatMap portTypes
      case ExprRep(_, expr)            => portTypes(expr)
      case e: ExprInt                  => List((expr.tpe, e))
      case call: ExprCall              => cc.combArgsBuiltInCall(call) flatMap portTypes
      case _                           => unreachable
    }

    val flowControlTypes = portTypes(expr) map {
      case (t, e) =>
        val fct = t match {
          case TypeIn(_, fct)     => fct
          case TypeOut(_, fct, _) => fct
          case _: TypeInstance    => FlowControlTypeReady
          case _: TypeInt         => FlowControlTypeNone
          case _: TypeParam       => FlowControlTypeNone
          case _: TypeConst       => FlowControlTypeNone
          case _                  => unreachable
        }
        (fct, e)
    }

    if (simpleExpr) {
      Some(flowControlTypes.head._1)
    } else {
      val noFlowControl = flowControlTypes collect {
        case (fct, e) if fct != FlowControlTypeNone =>
          cc.error(
            e,
            s"Port with flow control found in non-trivial expression on ${side} hand side of '->'")
      } isEmpty

      if (noFlowControl) Some(flowControlTypes.head._1) else None
    }
  }

  private def fctToSource(fct: FlowControlType): String = fct match {
    case FlowControlTypeNone   => "none"
    case FlowControlTypeValid  => "sync"
    case FlowControlTypeReady  => "sync ready"
    case FlowControlTypeAccept => "sync accept"
  }

  private def lhsIsLegal(lhs: Expr)(implicit cc: CompilerContext): Boolean = {

    val lhsValidExpr = lhs.isValidConnectLhs tap { b =>
      if (!b) {
        cc.error(lhs,
                 "Invalid port reference on left hand side of '->'",
                 "Only expressions which are purely wiring are permitted")
      }
    }

    def portDirectionsValid(expr: Expr): Boolean = expr match {
      case ExprSym(symbol) if symbol.kind.isInstance                     => true
      case ExprSym(symbol) if symbol.kind.isConst || symbol.kind.isParam => true
      case ExprSym(symbol) =>
        if (symbol.kind.isOut) {
          cc.error(expr, "Left hand side of '->' contains an output from enclosing entity")
        } else if (!symbol.kind.isIn) {
          cc.error(expr, s"Left hand side of '->' contains non-port type: ${symbol.kind.toSource}")
        }
        symbol.kind.isIn && !symbol.kind.isOut
      case InstancePortRef(iSymbol, symbol) =>
        if (symbol.kind.isIn) {
          cc.error(expr, s"Left hand side of '->' contains an input to instance '${iSymbol.name}'")
        }
        !symbol.kind.isIn
      case ExprSelect(expr, _, _)   => portDirectionsValid(expr)
      case ExprIndex(expr, _)       => portDirectionsValid(expr)
      case ExprSlice(expr, _, _, _) => portDirectionsValid(expr)
      case ExprCat(parts)           => parts forall { portDirectionsValid(_) }
      case ExprRep(_, expr)         => portDirectionsValid(expr)
      case _: ExprInt               => true
      case call: ExprCall           => cc.combArgsBuiltInCall(call) forall { portDirectionsValid(_) }
    }

    lhsValidExpr && portDirectionsValid(lhs)
  }

  private def rhsIsLegal(rhs: Expr)(implicit cc: CompilerContext): Boolean = {

    val rhsValidExpr = rhs.isValidConnectRhs tap { b =>
      if (!b) {
        cc.error(rhs,
                 "Invalid port reference on right hand side of '->'",
                 "Only expressions which are purely wiring are permitted")
      }
    }

    def portDirectionsValid(expr: Expr): Boolean = expr match {
      case ExprSym(symbol) if symbol.kind.isInstance => true
      case ExprSym(symbol) =>
        if (symbol.kind.isIn) {
          cc.error(expr, "Right hand side of '->' contains an input to enclosing entity")
        } else if (!symbol.kind.isOut) {
          cc.error(expr, s"Right hand side of '->' contains non-port type: ${symbol.kind.toSource}")
        }
        symbol.kind.isOut && !symbol.kind.isIn
      case InstancePortRef(iSymbol, symbol) =>
        if (symbol.kind.isOut) {
          cc.error(expr,
                   s"Right hand side of '->' contains an output from instance '${iSymbol.name}'")
        }
        !symbol.kind.isOut
      case ExprSelect(expr, _, _)   => portDirectionsValid(expr)
      case ExprIndex(expr, _)       => portDirectionsValid(expr)
      case ExprSlice(expr, _, _, _) => portDirectionsValid(expr)
      case ExprCat(parts)           => parts forall { portDirectionsValid(_) }
    }

    rhsValidExpr && portDirectionsValid(rhs)
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
        val lhsWidth = lhs.tpe.width
        val rhsWidth = rhs.tpe.width

        if (lhsWidth != rhsWidth) {
          cc.error(
            loc.copy(point = rhs.loc.start),
            s"Port widths do not match: ${lhsWidth} -> ${rhsWidth}"
          )
        }

        lhsWidth == rhsWidth
      }
    }
  }

  private def compatibleFlowControl(
      lhs: Expr,
      rhs: Expr
  )(implicit cc: CompilerContext): Boolean = {

    val fctlo = flowControlType(lhs, "left")
    val fctro = flowControlType(rhs, "right")
    (fctlo, fctro) match {
      case (Some(fctl), Some(fctr)) =>
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
      case _ => false
    }
  }

  private def sinkCountLegal(
      loc: Loc,
      lhs: Expr,
      rhss: List[Expr]
  )(implicit cc: CompilerContext): Boolean = {
    rhss.lengthCompare(1) == 0 || {
      flowControlType(lhs, "left") match {
        case Some(FlowControlTypeNone)  => true
        case Some(FlowControlTypeValid) => true
        case Some(other) => {
          cc.error(
            loc,
            s"Port with '${fctToSource(other)}' flow control cannot have multiple sinks"
          )
          false
        }
        case _ => unreachable
      }
    }
  }

  private def validStorage(loc: Loc, rhs: Expr)(implicit cc: CompilerContext): Boolean = {
    rhs collect {
      case ExprSym(symbol) => (symbol, symbol.kind)
    } collect {
      case (symbol, TypeOut(_, _, st)) if st != StorageTypeDefault =>
        cc.error(
          symbol,
          "Port driven by '->' must not specify output storage",
          s"'->' is at: ${loc.prefix}"
        )
    } isEmpty
  }

  private def noInitializer(loc: Loc, rhs: Expr)(implicit cc: CompilerContext): Boolean = {
    rhs collect {
      case e @ ExprSym(symbol) if symbol.attr.init.isSet && !e.isKnownConst =>
        cc.error(
          symbol,
          "Port driven by '->' must not have an initializer",
          s"'->' is at: ${loc.prefix}"
        )

    } isEmpty
  }

  // Return true if this is a well formed and typed Connect instance
  def apply(conn: EntConnect)(implicit cc: CompilerContext): Boolean = {
    // TODO: error on connect same thing on multiple lhss
    // TODO: error on connect same thing on multiple rhss

    val EntConnect(lhs, rhss) = conn

    // Check left hand side is legal
    val lhsOk = lhsIsLegal(lhs)

    // Check right hand side is legal
    val rhssOk = rhss map { rhsIsLegal } reduce { _ && _ }

    // Check types are compatible
    lazy val typeOk = rhss map { compatibleType(conn.loc, lhs, _) } reduce { _ && _ }

    // Check flow control is compatible
    lazy val flowControlOk = rhss map { compatibleFlowControl(lhs, _) } reduce { _ && _ }

    // Check number of right hand sides is legal
    lazy val sinkCountOk = sinkCountLegal(conn.loc, lhs, rhss)

    // Check storage specifier of rhs is default
    lazy val storageOk = rhss map { validStorage(conn.loc, _) } reduce { _ && _ }

    // Check rhs does not have an initializer
    lazy val initOk = rhss map { noInitializer(conn.loc, _) } reduce { _ && _ }

    // Force some value to yield more errors if they makes sense
    lhsOk && rhssOk && storageOk
    lhsOk && rhssOk && initOk

    // Everything needs to be OK
    lhsOk && rhssOk && typeOk && flowControlOk && sinkCountOk && storageOk && initOk
  }
}
