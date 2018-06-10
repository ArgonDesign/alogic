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
// Factory to build output register entities
////////////////////////////////////////////////////////////////////////////////
package com.argondesign.alogic.core

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.typer.TypeAssigner

object SyncRegFactory {

  /*

  // Register slice interface

  // Hardware interface:
  _ip
  _ip_valid

  _op
  _op_valid

  at beginning:
  _ip_valid = 1'b0

   */

  // Build an entity similar to the following Alogic FSM to be used as an
  // output register implementation. The body of the main function is filled
  // in by the above implementations.
  //
  // fsm sync_reg {
  //   // Upstream interface
  //   in payload_t ip;
  //   in bool ip_valid;
  //
  //   // Downstream interface
  //   out wire payload_t op;
  //   out wire bool op_valid;
  //
  //   // Local storage
  //   payload_t payload;
  //   bool valid = false;
  //
  //   void main() {
  //     if (ip_valid) {
  //       payload = ip;
  //     }
  //     valid = ip_valid;
  //   }
  //
  //   payload -> op;
  //   valid -> op_valid;
  // }
  private def buildSyncReg(
      name: String,
      loc: Loc,
      kind: Type,
      sep: String
  )(
      implicit cc: CompilerContext
  ): EntityLowered = {
    val fcn = FlowControlTypeNone
    val stw = StorageTypeWire

    val bool = TypeUInt(TypeAssigner(Expr(1) withLoc loc))

    lazy val ipSymbol = cc.newTermSymbol("ip", loc, TypeIn(kind, fcn))
    val ipvSymbol = cc.newTermSymbol(s"ip${sep}valid", loc, TypeIn(bool, fcn))

    lazy val opSymbol = cc.newTermSymbol("op", loc, TypeOut(kind, fcn, stw))
    val opvSymbol = cc.newTermSymbol(s"op${sep}valid", loc, TypeOut(bool, fcn, stw))

    lazy val pSymbol = cc.newTermSymbol("payload", loc, kind)
    val vSymbol = cc.newTermSymbol("valid", loc, bool)

    lazy val ipRef = ExprRef(ipSymbol)
    val ipvRef = ExprRef(ipvSymbol)

    lazy val opRef = ExprRef(opSymbol)
    val opvRef = ExprRef(opvSymbol)

    lazy val pRef = ExprRef(pSymbol)
    val vRef = ExprRef(vSymbol)

    val body = if (kind != TypeVoid) {
      List(
        StmtIf(
          ipvRef,
          StmtAssign(pRef, ipRef),
          None
        ),
        StmtAssign(vRef, ipvRef),
        StmtFence()
      )
    } else {
      List(
        StmtAssign(vRef, ipvRef),
        StmtFence()
      )
    }

    val stateSystem = StmtBlock(body)

    val ports = if (kind != TypeVoid) {
      List(ipSymbol, ipvSymbol, opSymbol, opvSymbol)
    } else {
      List(ipvSymbol, opvSymbol)
    }

    val symbols = if (kind != TypeVoid) pSymbol :: vSymbol :: ports else vSymbol :: ports

    val decls = symbols map { symbol =>
      Decl(symbol, None)
    }

    val connects = if (kind != TypeVoid) {
      List(
        Connect(pRef, List(opRef)),
        Connect(vRef, List(opvRef))
      )
    } else {
      List(
        Connect(vRef, List(opvRef))
      )
    }

    val entitySymbol = cc.newTypeSymbol(name, loc, TypeEntity(name, ports, Nil))
    entitySymbol.attr.variant set "fsm"
    val entity = EntityLowered(entitySymbol, decls, Nil, connects, List(stateSystem), Map())
    entity regularize loc
  }

  def apply(
      name: String,
      loc: Loc,
      kind: Type
  )(
      implicit cc: CompilerContext
  ): EntityLowered = {
    require(kind.isPacked)
    buildSyncReg(name, loc, kind, cc.sep)
  }

}
