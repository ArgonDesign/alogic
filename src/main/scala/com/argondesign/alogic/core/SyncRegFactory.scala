////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Factory to build output register entities
////////////////////////////////////////////////////////////////////////////////
package com.argondesign.alogic.core

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.enums.EntityVariant

import scala.util.ChainingSyntax

object SyncRegFactory extends ChainingSyntax {

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
      kind: TypeFund,
      sep: String
    )(
      implicit
      cc: CompilerContext
    ): (DeclEntity, DefnEntity) = {
    val fcn = FlowControlTypeNone
    val stw = StorageTypeWire

    lazy val ipSymbol = cc.newSymbol("ip", loc) tap { _.kind = TypeIn(kind, fcn) }
    val ipvSymbol = cc.newSymbol(s"ip${sep}valid", loc) tap { _.kind = TypeIn(TypeUInt(1), fcn) }
    ipvSymbol.attr.default.set(ExprInt(false, 1, 0) regularize loc)
    lazy val opSymbol = cc.newSymbol("op", loc) tap { _.kind = TypeOut(kind, fcn, stw) }
    val opvSymbol = cc.newSymbol(s"op${sep}valid", loc) tap {
      _.kind = TypeOut(TypeUInt(1), fcn, stw)
    }
    lazy val pSymbol = cc.newSymbol("payload", loc) tap { _.kind = kind }
    val vSymbol = cc.newSymbol("valid", loc) tap { _.kind = TypeUInt(1) }

    lazy val ipDecl = ipSymbol.mkDecl regularize loc
    val ipvDecl = ipvSymbol.mkDecl regularize loc
    lazy val opDecl = opSymbol.mkDecl regularize loc
    val opvDecl = opvSymbol.mkDecl regularize loc
    lazy val pDecl = pSymbol.mkDecl regularize loc
    val vDecl = vSymbol.mkDecl regularize loc

    lazy val ipDefn = ipSymbol.mkDefn
    val ipvDefn = ipvSymbol.mkDefn
    lazy val opDefn = opSymbol.mkDefn
    val opvDefn = opvSymbol.mkDefn
    lazy val pDefn = pSymbol.mkDefn
    val vDefn = vSymbol.mkDefn(ExprInt(false, 1, 0))

    lazy val ipRef = ExprSym(ipSymbol)
    val ipvRef = ExprSym(ipvSymbol)
    lazy val opRef = ExprSym(opSymbol)
    val opvRef = ExprSym(opvSymbol)
    lazy val pRef = ExprSym(pSymbol)
    val vRef = ExprSym(vSymbol)

    val statements = if (kind != TypeVoid) {
      List(
        StmtIf(
          ipvRef,
          List(StmtAssign(pRef, ipRef)),
          Nil
        ),
        StmtAssign(vRef, ipvRef)
      )
    } else {
      List(StmtAssign(vRef, ipvRef))
    }

    val decls = {
      if (kind != TypeVoid) {
        List(ipDecl, ipvDecl, opDecl, opvDecl, pDecl, vDecl)
      } else {
        List(ipvDecl, opvDecl, vDecl)
      }
    }

    val defns = {
      if (kind != TypeVoid) {
        List(ipDefn, ipvDefn, opDefn, opvDefn, pDefn, vDefn)
      } else {
        List(ipvDefn, opvDefn, vDefn)
      }
    } map EntSplice.apply

    val assigns = if (kind != TypeVoid) {
      List(EntAssign(opRef, pRef), EntAssign(opvRef, vRef))
    } else {
      List(EntAssign(opvRef, vRef))
    }

    val entitySymbol = cc.newSymbol(name, loc)
    val decl = DeclEntity(entitySymbol, decls) regularize loc
    val defn = DefnEntity(
      entitySymbol,
      EntityVariant.Fsm,
      defns ::: EntCombProcess(statements) :: assigns
    ) regularize loc
    (decl, defn)
  }

  def apply(
      name: String,
      loc: Loc,
      kind: TypeFund
    )(
      implicit
      cc: CompilerContext
    ): (DeclEntity, DefnEntity) = {
    require(kind.isPacked)
    buildSyncReg(name, loc, kind, cc.sep)
  }

}
