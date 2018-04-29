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
// Factory to build output slice entities
////////////////////////////////////////////////////////////////////////////////
package com.argondesign.alogic.core

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.typer.TypeAssigner

object SliceFactory {

  /*

  // Register slice interface

  // Hardware interface:
  _ip
  _ip_valid
  _ip_ready

  _op
  _op_valid
  _op_ready

  at beginning:
  _ip_valid = 1'b0

   */

  // slice logic for void payload:
  private def voidBody(
      ss: StorageSlice,
      ipvRef: ExprRef,
      oprRef: ExprRef,
      vRef: ExprRef
  ): List[Stmt] = ss match {
    case StorageSliceBubble => {
      // valid = ~valid & ip_valid | valid & ~op_ready;
      // fence;
      List(
        StmtAssign(vRef, ~vRef & ipvRef | vRef & ~oprRef),
        StmtFence()
      )
    }
    case StorageSliceFwd => {
      // valid = ip_valid | valid & ~op_ready;
      // fence;
      List(
        StmtAssign(vRef, ipvRef | vRef & ~oprRef),
        StmtFence()
      )
    }
    case StorageSliceBwd => {
      // valid = (valid | ip_valid) & ~op_ready;
      // fence;
      List(
        StmtAssign(vRef, (vRef | ipvRef) & ~oprRef),
        StmtFence()
      )
    }
  }

  // slice connects for void payload:
  private def voidConnects(
      ss: StorageSlice,
      ipvRef: ExprRef,
      iprRef: ExprRef,
      opvRef: ExprRef,
      oprRef: ExprRef,
      eRef: ExprRef,
      fRef: ExprRef,
      vRef: ExprRef
  ): List[Connect] = ss match {
    case StorageSliceBubble => {
      // valid -> op_valid;
      // ~valid -> ip_ready;
      // ~valid -> empty;
      // valid -> full;
      List(
        Connect(vRef, List(opvRef)),
        Connect(~vRef, List(iprRef)),
        Connect(~vRef, List(eRef)),
        Connect(vRef, List(fRef))
      )
    }
    case StorageSliceFwd => {
      // valid -> op_valid;
      // ~valid | op_ready -> ip_ready;
      // ~valid -> empty;
      // valid -> full;
      List(
        Connect(vRef, List(opvRef)),
        Connect(~vRef | oprRef, List(iprRef)),
        Connect(~vRef, List(eRef)),
        Connect(vRef, List(fRef))
      )
    }
    case StorageSliceBwd => {
      // valid | ip_valid -> op_valid;
      // ~valid -> ip_ready;
      // ~valid -> empty;
      // valid -> full;
      List(
        Connect(vRef | ipvRef, List(opvRef)),
        Connect(~vRef, List(iprRef)),
        Connect(~vRef, List(eRef)),
        Connect(vRef, List(fRef))
      )
    }
  }

  // slice logic for non-void payload:
  private def nonVoidBody(
      ss: StorageSlice,
      ipRef: ExprRef,
      ipvRef: ExprRef,
      oprRef: ExprRef,
      pRef: ExprRef,
      vRef: ExprRef
  ): List[Stmt] = ss match {
    case StorageSliceBubble => {
      // if (ip_valid & ~valid) {
      //   payload = ip;
      // }
      // valid = ~valid & ip_valid | valid & ~op_ready;
      // fence;
      List(
        StmtIf(
          ipvRef & ~vRef,
          StmtAssign(pRef, ipRef),
          None
        ),
        StmtAssign(vRef, ~vRef & ipvRef | vRef & ~oprRef),
        StmtFence()
      )
    }
    case StorageSliceFwd => {
      // if (ip_valid & (~valid | op_ready)) {
      //   payload = ip;
      // }
      // valid = ip_valid | valid & ~op_ready;
      // fence;
      List(
        StmtIf(
          ipvRef & (~vRef | oprRef),
          StmtAssign(pRef, ipRef),
          None
        ),
        StmtAssign(vRef, ipvRef | vRef & ~oprRef),
        StmtFence()
      )
    }
    case StorageSliceBwd => {
      // if (ip_valid & ~valid & ~op_ready) {
      //   payload = ip;
      // }
      // valid = (valid | ip_valid) & ~op_ready;
      // fence;
      List(
        StmtIf(
          ipvRef & ~vRef & ~oprRef,
          StmtAssign(pRef, ipRef),
          None
        ),
        StmtAssign(vRef, (vRef | ipvRef) & ~oprRef),
        StmtFence()
      )
    }
  }

  // slice connects for non-void payload:
  private def nonVoidConnects(
      ss: StorageSlice,
      ipRef: ExprRef,
      opRef: ExprRef,
      ipvRef: ExprRef,
      iprRef: ExprRef,
      opvRef: ExprRef,
      oprRef: ExprRef,
      eRef: ExprRef,
      fRef: ExprRef,
      pRef: ExprRef,
      vRef: ExprRef
  ): List[Connect] = ss match {
    case StorageSliceBubble => {
      // payload -> op ;
      // valid -> op_valid;
      // ~valid -> ip_ready;
      // ~valid -> empty;
      // valid -> full;
      List(
        Connect(vRef, List(opvRef)),
        Connect(pRef, List(opRef)),
        Connect(~vRef, List(iprRef)),
        Connect(~vRef, List(eRef)),
        Connect(vRef, List(fRef))
      )
    }
    case StorageSliceFwd => {
      // payload -> op;
      // valid -> op_valid;
      // ~valid | op_ready -> ip_ready;
      // ~valid -> empty;
      // valid -> full;
      List(
        Connect(pRef, List(opRef)),
        Connect(vRef, List(opvRef)),
        Connect(~vRef | oprRef, List(iprRef)),
        Connect(~vRef, List(eRef)),
        Connect(vRef, List(fRef))
      )
    }
    case StorageSliceBwd => {
      // valid ? payload : ip -> op;
      // valid | ip_valid -> op_valid;
      // ~valid -> ip_ready;
      // ~valid -> empty;
      // valid -> full;
      List(
        Connect(ExprTernary(vRef, pRef, ipRef), List(opRef)),
        Connect(vRef | ipvRef, List(opvRef)),
        Connect(~vRef, List(iprRef)),
        Connect(~vRef, List(eRef)),
        Connect(vRef, List(fRef))
      )
    }
  }

  // Build an entity similar to the following Alogic FSM to be used as an
  // output slice implementation. The body of the main function is filled
  // in by the above implementations.
  //
  // fsm slice_bubble {
  //   // Upstream interface
  //   in payload_t ip;
  //   in bool ip_valid;
  //   out wire bool ip_ready;
  //
  //   // Downstream interface
  //   out wire payload_t op;
  //   out wire bool op_valid;
  //   in bool op_ready;
  //
  //   // Status output
  //   out wire bool empty;
  //   out wire bool full;
  //
  //   // Local storage
  //   payload_t payload;
  //   bool valid = false;
  //
  //   void main() {
  //      <BODY>
  //   }
  //
  //   <CONNECTS>
  // }
  private def buildSlice(
      ss: StorageSlice,
      name: String,
      loc: Loc,
      kind: Type,
      sep: String
  )(
      implicit cc: CompilerContext
  ): Entity = {
    val fcn = FlowControlTypeNone
    val stw = StorageTypeWire

    val bool = TypeUInt(TypeAssigner(Expr(1) withLoc loc))

    lazy val ipSymbol = cc.newTermSymbol("ip", loc, TypeIn(kind, fcn))
    val ipvSymbol = cc.newTermSymbol(s"ip${sep}valid", loc, TypeIn(bool, fcn))
    val iprSymbol = cc.newTermSymbol(s"ip${sep}ready", loc, TypeOut(bool, fcn, stw))

    lazy val opSymbol = cc.newTermSymbol("op", loc, TypeOut(kind, fcn, stw))
    val opvSymbol = cc.newTermSymbol(s"op${sep}valid", loc, TypeOut(bool, fcn, stw))
    val oprSymbol = cc.newTermSymbol(s"op${sep}ready", loc, TypeIn(bool, fcn))

    val eSymbol = cc.newTermSymbol("empty", loc, TypeOut(bool, fcn, stw))
    val fSymbol = cc.newTermSymbol("full", loc, TypeOut(bool, fcn, stw))

    val pSymbol = cc.newTermSymbol("payload", loc, kind)
    val vSymbol = cc.newTermSymbol("valid", loc, bool)

    lazy val ipRef = ExprRef(Sym(ipSymbol))
    val ipvRef = ExprRef(Sym(ipvSymbol))
    val iprRef = ExprRef(Sym(iprSymbol))

    lazy val opRef = ExprRef(Sym(opSymbol))
    val opvRef = ExprRef(Sym(opvSymbol))
    val oprRef = ExprRef(Sym(oprSymbol))

    val eRef = ExprRef(Sym(eSymbol))
    val fRef = ExprRef(Sym(fSymbol))

    val pRef = ExprRef(Sym(pSymbol))
    val vRef = ExprRef(Sym(vSymbol))

    val body = if (kind != TypeVoid) {
      nonVoidBody(ss, ipRef, ipvRef, oprRef, pRef, vRef)
    } else {
      voidBody(ss, ipvRef, oprRef, vRef)
    }

    val state = State(ExprInt(false, 1, 0), body)

    val ports = if (kind != TypeVoid) {
      List(ipSymbol, ipvSymbol, iprSymbol, opSymbol, opvSymbol, oprSymbol, eSymbol, fSymbol)
    } else {
      List(ipvSymbol, iprSymbol, opvSymbol, oprSymbol, eSymbol, fSymbol)
    }

    val symbols = if (kind != TypeVoid) pSymbol :: vSymbol :: ports else vSymbol :: ports

    val decls = symbols map { symbol =>
      Decl(symbol, None)
    }

    val connects = if (kind != TypeVoid) {
      nonVoidConnects(ss, ipRef, opRef, ipvRef, iprRef, opvRef, oprRef, eRef, fRef, pRef, vRef)
    } else {
      voidConnects(ss, ipvRef, iprRef, opvRef, oprRef, eRef, fRef, vRef)
    }

    val entitySymbol = cc.newTypeSymbol(name, loc, TypeEntity(name, ports, Nil))
    val entity = Entity(Sym(entitySymbol), decls, Nil, connects, Nil, List(state), Nil, Nil, Map())
    entity withVariant "fsm" regularize loc
  }

  def apply(
      slices: List[StorageSlice],
      name: String,
      loc: Loc,
      kind: Type
  )(
      implicit cc: CompilerContext
  ): Entity = {
    require(kind.isPacked)
    // TODO: handle sequence of slices
    buildSlice(slices.head, name, loc, kind, cc.sep)
  }

}
