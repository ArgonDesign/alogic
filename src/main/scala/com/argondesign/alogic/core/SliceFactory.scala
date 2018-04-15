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

  // slice logic for void payload:
  private def voidBody(
      ss: StorageSlice,
      uVRef: ExprRef,
      uRRef: ExprRef,
      dVRef: ExprRef,
      dRRef: ExprRef,
      eRef: ExprRef,
      fRef: ExprRef,
      pRef: ExprRef,
      vRef: ExprRef
  ): List[Stmt] = ss match {
    case StorageSliceBubble => {
      // d_valid = valid;
      // u_ready = ~valid;
      // empty = ~valid;
      // full = valid;
      // valid = ~valid & u_valid | valid & ~d_ready;
      // fence;
      List(
        StmtAssign(dVRef, vRef),
        StmtAssign(uRRef, ~vRef),
        StmtAssign(eRef, ~vRef),
        StmtAssign(fRef, vRef),
        StmtAssign(vRef, ~vRef & uVRef | vRef & ~dRRef),
        StmtFence()
      )
    }
    case StorageSliceFwd => {
      // d_valid = valid;
      // u_ready = ~valid | d_ready;
      // empty = ~valid;
      // full = valid;
      // valid = u_valid | valid & ~d_ready;
      // fence;
      List(
        StmtAssign(dVRef, vRef),
        StmtAssign(uRRef, ~vRef | dRRef),
        StmtAssign(eRef, ~vRef),
        StmtAssign(fRef, vRef),
        StmtAssign(vRef, uVRef | vRef & ~dRRef),
        StmtFence()
      )
    }
    case StorageSliceBwd => {
      // d_valid = valid | u_valid;
      // u_ready = ~valid;
      // empty = ~valid;
      // full = valid;
      // valid = (valid | u_valid) & ~d_ready;
      // fence;
      List(
        StmtAssign(dVRef, vRef | uVRef),
        StmtAssign(uRRef, ~vRef),
        StmtAssign(eRef, ~vRef),
        StmtAssign(fRef, vRef),
        StmtAssign(vRef, (vRef | uVRef) & ~dRRef),
        StmtFence()
      )
    }
  }

  // slice logic for non-void payload:
  private def nonVoidBody(
      ss: StorageSlice,
      uPRef: ExprRef,
      dPRef: ExprRef,
      uVRef: ExprRef,
      uRRef: ExprRef,
      dVRef: ExprRef,
      dRRef: ExprRef,
      eRef: ExprRef,
      fRef: ExprRef,
      pRef: ExprRef,
      vRef: ExprRef
  ): List[Stmt] = ss match {
    case StorageSliceBubble => {
      // d_payload = payload;
      // d_valid = valid;
      // u_ready = ~valid;
      // empty = ~valid;
      // full = valid;
      // if (u_valid & ~valid) {
      //   payload = u_payload;
      // }
      // valid = ~valid & u_valid | valid & ~d_ready;
      // fence;
      List(
        StmtAssign(dPRef, pRef),
        StmtAssign(dVRef, vRef),
        StmtAssign(uRRef, ~vRef),
        StmtAssign(eRef, ~vRef),
        StmtAssign(fRef, vRef),
        StmtIf(
          uVRef & ~vRef,
          StmtAssign(pRef, uPRef),
          None
        ),
        StmtAssign(vRef, ~vRef & uVRef | vRef & ~dRRef),
        StmtFence()
      )
    }
    case StorageSliceFwd => {
      // d_payload = payload;
      // d_valid = valid;
      // u_ready = ~valid | d_ready;
      // empty = ~valid;
      // full = valid;
      // if (u_valid & (~valid | d_ready)) {
      //   payload = u_payload;
      // }
      // valid = u_valid | valid & ~d_ready;
      // fence;
      List(
        StmtAssign(dPRef, pRef),
        StmtAssign(dVRef, vRef),
        StmtAssign(uRRef, ~vRef | dRRef),
        StmtAssign(eRef, ~vRef),
        StmtAssign(fRef, vRef),
        StmtIf(
          uVRef & (~vRef | dRRef),
          StmtAssign(pRef, uPRef),
          None
        ),
        StmtAssign(vRef, uVRef | vRef & ~dRRef),
        StmtFence()
      )
    }
    case StorageSliceBwd => {
      // d_payload = valid ? payload : u_payload;
      // d_valid = valid | u_valid;
      // u_ready = ~valid;
      // empty = ~valid;
      // full = valid;
      // if (u_valid & ~valid & ~d_ready) {
      //   payload = u_payload;
      // }
      // valid = (valid | u_valid) & ~d_ready;
      // fence;
      List(
        StmtAssign(dPRef, ExprTernary(vRef, pRef, uPRef)),
        StmtAssign(dVRef, vRef | uVRef),
        StmtAssign(uRRef, ~vRef),
        StmtAssign(eRef, ~vRef),
        StmtAssign(fRef, vRef),
        StmtIf(
          uVRef & ~vRef & ~dRRef,
          StmtAssign(pRef, uPRef),
          None
        ),
        StmtAssign(vRef, (vRef | uVRef) & ~dRRef),
        StmtFence()
      )
    }
  }

  // Build an entity similar to the following Alogic FSM to be used as an
  // output slice implementation. The body of the main function is filled
  // in by the above implementations.
  //
  // fsm slice_bubble {
  //   // Upstream interface
  //   in payload_t u_payload;
  //   in bool u_valid;
  //   out wire bool u_ready;
  //
  //   // Downstream interface
  //   out wire payload_t d_payload;
  //   out wire bool d_valid;
  //   in bool d_ready;
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
  // }
  private def buildSlice(
      ss: StorageSlice,
      name: String,
      loc: Loc,
      kind: Type
  )(
      implicit cc: CompilerContext
  ): Entity = {
    val fcn = FlowControlTypeNone
    val stw = StorageTypeWire

    val bool = TypeUInt(TypeAssigner(Expr(1) withLoc loc))

    lazy val uPSymbol = cc.newTermSymbol("u_payload", loc, TypeIn(kind, fcn))
    val uVSymbol = cc.newTermSymbol("u_valid", loc, TypeIn(bool, fcn))
    val uRSymbol = cc.newTermSymbol("u_ready", loc, TypeOut(bool, fcn, stw))

    lazy val dPSymbol = cc.newTermSymbol("d_payload", loc, TypeOut(kind, fcn, stw))
    val dVSymbol = cc.newTermSymbol("d_valid", loc, TypeOut(bool, fcn, stw))
    val dRSymbol = cc.newTermSymbol("d_ready", loc, TypeIn(bool, fcn))

    val eSymbol = cc.newTermSymbol("empty", loc, TypeOut(bool, fcn, stw))
    val fSymbol = cc.newTermSymbol("full", loc, TypeOut(bool, fcn, stw))

    val pSymbol = cc.newTermSymbol("payload", loc, kind)
    val vSymbol = cc.newTermSymbol("valid", loc, kind)

    lazy val uPRef = ExprRef(Sym(uPSymbol))
    val uVRef = ExprRef(Sym(uVSymbol))
    val uRRef = ExprRef(Sym(uRSymbol))

    lazy val dPRef = ExprRef(Sym(dPSymbol))
    val dVRef = ExprRef(Sym(dVSymbol))
    val dRRef = ExprRef(Sym(dRSymbol))

    val eRef = ExprRef(Sym(eSymbol))
    val fRef = ExprRef(Sym(fSymbol))

    val pRef = ExprRef(Sym(pSymbol))
    val vRef = ExprRef(Sym(vSymbol))

    val body = if (kind != TypeVoid) {
      nonVoidBody(ss, uPRef, dPRef, uVRef, uRRef, dVRef, dRRef, eRef, fRef, pRef, vRef)
    } else {
      voidBody(ss, uVRef, uRRef, dVRef, dRRef, eRef, fRef, pRef, vRef)
    }

    val state = State(ExprInt(false, 1, 0), body)

    val ports = {
      val rest = List(
        uVSymbol,
        uRSymbol,
        dVSymbol,
        dRSymbol,
        eSymbol,
        fSymbol
      )

      if (kind != TypeVoid) uPSymbol :: dPSymbol :: rest else rest
    }

    val symbols = if (kind != TypeVoid) pSymbol :: vSymbol :: ports else vSymbol :: ports

    val decls = symbols map { symbol =>
      Decl(Sym(symbol), symbol.denot.kind, None)
    }

    val entitySymbol = cc.newTypeSymbol(name, loc, TypeEntity(name, ports, Nil))
    val entity = Entity(Sym(entitySymbol), decls, Nil, Nil, Nil, List(state), Nil, Nil, Map())
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
    buildSlice(slices.head, name, loc, kind)
  }

}
