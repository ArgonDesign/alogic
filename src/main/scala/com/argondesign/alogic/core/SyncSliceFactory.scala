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

import scala.collection.mutable.ListBuffer

object SyncSliceFactory {

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
  )(
      implicit cc: CompilerContext
  ): List[Stmt] = ss match {
    case StorageSliceBub => {
      // valid = ~valid & ip_valid | valid & ~op_ready;
      List(StmtAssign(vRef, ~vRef & ipvRef | vRef & ~oprRef))
    }
    case StorageSliceFwd => {
      // valid = ip_valid | valid & ~op_ready;
      List(StmtAssign(vRef, ipvRef | vRef & ~oprRef))
    }
    case StorageSliceBwd => {
      // valid = (valid | ip_valid) & ~op_ready;
      List(StmtAssign(vRef, (vRef | ipvRef) & ~oprRef))
    }
  }

  // slice connects for void payload:
  private def voidConnects(
      ss: StorageSlice,
      ipvRef: ExprRef,
      iprRef: ExprRef,
      opvRef: ExprRef,
      oprRef: ExprRef,
      sRef: ExprRef,
      vRef: ExprRef
  )(
      implicit cc: CompilerContext
  ): List[Connect] = ss match {
    case StorageSliceBub => {
      // valid -> op_valid;
      // ~valid -> ip_ready;
      // ~valid -> space;
      List(
        Connect(vRef, List(opvRef)),
        Connect(~vRef, List(iprRef)),
        Connect(~vRef, List(sRef))
      )
    }
    case StorageSliceFwd => {
      // valid -> op_valid;
      // ~valid | op_ready -> ip_ready;
      // ~valid -> space;
      List(
        Connect(vRef, List(opvRef)),
        Connect(~vRef | oprRef, List(iprRef)),
        Connect(~vRef, List(sRef))
      )
    }
    case StorageSliceBwd => {
      // valid | ip_valid -> op_valid;
      // ~valid -> ip_ready;
      // ~valid -> space;
      List(
        Connect(vRef | ipvRef, List(opvRef)),
        Connect(~vRef, List(iprRef)),
        Connect(~vRef, List(sRef))
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
  )(
      implicit cc: CompilerContext
  ): List[Stmt] = ss match {
    case StorageSliceBub => {
      // if (ip_valid & ~valid) {
      //   payload = ip;
      // }
      // valid = ~valid & ip_valid | valid & ~op_ready;
      List(
        StmtIf(
          ipvRef & ~vRef,
          StmtAssign(pRef, ipRef),
          None
        ),
        StmtAssign(vRef, ~vRef & ipvRef | vRef & ~oprRef)
      )
    }
    case StorageSliceFwd => {
      // if (ip_valid & (~valid | op_ready)) {
      //   payload = ip;
      // }
      // valid = ip_valid | valid & ~op_ready;
      List(
        StmtIf(
          ipvRef & (~vRef | oprRef),
          StmtAssign(pRef, ipRef),
          None
        ),
        StmtAssign(vRef, ipvRef | vRef & ~oprRef)
      )
    }
    case StorageSliceBwd => {
      // if (ip_valid & ~valid & ~op_ready) {
      //   payload = ip;
      // }
      // valid = (valid | ip_valid) & ~op_ready;
      List(
        StmtIf(
          ipvRef & ~vRef & ~oprRef,
          StmtAssign(pRef, ipRef),
          None
        ),
        StmtAssign(vRef, (vRef | ipvRef) & ~oprRef)
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
      sRef: ExprRef,
      pRef: ExprRef,
      vRef: ExprRef
  )(
      implicit cc: CompilerContext
  ): List[Connect] = ss match {
    case StorageSliceBub => {
      // payload -> op ;
      // valid -> op_valid;
      // ~valid -> ip_ready;
      // ~valid -> space;
      List(
        Connect(vRef, List(opvRef)),
        Connect(pRef, List(opRef)),
        Connect(~vRef, List(iprRef)),
        Connect(~vRef, List(sRef))
      )
    }
    case StorageSliceFwd => {
      // payload -> op;
      // valid -> op_valid;
      // ~valid | op_ready -> ip_ready;
      // ~valid -> space;
      List(
        Connect(pRef, List(opRef)),
        Connect(vRef, List(opvRef)),
        Connect(~vRef | oprRef, List(iprRef)),
        Connect(~vRef, List(sRef))
      )
    }
    case StorageSliceBwd => {
      // valid ? payload : ip -> op;
      // valid | ip_valid -> op_valid;
      // ~valid -> ip_ready;
      // ~valid -> space;
      List(
        Connect(ExprTernary(vRef, pRef, ipRef), List(opRef)),
        Connect(vRef | ipvRef, List(opvRef)),
        Connect(~vRef, List(iprRef)),
        Connect(~vRef, List(sRef))
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
  //   out wire bool space;
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
  ): EntityLowered = {
    val fcn = FlowControlTypeNone
    val stw = StorageTypeWire

    val bool = TypeUInt(TypeAssigner(Expr(1) withLoc loc))

    lazy val ipSymbol = cc.newTermSymbol("ip", loc, TypeIn(kind, fcn))
    val ipvSymbol = cc.newTermSymbol(s"ip${sep}valid", loc, TypeIn(bool, fcn))
    val iprSymbol = cc.newTermSymbol(s"ip${sep}ready", loc, TypeOut(bool, fcn, stw))
    iprSymbol.attr.dontCareUnless set ipvSymbol
    ipvSymbol.attr.dontCareUnless set iprSymbol

    lazy val opSymbol = cc.newTermSymbol("op", loc, TypeOut(kind, fcn, stw))
    val opvSymbol = cc.newTermSymbol(s"op${sep}valid", loc, TypeOut(bool, fcn, stw))
    val oprSymbol = cc.newTermSymbol(s"op${sep}ready", loc, TypeIn(bool, fcn))
    oprSymbol.attr.dontCareUnless set opvSymbol
    opvSymbol.attr.dontCareUnless set oprSymbol

    val sSymbol = cc.newTermSymbol("space", loc, TypeOut(bool, fcn, stw))

    lazy val pSymbol = cc.newTermSymbol("payload", loc, kind)
    val vSymbol = cc.newTermSymbol("valid", loc, bool)

    lazy val ipRef = ExprRef(ipSymbol)
    val ipvRef = ExprRef(ipvSymbol)
    val iprRef = ExprRef(iprSymbol)

    lazy val opRef = ExprRef(opSymbol)
    val opvRef = ExprRef(opvSymbol)
    val oprRef = ExprRef(oprSymbol)

    val sRef = ExprRef(sSymbol)

    lazy val pRef = ExprRef(pSymbol)
    val vRef = ExprRef(vSymbol)

    val statements = if (kind != TypeVoid) {
      nonVoidBody(ss, ipRef, ipvRef, oprRef, pRef, vRef)
    } else {
      voidBody(ss, ipvRef, oprRef, vRef)
    }

    val ports = if (kind != TypeVoid) {
      List(ipSymbol, ipvSymbol, iprSymbol, opSymbol, opvSymbol, oprSymbol, sSymbol)
    } else {
      List(ipvSymbol, iprSymbol, opvSymbol, oprSymbol, sSymbol)
    }

    val symbols = if (kind != TypeVoid) pSymbol :: vSymbol :: ports else vSymbol :: ports

    val decls = symbols map { symbol =>
      Decl(symbol, None)
    }

    val connects = if (kind != TypeVoid) {
      nonVoidConnects(ss, ipRef, opRef, ipvRef, iprRef, opvRef, oprRef, sRef, pRef, vRef)
    } else {
      voidConnects(ss, ipvRef, iprRef, opvRef, oprRef, sRef, vRef)
    }

    val eKind = TypeEntity(name, ports, Nil)
    val entitySymbol = cc.newTypeSymbol(name, loc, eKind)
    entitySymbol.attr.variant set "fsm"
    entitySymbol.attr.highLevelKind set eKind
    val entity = EntityLowered(entitySymbol, decls, Nil, connects, statements, Map())
    entity regularize loc
  }

  // Given a list of slice instances, build an entity that
  // instantiates each and connects them back to back
  private def buildCompoundSlice(
      slices: List[EntityLowered],
      name: String,
      loc: Loc,
      kind: Type,
      sep: String
  )(
      implicit cc: CompilerContext
  ): EntityLowered = {
    val nSlices = slices.length
    require(nSlices >= 2)

    val fcn = FlowControlTypeNone
    val stw = StorageTypeWire

    val bool = TypeUInt(TypeAssigner(Expr(1) withLoc loc))

    val ipName = "ip"
    val ipvName = s"${ipName}${sep}valid"
    val iprName = s"${ipName}${sep}ready"

    val opName = "op"
    val opvName = s"${opName}${sep}valid"
    val oprName = s"${opName}${sep}ready"

    lazy val ipSymbol = cc.newTermSymbol(ipName, loc, TypeIn(kind, fcn))
    val ipvSymbol = cc.newTermSymbol(ipvName, loc, TypeIn(bool, fcn))
    val iprSymbol = cc.newTermSymbol(iprName, loc, TypeOut(bool, fcn, stw))
    iprSymbol.attr.dontCareUnless set ipvSymbol
    ipvSymbol.attr.dontCareUnless set iprSymbol

    lazy val opSymbol = cc.newTermSymbol(opName, loc, TypeOut(kind, fcn, stw))
    val opvSymbol = cc.newTermSymbol(opvName, loc, TypeOut(bool, fcn, stw))
    val oprSymbol = cc.newTermSymbol(oprName, loc, TypeIn(bool, fcn))
    oprSymbol.attr.dontCareUnless set opvSymbol
    opvSymbol.attr.dontCareUnless set oprSymbol

    val sKind = TypeOut(TypeUInt(Expr(nSlices) regularize loc), fcn, stw)
    val sSymbol = cc.newTermSymbol("space", loc, sKind)

    lazy val ipRef = ExprRef(ipSymbol)
    val ipvRef = ExprRef(ipvSymbol)
    val iprRef = ExprRef(iprSymbol)

    lazy val opRef = ExprRef(opSymbol)
    val opvRef = ExprRef(opvSymbol)
    val oprRef = ExprRef(oprSymbol)

    val sRef = ExprRef(sSymbol)

    val instances = slices.zipWithIndex map {
      case (entity, index) =>
        val iSymbol = cc.newTermSymbol(s"slice_${index}", loc, TypeInstance(entity.symbol))
        Instance(Sym(iSymbol), Sym(entity.symbol), Nil, Nil)
    }

    val iRefs = for (Instance(Sym(iSymbol), _, _, _) <- instances) yield { ExprRef(iSymbol) }

    val connects = new ListBuffer[Connect]()

    // Create the cascade connection
    if (kind != TypeVoid) {
      // Payload
      connects append Connect(ipRef, List(iRefs.head select ipName))
      for ((aRef, bRef) <- iRefs zip iRefs.tail) {
        connects append Connect(aRef select opName, List(bRef select ipName))
      }
      connects append Connect(iRefs.last select opName, List(opRef))
    }

    // Valid
    connects append Connect(ipvRef, List(iRefs.head select ipvName))
    for ((aRef, bRef) <- iRefs zip iRefs.tail) {
      connects append Connect(aRef select opvName, List(bRef select ipvName))
    }
    connects append Connect(iRefs.last select opvName, List(opvRef))

    // Ready
    connects append Connect(oprRef, List(iRefs.last select oprName))
    for ((aRef, bRef) <- (iRefs zip iRefs.tail).reverse) {
      connects append Connect(bRef select iprName, List(aRef select oprName))
    }
    connects append Connect(iRefs.head select iprName, List(iprRef))

    // Build the space, empty and full signals
    connects append Connect(ExprCat(iRefs.reverse map { _ select "space" }), List(sRef))

    // Put it all together
    val ports = if (kind != TypeVoid) {
      List(
        ipSymbol,
        ipvSymbol,
        iprSymbol,
        opSymbol,
        opvSymbol,
        oprSymbol,
        sSymbol
      )
    } else {
      List(
        ipvSymbol,
        iprSymbol,
        opvSymbol,
        oprSymbol,
        sSymbol
      )
    }

    val decls = ports map { symbol =>
      Decl(symbol, None)
    }

    val eKind = TypeEntity(name, ports, Nil)
    val entitySymbol = cc.newTypeSymbol(name, loc, eKind)
    entitySymbol.attr.variant set "network"
    entitySymbol.attr.highLevelKind set eKind
    val entity = EntityLowered(entitySymbol, decls, instances, connects.toList, Nil, Map())
    entity regularize loc
  }

  def apply(
      slices: List[StorageSlice],
      prefix: String,
      loc: Loc,
      kind: Type
  )(
      implicit cc: CompilerContext
  ): List[EntityLowered] = {
    require(slices.nonEmpty)
    require(kind.isPacked)

    lazy val fslice = buildSlice(StorageSliceFwd, s"${prefix}${cc.sep}fslice", loc, kind, cc.sep)
    lazy val bslice = buildSlice(StorageSliceBwd, s"${prefix}${cc.sep}bslice", loc, kind, cc.sep)
    lazy val bubble = buildSlice(StorageSliceBub, s"${prefix}${cc.sep}bubble", loc, kind, cc.sep)

    val sliceEntities = slices map {
      case StorageSliceFwd => fslice
      case StorageSliceBwd => bslice
      case StorageSliceBub => bubble
    }

    if (sliceEntities.lengthCompare(1) == 0) {
      // If just one, we are done
      sliceEntities
    } else {
      // Otherwise build the compound entity
      val compoundName = s"${prefix}${cc.sep}slices"
      val compoundEntity = buildCompoundSlice(sliceEntities, compoundName, loc, kind, cc.sep)
      // The compound entity must be first, and add the distinct slices
      compoundEntity :: sliceEntities.distinct
    }
  }

}
