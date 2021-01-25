////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Factory to build sync ready output slice entities
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.enums.EntityVariant

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.chaining.scalaUtilChainingOps

final class SyncSliceFactory(implicit cc: CompilerContext) {

  private val cache = mutable.Map[(TypeFund, List[StorageSlice]), (DeclEntity, DefnEntity)]()

  def items: Iterator[((TypeFund, List[StorageSlice]), (DeclEntity, DefnEntity))] = cache.iterator

  // slice logic for void payload:
  private def voidBody(
      ss: StorageSlice,
      ivRef: ExprSym,
      orRef: ExprSym,
      vRef: ExprSym
    ): List[Stmt] = ss match {
    case StorageSliceBub =>
      // valid = ~valid & i_valid | valid & ~o_ready;
      List(StmtAssign(vRef, ~vRef & ivRef | vRef & ~orRef))
    case StorageSliceFwd =>
      // valid = i_valid | valid & ~o_ready;
      List(StmtAssign(vRef, ivRef | vRef & ~orRef))
    case StorageSliceBwd =>
      // valid = (valid | i_valid) & ~o_ready;
      List(StmtAssign(vRef, (vRef | ivRef) & ~orRef))
  }

  // slice net assignments for void payload:
  private def voidAssigns(
      ss: StorageSlice,
      ivRef: ExprSym,
      irRef: ExprSym,
      ovRef: ExprSym,
      orRef: ExprSym,
      sRef: ExprSym,
      vRef: ExprSym
    ): List[EntAssign] = ss match {
    case StorageSliceBub =>
      // o_valid <- valid;
      // i_ready <- ~valid;
      // space <- ~valid;
      List(
        EntAssign(ovRef, vRef),
        EntAssign(irRef, ~vRef),
        EntAssign(sRef, ~vRef)
      )
    case StorageSliceFwd =>
      // o_valid <- valid;
      // i_ready <- ~valid | o_ready;
      // space <- ~valid;
      List(
        EntAssign(ovRef, vRef),
        EntAssign(irRef, ~vRef | orRef),
        EntAssign(sRef, ~vRef)
      )
    case StorageSliceBwd =>
      // o_valid <- valid | i_valid;
      // i_ready <- ~valid;
      // space <- ~valid;
      List(
        EntAssign(ovRef, vRef | ivRef),
        EntAssign(irRef, ~vRef),
        EntAssign(sRef, ~vRef)
      )
  }

  // slice logic for non-void payload:
  private def nonVoidBody(
      ss: StorageSlice,
      ipRef: ExprSym,
      ivRef: ExprSym,
      orRef: ExprSym,
      pRef: ExprSym,
      vRef: ExprSym
    ): List[Stmt] = ss match {
    case StorageSliceBub =>
      // if (i_valid & ~valid) {
      //   payload = i_payload;
      // }
      // valid = ~valid & i_valid | valid & ~o_ready;
      List(
        StmtIf(
          ivRef & ~vRef,
          List(StmtAssign(pRef, ipRef)),
          Nil
        ),
        StmtAssign(vRef, ~vRef & ivRef | vRef & ~orRef)
      )
    case StorageSliceFwd =>
      // if (i_valid & (~valid | o_ready)) {
      //   payload = i_payload;
      // }
      // valid = i_valid | valid & ~o_ready;
      List(
        StmtIf(
          ivRef & (~vRef | orRef),
          List(StmtAssign(pRef, ipRef)),
          Nil
        ),
        StmtAssign(vRef, ivRef | vRef & ~orRef)
      )
    case StorageSliceBwd =>
      // if (i_valid & ~valid & ~o_ready) {
      //   payload = i_payload;
      // }
      // valid = (valid | i_valid) & ~o_ready;
      List(
        StmtIf(
          ivRef & ~vRef & ~orRef,
          List(StmtAssign(pRef, ipRef)),
          Nil
        ),
        StmtAssign(vRef, (vRef | ivRef) & ~orRef)
      )
  }

  // slice connects for non-void payload:
  private def nonVoidAssigns(
      ss: StorageSlice,
      ipRef: ExprSym,
      opRef: ExprSym,
      ivRef: ExprSym,
      irRef: ExprSym,
      ovRef: ExprSym,
      orRef: ExprSym,
      sRef: ExprSym,
      pRef: ExprSym,
      vRef: ExprSym
    ): List[EntAssign] = ss match {
    case StorageSliceBub =>
      // o_payload <- payload;
      // o_valid <- valid;
      // i_ready <- ~valid;
      // space <- ~valid;
      List(
        EntAssign(ovRef, vRef),
        EntAssign(opRef, pRef),
        EntAssign(irRef, ~vRef),
        EntAssign(sRef, ~vRef)
      )
    case StorageSliceFwd =>
      // o_payload <- payload;
      // o_valid <- valid;
      // i_ready <- ~valid | o_ready;
      // space <- ~valid;
      List(
        EntAssign(opRef, pRef),
        EntAssign(ovRef, vRef),
        EntAssign(irRef, ~vRef | orRef),
        EntAssign(sRef, ~vRef)
      )
    case StorageSliceBwd =>
      // o_payload <- valid ? payload : i_payload;
      // o_valid <- valid | i_valid;
      // i_ready <- ~valid;
      // space <- ~valid;
      List(
        EntAssign(opRef, ExprCond(vRef, pRef, ipRef)),
        EntAssign(ovRef, vRef | ivRef),
        EntAssign(irRef, ~vRef),
        EntAssign(sRef, ~vRef)
      )
  }

  // Build an entity similar to the following Alogic FSM to be used as an
  // output slice implementation. The body of the main function is filled
  // in by the above implementations.
  //
  // fsm slice_bubble {
  //   // Upstream interface
  //   in payload_t i_payload;
  //   in bool i_valid;
  //   out wire bool i_ready;
  //
  //   // Downstream interface
  //   out wire payload_t o_payload;
  //   out wire bool o_valid;
  //   in bool o_ready;
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
      name: String,
      kind: TypeFund,
      ss: StorageSlice
    ): (DeclEntity, DefnEntity) = {
    val fcn = FlowControlTypeNone
    val stw = StorageTypeWire
    val loc = kind match {
      case k: TypeRecord => k.symbol.loc
      case _             => Loc.synthetic
    }

    lazy val ipSymbol = Symbol("i_payload", loc) tap { _.kind = TypeIn(kind, fcn) }
    val ivSymbol = Symbol(s"i_valid", loc) tap { _.kind = TypeIn(TypeUInt(1), fcn) }
    ivSymbol.attr.default.set(ExprInt(false, 1, 0) regularize loc)
    val irSymbol = Symbol(s"i_ready", loc) tap { _.kind = TypeOut(TypeUInt(1), fcn, stw) }
    irSymbol.attr.dontCareUnless set ivSymbol
    ivSymbol.attr.dontCareUnless set irSymbol
    lazy val opSymbol = Symbol("o_payload", loc) tap { _.kind = TypeOut(kind, fcn, stw) }
    val ovSymbol = Symbol(s"o_valid", loc) tap { _.kind = TypeOut(TypeUInt(1), fcn, stw) }
    val orSymbol = Symbol(s"o_ready", loc) tap { _.kind = TypeIn(TypeUInt(1), fcn) }
    orSymbol.attr.default.set(ExprInt(false, 1, 0) regularize loc)
    orSymbol.attr.dontCareUnless set ovSymbol
    ovSymbol.attr.dontCareUnless set orSymbol
    val sSymbol = Symbol("space", loc) tap { _.kind = TypeOut(TypeUInt(1), fcn, stw) }
    lazy val pSymbol = Symbol("payload", loc) tap { _.kind = kind }
    val vSymbol = Symbol("valid", loc) tap { _.kind = TypeUInt(1) }

    lazy val ipDecl = ipSymbol.mkDecl regularize loc
    val ivDecl = ivSymbol.mkDecl regularize loc
    val irDecl = irSymbol.mkDecl regularize loc
    lazy val opDecl = opSymbol.mkDecl regularize loc
    val ovDecl = ovSymbol.mkDecl regularize loc
    val orDecl = orSymbol.mkDecl regularize loc
    val sDecl = sSymbol.mkDecl regularize loc
    lazy val pDecl = pSymbol.mkDecl regularize loc
    val vDecl = vSymbol.mkDecl regularize loc

    lazy val ipDefn = ipSymbol.mkDefn
    val ivDefn = ivSymbol.mkDefn
    val irDefn = irSymbol.mkDefn
    lazy val opDefn = opSymbol.mkDefn
    val ovDefn = ovSymbol.mkDefn
    val orDefn = orSymbol.mkDefn
    val sDefn = sSymbol.mkDefn
    lazy val pDefn = pSymbol.mkDefn
    val vDefn = vSymbol.mkDefn(ExprInt(false, 1, 0))

    lazy val ipRef = ExprSym(ipSymbol)
    val ivRef = ExprSym(ivSymbol)
    val irRef = ExprSym(irSymbol)
    lazy val opRef = ExprSym(opSymbol)
    val ovRef = ExprSym(ovSymbol)
    val orRef = ExprSym(orSymbol)
    val sRef = ExprSym(sSymbol)
    lazy val pRef = ExprSym(pSymbol)
    val vRef = ExprSym(vSymbol)

    val statements = if (kind != TypeVoid) {
      nonVoidBody(ss, ipRef, ivRef, orRef, pRef, vRef)
    } else {
      voidBody(ss, ivRef, orRef, vRef)
    }

    val decls = if (kind != TypeVoid) {
      List(ipDecl, ivDecl, irDecl, opDecl, ovDecl, orDecl, sDecl, pDecl, vDecl)
    } else {
      List(ivDecl, irDecl, ovDecl, orDecl, sDecl, vDecl)
    }

    val defns = if (kind != TypeVoid) {
      List(ipDefn, ivDefn, irDefn, opDefn, ovDefn, orDefn, sDefn, pDefn, vDefn) map EntSplice.apply
    } else {
      List(ivDefn, irDefn, ovDefn, orDefn, sDefn, vDefn) map EntSplice.apply
    }

    val connects = if (kind != TypeVoid) {
      nonVoidAssigns(ss, ipRef, opRef, ivRef, irRef, ovRef, orRef, sRef, pRef, vRef)
    } else {
      voidAssigns(ss, ivRef, irRef, ovRef, orRef, sRef, vRef)
    }

    val entitySymbol = Symbol(name, loc)
    val decl = DeclEntity(entitySymbol, decls) regularize loc
    val defn = DefnEntity(
      entitySymbol,
      EntityVariant.Fsm,
      defns ::: EntCombProcess(statements) :: connects
    ) regularize loc
    (decl, defn)
  }

  // Given a list of slice instances, build an entity that
  // instantiates each and connects them back to back
  private def buildCompoundSlice(
      name: String,
      kind: TypeFund,
      slices: List[Symbol]
    ): (DeclEntity, DefnEntity) = {
    val nSlices = slices.length
    require(nSlices >= 2)
    val loc = kind match {
      case k: TypeRecord => k.symbol.loc
      case _             => Loc.synthetic
    }

    val fcn = FlowControlTypeNone
    val stw = StorageTypeWire

    lazy val ipSymbol = Symbol("i_payload", loc) tap { _.kind = TypeIn(kind, fcn) }
    val ivSymbol = Symbol("i_valid", loc) tap { _.kind = TypeIn(TypeUInt(1), fcn) }
    ivSymbol.attr.default.set(ExprInt(false, 1, 0) regularize loc)
    val irSymbol = Symbol("i_ready", loc) tap { _.kind = TypeOut(TypeUInt(1), fcn, stw) }
    irSymbol.attr.dontCareUnless set ivSymbol
    ivSymbol.attr.dontCareUnless set irSymbol
    lazy val opSymbol = Symbol("o_payload", loc) tap { _.kind = TypeOut(kind, fcn, stw) }
    val ovSymbol = Symbol("o_valid", loc) tap { _.kind = TypeOut(TypeUInt(1), fcn, stw) }
    val orSymbol = Symbol("o_ready", loc) tap { _.kind = TypeIn(TypeUInt(1), fcn) }
    orSymbol.attr.default.set(ExprInt(false, 1, 0) regularize loc)
    orSymbol.attr.dontCareUnless set ovSymbol
    ovSymbol.attr.dontCareUnless set orSymbol
    val sSymbol = Symbol("space", loc) tap { _.kind = TypeOut(TypeUInt(nSlices), fcn, stw) }

    lazy val ipDecl = ipSymbol.mkDecl regularize loc
    val ivDecl = ivSymbol.mkDecl regularize loc
    val irDecl = irSymbol.mkDecl regularize loc
    lazy val opDecl = opSymbol.mkDecl regularize loc
    val ovDecl = ovSymbol.mkDecl regularize loc
    val orDecl = orSymbol.mkDecl regularize loc
    val sDecl = sSymbol.mkDecl regularize loc

    lazy val ipDefn = ipSymbol.mkDefn
    val ivDefn = ivSymbol.mkDefn
    val irDefn = irSymbol.mkDefn
    lazy val opDefn = opSymbol.mkDefn
    val ovDefn = ovSymbol.mkDefn
    val orDefn = orSymbol.mkDefn
    val sDefn = sSymbol.mkDefn

    lazy val ipRef = ExprSym(ipSymbol)
    val ivRef = ExprSym(ivSymbol)
    val irRef = ExprSym(irSymbol)
    lazy val opRef = ExprSym(opSymbol)
    val ovRef = ExprSym(ovSymbol)
    val orRef = ExprSym(orSymbol)
    val sRef = ExprSym(sSymbol)

    val iSymbols = slices.zipWithIndex map {
      case (eSymbol, index) =>
        val iSymbol = Symbol(s"slice_$index", loc)
        iSymbol.kind = eSymbol.kind.asType.kind
        iSymbol
    }

    val iDecls = iSymbols map { _.mkDecl regularize loc }

    val iDefns = iSymbols map { _.mkDefn }

    val iRefs = iSymbols map ExprSym.apply

    val assigns = new ListBuffer[EntAssign]()

    // Create the cascade connection
    if (kind != TypeVoid) {
      // Payload
      assigns append EntAssign(iRefs.head sel "i_payload", ipRef)
      for ((aRef, bRef) <- iRefs zip iRefs.tail) {
        assigns append EntAssign(bRef sel "i_payload", aRef sel "o_payload")
      }
      assigns append EntAssign(opRef, iRefs.last sel "o_payload")
    }

    // Valid
    assigns append EntAssign(iRefs.head sel "i_valid", ivRef)
    for ((aRef, bRef) <- iRefs zip iRefs.tail) {
      assigns append EntAssign(bRef sel "i_valid", aRef sel "o_valid")
    }
    assigns append EntAssign(ovRef, iRefs.last sel "o_valid")

    // Ready
    assigns append EntAssign(iRefs.last sel "o_ready", orRef)
    for ((aRef, bRef) <- (iRefs zip iRefs.tail).reverse) {
      assigns append EntAssign(aRef sel "o_ready", bRef sel "i_ready")
    }
    assigns append EntAssign(irRef, iRefs.head sel "i_ready")

    // Build the space, empty and full signals
    assigns append EntAssign(sRef, ExprCat(iRefs.reverse map { _ sel "space" }))

    // Put it all together
    val decls = if (kind != TypeVoid) {
      List(ipDecl, ivDecl, irDecl, opDecl, ovDecl, orDecl, sDecl) ::: iDecls
    } else {
      List(ivDecl, irDecl, ovDecl, orDecl, sDecl) ::: iDecls
    }

    val defns = if (kind != TypeVoid) {
      (ipDefn :: ivDefn :: irDefn :: opDefn :: ovDefn :: orDefn :: sDefn :: iDefns) map EntSplice.apply
    } else {
      (ivDefn :: irDefn :: ovDefn :: orDefn :: sDefn :: iDefns) map EntSplice.apply
    }

    val entitySymbol = Symbol(name, loc)
    val decl = DeclEntity(entitySymbol, decls) regularize loc
    val defn = DefnEntity(
      entitySymbol,
      EntityVariant.Net,
      defns ::: assigns.toList
    ) regularize loc
    (decl, defn)
  }

  def apply(kind: TypeFund, slices: List[StorageSlice]): Symbol = synchronized {
    require(kind.isPacked)
    require(slices.nonEmpty)

    // TODO: Sometimes record types attached to trees are a bit out of date. Fix...
    val fixedKind = kind match {
      case k: TypeRecord => k.symbol.kind.asType.kind
      case other         => other
    }

    def name(middle: String): String = "out_sync_ready_" + middle + cc.sep + fixedKind.toName

    // Build the individual component slices
    val sliceEntities = slices map {
      case StorageSliceFwd =>
        cache.getOrElseUpdate(
          (fixedKind, List(StorageSliceFwd)),
          buildSlice(name("fslice"), fixedKind, StorageSliceFwd)
        )
      case StorageSliceBwd =>
        cache.getOrElseUpdate(
          (fixedKind, List(StorageSliceBwd)),
          buildSlice(name("bslice"), fixedKind, StorageSliceBwd)
        )
      case StorageSliceBub =>
        cache.getOrElseUpdate(
          (fixedKind, List(StorageSliceBub)),
          buildSlice(name("bubble"), fixedKind, StorageSliceBub)
        )
    } map { _._1.symbol }

    if (sliceEntities.lengthIs == 1) {
      // If just one, we are done
      sliceEntities.head
    } else {
      // Otherwise build the compound slice
      def sliceNames: String = slices
        .map {
          case StorageSliceFwd => "fslice"
          case StorageSliceBwd => "bslice"
          case StorageSliceBub => "bubble"
        }
        .mkString("_")
      cache
        .getOrElseUpdate(
          (fixedKind, slices),
          buildCompoundSlice(name(sliceNames), fixedKind, sliceEntities)
        )
        ._1
        .symbol
    }
  }

}
