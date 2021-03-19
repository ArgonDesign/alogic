////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Factory to build sync output register entities
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.enums.EntityVariant

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

final class SyncRegFactory {

  private val cache = mutable.Map[TypeFund, (DeclEntity, DefnEntity)]()

  def items: Iterator[(TypeFund, (DeclEntity, DefnEntity))] = cache.iterator

  // Build an entity similar to the following Alogic FSM to be used as an
  // output register implementation. The body of the main function is filled
  // in by the above implementations.
  //
  // fsm sync_reg {
  //   // Upstream interface
  //   in payload_t i_payload;
  //   in bool i_valid;
  //
  //   // Downstream interface
  //   out wire payload_t o_payload;
  //   out wire bool o_valid;
  //
  //   // Local storage
  //   payload_t payload;
  //   bool valid = false;
  //
  //   void main() {
  //     if (i_valid) {
  //       payload = i_payload;
  //     }
  //     valid = i_valid;
  //   }
  //
  //   payload -> o_payload;
  //   valid -> o_valid;
  // }

  private def buildSyncReg(name: String, kind: TypeFund): (DeclEntity, DefnEntity) = {
    val fcn = FlowControlTypeNone
    val stw = StorageTypeWire
    val loc = kind match {
      case k: TypeRecord => k.symbol.loc
      case _             => Loc.synthetic
    }

    lazy val ipSymbol = Symbol("i_payload", loc) tap { _.kind = TypeIn(kind, fcn) }
    val ivSymbol = Symbol(s"i_valid", loc) tap { _.kind = TypeIn(TypeUInt(1), fcn) }
    ivSymbol.attr.default.set(ExprInt(false, 1, 0) regularize loc)
    lazy val opSymbol = Symbol("o_payload", loc) tap { _.kind = TypeOut(kind, fcn, stw) }
    val ovSymbol = Symbol(s"o_valid", loc) tap { _.kind = TypeOut(TypeUInt(1), fcn, stw) }
    lazy val pSymbol = Symbol("payload", loc) tap { _.kind = kind }
    val vSymbol = Symbol("valid", loc) tap { _.kind = TypeUInt(1) }

    lazy val ipDecl = ipSymbol.mkDecl regularize loc
    val ivDecl = ivSymbol.mkDecl regularize loc
    lazy val opDecl = opSymbol.mkDecl regularize loc
    val ovDecl = ovSymbol.mkDecl regularize loc
    lazy val pDecl = pSymbol.mkDecl regularize loc
    val vDecl = vSymbol.mkDecl regularize loc

    lazy val ipDefn = ipSymbol.mkDefn
    val ivDefn = ivSymbol.mkDefn
    lazy val opDefn = opSymbol.mkDefn
    val ovDefn = ovSymbol.mkDefn
    lazy val pDefn = pSymbol.mkDefn
    val vDefn = vSymbol.mkDefn(ExprInt(false, 1, 0))

    lazy val ipRef = ExprSym(ipSymbol)
    val ivRef = ExprSym(ivSymbol)
    lazy val opRef = ExprSym(opSymbol)
    val ovRef = ExprSym(ovSymbol)
    lazy val pRef = ExprSym(pSymbol)
    val vRef = ExprSym(vSymbol)

    val statements = if (kind != TypeVoid) {
      List(
        StmtIf(
          ivRef,
          List(StmtAssign(pRef, ipRef)),
          Nil
        ),
        StmtAssign(vRef, ivRef)
      )
    } else {
      List(StmtAssign(vRef, ivRef))
    }

    val decls = if (kind != TypeVoid) {
      List(ipDecl, ivDecl, opDecl, ovDecl, pDecl, vDecl)
    } else {
      List(ivDecl, ovDecl, vDecl)
    }

    val defns = if (kind != TypeVoid) {
      List(ipDefn, ivDefn, opDefn, ovDefn, pDefn, vDefn) map EntSplice.apply
    } else {
      List(ivDefn, ovDefn, vDefn) map EntSplice.apply
    }

    val assigns = if (kind != TypeVoid) {
      List(EntAssign(opRef, pRef), EntAssign(ovRef, vRef))
    } else {
      List(EntAssign(ovRef, vRef))
    }

    val entitySymbol = Symbol(name, loc)
    entitySymbol.attr.compilerGenerated.set(true)
    val decl = DeclEntity(entitySymbol, decls) regularize loc
    val defn = DefnEntity(
      entitySymbol,
      EntityVariant.Fsm,
      defns ::: EntCombProcess(statements) :: assigns
    ) regularize loc
    (decl, defn)
  }

  def apply(kind: TypeFund): Symbol = synchronized {
    require(kind.isPacked)

    // TODO: Sometimes record types attached to trees are a bit out of date. Fix...
    val fixedKind = kind match {
      case k: TypeRecord => k.symbol.kind.asType.kind
      case other         => other
    }

    cache
      .getOrElseUpdate(fixedKind, buildSyncReg("out_sync_" + fixedKind.toName, kind))
      ._1
      .symbol
  }

}
