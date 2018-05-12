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
// Factory to build sram entities
////////////////////////////////////////////////////////////////////////////////
package com.argondesign.alogic.core

import com.argondesign.alogic.ast.Trees.StmtAssign
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Math
import com.argondesign.alogic.typer.TypeAssigner

object SramFactory {

  /*
    fsm sram {
      in bool ce;
      in bool we;
      in ADDR addr;
      in TYPE wdata;
      out TYPE rdata;

      TYPE storage[DEPTH];

      void main() {
        if (ce) {
          if (we) {
            stroage[addr] = wdata;
            rdata = 0; // Or anything really
          } else {
            rdata = storage[addr];
          }
        }
        fence;
      }
    }

   */

  private def build(
      name: String,
      loc: Loc,
      kind: Type,
      depth: Int
  )(
      implicit cc: CompilerContext
  ): Entity = {
    require(depth >= 2)

    val fcn = FlowControlTypeNone

    val bool = TypeUInt(TypeAssigner(Expr(1) withLoc loc))

    val adKind = TypeUInt(Expr(Math.clog2(depth)) regularize loc)
    val stKind = TypeArray(kind, Expr(depth) regularize loc)

    val ceSymbol = cc.newTermSymbol("ce", loc, TypeIn(bool, fcn))
    val weSymbol = cc.newTermSymbol("we", loc, TypeIn(bool, fcn))
    val adSymbol = cc.newTermSymbol("addr", loc, TypeIn(adKind, fcn))
    val wdSymbol = cc.newTermSymbol("wdata", loc, TypeIn(kind, fcn))
    val rdSymbol = cc.newTermSymbol("rdata", loc, TypeOut(kind, fcn, StorageTypeReg))
    val stSymbol = cc.newTermSymbol("storage", loc, stKind)

    val ceRef = ExprRef(ceSymbol)
    val weRef = ExprRef(weSymbol)
    val adRef = ExprRef(adSymbol)
    val wdRef = ExprRef(wdSymbol)
    val rdRef = ExprRef(rdSymbol)
    val stRef = ExprRef(stSymbol)

    val body = List(
      StmtIf(
        ceRef,
        StmtIf(
          weRef,
          StmtBlock(
            List(
              StmtAssign(stRef index adRef, wdRef),
              StmtAssign(rdRef, ExprInt(false, kind.width.value.get.toInt, 0))
            )),
          Some(StmtAssign(rdRef, stRef index adRef))
        ),
        None
      ),
      StmtFence()
    )

    val state = State(ExprInt(false, 1, 0), body)

    val ports = List(ceSymbol, weSymbol, adSymbol, wdSymbol, rdSymbol)

    val symbols = stSymbol :: ports

    val decls = symbols map { Decl(_, None) }

    val entitySymbol = cc.newTypeSymbol(name, loc, TypeEntity(name, ports, Nil))
    val entity = Entity(Sym(entitySymbol), decls, Nil, Nil, Nil, List(state), Nil, Nil, Map())
    entity withVariant "fsm" regularize loc
  }

  def apply(
      name: String,
      loc: Loc,
      kind: Type,
      depth: Expr
  )(
      implicit cc: CompilerContext
  ): Entity = {
    require(kind.isPacked)
    require(kind != TypeVoid)

    depth.value match {
      case Some(v) => build(name, loc, kind, v.toInt)
      case None    => cc.ice(loc, "SRAM with non-computable dept")
    }
  }

}
