////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Factory to build sram entities
////////////////////////////////////////////////////////////////////////////////
package com.argondesign.alogic.core

import com.argondesign.alogic.ast.Trees.StmtAssign
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.enums.EntityVariant
import com.argondesign.alogic.lib.Math

import scala.util.ChainingSyntax

object SramFactory extends ChainingSyntax {

  /*
    fsm sram {
      in bool ce;
      in bool we;
      in uint($clog2(DEPTH)) addr;
      in uint(WIDTH) wdata;
      out uint(WIDTH) rdata;

      uint(WIDTH) storage[DEPTH];

      void main() {
        if (ce) {
          if (we) {
            stroage.write(addr, wdata);
            rdata = 0; // Or anything really
          } else {
            rdata = storage[addr];
          }
        }
        fence;
      }
    }

   */

  def apply(
      name: String,
      loc: Loc,
      width: Int,
      depth: Int
    ): (DeclEntity, DefnEntity) = {

    val fcn = FlowControlTypeNone

    val addrKind = TypeUInt(Math.clog2(depth))
    val dataKind = TypeUInt(width)

    val ceSymbol = Symbol("ce", loc) tap { _.kind = TypeIn(TypeUInt(1), fcn) }
    ceSymbol.attr.default.set(ExprInt(false, 1, 0) regularize loc)
    val weSymbol = Symbol("we", loc) tap { _.kind = TypeIn(TypeUInt(1), fcn) }
    val adSymbol = Symbol("addr", loc) tap { _.kind = TypeIn(addrKind, fcn) }
    val wdSymbol = Symbol("wdata", loc) tap { _.kind = TypeIn(dataKind, fcn) }
    val rdSymbol = Symbol("rdata", loc) tap {
      _.kind = TypeOut(dataKind, fcn, StorageTypeReg)
    }
    val stSymbol = Symbol("storage", loc) tap { _.kind = TypeArray(dataKind, depth) }

    val ceDecl = ceSymbol.mkDecl regularize loc
    val weDecl = weSymbol.mkDecl regularize loc
    val adDecl = adSymbol.mkDecl regularize loc
    val wdDecl = wdSymbol.mkDecl regularize loc
    val rdDecl = rdSymbol.mkDecl regularize loc
    val stDecl = stSymbol.mkDecl regularize loc

    val ceDefn = ceSymbol.mkDefn
    val weDefn = weSymbol.mkDefn
    val adDefn = adSymbol.mkDefn
    val wdDefn = wdSymbol.mkDefn
    val rdDefn = rdSymbol.mkDefn
    val stDefn = stSymbol.mkDefn

    val ceRef = ExprSym(ceSymbol)
    val weRef = ExprSym(weSymbol)
    val adRef = ExprSym(adSymbol)
    val wdRef = ExprSym(wdSymbol)
    val rdRef = ExprSym(rdSymbol)
    val stRef = ExprSym(stSymbol)

    val statements = List(
      StmtIf(
        ceRef,
        List(
          StmtIf(
            weRef,
            List(
              StmtExpr(ExprCall(stRef sel "write", List(ArgP(adRef), ArgP(wdRef))))
            ),
            List(
              StmtAssign(rdRef, stRef index adRef)
            )
          )
        ),
        Nil
      )
    )

    val decls = List(ceDecl, weDecl, adDecl, wdDecl, rdDecl, stDecl)
    val defns = List(ceDefn, weDefn, adDefn, wdDefn, rdDefn, stDefn) map EntSplice.apply

    val entitySymbol = Symbol(name, loc)
    val decl = DeclEntity(entitySymbol, decls) regularize loc
    val defn = DefnEntity(
      entitySymbol,
      EntityVariant.Fsm,
      EntCombProcess(statements) :: defns
    ) regularize loc
    entitySymbol.attr.sram set true
    (decl, defn)
  }

}
