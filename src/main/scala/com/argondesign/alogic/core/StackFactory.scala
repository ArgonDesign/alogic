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
// Factory to build stack entities
////////////////////////////////////////////////////////////////////////////////
package com.argondesign.alogic.core

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.enums.EntityVariant

import scala.util.ChainingSyntax

object StackFactory extends ChainingSyntax {

  // Build a stack
  //
  //  fsm stack {
  //    typedef _ TYPE;
  //    const uint DEPTH = _;
  //
  //    static assert @bit(TYPE) >= 1;
  //    static assert DEPTH >= 2;
  //
  //    in       bool push;
  //    in       bool pop;
  //    in       bool set;
  //    in       TYPE d;
  //    out wire TYPE q;
  //
  //    gen for (uint n < DEPTH) {
  //      TYPE s#[n] = 0;
  //    }
  //
  //    storage#[0] -> q;
  //
  //    void() main {
  //      if (push & pop) {
  //        // Nothing to do if both pushing and popping. More specifically
  //        // ensure the top is not updated, to preserve outer locals.
  //      } else if (pop) {
  //        gen for (uint n = 0 ; n < DEPTH-1 ; n++) {
  //          s#[n] = s#[n+1];
  //        }
  //        s#[DEPTH-1] = 0;
  //      } else if (push) {
  //        gen for (uint n = DEPTH-1 ; n > 0; n--) {
  //          s#[n] = s#[n-1];
  //        }
  //        s#[0] = d;
  //      } else if (set) {
  //        s#[0] = d;
  //      }
  //      fence;
  //    }
  //  }
  def apply(
      name: String,
      loc: Loc,
      kind: TypeFund,
      depth: Int
    )(
      implicit
      cc: CompilerContext
    ): (DeclEntity, DefnEntity) = {
    require(kind.isPacked)
    require(kind != TypeVoid)
    require(depth >= 2) // 1 deep stacks replaced in Replace1Stacks

    val width = kind.width.toInt
    val signed = kind.isSigned
    val fcn = FlowControlTypeNone
    val stw = StorageTypeWire

    val pusSymbol = cc.newSymbol("push", loc) tap { _.kind = TypeIn(TypeUInt(1), fcn) }
    val popSymbol = cc.newSymbol("pop", loc) tap { _.kind = TypeIn(TypeUInt(1), fcn) }
    val setSymbol = cc.newSymbol("set", loc) tap { _.kind = TypeIn(TypeUInt(1), fcn) }
    val dSymbol = cc.newSymbol("d", loc) tap { _.kind = TypeIn(kind, fcn) }
    val qSymbol = cc.newSymbol("q", loc) tap { _.kind = TypeOut(kind, fcn, stw) }
    val sSymbols = List from {
      (0 until depth).iterator map { n => cc.newSymbol(s"s$n", loc) tap { _.kind = kind } }
    }

    val pusDecl = pusSymbol.mkDecl regularize loc
    val popDecl = popSymbol.mkDecl regularize loc
    val setDecl = setSymbol.mkDecl regularize loc
    val dDecl = dSymbol.mkDecl regularize loc
    val qDecl = qSymbol.mkDecl regularize loc
    val sDecls = sSymbols map { sSymbol => sSymbol.mkDecl regularize loc }

    val pusDefn = pusSymbol.mkDefn
    val popDefn = popSymbol.mkDefn
    val setDefn = setSymbol.mkDefn
    val dDefn = dSymbol.mkDefn
    val qDefn = qSymbol.mkDefn
    val sDefns = sSymbols map { sSymbol =>
      sSymbol.mkDefn(ExprInt(signed, width, 0)) regularize loc
    }

    val pusRef = ExprSym(pusSymbol)
    val popRef = ExprSym(popSymbol)
    val setRef = ExprSym(setSymbol)
    val dRef = ExprSym(dSymbol)
    val qRef = ExprSym(qSymbol)
    val sRefs = sSymbols map ExprSym

    val statements = List(
      StmtIf(
        pusRef & popRef,
        Nil,
        List(
          StmtIf(
            popRef,
            List from {
              (0 until depth) map { n =>
                StmtAssign(
                  sRefs(n),
                  if (n == depth - 1) ExprInt(signed, width, 0) else sRefs(n + 1)
                )
              }
            },
            List(
              StmtIf(
                pusRef,
                List from {
                  ((depth - 1) to 0 by -1) map { n =>
                    StmtAssign(sRefs(n), if (n == 0) dRef else sRefs(n - 1))
                  }
                },
                List(
                  StmtIf(setRef, List(StmtAssign(sRefs.head, dRef)), Nil)
                )
              )
            )
          )
        )
      )
    )

    val assign = EntAssign(qRef, sRefs.head)

    val decls = pusDecl :: popDecl :: setDecl :: dDecl :: qDecl :: sDecls
    val defns = (pusDefn :: popDefn :: setDefn :: dDefn :: qDefn :: sDefns) map EntDefn

    val entitySymbol = cc.newSymbol(name, loc)
    val decl = DeclEntity(entitySymbol, decls) regularize loc
    val defn = DefnEntity(
      entitySymbol,
      EntityVariant.Fsm,
      EntCombProcess(statements) :: assign :: defns
    ) regularize loc
    (decl, defn)
  }

}
