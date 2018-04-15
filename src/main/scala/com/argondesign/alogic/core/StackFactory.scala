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
import com.argondesign.alogic.lib.Math
import com.argondesign.alogic.typer.TypeAssigner

object StackFactory {

  // Build an entity similar to the following Alogic FSM to be used as a
  // 1 entry stack implementation.
  //
  // fsm stack_1 {
  //   in TYPE d;
  //   in bool push;
  //   in bool pop;
  //
  //   out wire TYPE q;
  //   out wire bool empty;
  //   out wire bool full;
  //
  //   bool valid;
  //
  //   TYPE storage;
  //
  //   void main() {
  //     empty = ~valid;
  //     full = valid;
  //     q = storage;
  //     storage = d;
  //     valid = ~pop & (valid | push);
  //     fence;
  //   }
  // }
  private def buildStack1(
      name: String,
      loc: Loc,
      kind: Type
  )(
      implicit cc: CompilerContext
  ): Entity = {
    val fcn = FlowControlTypeNone
    val stw = StorageTypeWire

    val bool = TypeUInt(TypeAssigner(Expr(1) withLoc loc))

    val pusSymbol = cc.newTermSymbol("push", loc, TypeIn(bool, fcn))
    val popSymbol = cc.newTermSymbol("pop", loc, TypeIn(bool, fcn))
    val dSymbol = cc.newTermSymbol("d", loc, TypeIn(kind, fcn))

    val empSymbol = cc.newTermSymbol("empty", loc, TypeOut(bool, fcn, stw))
    val fulSymbol = cc.newTermSymbol("full", loc, TypeOut(bool, fcn, stw))
    val qSymbol = cc.newTermSymbol("q", loc, TypeOut(kind, fcn, stw))

    val valSymbol = cc.newTermSymbol("valid", loc, bool)

    val stoSymbol = cc.newTermSymbol("storage", loc, kind)

    val pusRef = ExprRef(Sym(pusSymbol))
    val popRef = ExprRef(Sym(popSymbol))
    val dRef = ExprRef(Sym(dSymbol))

    val empRef = ExprRef(Sym(empSymbol))
    val fulRef = ExprRef(Sym(fulSymbol))
    val qRef = ExprRef(Sym(qSymbol))

    val valRef = ExprRef(Sym(valSymbol))

    val stoRef = ExprRef(Sym(stoSymbol))

    val body = List(
      StmtAssign(empRef, ~valRef),
      StmtAssign(fulRef, valRef),
      StmtAssign(qRef, stoRef),
      StmtAssign(stoRef, dRef),
      StmtAssign(valRef, ~popRef & (valRef | pusRef)),
      StmtFence()
    )

    val state = State(ExprInt(false, 1, 0), body)

    val ports = List(
      dSymbol,
      pusSymbol,
      popSymbol,
      qSymbol,
      empSymbol,
      fulSymbol
    )

    val symbols = valSymbol :: stoSymbol :: ports

    val decls = symbols map { symbol =>
      Decl(Sym(symbol), symbol.denot.kind, None)
    }

    val entitySymbol = cc.newTypeSymbol(name, loc, TypeEntity(name, ports, Nil))
    val entity = Entity(Sym(entitySymbol), decls, Nil, Nil, Nil, List(state), Nil, Nil, Map())
    entity withVariant "fsm" regularize loc
  }

  // Build an entity similar to the following Alogic FSM to be used as an
  // N entry stack implementation with N >= 2.
  //
  // fsm stack_N {
  //   const DEPTH; // > 1
  //
  //   in TYPE d;
  //   in bool push;
  //   in bool pop;
  //
  //   out wire TYPE q;
  //   out wire bool empty;
  //   out wire bool full;
  //
  //   bool oreg_empty = true;
  //   bool oreg_full  = false;
  //
  //   TYPE storage[DEPTH];
  //
  //   uint($clog2(DEPTH)) ptr = 0; // Ptr to current entry
  //
  //   state main {
  //     empty = oreg_empty;
  //     full = oreg_full;
  //     q = storage[ptr];
  //
  //     if (pop) {
  //       oreg_empty = ptr == 0;
  //       oreg_full = false;
  //       ptr = ptr - ~oreg_empty;
  //     } else {
  //       ptr = ptr + (~oreg_full & push);
  //       storage[ptr] = d;
  //       oreg_empty = oreg_empty & ~push;
  //       oreg_full = ptr == DEPTH - 1;
  //     }
  //
  //     fence;
  //   }
  // }
  private def buildStackN(
      name: String,
      loc: Loc,
      kind: Type,
      depth: Int
  )(
      implicit cc: CompilerContext
  ): Entity = {
    require(depth >= 2)

    val fcn = FlowControlTypeNone
    val stw = StorageTypeWire

    val bool = TypeUInt(TypeAssigner(Expr(1) withLoc loc))

    val ptrWidth = Math.clog2(depth)

    val pusSymbol = cc.newTermSymbol("push", loc, TypeIn(bool, fcn))
    val popSymbol = cc.newTermSymbol("pop", loc, TypeIn(bool, fcn))
    val dSymbol = cc.newTermSymbol("d", loc, TypeIn(kind, fcn))

    val empSymbol = cc.newTermSymbol("empty", loc, TypeOut(bool, fcn, stw))
    val fulSymbol = cc.newTermSymbol("full", loc, TypeOut(bool, fcn, stw))
    val qSymbol = cc.newTermSymbol("q", loc, TypeOut(kind, fcn, stw))

    val oreSymbol = cc.newTermSymbol("oreg_empty", loc, bool)
    val orfSymbol = cc.newTermSymbol("oreg_full", loc, bool)

    val stoKind = TypeArray(kind, ExprNum(false, depth) regularize loc)
    val stoSymbol = cc.newTermSymbol("storage", loc, stoKind)
    val ptrKind = TypeUInt(Expr(ptrWidth) regularize loc)
    val ptrSymbol = cc.newTermSymbol("ptr", loc, ptrKind)

    val pusRef = ExprRef(Sym(pusSymbol))
    val popRef = ExprRef(Sym(popSymbol))
    val dRef = ExprRef(Sym(dSymbol))

    val empRef = ExprRef(Sym(empSymbol))
    val fulRef = ExprRef(Sym(fulSymbol))
    val qRef = ExprRef(Sym(qSymbol))

    val oreRef = ExprRef(Sym(oreSymbol))
    val orfRef = ExprRef(Sym(orfSymbol))

    val stoRef = ExprRef(Sym(stoSymbol))
    val ptrRef = ExprRef(Sym(ptrSymbol))

    def zextPtrWidth(bool: Expr): Expr = {
      if (ptrWidth == 1) {
        bool
      } else {
        ExprCat(List(ExprInt(false, ptrWidth - 1, 0), bool))
      }
    }

    val body = List(
      StmtAssign(empRef, oreRef),
      StmtAssign(fulRef, orfRef),
      StmtAssign(qRef, ExprIndex(stoRef, ptrRef)),
      StmtIf(
        popRef,
        StmtBlock(
          List(
            StmtAssign(oreRef, ExprBinary(ptrRef, "==", ExprInt(false, ptrWidth, 0))),
            StmtAssign(orfRef, ExprInt(false, 1, 0)),
            StmtAssign(ptrRef, ptrRef - zextPtrWidth(~oreRef))
          )),
        Some(
          StmtBlock(
            List(
              StmtAssign(ptrRef, ptrRef + zextPtrWidth(~orfRef & pusRef)),
              StmtAssign(ExprIndex(stoRef, ptrRef), dRef),
              StmtAssign(oreRef, oreRef & ~pusRef),
              StmtAssign(orfRef, ExprBinary(ptrRef, "==", ExprInt(false, ptrWidth, depth - 1)))
            ))
        )
      ),
      StmtFence()
    )

    val state = State(ExprInt(false, 1, 0), body)

    val ports = List(
      dSymbol,
      pusSymbol,
      popSymbol,
      qSymbol,
      empSymbol,
      fulSymbol
    )

    val symbols = oreSymbol :: orfSymbol :: stoSymbol :: ptrSymbol :: ports

    val decls = symbols map { symbol =>
      val init = if (symbol == oreSymbol) Some(ExprInt(false, 1, 1)) else None
      Decl(Sym(symbol), symbol.denot.kind, init)
    }

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
      case Some(v) if v == 1 => buildStack1(name, loc, kind)
      case Some(v)           => buildStackN(name, loc, kind, v.toInt)
      case None              => cc.ice(loc, "Stack with non-computable dept")
    }
  }

}
