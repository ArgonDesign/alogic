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
import com.argondesign.alogic.lib.Math

import scala.util.ChainingSyntax

object StackFactory extends ChainingSyntax {

  /*

  // Hardware stack interface

  stack<i8>(10) s;

  s.push(addr); // i8 => void
  s.pop();      // void => i8
  s.set(addr);  // i8 => void // Same as 's = addr' if s is an automatic variable
  s.top;        // i8 // Same as 's' if s is an automatic variable
  s.full;       // bool
  s.empty;      // bool

  restrictions:
   - Can only do a single push, pop, or set in the same cycle, compiler should err otherwise
   - .push() when full is error
   - .pop() when empty is error
   - .top when empty is error

  // Function call/return handling ('rs' is for 'return-stack':

  foo();    // maps to rs.push(return-state); goto foo;
  goto foo; // nothing to do here, just goto foo;
  return;   // maps to goto rs.pop();

  // Function argument handling

  At call site: arg.push(value)
  At exit: arg.pop()

  // Function local variable handling

  At definition of variable: local.push(initializer)
  At death/exit from function: local.pop()

  // Hardware interface:

  _en
  _d
  _q

  _push
  _pop
  _full
  _empty

  at beginning:
  _en = 1'b0

   */

  // Build an entity similar to the following Alogic FSM to be used as a
  // 1 entry stack implementation.
  //
  // fsm stack_1 {
  //   in bool en;
  //
  //   in TYPE d;
  //   in bool push;
  //   in bool pop;
  //
  //   out wire TYPE q;
  //   out wire bool empty;
  //   out wire bool full;
  //
  //   bool valid = false;
  //
  //   TYPE storage;
  //
  //   void main() {
  //     if (en) {
  //       storage = pop ? 0 : d;
  //       valid = ~pop & (valid | push);
  //     }
  //     fence;
  //   }
  //
  //  ~valid -> empty;
  //  valid -> full;
  //  storage -> q;
  // }
  private def buildStack1(
      name: String,
      loc: Loc,
      kind: TypeFund
  )(
      implicit cc: CompilerContext
  ): (DeclEntity, DefnEntity) = {
    val fcn = FlowControlTypeNone
    val stw = StorageTypeWire

    val enSymbol = cc.newSymbol("en", loc) tap { _.kind = TypeIn(TypeUInt(1), fcn) }
    enSymbol.attr.default.set(ExprInt(false, 1, 0) regularize loc)
    val pusSymbol = cc.newSymbol("push", loc) tap { _.kind = TypeIn(TypeUInt(1), fcn) }
    val popSymbol = cc.newSymbol("pop", loc) tap { _.kind = TypeIn(TypeUInt(1), fcn) }
    val dSymbol = cc.newSymbol("d", loc) tap { _.kind = TypeIn(kind, fcn) }
    val empSymbol = cc.newSymbol("empty", loc) tap { _.kind = TypeOut(TypeUInt(1), fcn, stw) }
    val fulSymbol = cc.newSymbol("full", loc) tap { _.kind = TypeOut(TypeUInt(1), fcn, stw) }
    val qSymbol = cc.newSymbol("q", loc) tap { _.kind = TypeOut(kind, fcn, stw) }
    val valSymbol = cc.newSymbol("valid", loc) tap { _.kind = TypeUInt(1) }
    val stoSymbol = cc.newSymbol("storage", loc) tap { _.kind = kind }

    val enDecl = enSymbol.mkDecl regularize loc
    val pusDecl = pusSymbol.mkDecl regularize loc
    val popDecl = popSymbol.mkDecl regularize loc
    val dDecl = dSymbol.mkDecl regularize loc
    val empDecl = empSymbol.mkDecl regularize loc
    val fulDecl = fulSymbol.mkDecl regularize loc
    val qDecl = qSymbol.mkDecl regularize loc
    val valDecl = valSymbol.mkDecl regularize loc
    val stoDecl = stoSymbol.mkDecl regularize loc

    val enDefn = enSymbol.mkDefn
    val pusDefn = pusSymbol.mkDefn
    val popDefn = popSymbol.mkDefn
    val dDefn = dSymbol.mkDefn
    val empDefn = empSymbol.mkDefn
    val fulDefn = fulSymbol.mkDefn
    val qDefn = qSymbol.mkDefn
    val valDefn = valSymbol.mkDefn(ExprInt(false, 1, 0))
    val stoDefn = stoSymbol.mkDefn

    val enRef = ExprSym(enSymbol)
    val pusRef = ExprSym(pusSymbol)
    val popRef = ExprSym(popSymbol)
    val dRef = ExprSym(dSymbol)
    val empRef = ExprSym(empSymbol)
    val fulRef = ExprSym(fulSymbol)
    val qRef = ExprSym(qSymbol)
    val valRef = ExprSym(valSymbol)
    val stoRef = ExprSym(stoSymbol)

    val statements = List(
      StmtIf(enRef,
             List(
               StmtAssign(stoRef, ExprTernary(popRef, ExprInt(kind.isSigned, kind.width.toInt, 0), dRef)),
               StmtAssign(valRef, ~popRef & (valRef | pusRef))
             ),
             Nil)
    )

    val decls = List(enDecl, pusDecl, popDecl, dDecl, empDecl, fulDecl, qDecl, valDecl, stoDecl)

    val defns = List(enDefn, pusDefn, popDefn, dDefn, empDefn, fulDefn, qDefn, valDefn, stoDefn) map EntDefn

    val connects = List(
      EntConnect(~valRef, List(empRef)),
      EntConnect(valRef, List(fulRef)),
      EntConnect(stoRef, List(qRef))
    )

    val entitySymbol = cc.newSymbol(name, loc)
    val decl = DeclEntity(entitySymbol, decls) regularize loc
    val defn = DefnEntity(
      entitySymbol,
      EntityVariant.Fsm,
      defns ::: EntCombProcess(statements) :: connects
    ) regularize loc
    (decl, defn)
  }

  // Build an entity similar to the following Alogic FSM to be used as an
  // N entry stack implementation with N >= 2.
  //
  // fsm stack_N {
  //   const DEPTH; // > 1
  //
  //   in bool en;
  //
  //   in TYPE d;
  //   in bool push;
  //   in bool pop;
  //
  //   out wire TYPE q;
  //   out bool empty = true;
  //   out bool full = false;
  //
  //   TYPE storage[DEPTH];
  //
  //   uint($clog2(DEPTH)) ptr = 0; // Ptr to current entry
  //
  //   state main {
  //     if (en) {
  //       if (pop) {
  //         empty = ptr == 0;
  //         full = false;
  //         ptr = ptr - ~empty;
  //       } else {
  //         ptr = ptr + (~empty & ~full & push);
  //         storage.write(ptr, d);
  //         empty = empty & ~push;
  //         full = ptr == DEPTH - 1;
  //       }
  //     }
  //     fence;
  //   }
  //
  //   (empty ? 0 : storage[ptr]) -> q;
  // }
  private def buildStackN(
      name: String,
      loc: Loc,
      kind: TypeFund,
      depth: BigInt
  )(
      implicit cc: CompilerContext
  ): (DeclEntity, DefnEntity) = {
    require(depth >= 2)

    val fcn = FlowControlTypeNone
    val stw = StorageTypeWire
    val str = StorageTypeReg

    val ptrWidth = Math.clog2(depth)

    val enSymbol = cc.newSymbol("en", loc) tap { _.kind = TypeIn(TypeUInt(1), fcn) }
    val pusSymbol = cc.newSymbol("push", loc) tap { _.kind = TypeIn(TypeUInt(1), fcn) }
    val popSymbol = cc.newSymbol("pop", loc) tap { _.kind = TypeIn(TypeUInt(1), fcn) }
    val dSymbol = cc.newSymbol("d", loc) tap { _.kind = TypeIn(kind, fcn) }
    val empSymbol = cc.newSymbol("empty", loc) tap { _.kind = TypeOut(TypeUInt(1), fcn, str) }
    val fulSymbol = cc.newSymbol("full", loc) tap { _.kind = TypeOut(TypeUInt(1), fcn, str) }
    val qSymbol = cc.newSymbol("q", loc) tap { _.kind = TypeOut(kind, fcn, stw) }
    val stoSymbol = cc.newSymbol("storage", loc) tap { _.kind = TypeArray(kind, depth) }
    val ptrSymbol = cc.newSymbol("ptr", loc) tap { _.kind = TypeUInt(ptrWidth) }

    val enDecl = enSymbol.mkDecl regularize loc
    val pusDecl = pusSymbol.mkDecl regularize loc
    val popDecl = popSymbol.mkDecl regularize loc
    val dDecl = dSymbol.mkDecl regularize loc
    val empDecl = empSymbol.mkDecl regularize loc
    val fulDecl = fulSymbol.mkDecl regularize loc
    val qDecl = qSymbol.mkDecl regularize loc
    val stoDecl = stoSymbol.mkDecl regularize loc
    val ptrDecl = ptrSymbol.mkDecl regularize loc

    val enDefn = enSymbol.mkDefn
    val pusDefn = pusSymbol.mkDefn
    val popDefn = popSymbol.mkDefn
    val dDefn = dSymbol.mkDefn
    val empDefn = empSymbol.mkDefn(ExprInt(false, 1, 1))
    val fulDefn = fulSymbol.mkDefn(ExprInt(false, 1, 0))
    val qDefn = qSymbol.mkDefn
    val stoDefn = stoSymbol.mkDefn
    val ptrDefn = ptrSymbol.mkDefn(ExprInt(false, ptrWidth, 0))

    val enRef = ExprSym(enSymbol)
    val pusRef = ExprSym(pusSymbol)
    val popRef = ExprSym(popSymbol)
    val dRef = ExprSym(dSymbol)
    val empRef = ExprSym(empSymbol)
    val fulRef = ExprSym(fulSymbol)
    val qRef = ExprSym(qSymbol)
    val stoRef = ExprSym(stoSymbol)
    val ptrRef = ExprSym(ptrSymbol)

    def zextPtrWidth(bool: Expr): Expr = {
      if (ptrWidth == 1) {
        bool
      } else {
        ExprCat(List(ExprInt(false, ptrWidth - 1, 0), bool))
      }
    }

    val statements = List(
      StmtIf(
        enRef,
        List(
          StmtIf(
            popRef,
            List(
              StmtAssign(empRef, ExprBinary(ptrRef, "==", ExprInt(false, ptrWidth, 0))),
              StmtAssign(fulRef, ExprInt(false, 1, 0)),
              StmtAssign(ptrRef, ptrRef - zextPtrWidth(~empRef))
            ),
            List(
              StmtAssign(ptrRef, ptrRef + zextPtrWidth(~empRef & ~fulRef & pusRef)),
              StmtExpr(ExprCall(stoRef select "write", List(ArgP(ptrRef), ArgP(dRef)))),
              StmtAssign(empRef, empRef & ~pusRef),
              StmtAssign(fulRef, ExprBinary(ptrRef, "==", ExprInt(false, ptrWidth, depth - 1)))
            )
          )
        ),
        Nil
      )
    )

    val decls = List(enDecl, pusDecl, popDecl, dDecl, empDecl, fulDecl, qDecl, stoDecl, ptrDecl)

    val defns = List(enDefn, pusDefn, popDefn, dDefn, empDefn, fulDefn, qDefn, stoDefn, ptrDefn) map EntDefn

    val connects = List(
      EntConnect(ExprTernary(empRef, ExprInt(kind.isSigned, kind.width.toInt, 0), ExprIndex(stoRef, ptrRef)), List(qRef))
    )

    val entitySymbol = cc.newSymbol(name, loc)
    val decl = DeclEntity(entitySymbol, decls) regularize loc
    val defn = DefnEntity(
      entitySymbol,
      EntityVariant.Fsm,
      defns ::: EntCombProcess(statements) :: connects
    ) regularize loc
    (decl, defn)
  }

  def apply(
      name: String,
      loc: Loc,
      kind: TypeFund,
      depth: BigInt
  )(
      implicit cc: CompilerContext
  ): (DeclEntity, DefnEntity) = {
    require(kind.isPacked)
    require(kind != TypeVoid)

    if (depth == 1) {
      buildStack1(name, loc, kind)
    } else {
      buildStackN(name, loc, kind, depth)
    }
  }

}
