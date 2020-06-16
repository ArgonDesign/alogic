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
// Add implicit casts
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Math.clog2
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

final class AddCasts(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  private def cast(kind: Type, expr: Expr) = {
    val (castType, castExpr) = if (kind.underlying.isNum) {
      (TypeNum(expr.tpe.isSigned), expr.simplify)
    } else {
      (TypeInt(expr.tpe.isSigned, kind.width), expr)
    }
    if (castType.isNum && !castExpr.isKnownConst) {
      cc.ice(expr, s"Trying to cast non-constant expression to type '${castType.toSource}'")
    }
    castExpr cast castType
  }

  override def transform(tree: Tree): Tree = {
    val result = tree match {
      case defn: Defn
          if defn.symbol.kind.isPacked && (defn.initializer exists { _.tpe.underlying.isNum }) =>
        val newInit = cast(defn.symbol.kind, defn.initializer.get)
        defn match {
          case d: DefnVar   => d.copy(initOpt = Some(newInit))
          case d: DefnVal   => d.copy(init = newInit)
          case d: DefnOut   => d.copy(initOpt = Some(newInit))
          case d: DefnConst => d.copy(init = newInit)
          case d: DefnGen   => d.copy(init = newInit)
          case _            => unreachable
        }

      case defn: Defn if defn.symbol.kind.underlying.isNum && (defn.initializer exists {
            _.tpe.isSigned != defn.symbol.kind.isSigned
          }) =>
        val init = defn.initializer.get
        val newInit = if (defn.symbol.kind.isSigned) {
          cc.makeBuiltinCall("$signed", init.loc, List(init))
        } else {
          cc.makeBuiltinCall("$unsigned", init.loc, List(init))
        }
        defn match {
          case d: DefnVar   => d.copy(initOpt = Some(newInit))
          case d: DefnVal   => d.copy(init = newInit)
          case d: DefnOut   => d.copy(initOpt = Some(newInit))
          case d: DefnConst => d.copy(init = newInit)
          case d: DefnGen   => d.copy(init = newInit)
          case _            => unreachable
        }

      case stmt @ StmtAssign(lhs, rhs) if lhs.tpe.isPacked && rhs.tpe.underlying.isNum =>
        stmt.copy(rhs = cast(lhs.tpe, rhs))

      case stmt @ StmtUpdate(lhs, "&" | "|" | "^" | "*" | "/" | "%" | "+" | "-", rhs)
          if lhs.tpe.isPacked && rhs.tpe.underlying.isNum =>
        stmt.copy(rhs = cast(lhs.tpe, rhs))

      case stmt @ StmtReturn(_, Some(expr)) =>
        enclosingSymbols.headOption map { fSymbol =>
          stmt.copy(exprOpt = Some(cast(fSymbol.kind.asCallable.retType, expr)))
        } getOrElse tree

      case expr @ ExprIndex(tgt, idx) if idx.tpe.underlying.isNum =>
        tgt.tpe.shapeIter.nextOption map { size =>
          expr.copy(index = cast(TypeUInt(clog2(size) max 1), idx))
        } getOrElse tree

      case expr @ ExprSlice(tgt, lIdx, op, rIdx)
          if lIdx.tpe.underlying.isNum || rIdx.tpe.underlying.isNum =>
        tgt.tpe.shapeIter.nextOption map { size =>
          val lWidth = clog2(size) max 1
          val rWidth = if (op == ":") lWidth else clog2(size + 1)
          val lType = TypeUInt(lWidth)
          val rType = TypeUInt(rWidth)
          val newLIdx = if (lIdx.tpe.underlying.isNum) cast(lType, lIdx) else lIdx
          val newRIdx = if (rIdx.tpe.underlying.isNum) cast(rType, rIdx) else rIdx
          expr.copy(lIdx = newLIdx, rIdx = newRIdx)
        } getOrElse tree

      case expr @ ExprCall(func, args) =>
        val kinds = func.tpe match {
          case TypeCombFunc(_, _, argTypes)     => argTypes
          case TypeCtrlFunc(_, _, argTypes)     => argTypes
          case TypeXenoFunc(_, _, argTypes)     => argTypes
          case TypeStaticMethod(_, _, argTypes) => argTypes
          case TypeNormalMethod(_, _, argTypes) => argTypes
          case _                                => unreachable
        }

        val needsCasts = kinds zip args map {
          case (k, a: ArgP) => k.isPacked && a.expr.tpe.underlying.isNum
          case _            => unreachable
        }

        if (needsCasts exists identity) {
          val newArgs = List from {
            for {
              (needsCast, kind, arg) <- needsCasts lazyZip kinds lazyZip args
            } yield {
              if (needsCast) {
                arg match {
                  case ArgP(e) => TypeAssigner(ArgP(cast(kind, e)) withLoc arg.loc)
                  case _       => unreachable
                }
              } else arg
            }
          }
          expr.copy(args = newArgs)
        } else {
          tree
        }

      case expr @ ExprBinary(
            lhs,
            "&" | "|" | "^" | "*" | "/" | "%" | "+" | "-" | ">" | ">=" | "<" | "<=" | "==" | "!=",
            rhs
          ) if lhs.tpe.underlying.isNum != rhs.tpe.underlying.isNum =>
        if (lhs.tpe.underlying.isNum) {
          expr.copy(lhs = cast(rhs.tpe, lhs))
        } else {
          expr.copy(rhs = cast(lhs.tpe, rhs))
        }

      case expr @ ExprTernary(_, tExpr, eExpr)
          if tExpr.tpe.underlying.isNum != eExpr.tpe.underlying.isNum =>
        if (tExpr.tpe.underlying.isNum) {
          expr.copy(thenExpr = cast(eExpr.tpe, tExpr))
        } else {
          expr.copy(elseExpr = cast(tExpr.tpe, eExpr))
        }

      case _ => tree
    }

    if (result ne tree) TypeAssigner(result withLoc tree.loc) else tree
  }

}

object AddCasts extends PairTransformerPass {
  val name = "add-casts"

  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) = {
    val addCasts = new AddCasts
    (addCasts(decl), addCasts(defn))
  }

}
