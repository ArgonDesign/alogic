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

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Math.clog2
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

final class AddCasts(implicit cc: CompilerContext) extends TreeTransformer {

  private def cast(kind: Type, expr: Expr) = {
    val (castType, castExpr) = if (kind.isNum) {
      (TypeNum(expr.tpe.isSigned), expr.simplify)
    } else {
      (TypeInt(expr.tpe.isSigned, Expr(kind.width) regularize expr.loc), expr)
    }
    if (castType.isNum && !castExpr.isKnownConst) {
      cc.ice(expr, s"Trying to cast non-constant expression to type '${castType.toSource}'")
    }
    castExpr cast castType
  }

  override def transform(tree: Tree): Tree = {
    val result = tree match {
      case decl @ Decl(symbol, Some(init)) if symbol.kind.isPacked && init.tpe.underlying.isNum => {
        val newInit = cast(symbol.kind, init)
        if (symbol.kind.isConst) {
          symbol.attr.init set newInit
        }
        decl.copy(init = Some(newInit))
      }

      case decl @ Decl(symbol, Some(init))
          if symbol.kind.underlying.isNum && symbol.kind.isSigned != init.tpe.isSigned => {
        val newInit = if (symbol.kind.isSigned) {
          cc.makeBuiltinCall("$signed", init.loc, List(init))
        } else {
          cc.makeBuiltinCall("$unsigned", init.loc, List(init))
        }
        if (symbol.kind.isConst) {
          symbol.attr.init set newInit
        }
        decl.copy(init = Some(newInit))
      }

      case stmt @ StmtAssign(lhs, rhs) if lhs.tpe.isPacked && rhs.tpe.underlying.isNum => {
        stmt.copy(rhs = cast(lhs.tpe, rhs))
      }

      case stmt @ StmtUpdate(lhs, "&" | "|" | "^" | "*" | "/" | "%" | "+" | "-", rhs)
          if lhs.tpe.isPacked && rhs.tpe.underlying.isNum => {
        stmt.copy(rhs = cast(lhs.tpe, rhs))
      }

      case expr @ ExprIndex(tgt, idx) if !tgt.tpe.underlying.isNum && idx.tpe.underlying.isNum => {
        val width = clog2(tgt.tpe.shapeIter.next) max 1
        val widthExpr = TypeAssigner(Expr(width) withLoc expr.loc)
        expr.copy(index = cast(TypeUInt(widthExpr), idx))
      }

      case expr @ ExprSlice(tgt, lIdx, op, rIdx)
          if !tgt.tpe.underlying.isNum && (lIdx.tpe.underlying.isNum || rIdx.tpe.underlying.isNum) => {
        val size = tgt.tpe.shapeIter.next
        val lWidth = clog2(size) max 1
        val rWidth = if (op == ":") lWidth else clog2(size + 1)
        val lType = TypeUInt(Expr(lWidth) regularize expr.loc)
        val rType = TypeUInt(Expr(rWidth) regularize expr.loc)
        val newLIdx = if (lIdx.tpe.isNum) cast(lType, lIdx) else lIdx
        val newRIdx = if (rIdx.tpe.isNum) cast(rType, rIdx) else rIdx
        expr.copy(lidx = newLIdx, ridx = newRIdx)
      }

      case expr @ ExprCall(func, args) => {
        val kinds = func.tpe match {
          case TypeCombFunc(argTypes, _) => argTypes
          case TypeCtrlFunc(argTypes, _) => argTypes
          case _                         => unreachable
        }

        val needsCasts = kinds zip args map {
          case (k, a) => k.isPacked && a.tpe.underlying.isNum
        }

        if (needsCasts exists identity) {
          val newArgs = List from {
            for {
              (needsCast, kind, arg) <- needsCasts lazyZip kinds lazyZip args
            } yield {
              if (needsCast) cast(kind, arg) else arg
            }
          }
          expr.copy(args = newArgs)
        } else {
          tree
        }
      }

      case expr @ ExprBinary(lhs,
                             "&" | "|" | "^" | "*" | "/" | "%" | "+" | "-" | ">" | ">=" | "<" |
                             "<=" | "==" | "!=",
                             rhs) if lhs.tpe.underlying.isNum != rhs.tpe.underlying.isNum => {
        if (lhs.tpe.isNum) {
          expr.copy(lhs = cast(rhs.tpe, lhs))
        } else {
          expr.copy(rhs = cast(lhs.tpe, rhs))
        }
      }

      case expr @ ExprTernary(_, tExpr, eExpr)
          if tExpr.tpe.underlying.isNum != eExpr.tpe.underlying.isNum => {
        if (tExpr.tpe.isNum) {
          expr.copy(thenExpr = cast(eExpr.tpe, tExpr))
        } else {
          expr.copy(elseExpr = cast(tExpr.tpe, eExpr))
        }
      }

      case _ => tree
    }

    if (result ne tree) TypeAssigner(result withLoc tree.loc) else tree
  }
}

object AddCasts extends TreeTransformerPass {
  val name = "add-casts"
  def create(implicit cc: CompilerContext) = new AddCasts
}
