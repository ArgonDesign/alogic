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
// Lower TypeVector to TypeUInt by flattening dimensions
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Stack
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

final class LowerVectors(implicit cc: CompilerContext) extends TreeTransformer {

  private[this] val vectorType = mutable.Set[TermSymbol]()

  private[this] val tgtTpe = Stack[Type]()

  private[this] var lvalueLevel = 0

  private[this] var catLevel = 0

  override def enter(tree: Tree): Unit = tree match {
    case entity: EntityLowered => {
      // Change types of all vectors to plain TypeUInt
      // TODO: arrays as well
      for {
        Decl(symbol, _) <- entity.declarations
        if symbol.kind.underlying.isVector
      } {
        vectorType add symbol

        val newUnderlying = TypeUInt(Expr(symbol.kind.width) regularize symbol.loc)
        val newKind = {
          symbol.kind match {
            case kind: TypeIn     => kind.copy(kind = newUnderlying)
            case kind: TypeOut    => kind.copy(kind = newUnderlying)
            case kind: TypeConst  => kind.copy(kind = newUnderlying)
            case kind: TypeVector => newUnderlying
            case _                => unreachable
          }
        }

        symbol.kind = newKind
      }
    }

    case _: StmtAssign => {
      lvalueLevel += 1
    }

    case expr: Expr => {
      if (lvalueLevel > 0) {
        lvalueLevel += 1
        if (expr.isInstanceOf[ExprCat]) {
          catLevel += 1
        }
      }
      expr match {
        case ExprIndex(tgt, _)       => tgtTpe push tgt.tpe.underlying
        case ExprSlice(tgt, _, _, _) => tgtTpe push tgt.tpe.underlying
        case _: Expr                 => tgtTpe push TypeError
      }
    }

    case _ => ()
  }

  def fixSign(expr: Expr, makeSigned: Boolean): Expr = {
    // Turn it into a signed value if required,
    // unless it is the target of the assignment
    if (makeSigned && (lvalueLevel - catLevel) != 2) {
      cc.makeBuiltinCall("$signed", expr.loc, List(expr))
    } else {
      expr
    }
  }

  override def transform(tree: Tree): Tree = {
    val result = tree match {

      // The type of symbols changed, so re-type references
      case ExprRef(symbol: TermSymbol) if vectorType contains symbol => {
        ExprRef(symbol) regularize tree.loc
      }

      // Slice over slice

      // Slice over index

      // Slice over something else

      // Index over a slice
      case ExprIndex(tgt @ ExprSlice(expr, lidx, "+:", ridx), index) if tgtTpe.top.isVector => {
        // By this point the target should not have a Vector type
        assert(!tgt.tpe.isVector)
        val TypeVector(eKind, _) = tgtTpe.top
        val sExpr = Expr(eKind.width)
        val res = ExprSlice(expr, lidx + sExpr * index, "+:", sExpr) regularize tgt.loc
        fixSign(res, eKind.isSigned)
      }

      // Index over something else
      case ExprIndex(tgt, index) if tgtTpe.top.isVector => {
        // By this point the target should not have a Vector type
        assert(!tgt.tpe.isVector)
        val TypeVector(eKind, _) = tgtTpe.top
        val sExpr = Expr(eKind.width)
        val res = ExprSlice(tgt, sExpr * index, "+:", sExpr) regularize tgt.loc
        fixSign(res, eKind.isSigned)
      }

      case _ => tree
    }

    // If we have just processed an Expr, pop the subjectTypeStack and update
    // the lvalueLevel and catLevel
    tree match {
      case expr: Expr => {
        tgtTpe.pop
        if (lvalueLevel > 0 && expr.isInstanceOf[ExprCat]) {
          catLevel -= 1
        }
        if (lvalueLevel > 2) {
          lvalueLevel -= 1
        } else {
          lvalueLevel = 0
        }
      }
      case _ => ()
    }

    result
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(tgtTpe.isEmpty)
    assert(lvalueLevel == 0)
    assert(catLevel == 0)
  }

}

object LowerVectors extends TreeTransformerPass {
  val name = "lower-vectors"
  def create(implicit cc: CompilerContext) = new LowerVectors
}
