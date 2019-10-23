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
import com.argondesign.alogic.util.unreachable
import com.argondesign.alogic.lib.Math.clog2

import scala.collection.mutable

final class LowerVectors(implicit cc: CompilerContext) extends TreeTransformer {

  private[this] val vectorType = mutable.Set[TermSymbol]()

  private[this] val tgtTpe = mutable.Stack[Type]()

  private[this] var lvalueLevel = 0

  private[this] var catLevel = 0

  override def enter(tree: Tree): Unit = tree match {
    case entity: Entity => {
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
      case ExprSym(symbol: TermSymbol) if vectorType contains symbol => {
        ExprSym(symbol) regularize tree.loc
      }

      // Slice
      case ExprSlice(tgt, lidx, op, ridx) =>
        tgtTpe.top match {
          case TypeVector(eKind, _) =>
            assert(!tgt.tpe.isVector) // By this point the target should not have a Vector type

            val lsbExpr = {
              val lidxWidth = clog2(tgt.tpe.shapeIter.next) max 1
              op match {
                case ":"  => ridx zx lidxWidth
                case "+:" => lidx zx lidxWidth
                case "-:" => (lidx zx lidxWidth) - (ridx.value.get.toInt - 1)
              }
            }

            val sliceWidth = op match {
              case ":" => lidx.value.get - ridx.value.get + 1
              case _   => ridx.value.get
            }

            val widthExpr = Expr(eKind.width * sliceWidth) regularize lidx.loc
            val sExpr = Expr(eKind.width) regularize lidx.loc
            ExprSlice(tgt, sExpr * lsbExpr, "+:", widthExpr) regularize tgt.loc
          case _ => tree
        }

      // Index
      case ExprIndex(tgt, index) =>
        tgtTpe.top match {
          case TypeVector(eKind, size) =>
            assert(!tgt.tpe.isVector) // By this point the target should not have a Vector type
            if (size.value.get == 1) {
              fixSign(tgt, eKind.isSigned)
            } else {
              val sExpr = Expr(eKind.width) regularize index.loc
              val idxWidth = clog2(tgt.tpe.shapeIter.next) max 1
              val res = ExprSlice(tgt, sExpr * (index zx idxWidth), "+:", sExpr) regularize tgt.loc
              fixSign(res, eKind.isSigned)
            }
          case _ => tree
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
