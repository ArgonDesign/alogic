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

  override def enter(tree: Tree): Unit = tree match {
    case entity: Entity => {
      // Change types of all vectors to plain TypeUInt
      // TODO: arrays as well
      for {
        Decl(symbol, _) <- entity.declarations
        if symbol.kind.underlying.isVector
      } {
        vectorType add symbol

        val newUnderlying = TypeUInt(Expr(symbol.kind.width.value.get) regularize symbol.loc)
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

    case ExprIndex(tgt, _) => tgtTpe push tgt.tpe.underlying

    case ExprSlice(tgt, _, _, _) => tgtTpe push tgt.tpe.underlying

    case _: Expr => tgtTpe push TypeError

    case _ => ()
  }

  override def transform(tree: Tree): Tree = {
    val result = tree match {

      // The type of symbols changed, so re-type references
      case ExprRef(symbol: TermSymbol) if vectorType contains symbol => ExprRef(symbol)

      // Slice over slice

      // Slice over index

      // Slice over something else

      // Index over a slice
      case ExprIndex(tgt @ ExprSlice(expr, lidx, "+:", ridx), index) if tgtTpe.top.isVector => {
        // By this point the target should not have a Vector type
        assert(!tgt.tpe.isVector)
        val TypeVector(eKind, _) = tgtTpe.top
        val eSize = eKind.width.value.get.toInt
        val sExpr = Expr(eSize) regularize tgt.loc
        ExprSlice(expr, lidx + sExpr * index, "+:", sExpr)
      }

      // Index over something else
      case ExprIndex(tgt, index) if tgtTpe.top.isVector => {
        // By this point the target should not have a Vector type
        assert(!tgt.tpe.isVector)
        val TypeVector(eKind, _) = tgtTpe.top
        val eSize = eKind.width.value.get.toInt
        val sExpr = Expr(eSize) regularize tgt.loc
        ExprSlice(tgt, sExpr * index, "+:", sExpr)
      }

      case _ => tree
    }

    // If we have just processed an Expr, pop the subjectTypeStack
    tree match {
      case _: Expr => tgtTpe.pop
      case _       => ()
    }

    // If we have rewritten the tree, regularize it
    if (result ne tree) {
      result regularize tree.loc
    }

    result
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(tgtTpe.isEmpty)
  }

}

object LowerVectors extends TreeTransformerPass {
  val name = "lower-vectors"
  def create(implicit cc: CompilerContext) = new LowerVectors
}
