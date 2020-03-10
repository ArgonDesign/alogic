////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018-2020 Argon Design Ltd. All rights reserved.
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

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Math.clog2

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

final class LowerVectorsA(
    globalReplacements: mutable.Map[Symbol, Symbol],
)(implicit cc: CompilerContext)
    extends StatefulTreeTransformer {

  private[this] val tgtTpe = mutable.Stack[Type]()

  private[this] var lvalueLevel = 0

  private[this] var catLevel = 0

  override def replace(symbol: Symbol): Boolean =
    enclosingSymbols.isEmpty || symbol.kind.underlying.isVector

  override def enter(tree: Tree): Option[Tree] = {
    tree match {
      case DeclEntity(symbol, _) => globalReplacements(orig(symbol)) = symbol

      case _: StmtAssign => lvalueLevel += 1

      case expr: Expr =>
        if (lvalueLevel > 0) {
          lvalueLevel += 1
          if (expr.isInstanceOf[ExprCat]) {
            catLevel += 1
          }
        }
        expr match {
          case ExprIndex(tgt, _)       => tgtTpe push tgt.tpe.underlying
          case ExprSlice(tgt, _, _, _) => tgtTpe push tgt.tpe.underlying
          case _: Expr                 => tgtTpe push TypeMisc
        }

      case _ =>
    }
    None
  }

  private def fixSign(expr: Expr, makeSigned: Boolean): Expr = {
    // Turn it into a signed value if required,
    // unless it is the target of the assignment
    if (makeSigned && (lvalueLevel - catLevel) != 2) {
      cc.makeBuiltinCall("$signed", expr.loc, List(expr))
    } else {
      expr
    }
  }

  private def vecSpec(symbol: Symbol): Expr = {
    assert(symbol.kind.underlying.isVector)
    ExprType(TypeUInt(symbol.kind.width))
  }

  override def transform(tree: Tree): Tree = {
    val result = tree match {

      // TODO: handle arrays as well

      //////////////////////////////////////////////////////////////////////////
      // Update declarations of vectors
      //////////////////////////////////////////////////////////////////////////

      case decl @ DeclVar(symbol, _) if symbol.kind.underlying.isVector =>
        decl.copy(spec = vecSpec(symbol)) regularize tree.loc

      case decl @ DeclIn(symbol, _, _) if symbol.kind.underlying.isVector =>
        decl.copy(spec = vecSpec(symbol)) regularize tree.loc

      case decl @ DeclOut(symbol, _, _, _) if symbol.kind.underlying.isVector =>
        decl.copy(spec = vecSpec(symbol)) regularize tree.loc

      case decl @ DeclConst(symbol, _) if symbol.kind.underlying.isVector =>
        decl.copy(spec = vecSpec(symbol)) regularize tree.loc

      //////////////////////////////////////////////////////////////////////////
      // Transform vector operations
      //////////////////////////////////////////////////////////////////////////

      // Slice
      case ExprSlice(tgt, lIdx, op, rIdx) =>
        tgtTpe.top match {
          case TypeVector(eKind, _) =>
            assert(!tgt.tpe.isVector) // By this point the target should not have a Vector type

            val lsbExpr = {
              val lIdxWidth = clog2(tgt.tpe.shapeIter.next) max 1
              op match {
                case ":"  => rIdx zx lIdxWidth
                case "+:" => lIdx zx lIdxWidth
                case "-:" => (lIdx zx lIdxWidth) - (rIdx.value.get.toInt - 1)
              }
            }

            val sliceWidth = op match {
              case ":" => lIdx.value.get - rIdx.value.get + 1
              case _   => rIdx.value.get
            }

            val widthExpr = Expr(eKind.width * sliceWidth) regularize lIdx.loc
            val sExpr = Expr(eKind.width) regularize lIdx.loc
            ExprSlice(tgt, sExpr * lsbExpr, "+:", widthExpr) regularize tgt.loc
          case _ => tree
        }

      // Index
      case ExprIndex(tgt, index) =>
        tgtTpe.top match {
          case TypeVector(eKind, size) =>
            assert(!tgt.tpe.isVector) // By this point the target should not have a Vector type
            if (size == 1) {
              fixSign(tgt, eKind.isSigned)
            } else {
              val sExpr = Expr(eKind.width) regularize index.loc
              val idxWidth = clog2(tgt.tpe.shapeIter.next) max 1
              val res = ExprSlice(tgt, sExpr * (index zx idxWidth), "+:", sExpr) regularize tgt.loc
              fixSign(res, eKind.isSigned)
            }
          case _ => tree
        }

      //
      case _ => tree
    }

    // If we have just processed an Expr, pop the subjectTypeStack and update
    // the lvalueLevel and catLevel
    tree match {
      case expr: Expr =>
        tgtTpe.pop
        if (lvalueLevel > 0 && expr.isInstanceOf[ExprCat]) {
          catLevel -= 1
        }
        if (lvalueLevel > 2) {
          lvalueLevel -= 1
        } else {
          lvalueLevel = 0
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

final class LowerVectorsB(
    globalReplacements: mutable.Map[Symbol, Symbol],
)(implicit cc: CompilerContext)
    extends StatefulTreeTransformer {

  override def replace(symbol: Symbol): Boolean = symbol.kind match {
    case TypeEntity(eSymbol, _) => globalReplacements contains eSymbol
    case _                      => false
  }

  override def transform(tree: Tree): Tree = tree match {
    //////////////////////////////////////////////////////////////////////////
    // Update instance types
    //////////////////////////////////////////////////////////////////////////

    case decl @ DeclInstance(_, ExprSym(eSymbol)) =>
      globalReplacements.get(eSymbol) map { nSymbol =>
        decl.copy(spec = ExprSym(nSymbol)) regularize tree.loc
      } getOrElse tree

    //
    case _ => tree
  }

  override def finish(tree: Tree): Tree = {
    tree match {
      case defn: DefnEntity =>
        val icos = defn.symbol.attr.interconnectClearOnStall.getOrElse(Nil)
        defn.symbol.attr.interconnectClearOnStall set {
          icos map {
            case (symbol, name) => (repl(symbol).getOrElse(symbol), name)
          }
        }
      case _ =>
    }
    tree
  }

  override def finalCheck(tree: Tree): Unit = {
    tree visit {
      case t: Tree if t.tpe.underlying.isVector => cc.ice(t, "Tree with vector type remains")
    }
  }

}

object LowerVectors {
  def apply(): Pass[List[(Decl, Defn)], List[(Decl, Defn)]] = {
    val globalReplacements = TrieMap[Symbol, Symbol]()

    new EntityTransformerPass(declFirst = true) {
      val name = "lower-vectors-a"

      def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
        new LowerVectorsA(globalReplacements)
    } andThen new EntityTransformerPass(declFirst = true) {
      val name = "lower-vectors-b"

      def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
        new LowerVectorsB(globalReplacements)
    }
  }
}
