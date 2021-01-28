////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Sign off remaining unused signals
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.analysis.ReadSymbolBits
import com.argondesign.alogic.analysis.SymbolBitSet
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.enums.EntityVariant
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.util.unreachable
import com.argondesign.alogic.util.BitSetOps._
import com.argondesign.alogic.util.IteratorOps._

import scala.collection.immutable.BitSet
import scala.collection.parallel.CollectionConverters.IterableIsParallelizable

object SignOffUnused extends PairsTransformerPass {
  val name = "sign-off-unused"

  private def signOffUnused(
      decl: DeclEntity,
      defn: DefnEntity,
      usedSymbolBits: SymbolBitSet
    )(
      implicit
      cc: CompilerContext
    ): (Decl, Defn) = {

    // Turns (6, 5, 4, 2, 1, 0) into ((6, 4), (2, 1), (0, 0)), etc.
    def collapseConsecutiveValues(it: Iterator[Int]): Iterator[(Int, Int)] = {
      require(it.hasNext)

      def f(hi: Int, lo: Int): Iterator[(Int, Int)] = it.nextOption() match {
        case None                   => Iterator.single((hi, lo))
        case Some(n) if n == lo - 1 => f(hi, n)
        case Some(n)                => Iterator.single((hi, lo)) concat f(n, n)
      }

      val first = it.next()
      f(first, first)
    }

    val unusedTerms = decl.decls.iterator map {
      _.symbol
    } flatMap {
      case symbol if !symbol.kind.isPacked => Iterator.empty
      case symbol =>
        val usedBits = usedSymbolBits.getOrElse(symbol, BitSet.empty)
        if (usedBits.size == symbol.kind.width) {
          Iterator.empty
        } else if (usedBits.isEmpty) {
          Iterator.single(TypeAssigner(ExprSym(symbol) withLoc Loc.synthetic))
        } else {
          val unusedBits = Iterator.range(symbol.kind.width.toInt - 1, -1, -1).filterNot(usedBits)

          def ref: ExprSym = TypeAssigner(ExprSym(symbol) withLoc Loc.synthetic)

          collapseConsecutiveValues(unusedBits) map {
            case (hi, lo) if hi == lo => ref index hi
            case (hi, lo)             => ref.slice(hi, ":", lo)
          }
        }
    }

    if (!unusedTerms.hasNext) {
      (decl, defn)
    } else {
      val unusedSymbol = cc.newSymbol("_unused", Loc.synthetic)
      unusedSymbol.kind = TypeUInt(1)
      unusedSymbol.attr.combSignal set true
      val unusedDecl = unusedSymbol.mkDecl regularize Loc.synthetic
      val unusedDefn = EntSplice(unusedSymbol.mkDefn) regularize Loc.synthetic
      val reduction = ExprUnary("&", ExprCat(ExprInt(false, 1, 0) :: unusedTerms.toList))
      val unusedAssign = EntAssign(ExprSym(unusedSymbol), reduction) regularize Loc.synthetic
      val newDecl = TypeAssigner(decl.copy(decls = unusedDecl :: decl.decls) withLocOf decl)
      val newDefn = TypeAssigner(
        defn.copy(body = unusedDefn :: unusedAssign :: defn.body) withLocOf defn
      )
      (newDecl, newDefn)
    }
  }

  def process(
      pairs: Iterable[(Decl, Defn)]
    )(
      implicit
      cc: CompilerContext
    ): Iterable[(Decl, Defn)] = {
    // We can do a lot in parallel
    val parPairs = pairs.par

    // Gather all used symbol bits. This is similar to the gathering in
    // RemoveUnused, but with bitwise precision and slight differences.
    val usedLocalSymbolBits: Map[Symbol, SymbolBitSet] = Map from {
      parPairs.iterator
        .map {
          case (DeclEntity(symbol, decls), defn: DefnEntity) =>
            symbol ->
              // Assume all output ports are used. If they were wholly unused,
              // they would have been removed in RemoveUnused. Partially unused
              // ranges are signed off on the instantiation side if needed.
              decls.iterator
                .map(_.symbol)
                .filter(_.kind.isOut)
                .map(symbol => SymbolBitSet(symbol, BitSet.whole(symbol)))
                .concat {
                  Iterator.when(defn.variant == EntityVariant.Ver) thenIterator {
                    // Assume everything is used in verbatim entities as signals
                    // might be used in verbatim blocks
                    decls.iterator.map(decl => SymbolBitSet(decl.symbol, BitSet.whole(decl.symbol)))
                  }
                }
                .concat {
                  defn collect {
                    case EntAssign(lhs, _: ExprSel) =>
                      ReadSymbolBits.lval(lhs)
                    case EntAssign(_: ExprSel, rhs) =>
                      ReadSymbolBits.rval(rhs)
                    case EntAssign(lhs, rhs) =>
                      ReadSymbolBits.lval(lhs) union ReadSymbolBits.rval(rhs)
                    case StmtAssign(lhs, rhs) =>
                      ReadSymbolBits.lval(lhs) union ReadSymbolBits.rval(rhs)
                    case StmtDelayed(lhs, rhs) =>
                      ReadSymbolBits.lval(lhs) union ReadSymbolBits.rval(rhs)
                    case StmtOutcall(lhs, f, rhss) =>
                      (f :: rhss).foldLeft(ReadSymbolBits.lval(lhs))(
                        _ union ReadSymbolBits.rval(_)
                      )
                    case expr: Expr => ReadSymbolBits.rval(expr)
                  }
                }
                .foldLeft(SymbolBitSet.empty)(_ union _)
          case _ => unreachable
        }
    }

    parPairs.map {
      case (decl: DeclEntity, defn: DefnEntity) =>
        signOffUnused(decl, defn, usedLocalSymbolBits(decl.symbol))
      case other => other
    }.seq
  }

}
