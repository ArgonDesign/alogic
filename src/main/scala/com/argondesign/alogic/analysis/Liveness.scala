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
// Liveness analysis
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.analysis

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.util.BigIntOps._
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

object Liveness {

  private def usedRvalMaps(expr: Expr)(
      implicit cc: CompilerContext
  ): Iterator[Map[TermSymbol, BigInt]] = expr collect {
    // TODO: Deal better with arrays when necessary, currently simply ignored
    case ExprIndex(ExprRef(Sym(symbol: TermSymbol)), idx) if symbol.kind.isPacked => {
      idx.value map { bit =>
        Map(symbol -> BigInt.oneHot(bit))
      } getOrElse {
        Map(symbol -> BigInt.mask(symbol.kind.width.value.get))
      }
    }
    case ExprSlice(ExprRef(Sym(symbol: TermSymbol)), lidx, op, ridx) if symbol.kind.isPacked => {
      lidx.value flatMap { l =>
        ridx.value map { r =>
          val (width, lsb) = op match {
            case ":"  => (l - r + 1, r)
            case "+:" => (r, l)
            case "-:" => (r, l - r + 1)
            case _    => unreachable
          }
          Map(symbol -> (BigInt.mask(width) << lsb.toInt))
        }
      } getOrElse {
        Map(symbol -> BigInt.mask(symbol.kind.width.value.get))
      }
    }
    case ExprRef(Sym(symbol: TermSymbol)) if symbol.kind.isPacked => {
      Map(symbol -> BigInt.mask(symbol.kind.width.value.get))
    }
  }

  private def incorporate(
      acc: mutable.Map[TermSymbol, BigInt],
      it: Iterator[Map[TermSymbol, BigInt]]
  ): Unit = {
    for {
      map <- it
      (symbol, mask) <- map
    } {
      acc(symbol) |= mask
    }
  }

  // Given an expression, return a SymbolBitSet that holds bits that might be
  // read if this expression is not used on the left hand side of an assignment
  def usedRv(expr: Expr)(implicit cc: CompilerContext): SymbolBitSet = {
    val acc = mutable.Map[TermSymbol, BigInt]() withDefaultValue BigInt(0)
    incorporate(acc, usedRvalMaps(expr))
    acc.toMap mapValues { _.toBitSet }
  }

  // Given an expression, return a SymbolBitSet that holds bits that might be
  // read if this expression is used on the left hand side of an assignment
  def usedLv(lval: Expr)(implicit cc: CompilerContext): SymbolBitSet = {
    val acc = mutable.Map[TermSymbol, BigInt]() withDefaultValue BigInt(0)

    def gather(expr: Expr): Unit = expr match {
      case ExprRef(Sym(symbol: TermSymbol)) => ()
      case ExprIndex(ExprRef(_: Sym), idx) => {
        incorporate(acc, usedRvalMaps(idx))
      }
      case ExprSlice(ExprRef(_: Sym), lidx, _, ridx) => {
        incorporate(acc, usedRvalMaps(lidx))
        incorporate(acc, usedRvalMaps(ridx))
      }
      case ExprCat(parts) => parts map gather
      case other => {
        cc.ice(other, "Don't know how to extract read variables from lval", other.toSource)
      }
    }

    gather(lval)

    acc.toMap mapValues { _.toBitSet }
  }

  // Given an expression, return a SymbolBitSet that holds bits that are known
  // to be written, should this expression be used on the left hand side of an
  // assignment
  def killed(lval: Expr)(implicit cc: CompilerContext): SymbolBitSet = {

    def loop(expr: Expr): Map[TermSymbol, BigInt] = {
      expr match {
        case ExprRef(Sym(symbol: TermSymbol)) => {
          val width = symbol.kind.width.value.get
          Map(symbol -> BigInt.mask(width))
        }
        case ExprIndex(ExprRef(Sym(symbol: TermSymbol)), idx) => {
          idx.value map { bit =>
            Map(symbol -> BigInt.oneHot(bit))
          } getOrElse Map.empty
        }
        case ExprSlice(ExprRef(Sym(symbol: TermSymbol)), lidx, op, ridx) => {
          lidx.value flatMap { l =>
            ridx.value map { r =>
              val (width, lsb) = op match {
                case ":"  => (l - r + 1, r)
                case "+:" => (r, l)
                case "-:" => (r, l - r + 1)
                case _    => unreachable
              }
              Map(symbol -> (BigInt.mask(width) << lsb.toInt))
            }
          } getOrElse Map.empty
        }
        case ExprCat(parts) => {
          val acc = mutable.Map[TermSymbol, BigInt]() withDefaultValue BigInt(0)
          for {
            map <- parts map loop
            (symbol, mask) <- map
          } {
            acc(symbol) |= mask
          }
          acc.toMap
        }
        case other => {
          cc.ice(other, "Don't know how to extract written variables from", other.toSource)
        }
      }
    }

    loop(lval) mapValues { _.toBitSet }
  }

  // Perform bit-wise accurate liveness analysis of given statements. It
  // returns the 'live' and 'dead' bit sets. The 'live' map  contains bits
  // that might be read, before being assigned in the statements, and the
  // 'dead' map contains bits that are definitely assigned before being
  // read in the statements. Note that a bit that is not present in either
  // maps only means that those bits are neither read, nor assigned in the
  // given statements.
  def apply(stmts: List[Stmt])(implicit cc: CompilerContext): (SymbolBitSet, SymbolBitSet) = {

    def analyse(
        cLive: SymbolBitSet,
        cDead: SymbolBitSet,
        stmts: List[Stmt]
    ): (SymbolBitSet, SymbolBitSet) = {
      if (stmts.isEmpty) {
        (cLive, cDead)
      } else {
        val head :: tail = stmts

        val (nLive, nDead) = head match {
          case StmtAssign(lhs, rhs) => {
            val readRhs = usedRv(rhs)
            val readLhs = usedLv(lhs)
            // Everything read is born, unless it's already dead
            val born = (readRhs union readLhs) diff cDead
            val live = cLive union born
            // Everything written is killed unless it's already live
            val kill = killed(lhs) diff live
            val dead = cDead union kill
            (live, dead)
          }

          case StmtIf(cond, thenStmt, None) => {
            val born = usedRv(cond) diff cDead
            val dLive = cLive union born

            val (live, _) = analyse(dLive, cDead, List(thenStmt))
            val dead = cDead

            (live, dead)
          }

          case StmtIf(cond, thenStmt, Some(elseStmt)) => {
            val born = usedRv(cond) diff cDead
            val dLive = cLive union born

            val (tLive, tDead) = analyse(dLive, cDead, List(thenStmt))
            val (eLive, eDead) = analyse(dLive, cDead, List(elseStmt))

            val live = tLive union eLive
            val dead = tDead intersect eDead

            (live, dead)
          }

          case StmtCase(expr, cases, default) => {
            val readers = expr :: (cases flatMap { case CaseClause(cond, _) => cond })
            val reads = readers map { usedRv }
            val dLive = reads reduce { _ union _ } diff cDead

            val (bLive, bDead) = {
              val sets = analyse(dLive, cDead, default) :: {
                cases map {
                  case CaseClause(_, body) => analyse(dLive, cDead, List(body))
                }
              }
              sets.unzip
            }

            val live = bLive reduce { _ union _ }
            val dead = bDead reduce { _ intersect _ }

            (live, dead)
          }

          case StmtBlock(body) => analyse(cLive, cDead, body)
          case _: StmtStall    => (cLive, cDead)
          case _: StmtFence    => (cLive, cDead)
          case _               => unreachable
        }

        analyse(nLive, nDead, tail)
      }
    }

    analyse(SymbolBitSet.empty, SymbolBitSet.empty, stmts)
  }

}
