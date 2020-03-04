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
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.util.BigIntOps._
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

object Liveness {

  private def usedRvalMaps(expr: Expr)(
      implicit cc: CompilerContext
  ): Iterator[Map[Symbol, BigInt]] = expr flatCollect {
    case ExprIndex(ExprSym(symbol), idx) if symbol.kind.isPacked => {
      val m = idx.value map { bit =>
        Map(symbol -> BigInt.oneHot(bit))
      } getOrElse {
        Map(symbol -> BigInt.mask(symbol.kind.width))
      }
      Iterator.single(m) ++ usedRvalMaps(idx)
    }
    case ExprSlice(ExprSym(symbol), lidx, op, ridx) if symbol.kind.isPacked => {
      val m = lidx.value flatMap { l =>
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
        Map(symbol -> BigInt.mask(symbol.kind.width))
      }
      Iterator.single(m) ++ usedRvalMaps(lidx) ++ usedRvalMaps(ridx)
    }
    case ExprSym(symbol) if symbol.kind.isPacked => {
      Iterator.single(Map(symbol -> BigInt.mask(symbol.kind.width)))
    }
  }

  private def incorporate(
      acc: mutable.Map[Symbol, BigInt],
      it: Iterator[Map[Symbol, BigInt]]
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
    val acc = mutable.Map[Symbol, BigInt]() withDefaultValue BigInt(0)
    incorporate(acc, usedRvalMaps(expr))
    (acc.view mapValues { _.toBitSet }).toMap
  }

  // Given an expression, return a SymbolBitSet that holds bits that might be
  // read if this expression is used on the left hand side of an assignment
  def usedLv(lval: Expr)(implicit cc: CompilerContext): SymbolBitSet = {
    val acc = mutable.Map[Symbol, BigInt]() withDefaultValue BigInt(0)

    def gather(expr: Expr): Unit = expr match {
      case ExprSym(symbol) => ()
      case ExprIndex(_: ExprSym, idx) => {
        incorporate(acc, usedRvalMaps(idx))
      }
      case ExprSlice(_: ExprSym, lidx, _, ridx) => {
        incorporate(acc, usedRvalMaps(lidx))
        incorporate(acc, usedRvalMaps(ridx))
      }
      case ExprCat(parts) => parts map gather
      case other => {
        cc.ice(other, "Don't know how to extract read variables from lval", other.toSource)
      }
    }

    gather(lval)

    (acc.view mapValues { _.toBitSet }).toMap
  }

  // Given an expression, return a SymbolBitSet that holds bits that are known
  // to be written, should this expression be used on the left hand side of an
  // assignment
  def killed(lval: Expr)(implicit cc: CompilerContext): SymbolBitSet = {

    def loop(expr: Expr): Map[Symbol, BigInt] = {
      expr match {
        case ExprSym(symbol) => {
          Map(symbol -> BigInt.mask(symbol.kind.width))
        }
        case ExprIndex(ExprSym(symbol), idx) => {
          idx.value map { bit =>
            Map(symbol -> BigInt.oneHot(bit))
          } getOrElse Map.empty
        }
        case ExprSlice(ExprSym(symbol), lidx, op, ridx) => {
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
          val acc = mutable.Map[Symbol, BigInt]() withDefaultValue BigInt(0)
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

    (loop(lval).view mapValues { _.toBitSet }).toMap
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
    ): (SymbolBitSet, SymbolBitSet) = stmts match {
      case Nil => (cLive, cDead)
      case head :: tail =>
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

          case StmtIf(cond, thenStmts, elseStmts) => {
            val born = usedRv(cond) diff cDead
            val dLive = cLive union born

            val (tLive, tDead) = analyse(dLive, cDead, thenStmts)
            val (eLive, eDead) = analyse(dLive, cDead, elseStmts)

            val live = tLive union eLive
            val dead = tDead intersect eDead

            (live, dead)
          }

          case StmtCase(expr, cases) => {
            val caseReaders = cases flatMap {
              case CaseRegular(cond, _) => cond
              case _: CaseDefault       => Nil
              case _: CaseGen           => unreachable
            }
            val readers = expr :: caseReaders
            val reads = readers map { usedRv }
            val born = reads reduce { _ union _ } diff cDead
            val dLive = cLive union born

            val (bLive, bDead) = {
              val sets = cases map {
                case CaseRegular(_, stmt) => analyse(dLive, cDead, stmt)
                case CaseDefault(stmt)    => analyse(dLive, cDead, stmt)
                case _: CaseGen           => unreachable
              }

              val explicitDefault = cases exists {
                case _: CaseDefault => true
                case _              => false
              }

              (if (explicitDefault) sets else (dLive, cDead) :: sets).unzip
            }

            val live = bLive reduce { _ union _ }
            val dead = bDead reduce { _ intersect _ }

            (live, dead)
          }

          case StmtStall(cond) => {
            val born = usedRv(cond) diff cDead
            val live = cLive union born
            (live, cDead)
          }

          case StmtExpr(expr) => {
            val born = usedRv(expr) diff cDead
            val live = cLive union born
            (live, cDead)
          }

          case StmtBlock(body) => analyse(cLive, cDead, body)

          case _: StmtComment => (cLive, cDead)
          case _              => unreachable
        }

        analyse(nLive, nDead, tail)
    }

    analyse(SymbolBitSet.empty, SymbolBitSet.empty, stmts)
  }

}
