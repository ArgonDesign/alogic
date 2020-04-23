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
// Remove local variable and port symbols which are never used
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.analysis.ReadSymbols
import com.argondesign.alogic.analysis.WrittenSymbols
import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees.Expr.InstancePortRef
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.enums.EntityVariant
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

final class RemoveUnused(unusedSymbols: Set[Symbol])(implicit cc: CompilerContext)
    extends StatefulTreeTransformer {

  override def replace(symbol: Symbol): Boolean = !unusedSymbols(symbol) && {
    symbol.kind match {
      case TypeType(TypeEntity(_, publicSymbols)) => publicSymbols exists unusedSymbols
      case TypeEntity(_, publicSymbols)           => publicSymbols exists unusedSymbols
      case _                                      => false
    }
  }

  def emptyStmt(stmt: Stmt): Boolean = stmt match {
    case StmtBlock(body)         => body forall emptyStmt
    case StmtIf(_, eBody, tBody) => (eBody forall emptyStmt) && (tBody forall emptyStmt)
    case StmtCase(_, cases) =>
      cases forall {
        case CaseRegular(_, stmts) => stmts forall emptyStmt
        case CaseDefault(stmts)    => stmts forall emptyStmt
        case _: CaseGen            => unreachable
      }
    case _ => false
  }

  override def enter(tree: Tree): Option[Tree] = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Remove connects driving only unused symbols
    ////////////////////////////////////////////////////////////////////////////

    case EntConnect(_, List(rhs)) if WrittenSymbols(rhs) forall unusedSymbols => Some(Stump)

    case EntConnect(_, List(InstancePortRef(_, pSymbol))) if unusedSymbols(pSymbol) => Some(Stump)

    case _ => None
  }

  override def transform(tree: Tree): Tree = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Fold empty statements
    ////////////////////////////////////////////////////////////////////////////

    case stmt: Stmt if emptyStmt(stmt) => Stump

    ////////////////////////////////////////////////////////////////////////////
    // Remove assignments that write only unused symbols
    ////////////////////////////////////////////////////////////////////////////

    case StmtAssign(lhs, _) if WrittenSymbols(lhs) forall unusedSymbols  => Stump
    case StmtDelayed(lhs, _) if WrittenSymbols(lhs) forall unusedSymbols => Stump

    ////////////////////////////////////////////////////////////////////////////
    // Remove decl/defn of unused symbols
    ////////////////////////////////////////////////////////////////////////////

    case Decl(symbol) if unusedSymbols(symbol) =>
      // If we are removing a _q, drop the suffix from the _d
      symbol.attr.flop.get foreach { dSymbol =>
        assert(dSymbol.name endsWith "_d")
        dSymbol.name = dSymbol.name.dropRight(2)
        dSymbol.attr.combSignal set true
      }
      Stump

    case Defn(symbol) if unusedSymbols(symbol) => Stump

    //
    case _ => tree
  }

}

object RemoveUnused extends PairsTransformerPass {
  val name = "remove-unused"

  private def gather(
      pairs: List[(Decl, Defn)]
    )(
      f: (DeclEntity, DefnEntity) => Iterator[Symbol]
    ): Set[Symbol] =
    HashSet from {
      (pairs.iterator collect {
        case (decl: DeclEntity, defn: DefnEntity) => f(decl, defn)
      }).flatten
    }

  @tailrec
  def process(pairs: List[(Decl, Defn)])(implicit cc: CompilerContext): List[(Decl, Defn)] = {
    // TODO: Could prune every entity completely that has only inputs left
    // (unless it has non-pure contents like foreign function calls..)

    // TODO: Rename interconnect for removed ports

    // Gather all symbols considered for removal
    val candidateSymbols = gather(pairs) { (decl, defn) =>
      val eSymbol = decl.symbol

      val isTopLevel = eSymbol.attr.topLevel.get contains true

      val eSymbols = if (isTopLevel) Iterator.empty else Iterator.single(eSymbol)

      val dSymbols = if (defn.variant == EntityVariant.Ver) {
        // Retain all definitions for verbatim entities
        Iterator.empty
      } else {
        decl.decls.iterator map {
          _.symbol
        } filter { symbol =>
          // Retain inputs and outputs of top level entities,
          // remove clock and reset if possible)
          symbol.kind match {
            case _: TypeIn  => !isTopLevel || symbol.attr.clk.isSet || symbol.attr.rst.isSet
            case _: TypeOut => !isTopLevel
            case _          => true
          }
        }
      }
      eSymbols ++ dSymbols
    }

    // Gather all used symbols. A symbol is used if it's value is consumed,
    // this can happen when it is read in an rvalue, read in an lvalue, or
    // is instantiated. Furthermore all flop _d signals and array
    // _we/_waddr/_wdata signals are used. At the moment we also cannot remove
    // symbols that are written through a concatenation lvalue, as they are
    // required as placeholders
    val usedSymbols = gather(pairs) { (decl, defn) =>
      val partA = decl flatCollect {
        case DeclInstance(_, ExprSym(eSymbol)) =>
          // Instantiated entity
          Iterator.single(eSymbol)
        case ExprSym(symbol) if !symbol.kind.isType =>
          // Any other reference is used (unless it's a type)
          Iterator.single(symbol)
      }
      val partB = defn flatCollect {
        case EntConnect(lhs, List(rhs: ExprCat)) =>
          // Concatenation on the right, everything is used, if only as a placeholder
          // TODO: if any symbol in the concatenation is used, then all are used
          val lSymbols = lhs match {
            case InstancePortRef(iSymbol, pSymbol) => Iterator(iSymbol, pSymbol)
            case _                                 => ReadSymbols.rval(lhs)
          }
          val rSymbols = rhs collect { case ExprSym(symbol) => symbol }
          lSymbols ++ rSymbols
        case EntConnect(InstancePortRef(iSymbol, pSymbol), List(rhs)) =>
          // instance.port on left hand side
          Iterator(iSymbol, pSymbol) ++ ReadSymbols.lval(rhs)
        case EntConnect(lhs, List(InstancePortRef(_, _))) =>
          // instance.port on right hand side
          ReadSymbols.rval(lhs)
        case EntConnect(lhs, List(rhs)) =>
          // Everything on the left, but on the right only stuff that is read
          ReadSymbols.rval(lhs) ++ ReadSymbols.lval(rhs)
        case stmt @ StmtAssign(_: ExprCat, _) =>
          // Concatenation on the left, everything is used, if only as a placeholder
          // TODO: if any symbol in the concatenation is used, then all are used
          stmt collect { case ExprSym(symbol) => symbol }
        case StmtAssign(lhs, rhs) =>
          // Everything on the right, but on the left only stuff that is read
          ReadSymbols.lval(lhs) ++ ReadSymbols.rval(rhs)
        case StmtDelayed(lhs, rhs) =>
          // Everything on the right, but on the left only stuff that is read
          ReadSymbols.lval(lhs) ++ ReadSymbols.rval(rhs)
        // TODO: Same as assign for StmtOutcall if it's a pure function,
        // currently all are assumed to be non-pure...
        case ExprSym(symbol) if !symbol.kind.isType =>
          // Any other reference is used (unless it's a type)
          Iterator.single(symbol)
      }
      partA concat partB
    }

    // Compute the unused ports
    val unusedSymbols = candidateSymbols diff usedSymbols

    if (unusedSymbols.isEmpty) {
      // Stop if we no longer have any unused symbols
      pairs
    } else {
      // Remove unused pairs
      val usedPairs = pairs filterNot { unusedSymbols contains _._1.symbol }

      // Remove unused symbols
      val results = List from {
        val transform = new RemoveUnused(unusedSymbols)(cc)
        usedPairs map { case (decl, defn) => (decl rewrite transform, defn rewrite transform) }
      }

      // Go again to check for symbols that have now became unused
      process(results)
    }
  }

}
