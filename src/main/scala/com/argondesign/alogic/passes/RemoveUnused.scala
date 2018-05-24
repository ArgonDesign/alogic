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
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.Expr.InstancePortRef
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.collection.mutable

final class RemoveUnused(unusedSymbols: Set[Symbol])(implicit cc: CompilerContext)
    extends TreeTransformer {

  private[this] val ourUnused = mutable.HashSet[Symbol]()

  override def enter(tree: Tree): Unit = tree match {
    case entity: Entity => {
      for (Decl(symbol, _) <- entity.declarations if unusedSymbols contains symbol) {
        ourUnused add symbol
      }
    }

    case _ => ()
  }

  def emptyStmt(stmt: Stmt): Boolean = stmt match {
    case StmtBlock(Nil)                => true
    case _: StmtFence                  => true // TODO: Strip fences earlier
    case StmtBlock(body)               => body forall emptyStmt
    case StmtIf(_, eBody, None)        => emptyStmt(eBody)
    case StmtIf(_, eBody, Some(tBody)) => emptyStmt(eBody) && emptyStmt(tBody)
    case StmtCase(_, cases, defaults) => {
      (defaults forall emptyStmt) && {
        cases forall { case CaseClause(_, body) => emptyStmt(body) }
      }
    }
    case _ => false
  }

  override def transform(tree: Tree): Tree = tree match {

    ////////////////////////////////////////////////////////////////////////////
    // Fold empty statements, unless they are already empty
    ////////////////////////////////////////////////////////////////////////////

    case StmtBlock(Nil) => tree

    case stmt: Stmt if emptyStmt(stmt) => {
      TypeAssigner(StmtBlock(Nil) withLoc tree.loc)
    }

    ////////////////////////////////////////////////////////////////////////////
    // Remove assignments that write only unused symbols
    ////////////////////////////////////////////////////////////////////////////

    case StmtAssign(lhs, _) if WrittenSymbols(lhs) forall ourUnused.contains => {
      TypeAssigner(StmtBlock(Nil) withLoc tree.loc)
    }

    ////////////////////////////////////////////////////////////////////////////
    // Remove declarations and connections to unused symbols
    ////////////////////////////////////////////////////////////////////////////

    case entity: Entity => {
      // If we are removing a _q, drop the suffix from the _d
      for {
        qSymbol <- ourUnused
        dSymbol <- qSymbol.attr.flop.get
      } {
        assert(dSymbol.name endsWith "_d")
        dSymbol rename dSymbol.name.dropRight(2)
        dSymbol.attr.combSignal set true
      }

      // Remove declarations of unused symbols
      val decls = entity.declarations filterNot {
        case Decl(symbol, _) => ourUnused contains symbol
        case _               => unreachable
      }

      // Remove unused instances
      val insts = entity.instances filterNot {
        case Instance(Sym(symbol), _, _, _) => unusedSymbols contains symbol
        case _                              => unreachable
      }

      // Remove connects driving only unused symbols
      val conns = entity.connects filterNot {
        case Connect(_, List(InstancePortRef(iSymbol, pSymbol))) => {
          unusedSymbols.contains(iSymbol) || unusedSymbols.contains(pSymbol)
        }
        case Connect(_, List(rhs)) => WrittenSymbols(rhs) forall ourUnused.contains
        case _                     => false
      }

      TypeAssigner {
        entity.copy(
          declarations = decls,
          instances = insts,
          connects = conns
        ) withVariant entity.variant withLoc tree.loc
      }
    }

    case _ => tree
  }

}

object RemoveUnused extends Pass {
  val name = "remove-unused"

  private def gather(trees: List[Tree])(f: Entity => Iterator[Symbol]): Set[Symbol] = {
    HashSet() ++ {
      trees.par flatMap {
        case entity: Entity => f(entity)
        case _              => unreachable
      }
    }
  }

  @tailrec
  private def loop(trees: List[Tree])(implicit cc: CompilerContext): List[Tree] = {

    // TODO: Could prune every entity completely that has only inputs left

    // TODO: Rename interconnect for removed ports

    // Gather all symbols considered for removal
    val candidateSymbols = gather(trees) { entity =>
      val Sym(eSymbol) = entity.ref
      val isTopLevel = eSymbol.attr.topLevel.get contains true
      val isVerbatim = entity.variant == "verbatim"

      val eSymbols = if (isTopLevel) Iterator.empty else Iterator.single(eSymbol)

      val dSymbols = if (isVerbatim) {
        // Retain all definitions for verbatim entities
        Iterator.empty
      } else {
        val stateVarQ = eSymbol.attr.stateVar.get
        val stateVarD = stateVarQ map { _.attr.flop.value }
        entity.declarations.iterator collect {
          case Decl(symbol, _) => symbol
        } filterNot { symbol =>
          // Retain the state variables if they exist
          (stateVarQ contains symbol) || (stateVarD contains symbol)
        } filter { // Retain inputs and outputs of top level entities
          _.kind match {
            case _: TypeIn  => !isTopLevel
            case _: TypeOut => !isTopLevel
            case _          => true
          }
        }
      }

      val iSymbols = entity.instances.iterator collect {
        case Instance(Sym(iSymbol), _, _, _) => iSymbol
      }

      eSymbols ++ dSymbols ++ iSymbols
    }

    // Gather all used symbols. A symbol is used if it's value is consumed,
    // this can happen when it is read in an rvalue, read in an lvalue, or
    // is instantiated. Furthermore all flop _d signals and array
    // _we/_waddr/_wdata signals are used. At the moment we also cannot remove
    // symbols that are written through a concatenation lvalue, as they are
    // required as placeholders
    val usedSymbols = gather(trees) {
      _ flatCollect {
        case Instance(_, Sym(eSymbol), _, _) => Iterator.single(eSymbol)
        case Connect(lhs, List(rhs: ExprCat)) => {
          // Concatenation on the right, everything is used, if only as a placeholder
          // TODO: if any symbol in the concatenation is used, then all are used
          val lSymbols = lhs match {
            case InstancePortRef(iSymbol, pSymbol) => Iterator(iSymbol, pSymbol)
            case other                             => ReadSymbols.rval(lhs)
          }
          val rSymbols = rhs collect { case ExprRef(symbol) => symbol }
          lSymbols ++ rSymbols
        }
        case Connect(InstancePortRef(iSymbol, pSymbol), List(rhs)) => {
          // instance.port on left hand side
          Iterator(iSymbol, pSymbol) ++ ReadSymbols.lval(rhs)
        }
        case Connect(lhs, List(InstancePortRef(_, _))) => {
          // instance.port on right hand side
          ReadSymbols.rval(lhs)
        }
        case Connect(lhs, List(rhs)) => {
          // Everything on the left, but on the right only stuff that is read
          ReadSymbols.rval(lhs) ++ ReadSymbols.lval(rhs)
        }
        case stmt @ StmtAssign(_: ExprCat, _) => {
          // Concatenation on the left, everything is used, if only as a placeholder
          // TODO: if any symbol in the concatenation is used, then all are used
          stmt collect { case ExprRef(symbol) => symbol }
        }
        case StmtAssign(lhs, rhs) => {
          // Everything on the right, but on the left only stuff that is read
          ReadSymbols.lval(lhs) ++ ReadSymbols.rval(rhs)
        }
        case Decl(symbol, _) if symbol.attr.flop.isSet => {
          // Flop _d
          symbol.attr.flop.get.iterator
        }
        case Decl(symbol, _) if symbol.attr.memory.isSet => {
          // Array _we/_waddr/_wdata
          val (we, waddr, wdata) = symbol.attr.memory.value
          Iterator(we, waddr, wdata)
        }
        case ExprRef(symbol) => {
          // Any other reference is used
          Iterator.single(symbol)
        }
      }
    }

    // Compute the unused ports
    val unusedSymbols = candidateSymbols diff usedSymbols

    if (unusedSymbols.isEmpty) {
      trees
    } else {
      // Remove unused entities
      val usedEntities = trees filterNot {
        case Entity(Sym(eSymbol), _, _, _, _, _, _, _, _) => unusedSymbols contains eSymbol
        case _                                            => unreachable
      }

      // Remove symbols
      val results = {
        usedEntities.par map { tree =>
          (new RemoveUnused(unusedSymbols)(cc))(tree).asInstanceOf[Entity]
        }
      }.seq.toList

      // Update type of the entities for removed ports
      results foreach { entity =>
        val portSymbols = entity.declarations collect {
          case Decl(symbol, _) if symbol.kind.isInstanceOf[TypeIn]  => symbol
          case Decl(symbol, _) if symbol.kind.isInstanceOf[TypeOut] => symbol
        }
        val Sym(entitySymbol: TypeSymbol) = entity.ref
        val newKind = entitySymbol.kind match {
          case kind: TypeEntity => kind.copy(portSymbols = portSymbols)
          case _                => unreachable
        }
        entitySymbol.kind = newKind
      }

      // Iterate until we no longer have any unused ports
      loop(results)
    }
  }

  def apply(trees: List[Tree])(implicit cc: CompilerContext): List[Tree] = loop(trees)
}
