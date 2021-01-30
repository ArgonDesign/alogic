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
// Any symbol driven combinationally through the FSM logic must be assigned a
// value on all code paths to avoid latches. In this phase we add all such
// default assignments.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.analysis.Liveness
import com.argondesign.alogic.analysis.WrittenSymbols
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.transform.StatementFilter
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

object DefaultAssignments extends PairTransformerPass(parallel = true) {
  val name = "default-assignments"

  override def skip(decl: Decl, defn: Defn)(implicit cc: CompilerContext): Boolean = defn match {
    case d: DefnEntity => d.combProcesses.isEmpty
    case _             => true
  }

  override def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) = {

    val entityDefn = defn.asInstanceOf[DefnEntity]

    val needsDefault = mutable.Set[Symbol]()

    decl.decls.iterator foreach {
      case DeclVar(symbol, _) if !symbol.attr.flop.isSet       => needsDefault += symbol
      case DeclOut(symbol, _, _, _) if !symbol.attr.flop.isSet => needsDefault += symbol
      case _                                                   =>
    }

    if (needsDefault.isEmpty) {
      (decl, defn)
    } else {

      // Remove any nets driven through an assign
      for (EntAssign(lhs, _) <- entityDefn.assigns) {
        lhs.visit {
          case ExprSym(symbol) => needsDefault remove symbol
        }
      }

      // Remove any nets written in a clocked process
      for {
        EntClockedProcess(_, _, stmts) <- entityDefn.clockedProcesses
        stmt <- stmts
      } {
        stmt visit {
          case StmtAssign(lhs, _)     => WrittenSymbols(lhs) foreach needsDefault.remove
          case StmtOutcall(out, _, _) => WrittenSymbols(out) foreach needsDefault.remove
        }
      }

      // Remove symbols that are dead at the beginning of the cycle.
      val (liveSymbolBits, deadSymbolBits) = Liveness(entityDefn.combProcesses.head.stmts)

      if (needsDefault.nonEmpty) {
        assert(entityDefn.combProcesses.lengthIs == 1)

        // Keep only the symbols with all bits dead
        val deadSymbols = Set from {
          deadSymbolBits collect {
            case (symbol, set) if set.size == symbol.kind.width => symbol
          }
        }

        // Now retain only the symbols that are not dead
        needsDefault filterInPlace { symbol =>
          !(deadSymbols contains symbol)
        }
      }

      // If a symbol is live or we use it's _q, initialize to its default value
      // otherwise zero.
      val initializeToDefault = Set from {
        def collect(tree: Tree): Iterator[Symbol] = tree flatCollect {
          case StmtDelayed(_: ExprSym, rhs) => collect(rhs)
          case ExprSym(symbol)              => symbol.attr.flop.get.iterator
        }
        collect(entityDefn)
      } union liveSymbolBits.keySet

      // Any Q symbols which are referenced  (other than in the clocked blocks)
      val referencedQSymbols = Set from {
        defn flatCollect {
          case _: EntClockedProcess                      => None // Stop descent
          case ExprSym(symbol) if symbol.attr.flop.isSet => Some(symbol)
        }
      }

      val newBody = entityDefn.body flatMap {
        case ent @ EntCombProcess(stmts) =>
          Some {
            // Add default assignments
            val leading = for {
              Defn(symbol) <- entityDefn.defns
              if needsDefault contains symbol
            } yield {
              val init = if ((initializeToDefault contains symbol) && symbol.attr.default.isSet) {
                symbol.attr.default.value
              } else {
                val kind = symbol.kind
                ExprInt(kind.isSigned, kind.width.toInt, 0)
              }
              StmtAssign(ExprSym(symbol), init) regularize symbol.loc
            }
            TypeAssigner(EntCombProcess(leading ::: stmts) withLoc ent.loc)
          }
        case ent @ EntClockedProcess(__, _, stmts) =>
          // Drop delayed assignments to unused flops
          val filter = StatementFilter {
            case StmtDelayed(ExprSym(qSymbol), _) if !referencedQSymbols(qSymbol) =>
              qSymbol.attr.flop.get match {
                case None          => true // Keep non-flops
                case Some(dSymbol) => needsDefault(dSymbol) && initializeToDefault(dSymbol)
              }
          }
          // Keep comments directly in the process, filter the rest
          val newStmts = stmts flatMap {
            case stmt: StmtComment => Some(stmt)
            case stmt: Stmt =>
              filter(stmt) match {
                case Stump       => None
                case other: Stmt => Some(other)
                case _           => unreachable
              }
          }
          // Drop the whole block if there are only comments left
          Option.unless(newStmts forall { _.isInstanceOf[StmtComment] }) {
            TypeAssigner(ent.copy(stmts = newStmts) withLoc ent.loc)
          }
        case other => Some(other)
      }

      val newDefn = TypeAssigner(entityDefn.copy(body = newBody) withLoc entityDefn.loc)

      (decl, newDefn)
    }
  }

}
