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
import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.transform.StatementFilter
import com.argondesign.alogic.typer.TypeAssigner

import scala.collection.mutable

final class DefaultAssignments(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  private val needsDefault = mutable.Set[Symbol]()

  override def skip(tree: Tree): Boolean = tree match {
    case defn: DefnEntity => defn.combProcesses.isEmpty
    case _                => false
  }

  override def enter(tree: Tree): Option[Tree] = {
    tree match {
      case DeclVar(symbol, _) if !symbol.attr.flop.isSet       => needsDefault += symbol
      case DeclOut(symbol, _, _, _) if !symbol.attr.flop.isSet => needsDefault += symbol
      case _                                                   =>
    }
    None
  }

  override def transform(tree: Tree): Tree = tree match {
    case defn: DefnEntity if needsDefault.nonEmpty =>
      // Remove any nets driven through a connect
      for (EntConnect(_, List(rhs)) <- defn.connects) {
        rhs.visit {
          case ExprSym(symbol) => needsDefault remove symbol
        }
      }

      // Remove symbols that are dead at the beginning of the cycle.
      val (liveSymbolBits, deadSymbolBits) = Liveness(defn.combProcesses.head.stmts)

      if (needsDefault.nonEmpty) {
        assert(defn.combProcesses.lengthIs == 1)

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

      // If a symbol is live or drives a connection, initialize to its
      // default value otherwise zero.
      val initializeToDefault = {
        val symbolsDrivingConnect = Set from {
          defn.connects.iterator flatMap {
            case EntConnect(lhs, _) =>
              lhs.collect {
                case ExprSym(symbol) => symbol.attr.flop.getOrElse(symbol)
              }
          }
        }

        liveSymbolBits.keySet union symbolsDrivingConnect
      }

      // Any Q symbols which are referenced  (other than in the clocked blocks)
      val referencedQSymbols = Set from {
        defn flatCollect {
          case _: EntClockedProcess                      => None // Stop descent
          case ExprSym(symbol) if symbol.attr.flop.isSet => Some(symbol)
        }
      }

      val newBody = defn.body flatMap {
        case ent @ EntCombProcess(stmts) =>
          // Add default assignments
          val leading = for {
            Defn(symbol) <- defn.defns
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
          List(TypeAssigner(EntCombProcess(leading ::: stmts) withLoc ent.loc))
        case ent @ EntClockedProcess(_, stmts) =>
          // Drop delayed assignments to unused flops
          val filter = StatementFilter {
            case _: StmtComment => true // Keep comments
            case StmtDelayed(ExprSym(qSymbol), _) if !referencedQSymbols(qSymbol) =>
              qSymbol.attr.flop.get match {
                case None          => true // Keep non-flops
                case Some(dSymbol) => needsDefault(dSymbol) && initializeToDefault(dSymbol)
              }
          }
          val newStmts = stmts map filter collect {
            // Keep it if there are any assignments left, or it's a top level comment
            case stmt: StmtComment => stmt
            case stmt: Stmt if stmt exists { case _: StmtDelayed => true } => stmt
          }
          val drop = newStmts forall {
            case _: StmtComment => true
            case _              => false
          }
          if (drop) Nil else List(TypeAssigner(ent.copy(stmts = newStmts) withLoc ent.loc))
        case other => List(other)
      }

      TypeAssigner {
        defn.copy(body = newBody) withLoc tree.loc
      }

    case _ => tree
  }

}

object DefaultAssignments extends EntityTransformerPass(declFirst = true) {
  val name = "default-assignments"
  def create(symbol: Symbol)(implicit cc: CompilerContext) = new DefaultAssignments
}
