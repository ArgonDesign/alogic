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
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.typer.TypeAssigner

import scala.collection.mutable

final class DefaultAssignments(implicit cc: CompilerContext) extends TreeTransformer {

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
      lazy val (liveSymbolBits, deadSymbolBits) = Liveness(defn.combProcesses.head.stmts)

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

      if (needsDefault.isEmpty) {
        tree
      } else {
        // Symbols have default assignments set as follows:
        // If a symbol is live or drives a connection, initialize to its default value
        // otherwise zero.
        val initializeToRegisteredVal = {
          val liveSymbols = liveSymbolBits.underlying.keySet

          val symbolsDrivingConnect = Set from {
            defn.connects.iterator flatMap {
              case EntConnect(lhs, _) =>
                lhs.collect {
                  case ExprSym(symbol) => symbol.attr.flop.getOrElse(symbol)
                }
            }
          }

          liveSymbols union symbolsDrivingConnect
        }

        val leading = for {
          Defn(symbol) <- defn.defns
          if needsDefault contains symbol
        } yield {
          val init = if ((initializeToRegisteredVal contains symbol) && symbol.attr.default.isSet) {
            symbol.attr.default.value
          } else {
            val kind = symbol.kind
            ExprInt(kind.isSigned, kind.width.toInt, 0)
          }
          StmtAssign(ExprSym(symbol), init) regularize symbol.loc
        }

        val newBody = defn.body map {
          case ent @ EntCombProcess(stmts) =>
            TypeAssigner(EntCombProcess(leading ::: stmts) withLoc ent.loc)
          case other => other
        }

        TypeAssigner {
          defn.copy(body = newBody) withLoc tree.loc
        }
      }

    case _ => tree
  }

}

object DefaultAssignments extends EntityTransformerPass(declFirst = true) {
  val name = "default-assignments"
  def create(symbol: Symbol)(implicit cc: CompilerContext) = new DefaultAssignments
}
