////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Simplify state bodies locally, remove unreachable states.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.Symbols.Symbol

import scala.annotation.tailrec
import scala.collection.mutable

final class SimplifyStates(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  override protected def skip(tree: Tree): Boolean = tree match {
    case _: DeclEntity | _: DefnEntity     => false
    case EntSplice(_: Defn) | _: DefnState => false
    case _                                 => true
  }

  // Pairs of states where the first can reach the second. Note this is not
  // complete, but is sufficient to check which states are unreachable.
  private val reachability = mutable.Set[(Symbol, Symbol)]()
  // Set of reachable states
  private var reachable: Set[Symbol] = Set.empty

  override protected def enter(tree: Tree): Option[Tree] = tree match {
    case defn: DefnState =>
      Some {
        // Simplify bodies then gather reachability pairs
        defn rewrite new InlineKnownVars rewrite new Fold tap { simplified =>
          reachability addAll {
            simplified.body.iterator flatMap {
              _ collect { case ExprSym(symbol) if symbol.kind.isState => defn.symbol -> symbol }
            }
          }
        }
      }
    case _ => None
  }

  override protected def transform(tree: Tree): Tree = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Drop unreachable states
    ////////////////////////////////////////////////////////////////////////////

    case defn: DefnEntity =>
      // Compute reachable states
      val entryState = (defn.states find { _.symbol.attr.entry contains true }).get.symbol
      val followers = reachability.groupMap(_._1)(_._2) withDefaultValue Set.empty
      @tailrec
      def loop(reachable: Set[Symbol]): Set[Symbol] =
        reachable flatMap followers filterNot reachable match {
          case empty if empty.isEmpty => reachable
          case extra                  => loop(reachable union extra)
        }
      reachable = loop(Set(entryState))
      // Drop unreachable states
      if (reachable.size == defn.states.size) {
        tree
      } else {
        val keptBody = defn.body filter {
          case EntSplice(DefnState(symbol, _)) => reachable(symbol)
          case _                               => true
        }
        TypeAssigner(defn.copy(body = keptBody) withLoc tree.loc)
      }

    case decl: DeclEntity if reachable.size != decl.states.size =>
      val keptDecls = decl.decls filter {
        case DeclState(symbol) => reachable(symbol)
        case _                 => true
      }
      TypeAssigner(decl.copy(decls = keptDecls) withLoc tree.loc)

    //
    case _ => tree
  }

}

object SimplifyStates extends EntityTransformerPass(declFirst = false) {
  val name = "simplify-states"

  override protected def skip(decl: Decl, defn: Defn)(implicit cc: CompilerContext): Boolean =
    super.skip(decl, defn) || defn.asInstanceOf[DefnEntity].states.isEmpty

  override protected def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
    new SimplifyStates
}
