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
// - Inline any variables that we know the value of at the end of the cycle
// - Use a connect or output ports
//
// Assumes that this kind of stuff does not happen:
//   a = 1
//   b = a
//   a = 2
// This is ensured by FoldStmt earlier
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.analysis.StaticEvaluation
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.util.unreachable

final class InlineKnownVars(implicit cc: CompilerContext) extends TreeTransformer {

  private var bindings: Map[Symbol, Expr] = Map.empty

  override def start(tree: Tree): Unit = tree match {
    case defn: DefnEntity =>
      assert(defn.combProcesses.lengthIs <= 1)
      defn.combProcesses.headOption foreach {
        case EntCombProcess(body) =>
          val magic = Set from {
            defn.defns.iterator flatMap {
              // Flop D signals
              case Defn(symbol) if symbol.attr.flop.isSet =>
                Iterator.single(symbol.attr.flop.value)
              // Memory signals
              case Defn(symbol) if symbol.attr.memory.isSet =>
                val (a, b, c) = symbol.attr.memory.value
                Iterator(a, b, c)
              case _ => Iterator.empty
            }
          }
          bindings = Map from {
            StaticEvaluation(StmtBlock(body), Nil)._2.view mapValues {
              _.simplify
            } filter {
              case (symbol, _: ExprInt) =>
                !(symbol.kind.isIn || symbol.kind.isOut) && !magic(symbol)
              case _ => false
            }
          }
      }
    case _: DeclEntity =>
    case _             => unreachable
  }

  override def skip(tree: Tree): Boolean = bindings.isEmpty

  override def enter(tree: Tree): Option[Tree] = tree match {
    // Drop assignments to dropped symbols
    case StmtAssign(ExprSym(symbol), _) if bindings contains symbol => Some(Stump)
    case _                                                          => None
  }

  override def transform(tree: Tree): Tree = tree match {
    // Drop Decl/Defn
    case DeclVar(symbol, _) if bindings contains symbol => Stump
    case DefnVar(symbol, _) if bindings contains symbol => Stump

    case ExprSym(symbol) => bindings.getOrElse(symbol, tree)

    case _ => tree
  }

}

object InlineKnownVars extends EntityTransformerPass(declFirst = false) {
  val name = "inline-known-wars"

  override def skip(decl: Decl, defn: Defn)(implicit cc: CompilerContext): Boolean =
    super.skip(decl, defn) || defn.asInstanceOf[DefnEntity].combProcesses.isEmpty

  def create(symbol: Symbol)(implicit cc: CompilerContext) = new InlineKnownVars
}
