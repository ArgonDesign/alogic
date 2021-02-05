////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.analysis.BitwiseLiveVariableAnalysis
import com.argondesign.alogic.analysis.ReadSymbols
import com.argondesign.alogic.analysis.SymbolBitSet
import com.argondesign.alogic.analysis.WrittenSymbolBits
import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.util.unreachable
import com.argondesign.alogic.util.BigIntOps._

final class RemoveRedundantAssignments extends StatelessTreeTransformer {

  private var variablesLiveAtEndOfCycle: SymbolBitSet = _

  private var liveVariableMaps: Map[Int, (SymbolBitSet, SymbolBitSet)] = Map.empty

  override def start(tree: Tree): Unit = tree match {
    case defn: DefnEntity =>
      // Gather symbols read outside of combinational processes
      variablesLiveAtEndOfCycle = SymbolBitSet from {
        defn
          .flatCollect {
            case DefnOut(symbol, _) =>
              Iterator.single(symbol)
            case _: EntCombProcess =>
              Iterator.empty
            case EntAssign(lhs, rhs) =>
              ReadSymbols.lval(lhs) concat ReadSymbols.rval(rhs)
            case StmtAssign(lhs, rhs) =>
              ReadSymbols.lval(lhs) concat ReadSymbols.rval(rhs)
            case StmtDelayed(lhs, rhs) =>
              ReadSymbols.lval(lhs) concat ReadSymbols.rval(rhs)
            case StmtOutcall(output, func, inputs) =>
              ReadSymbols
                .lval(output)
                .concat(ReadSymbols.rval(func))
                .concat(inputs.iterator.flatMap(ReadSymbols.rval))
            case expr: Expr =>
              ReadSymbols.rval(expr)
          }
          .filter(_.kind.isPacked)
          .map(symbol => symbol -> BigInt.mask(symbol.kind.width.toInt))
      }

    case _ => unreachable
  }

  override def enter(tree: Tree): Option[Tree] = tree match {
    case EntCombProcess(stmts) =>
      liveVariableMaps = BitwiseLiveVariableAnalysis(stmts, variablesLiveAtEndOfCycle)
      None

    // Remove assignments to dead variables
    case StmtAssign(lhs, _) =>
      liveVariableMaps.get(tree.id).map {
        case (_, liveVariables) =>
          val mightWriteLiveBit = WrittenSymbolBits.possibly(lhs).exists {
            case (symbol, writtenBits) =>
              liveVariables.get(symbol).exists(liveBits => (writtenBits & liveBits) != 0)
          }
          if (mightWriteLiveBit) tree else Stump
      } orElse Some(tree)

    // Skip
    case _: Ent | _: Expr => Some(tree)

    case _ => None
  }

  override protected def transform(tree: Tree): Tree = tree match {
    case _: EntCombProcess =>
      liveVariableMaps = Map.empty
      tree

    case _ => tree
  }

  override protected def finalCheck(tree: Tree): Unit = {
    assert(liveVariableMaps == Map.empty)
  }

}

object RemoveRedundantAssignments extends PairTransformerPass(parallel = true) {
  val name = "remove-redundant-assignments"

  override def skip(decl: Decl, defn: Defn)(implicit cc: CompilerContext): Boolean = defn match {
    case d: DefnEntity => d.combProcesses.isEmpty
    case _             => true
  }

  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) =
    (decl, defn rewrite new RemoveRedundantAssignments)
}
