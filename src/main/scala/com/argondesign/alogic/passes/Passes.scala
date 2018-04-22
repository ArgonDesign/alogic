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
// Driver to apply all compiler passes to trees
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.typer.Typer

object Passes {

  private def doPhase(trees: List[Tree], factory: () => TreeTransformer): List[Tree] = {
    // Apply phase to all trees in parallel
    val results = trees.par map { tree =>
      val phase = factory()
      phase(tree)
    }

    // Collect the results and flatten Thickets
    results.seq.toList flatMap {
      case Thicket(trees) => trees
      case other          => List(other)
    }
  }

  def apply(trees: List[Tree])(implicit cc: CompilerContext): List[Tree] = {
    // Each phase is applied in parallel to all trees,
    // but all trees are transformed with the given phase
    // before the next phase started
    val passes = List(
      () => new Checker,
      () => new Namer,
      () => new Desugar,
      () => new Typer,
      () => new ConvertMultiConnect,
      () => new ConvertDeclInit,
      () => new FoldExpr(assignTypes = true)(cc),
      () => new AnalyseParamConst,
      () => new SpecializeParam,
      () => new SpecializeInstance,
      () => new InlineConst,
      () => new FoldExpr(assignTypes = true)(cc),
      () => new LowerPipeline,
      () => new LiftEntities,
      () => new LowerLoops,
      () => new AnalyseCallGraph,
      () => new ConvertControl,
      () => new AllocStates,
      () => new Replace1Stacks,
      () => new DefaultStorage,
      () => new LowerFlowControlA,
      () => new LowerFlowControlB,
      () => new LowerFlowControlC,
      () => new LowerRegPorts,
      () => new LowerStacks,
      () => new SplitStructsA,
      () => new SplitStructsB,
      () => new SplitStructsC,
      () => new SimplifyCat,
      () => new RemoveNestedBlocks,
      // TypeFlop ???
      // Lower Array ???
      // oreg naming
      // name mangling
      // output
    )

    // Fold passes over the trees
    (trees /: passes) { doPhase(_, _) }
  }

}
