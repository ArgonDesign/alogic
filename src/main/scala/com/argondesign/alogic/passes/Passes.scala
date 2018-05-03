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
import com.argondesign.alogic.util.FollowedBy._

object Passes {

  private def doPhase(
      trees: List[Tree],
      factory: () => TreeTransformer
  )(
      implicit cc: CompilerContext
  ): List[Tree] = {
    if (cc.hasError) {
      // If we have encountered errors in an earlier pass, skip any later passes
      trees
    } else {
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
  } followedBy {
    cc.emitMessages(Console.err)
  }

  def apply(trees: List[Tree])(implicit cc: CompilerContext): List[Tree] = {
    // Each phase is applied in parallel to all trees, but all trees
    // are transformed with the given phase before the next phase begins
    val passes = List(
      ////////////////////////////////////////////////////////////////////////
      // Front-end
      ////////////////////////////////////////////////////////////////////////
      () => new Checker,
      () => new Namer,
      () => new Desugar,
      () => new Typer,
      ////////////////////////////////////////////////////////////////////////
      // Middle-end
      ////////////////////////////////////////////////////////////////////////
      () => new ConvertMultiConnect,
      () => new FoldExpr(assignTypes = true, foldRefs = false)(cc),
      () => new SpecializeParamA,
      () => new SpecializeParamB,
      () => new SpecializeParamC,
      () => new FoldExpr(assignTypes = true, foldRefs = false)(cc),
      () => new LowerPipeline,
      () => new LiftEntities,
      () => new LowerLoops,
      () => new AnalyseCallGraph,
      () => new ConvertLocalDecls,
      () => new ConvertControl,
      () => new AllocStates,
      () => new Replace1Stacks,
      // TODO: Replace1Arrays
      () => new DefaultStorage,
      // TODO: CheckAcceptUsage
      () => new LowerFlowControlA,
      () => new LowerFlowControlB,
      () => new LowerFlowControlC,
      // TODO: CheckPureExpressionInStatementPosition
      () => new LowerRegPorts,
      () => new LowerStacks,
      () => new SplitStructsA,
      () => new SplitStructsB,
      () => new SplitStructsC,
      () => new FoldExpr(assignTypes = true, foldRefs = false)(cc),
      () => new SimplifyCat,
      ////////////////////////////////////////////////////////////////////////
      // Back-end
      ////////////////////////////////////////////////////////////////////////
      () => new LowerFlops,
      () => new LowerArrays,
      () => new LowerInterconnect,
      () => new DefaultAssignments,
      // TODO: LowerGo
      () => new RemoveRedundantBlocks,
      () => new RenameClashingTerms
      // TODO: RenameKeywords
      // TODO: final check pass to make sure everything is well-formed
    )

    // Fold passes over the trees
    (trees /: passes) { doPhase(_, _) }
  }

}
