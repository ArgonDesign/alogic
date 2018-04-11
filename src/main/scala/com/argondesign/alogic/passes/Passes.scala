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

  private def doGroup(trees: List[Tree], factory: () => List[TreeTransformer]): List[Tree] = {
    // Apply group to all trees in parallel
    val results = trees.par map { tree =>
      // Fold the group over the tree
      (tree /: factory()) { (tree, pass) =>
        pass(tree)
      }
    }

    // Collect the results and flatten Thickets
    results.seq.toList flatMap {
      case Thicket(trees) => trees
      case other          => List(other)
    }
  }

  def apply(trees: List[Tree])(implicit cc: CompilerContext): List[Tree] = {

    // Each sub-list is applied in parallel, but the whole sublist is applied
    // to all trees before the next sublist is started
    val passes = List(
      () =>
        List(
          new Checker,
          new Namer,
          new Desugar
      ),
      // - Typer needs cross entity information to resolve instances
      () => List(new Typer),
      // - Later stages may need forward type information
      () =>
        List(
          new FoldExpr(assignTypes = true)(cc),
          new LowerPipeline,
          new LiftEntities,
          new DefaultStorage,
          new LowerLoops
      ),
      // - Need to flatten Thickets for AnalyseCallGraph
      () =>
        List(
          new AnalyseCallGraph,
          new LowerFlowControl
      ),
      // - Need to flatten Thickets for LowerRegPorts
      // - ConvertControl requires the return stack allocated in
      //   AllocateReturnStack early
      () =>
        List(
          new LowerRegPorts,
          new ConvertControl
      )
    )

    // Fold groups over the trees
    (trees /: passes) { doGroup(_, _) }
  }

}
