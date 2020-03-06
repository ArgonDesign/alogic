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

import com.argondesign.alogic.backend.CodeGeneration
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.frontend.Parse

import scala.util.ChainingSyntax

object Passes extends ChainingSyntax {

  // All trees are transformed with the given pass before the next pass begins
  def apply(topLevels: List[String])(implicit cc: CompilerContext) = {
    // Define the passes to apply
    val passes =
      ////////////////////////////////////////////////////////////////////////
      // Front-end
      ////////////////////////////////////////////////////////////////////////
      Parse andThen
        Checker andThen
        Namer andThen
        UnusedCheck andThen
        Elaborate andThen
////      UnusedCheck(postElaborate = true),
        // Any passes between here and the middle end can only perform checks
        // and cannot re-write any trees unless errors have been detected
        TypeCheck andThen
        PortCheckA andThen
        ////////////////////////////////////////////////////////////////////////
        // Middle-end
        ////////////////////////////////////////////////////////////////////////
        ReplaceUnaryTicks andThen // This must be first as TypeAssigner cannot handle unary '
        ResolvePolyFunc andThen
        AddCasts andThen
        Desugar andThen
        InlineUnsizedConst andThen
        FoldTypeAliases andThen
        FoldExpr andThen
        PortCheckB andThen
        ConvertMultiConnect andThen
        LowerPipeline andThen
        LiftEntities() andThen
        LowerLoops andThen
        AnalyseCallGraph andThen
        ConvertLocalDecls andThen
        RemoveStructuralSharing andThen
        ConvertControl andThen
        CreateStateSystem andThen
        Replace1Stacks andThen
        // TODO: Replace1Arrays
        DefaultStorage andThen
        // TODO: CheckAcceptUsage
        LowerFlowControl() andThen
        LowerSrams() andThen
        LowerStacks andThen
        LowerRegPorts andThen
        LowerArrays andThen
        LiftSrams andThen
        SplitStructs() andThen
        LowerVectors() andThen
        AddCasts andThen
        FoldExpr andThen
        SimplifyCat andThen
        InferImplications andThen
        FoldStmt andThen
        SimplifyConditionals andThen
        ////////////////////////////////////////////////////////////////////////
        // Back-end
        ////////////////////////////////////////////////////////////////////////
        RenameSymbols andThen
        LowerVariables andThen
        LowerInterconnect andThen
        PropagateImplications andThen
        RemoveStructuralSharing andThen
        FoldStmt andThen
        OptimizeClearOnStall andThen
        LowerStalls andThen
//        InlineKnownVars andThen
        DefaultAssignments andThen
        RemoveUnused andThen
        RemoveRedundantBlocks andThen
        RenameSymbols andThen
//      // TODO: RenameKeywords
//      // TODO: final check pass to make sure everything is well-formed
        WriteModuleManifest andThen
        CodeGeneration

    // Apply the passes to the trees
    passes(topLevels)
  }

}
