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
// Stateless tree transformer instances
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.passes.AddCasts
import com.argondesign.alogic.passes.ConvertMultiConnect
import com.argondesign.alogic.passes.Desugar
import com.argondesign.alogic.passes.Fold
import com.argondesign.alogic.passes.InferImplications
import com.argondesign.alogic.passes.OptimizeClearOnStall
import com.argondesign.alogic.passes.PortCheckA
import com.argondesign.alogic.passes.PortCheckB
import com.argondesign.alogic.passes.PropagateImplications
import com.argondesign.alogic.passes.ReplaceUnaryTicks
import com.argondesign.alogic.passes.ResolvePolyFunc
import com.argondesign.alogic.passes.SimplifyCat
import com.argondesign.alogic.transform.SimplifyExpr

trait StatelessTransforms { this: CompilerContext =>
  val simpifyExpr: StatelessTreeTransformer = new SimplifyExpr()(cc = this)

  val portCheckA: StatelessTreeTransformer = new PortCheckA()(cc = this)
  val replaceUnaryTicks: StatelessTreeTransformer = new ReplaceUnaryTicks()(cc = this)
  val resolvePolyFunc: StatelessTreeTransformer = new ResolvePolyFunc()(cc = this)
  val addCasts: StatelessTreeTransformer = new AddCasts()(cc = this)
  val desugar: StatelessTreeTransformer = new Desugar()(cc = this)
  val fold: StatelessTreeTransformer = new Fold()(cc = this)
  val portCheckB: StatelessTreeTransformer = new PortCheckB()(cc = this)
  val convertMultiConnect: StatelessTreeTransformer = new ConvertMultiConnect()(cc = this)
  val simplifyCat: StatelessTreeTransformer = new SimplifyCat()(cc = this)
  val inferImplications: StatelessTreeTransformer = new InferImplications()(cc = this)
  val propagateImplications: StatelessTreeTransformer = new PropagateImplications()(cc = this)
  val optimizeClearOnStall: StatelessTreeTransformer = new OptimizeClearOnStall()(cc = this)
}
