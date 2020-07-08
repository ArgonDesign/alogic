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
import com.argondesign.alogic.passes._
import com.argondesign.alogic.transform.SimplifyExpr

trait StatelessTransforms { this: CompilerContext =>
  val simpifyExpr: StatelessTreeTransformer = new SimplifyExpr()(cc = this)

  val replaceUnaryTicks: StatelessTreeTransformer = new ReplaceUnaryTicks()(cc = this)
  val resolvePolyFunc: StatelessTreeTransformer = new ResolvePolyFunc()(cc = this)
  val desugar: StatelessTreeTransformer = new Desugar()(cc = this)
  val fold: StatelessTreeTransformer = new Fold()(cc = this)
  val portCheck: StatelessTreeTransformer = new PortCheck()(cc = this)
  val simplifyCat: StatelessTreeTransformer = new SimplifyCat()(cc = this)
  val inferImplications: StatelessTreeTransformer = new InferImplications()(cc = this)
  val propagateImplications: StatelessTreeTransformer = new PropagateImplications()(cc = this)
  val optimizeClearOnStall: StatelessTreeTransformer = new OptimizeClearOnStall()(cc = this)
  val removeAssume: StatelessTreeTransformer = new RemoveAssume()(cc = this)
}
