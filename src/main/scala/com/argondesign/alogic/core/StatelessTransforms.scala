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

  val dropPackages = new DropPackageAndParametrizedDescs()(cc = this)
  val descToDeclDefn = new DescToDeclDefn()(cc = this)
  val desugar = new Desugar()(cc = this)
  val fold = new Fold()(cc = this)
  val portCheck = new PortCheck()(cc = this)
  val simplifyCat = new SimplifyCat()(cc = this)
  val inferImplications = new InferImplications()(cc = this)
  val propagateImplications = new PropagateImplications()(cc = this)
  val optimizeClearOnStall = new OptimizeClearOnStall()(cc = this)
  val removeAssume = new RemoveAssume()(cc = this)
}
