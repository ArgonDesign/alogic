////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Stateless tree transformer instances
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.passes._

trait StatelessTransforms { this: CompilerContext =>
  val dropPackages = new DropPackageAndParametrizedDescs
  val descToDeclDefn = new DescToDeclDefn
  val desugar = new Desugar()(cc = this)
  val combineStatements = new CombineStatements
  val fold = new Fold()(cc = this)
  val portCheck = new PortCheck()(cc = this)
  val simplifyCat = new SimplifyCat
  val inferImplications = new InferImplications
  val propagateImplications = new PropagateImplications
  val removeAssume = new RemoveAssume
}
