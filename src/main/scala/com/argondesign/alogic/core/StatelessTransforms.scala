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
import com.argondesign.alogic.passes.ReplaceUnaryTicks
import com.argondesign.alogic.passes.ResolvePolyFunc
import com.argondesign.alogic.transform.SimplifyExpr

trait StatelessTransforms { this: CompilerContext =>
  val replaceUnaryTicks: StatelessTreeTransformer = new ReplaceUnaryTicks()(cc = this)
  val resolvePolyFunc: StatelessTreeTransformer = new ResolvePolyFunc()(cc = this)
  val addCasts: StatelessTreeTransformer = new AddCasts()(cc = this)
  val simpifyExpr: StatelessTreeTransformer = new SimplifyExpr()(cc = this)
}
