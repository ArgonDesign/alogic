////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Stateless tree transformer instances
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.passes._

trait StatelessTransforms { this: CompilerContext =>
  val desugar = new Desugar()(cc = this)
  val portCheck = new PortCheck()(cc = this)
}
