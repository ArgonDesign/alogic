////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Entry point to the specialization algorithm
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.specialize

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext

object Specialize {

  def apply(specs: Iterable[Expr])(implicit cc: CompilerContext): Option[Iterable[(Decl, Defn)]] = {
    (new SpecializeDesc()(cc))(specs)
  }

}
