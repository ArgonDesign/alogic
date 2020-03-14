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
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.Trees.Decl
import com.argondesign.alogic.ast.Trees.Defn
import com.argondesign.alogic.core.CompilerContext

object TypeCheck extends PairsTransformerPass {
  val name = "type-check"

  def process(pairs: List[(Decl, Defn)])(implicit cc: CompilerContext): List[(Decl, Defn)] =
    pairs filter {
      case (decl, defn) => cc.typeCheck(decl) && cc.typeCheck(defn)
    }

}
