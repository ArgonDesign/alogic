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
// A pass is anything that takes a list of trees and returns a new list
// of trees. While most of them are implemented as a TreeTransformer,
// other implementations are possible but discouraged. If a pass can
// reasonably be implemented as a TreeTransformer, it should be.
// Passes should implement a single transformation or analysis function.
// All passes should process all trees in parallel whenever possible.
// Splitting a pass into multiple smaller passes is recommended if this
// enables running the smaller passes in parallel across all trees.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext

// The very minimal Pass interface
trait Pass {
  // Name of pass for debugging
  val name: String
  // The implementation of the pass
  def apply(trees: List[Tree])(implicit cc: CompilerContext): List[Tree]
}

// Simple pass that applies a new instance of a TreeTransformer to all trees
trait TreeTransformerPass extends Pass {

  // Factory method to create a new instance of the tree transformer
  protected def create(implicit cc: CompilerContext): TreeTransformer

  override def apply(trees: List[Tree])(implicit cc: CompilerContext): List[Tree] = {
    // Apply pass to all trees in parallel
    val transformed = trees.par map { tree =>
      create(cc)(tree)
    }

    // Collect the results and flatten Thickets
    val results = transformed.seq.toList flatMap {
      case Thicket(trees) => trees
      case other          => List(other)
    }

    // Dump entities if required
    if (cc.settings.dumpTrees) {
      results foreach {
        case entity: Entity => cc.dumpEntity(entity, s".${cc.passNumber}.${name}")
        case _              => ()
      }
    }

    // Return the resulting trees
    results
  }
}
