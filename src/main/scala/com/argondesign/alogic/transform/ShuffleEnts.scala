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
// Randomly shuffle order of List[Ent] in Trees (for testing purposes)
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.transform

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.typer.TypeAssigner

import scala.util.Random

final class ShuffleEnts(override val typed: Boolean, val seed: Int)(implicit cc: CompilerContext)
    extends StatefulTreeTransformer {

  private[this] def shuffle(name: String, ents: List[Ent]): List[Ent] = {
    val random = new Random(name.foldLeft(seed)(_ * _))
    random shuffle ents
  }

  override def transform(tree: Tree): Tree = tree match {
    case defn: DefnEntity =>
      val newDefn = defn.copy(body = shuffle(defn.symbol.name, defn.body)) withLoc defn.loc
      if (tree.hasTpe) {
        TypeAssigner(newDefn)
      }
      newDefn
    case defn: DefnSingleton =>
      val newDefn = defn.copy(body = shuffle(defn.symbol.name, defn.body)) withLoc defn.loc
      if (tree.hasTpe) {
        TypeAssigner(newDefn)
      }
      newDefn
    case _ =>
      tree
  }
}
