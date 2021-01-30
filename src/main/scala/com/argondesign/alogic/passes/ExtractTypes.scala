////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Extract nested type definitions and make them all top level.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

final class ExtractTypes extends StatelessTreeTransformer {

  private val decls = mutable.ListBuffer[Decl]()
  private val defns = mutable.Map[Symbol, Defn]()

  def types: List[(Decl, Defn)] = List from {
    assert(decls.size == defns.size)
    decls.iterator map { decl => (decl, defns(decl.symbol)) }
  }

  private var root: Symbol = _

  override def start(tree: Tree): Unit = tree match {
    case Decl(symbol) => root = symbol
    case _: Defn      =>
    case _            => unreachable
  }

  override def transform(tree: Tree): Tree = tree match {
    // Leave he root Decl/Defn alone
    case Decl(symbol) if symbol == root => tree
    case Defn(symbol) if symbol == root => tree

    // Extract type Decl/Defn
    case decl @ Decl(symbol) if symbol.kind.isType =>
      decls append decl
      Stump
    case defn @ Defn(symbol) if symbol.kind.isType =>
      defns(symbol) = defn
      Stump

    //
    case _ => tree
  }

}

object ExtractTypes extends PairTransformerPass(parallel = true) {
  val name = "extract-types"

  protected def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) = {
    val transform = new ExtractTypes
    val head = (transform(decl), transform(defn))
    val (decls, defns) = (head :: transform.types).unzip
    (Thicket(decls), Thicket(defns))
  }

}
