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
// Specialize parameters and process 'gen' constructs
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.specialize.Specialize

import scala.collection.parallel.CollectionConverters._
import scala.util.ChainingSyntax

object Elaborate
    extends Pass[(Iterable[Root], Iterable[Expr]), Iterable[(Decl, Defn)]]
    with ChainingSyntax {
  val name = "elaborate"

  override def dump(
      result: Iterable[(Decl, Defn)],
      tag: String
    )(
      implicit
      cc: CompilerContext
    ): Unit =
    result foreach {
      case (decl, defn) => cc.dump(decl, defn, "." + tag)
    }

  // Visitor that sets sourceName fields of symbols
  // TODO: Subsume this into TypeCheck Pass ?
  class SourceNameSetter(implicit cc: CompilerContext) extends StatefulTreeTransformer {
    override val typed = false

    override def enter(tree: Tree): Option[Tree] = {
      tree match {
        case Decl(symbol) =>
          // TODO: disambiguate parametrized types and dictidents
          symbol.sourceName = enclosingSymbols.headOption match {
            case Some(enclosingSymbol) => s"${enclosingSymbol.sourceName}.${symbol.name}"
            case None                  => symbol.name
          }
        case _ =>
      }
      None
    }

  }

  def process(
      input: (Iterable[Root], Iterable[Expr])
    )(
      implicit
      cc: CompilerContext
    ): Iterable[(Decl, Defn)] = {
    val topLevelSpecs = input._2
    Specialize(topLevelSpecs) match {
      case Some(results) =>
        // Set symbol sourceName values
        results.par foreach { case (decl, _) => (new SourceNameSetter()(cc))(decl) }
        // Yield results
        results
      case None => Nil
    }
  }

}
