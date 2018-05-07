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
// Mangle symbol names that have multiple declarations.
// Do not touch ports or consts.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.core.Types._

import scala.collection.mutable

final class RenameClashingTerms(implicit cc: CompilerContext) extends TreeTransformer {

  private[this] val nameMap = mutable.Map[String, List[TermSymbol]]() withDefaultValue Nil

  override def enter(tree: Tree): Unit = tree match {
    case Decl(symbol, _) => nameMap(symbol.name) ::= symbol

    case _ => ()
  }

  override def transform(tree: Tree): Tree = {
    tree match {
      case _: Entity => {
        // Process symbols that map to the same name
        for ((name, symbols) <- nameMap if symbols.size > 1) {
          val sortedSymbols = symbols sortBy { _.loc.start }
          val newNames = for (symbol <- sortedSymbols) yield {
            symbol.kind match {
              case _: TypeIn    => name
              case _: TypeOut   => name
              case _: TypeConst => name
              case _            => name + cc.sep + s"l${symbol.loc.line}"
            }
          }

          lazy val seq = Stream.from(0).toIterator

          for ((symbol, newName) <- sortedSymbols zip newNames) {
            // Ensure uniqueness, even if defined on the same line
            val finalName = if (newNames.count(_ == newName) > 1) {
              s"${newName}_${seq.next()}"
            } else {
              newName
            }

            symbol rename finalName
          }
        }
      }

      case _ => ()
    }

    tree
  }

}

object RenameClashingTerms extends TreeTransformerPass {
  val name = "rename-clashing-terms"
  def create(implicit cc: CompilerContext) = new RenameClashingTerms
}
