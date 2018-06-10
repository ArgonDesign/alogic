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
// Make symbol names unique, and rename them if necessary.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.core.Types._

import scala.collection.mutable

final class RenameSymbols(implicit cc: CompilerContext) extends TreeTransformer {

  override protected def skip(tree: Tree): Boolean = tree match {
    case _: EntityLowered => false
    case _: Decl          => false
    case _                => true
  }

  private[this] val nameMap = mutable.Map[String, List[TermSymbol]]() withDefaultValue Nil

  override def enter(tree: Tree): Unit = tree match {
    case Decl(symbol, _) => nameMap(symbol.name) ::= symbol

    case _ => ()
  }

  override def transform(tree: Tree): Tree = {
    tree match {
      case entity: EntityLowered => {
        // Rename symbol with the same name
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

        // Add entity prefix
        val ep = cc.settings.ensurePrefix
        val prefix = (0 to ep.length) collectFirst {
          case n if entitySymbol.name startsWith ep.drop(n) => ep.take(n)
        }
        entitySymbol rename (prefix.get + entitySymbol.name)
      }

      case _ => ()
    }

    tree
  }

}

object RenameSymbols extends TreeTransformerPass {
  val name = "rename-symbols"
  def create(implicit cc: CompilerContext) = new RenameSymbols
}
