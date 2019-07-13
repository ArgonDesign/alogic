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
import com.argondesign.alogic.util.SequenceNumbers

import scala.collection.mutable

final class RenameSymbols(implicit cc: CompilerContext) extends TreeTransformer {

  override protected def skip(tree: Tree): Boolean = tree match {
    case _: Entity      => false
    case _: EntDecl     => false
    case _: EntInstance => false
    case _              => true
  }

  private[this] val nameMap = mutable.Map[String, List[TermSymbol]]() withDefaultValue Nil

  override def enter(tree: Tree): Unit = tree match {
    case EntDecl(Decl(symbol, _)) => nameMap(symbol.name) ::= symbol

    case EntInstance(Sym(symbol: TermSymbol), _, _, _) => nameMap(symbol.name) ::= symbol

    case _ => ()
  }

  override def transform(tree: Tree): Tree = {
    tree match {
      case entity: Entity => {
        // Rename symbol with the same name
        for ((name, symbols) <- nameMap if symbols.size > 1) {
          // Sort by location, but first reverse so identical locations come
          // out in tree pre-order, which should be the same as source/gen
          // order for user defined symbols
          val sortedSymbols = symbols.reverse sortBy { _.loc.start }

          // Only add line number if there are definitions on multiple lines
          val addLineNumber = (sortedSymbols.view map { _.loc.line }).distinct.sizeIs > 1

          val newNames = for (symbol <- sortedSymbols) yield {
            symbol.kind match {
              case _: TypeIn          => name
              case _: TypeOut         => name
              case _: TypeConst       => name
              case _ if addLineNumber => s"${name}${cc.sep}l${symbol.loc.line}"
              case _                  => name
            }
          }

          lazy val seq = new SequenceNumbers

          for ((symbol, newName) <- sortedSymbols lazyZip newNames) {
            // Ensure uniqueness, even if defined on the same line
            val finalName = if (newNames.count(_ == newName) > 1) {
              s"${newName}${cc.sep}${seq.next}"
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
