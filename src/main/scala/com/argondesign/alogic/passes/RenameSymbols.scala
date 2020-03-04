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

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.SequenceNumbers

object RenameSymbols extends PairsTransformerPass {
  val name = "rename-symbols"

  override protected def process(input: List[(Decl, Defn)])(
      implicit cc: CompilerContext): List[(Decl, Defn)] = {

    // Rename symbol within entities that have the same name
    input.iterator collect {
      case (decl: DeclEntity, _) => decl
    } foreach {
      case DeclEntity(eSymbol, decls) =>
        val nameGroups = decls.groupMap(_.symbol.name)(_.symbol)

        for ((name, symbols) <- nameGroups if symbols.size > 1) {
          // Sort by location
          val sortedSymbols = symbols sortBy { _.loc.start }

          // Only add line number if there are definitions on multiple lines
          val addLineNumber = (sortedSymbols.view map { _.loc.line }).distinct.sizeIs > 1

          val newNames = for (symbol <- sortedSymbols) yield {
            symbol.kind match {
              case _: TypeIn          => name
              case _: TypeOut         => name
              case _: TypeConst       => name
              case _ if addLineNumber => s"$name${cc.sep}l${symbol.loc.line}"
              case _                  => name
            }
          }

          val seq = new SequenceNumbers

          for {
            (symbol, newName) <- sortedSymbols lazyZip newNames
            if !symbol.kind.isIn && !symbol.kind.isOut
          } {
            // Ensure uniqueness, even if defined on the same line
            symbol.name = if (newNames.count(_ == newName) > 1) {
              s"$newName${cc.sep}${seq.next}"
            } else {
              newName
            }
          }
        }

        // Add entity prefix
        val ep = cc.settings.ensurePrefix
        val prefix = (0 to ep.length) collectFirst {
          case n if eSymbol.name startsWith ep.drop(n) => ep.take(n)
        }
        eSymbol.name = prefix.get + eSymbol.name
    }

    assert {
      val topNames = input map { _._1.symbol.name }
      topNames.lengthIs == topNames.distinct.length
    }

    input
  }
}
