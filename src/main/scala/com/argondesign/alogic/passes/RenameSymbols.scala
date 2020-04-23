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

import scala.annotation.tailrec
import scala.collection.mutable

object RenameSymbols extends PairsTransformerPass {
  val name = "rename-symbols"

  override protected def process(
      input: List[(Decl, Defn)]
    )(
      implicit
      cc: CompilerContext
    ): List[(Decl, Defn)] = {

    val topNames = mutable.Set[String]()

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

        val nameWithPrefix = prefix.get + eSymbol.name

        // Enforce max name length if provided
        val entityNameWithLegalLen = cc.settings.outputNameMaxLength match {
          case Some(limit) if nameWithPrefix.length > limit && !eSymbol.attr.topLevel.isSet =>
            val (nameKeep, nameDrop) = nameWithPrefix.splitAt(limit - 16)

            @tailrec
            def generateUniqueShortName(salt: Int): String = {
              val shortName = {
                val salted = salt.toString + nameDrop
                val md5Buf = java.security.MessageDigest.getInstance("MD5").digest(salted.getBytes)
                val md5Str = md5Buf take 7 map { "%02x" format _ } mkString ""
                s"${nameKeep}_h$md5Str"
              }

              // Try again on hash collision
              if (topNames(shortName)) generateUniqueShortName(salt + 1) else shortName
            }

            generateUniqueShortName(0)
          case _ => nameWithPrefix
        }

        eSymbol.name = entityNameWithLegalLen

        if (topNames(eSymbol.name)) {
          cc.ice(eSymbol.loc, "entity name collision")
        }

        topNames += eSymbol.name
    }

    input
  }

}
