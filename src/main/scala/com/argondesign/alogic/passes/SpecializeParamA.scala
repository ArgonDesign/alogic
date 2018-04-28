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
// For each param and const symbol, gather all expressions they can evaluate
// to and attach them as attributes to the symbols. Furthermore, attach all
// combinations of parameter instantiations to the entity symbol that defines
// the parameters. This is used in a second pass to specialize values and clone
// modules with multiple parameter instantiations. Note that multiple threads
// might be working on separate entities and they are updating the attributes
// in external symbols, so we need to synchronize on them.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types._

final class SpecializeParamA(implicit cc: CompilerContext) extends TreeTransformer {

  override def enter(tree: Tree): Unit = tree match {

    case Instance(Sym(iSymbol), Sym(eSymbol), paramNames, paramExprs) => {
      val entityKind = eSymbol.denot.kind.asInstanceOf[TypeEntity]

      if (entityKind.paramSymbols.nonEmpty) {
        // Gather particular parameter bindings
        val paramMap = (paramNames zip paramExprs).toMap

        val paramBindings = {
          val pairs = for {
            symbol <- entityKind.paramSymbols
            expr <- paramMap.get(symbol.name)
          } yield {
            symbol -> expr
          }
          pairs.toMap
        }

        // Append attribute to entity so we can specialize it
        eSymbol synchronized {
          eSymbol.attr.paramBindings append paramBindings
        }

        // Append attribute to instance so we can replace the entity with the specialized version
        iSymbol.attr.paramBinding set paramBindings
      }
    }

    case _ =>
  }

}
