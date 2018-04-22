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
// Split structures to constituent signals
//   - Update instance ports selects
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.core.Types._

final class SplitStructsB(implicit cc: CompilerContext) extends TreeTransformer {

  override def enter(tree: Tree): Unit = tree match {
    case entity: Entity => {
      // Update instance types to reflect added ports
      for (Instance(Sym(iSymbol: TermSymbol), Sym(eSymbol), _, _) <- entity.instances) {
        iSymbol withDenot iSymbol.denot.copy(kind = eSymbol.denot.kind)
      }
    }
    case _ => ()
  }

  override def transform(tree: Tree): Tree = tree match {

    case ExprSelect(ExprRef(Sym(isymbol)), sel) => {
      isymbol.denot.kind match {
        case kind: TypeEntity => {
          // Rewrite selects of form instance.port
          val pSymbol = kind.portSymbol(sel).get
          pSymbol.attr.fieldSymbols.get map { fSymbols =>
            val it = fSymbols.toIterator
            def cat(struct: TypeStruct): ExprCat = ExprCat {
              for (fType <- struct.fieldTypes) yield {
                fType match {
                  case struct: TypeStruct => cat(struct)
                  case _                  => ExprSelect(ExprRef(Sym(isymbol)), it.next().name)
                }
              }
            }
            cat(pSymbol.denot.kind.chase.underlying.asInstanceOf[TypeStruct]) regularize tree.loc
          } getOrElse {
            tree
          }
        }
        case _ => tree
      }
    }

    case _ => tree
  }

}
