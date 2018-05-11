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
import com.argondesign.alogic.core.Types._

final class SplitStructsB(implicit cc: CompilerContext) extends TreeTransformer {

  override def transform(tree: Tree): Tree = tree match {

    case ExprSelect(ExprRef(iSymbol), sel) => {
      // Rewrite selects of form instance.port
      val kind = iSymbol.kind.asInstanceOf[TypeInstance]
      val pSymbol = kind.portSymbol(sel).get
      pSymbol.attr.fieldSymbols.get map { fSymbols =>
        val it = fSymbols.toIterator
        def cat(struct: TypeStruct): ExprCat = ExprCat {
          for (fType <- struct.fieldTypes) yield {
            fType match {
              case struct: TypeStruct => cat(struct)
              case _                  => ExprSelect(ExprRef(iSymbol), it.next().name)
            }
          }
        }
        cat(pSymbol.kind.underlying.asInstanceOf[TypeStruct]) regularize tree.loc
      } getOrElse {
        tree
      }
    }

    case _ => tree
  }

}

object SplitStructsB extends TreeTransformerPass {
  val name = "split-structs-b"
  def create(implicit cc: CompilerContext) = new SplitStructsB
}
