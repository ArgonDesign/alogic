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
// Rewrite StmtDecl with initializer as decl without initializer, followed
// by assignment of initial value
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext

final class ConvertDeclInit(implicit cc: CompilerContext) extends TreeTransformer {

  override def transform(tree: Tree): Tree = tree match {

    case StmtDecl(Decl(Sym(symbol), kind, Some(init))) => {
      StmtBlock(
        List(
          StmtDecl(Decl(Sym(symbol), kind, None)),
          StmtAssign(ExprRef(Sym(symbol)), init)
        )
      ) regularize tree.loc
    }

    case _ => tree
  }

}
