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
// Remove StmtDecl instances:
//  - Lift StmtDecl to entity Decls
//  - Insert local initializer
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.typer.TypeAssigner

import scala.collection.mutable.ListBuffer

final class ConvertLocalDecls(implicit cc: CompilerContext) extends TreeTransformer {

  private[this] val localDecls = ListBuffer[Decl]()

  override def transform(tree: Tree): Tree = tree match {

    case StmtDecl(decl @ Decl(Sym(symbol), kind, initOpt)) => {
      localDecls.append(decl.copy(init = None) regularize decl.loc)
      val stmt = initOpt match {
        case None       => StmtBlock(Nil)
        case Some(init) => StmtAssign(ExprRef(Sym(symbol)), init)

      }
      stmt regularize tree.loc
    }

    case entity: Entity if localDecls.nonEmpty => {
      localDecls.prependAll(entity.declarations)
      TypeAssigner {
        entity.copy(declarations = localDecls.toList) withLoc tree.loc withVariant entity.variant
      }
    }

    case _ => tree
  }

  override protected def finalCheck(tree: Tree): Unit = {
    tree visit {
      case node: StmtDecl => cc.ice(node, "Local declaration remains")
    }
  }

}
