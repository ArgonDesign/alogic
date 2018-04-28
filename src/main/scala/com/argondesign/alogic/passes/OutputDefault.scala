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
// Set default values of output ports that are not driven through a connect
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.FollowedBy

import scala.collection.mutable

final class OutputDefault(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy {

  private var inConnect = false

  private val defaultedSet = mutable.Set[TermSymbol]()

  override def enter(tree: Tree): Unit = tree match {

    case ExprRef(Sym(symbol: TermSymbol)) if !inConnect => {
      symbol.denot.kind match {
        case _: TypeOut => defaultedSet add symbol
        case _          => ()
      }
    }

    case _: Connect => inConnect = true

    case _ =>
  }

  override def transform(tree: Tree): Tree = tree match {

    //////////////////////////////////////////////////////////////////////////
    // Exit connect
    //////////////////////////////////////////////////////////////////////////

    case _: Connect => {
      tree
    } followedBy {
      inConnect = false
    }

    //////////////////////////////////////////////////////////////////////////
    // Add defaults
    //////////////////////////////////////////////////////////////////////////

    case entity: Entity => {
      val assignments = entity.declarations collect {
        case decl @ Decl(symbol, _) if defaultedSet contains symbol => {
          val kind = symbol.denot.kind
          val width = kind.width.value.get.toInt
          StmtAssign(ExprRef(Sym(symbol)), ExprInt(kind.isSigned, width, 0)) regularize decl.loc
        }
      }

      if (assignments.isEmpty) {
        tree
      } else {
        TypeAssigner {
          entity.copy(
            fenceStmts = assignments ::: entity.fenceStmts
          ) withVariant entity.variant withLoc tree.loc
        }
      }
    }

    case _ => tree
  }

}
