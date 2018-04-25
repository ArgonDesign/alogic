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
// Lower flops (anything that is TypeInt at this stage)
// to constituent _q and _d signals
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Names.TermName
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.FollowedBy

final class LowerFlops(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy {

  // TODO: Generate clock enables

  private var inConnect = false

  override def enter(tree: Tree): Unit = tree match {

    case Decl(Sym(symbol: TermSymbol), kind: TypeInt, _) => {
      val loc = tree.loc
      val name = symbol.name
      // Append _q to the name of the symbol
      symbol withDenot symbol.denot.copy(name = TermName(s"${name}_q"))
      // Create the _d symbol
      val dSymbol = cc.newTermSymbol(s"${name}_d", loc, kind)
      // Set attributes
      symbol.attr.flop set dSymbol
    }

    case _: Connect => inConnect = true

    case _ =>
  }

  override def transform(tree: Tree): Tree = tree match {

    //////////////////////////////////////////////////////////////////////////
    // Rewrite references
    //////////////////////////////////////////////////////////////////////////

    case ExprRef(Sym(qSymbol)) if !inConnect => {
      // Rewrite references to flops as references to the _d,
      // but not in connect expressions, connects always use the _q
      qSymbol.attr.flop.get map { dSymbol =>
        ExprRef(Sym(dSymbol)) regularize tree.loc
      } getOrElse {
        tree
      }
    }

    //////////////////////////////////////////////////////////////////////////
    // Add declarations
    //////////////////////////////////////////////////////////////////////////

    case decl @ Decl(Sym(qSymbol: TermSymbol), _, _) => {
      qSymbol.attr.flop.get map { dSymbol =>
        val decls = List(
          decl,
          Decl(Sym(dSymbol), dSymbol.denot.kind, None)
        )
        Thicket(decls) regularize tree.loc
      } getOrElse {
        tree
      }
    }

    //////////////////////////////////////////////////////////////////////////
    // Exit connect
    //////////////////////////////////////////////////////////////////////////

    case _: Connect => {
      tree
    } followedBy {
      inConnect = false
    }

    //////////////////////////////////////////////////////////////////////////
    // Add _d = _q fence statements
    //////////////////////////////////////////////////////////////////////////

    case entity: Entity => {
      val fenceStmts = entity.declarations collect {
        case decl @ Decl(Sym(qSymbol), _, _) if qSymbol.attr.flop.isSet => {
          val dSymbol = qSymbol.attr.flop.value
          StmtAssign(ExprRef(Sym(dSymbol)), ExprRef(Sym(qSymbol))) regularize decl.loc
        }
      }

      TypeAssigner {
        entity.copy(
          fenceStmts = fenceStmts ::: entity.fenceStmts
        ) withVariant entity.variant withLoc tree.loc
      }
    }

    case _ => tree
  }

}
