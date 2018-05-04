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
// Lower arrays into constituent signals
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Names.TermName
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Math
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.FollowedBy

final class LowerArrays(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy {

  // TODO: Does not work if array is assigned in a concatenation on the left.
  //       Need to teach SimplifyCat to handle more cases if this is a problem

  private[this] def intType(loc: Loc, signed: Boolean, width: Int): TypeUInt = {
    TypeUInt {
      TypeAssigner {
        ExprNum(signed, width) withLoc loc
      }
    }
  }

  override def enter(tree: Tree): Unit = tree match {

    case Decl(symbol, _) if symbol.denot.kind.isInstanceOf[TypeArray] => {
      val loc = tree.loc
      val name = symbol.name
      val TypeArray(kind, size) = symbol.denot.kind
      // Append _q to array name symbol
      symbol withDenot symbol.denot.copy(name = TermName(s"${name}_q"))
      // Create we symbol
      val weSymbol = cc.newTermSymbol(s"${name}_we", loc, intType(loc, false, 1))
      // Create waddr symbol
      val abits = Math.clog2(size.value.get) ensuring { _ > 0 }
      val waSymbol = cc.newTermSymbol(s"${name}_waddr", loc, intType(loc, false, abits))
      // Create wdata symbol
      val dbits = kind.width.value.get.toInt
      val wdSymbol = cc.newTermSymbol(s"${name}_wdata", loc, intType(loc, kind.isSigned, dbits))
      // Set attributes
      symbol.attr.arr.set((weSymbol, waSymbol, wdSymbol))
    }

    case _ =>
  }

  private def makeExprInt(symbol: TermSymbol, value: Int): ExprInt = {
    val kind = symbol.denot.kind
    val width = kind.width.value.get.toInt
    ExprInt(kind.isSigned, width, value)
  }

  override def transform(tree: Tree): Tree = tree match {

    //////////////////////////////////////////////////////////////////////////
    // Rewrite Assignments
    //////////////////////////////////////////////////////////////////////////

    case StmtAssign(ExprIndex(ExprRef(Sym(symbol)), idx), rhs) => {
      // Rewrite assignments to array elements
      symbol.attr.arr.get map {
        case (weSymbol, waSymbol, wdSymbol) => {
          val stmts = List(
            StmtAssign(ExprRef(Sym(weSymbol)), makeExprInt(weSymbol, 1)),
            StmtAssign(ExprRef(Sym(waSymbol)), idx),
            StmtAssign(ExprRef(Sym(wdSymbol)), rhs)
          )
          StmtBlock(stmts) regularize tree.loc
        }
      } getOrElse {
        tree
      }
    }

    //////////////////////////////////////////////////////////////////////////
    // Add declarations
    //////////////////////////////////////////////////////////////////////////

    case decl @ Decl(symbol, _) => {
      symbol.attr.arr.get map {
        case (weSymbol, waSymbol, wdSymbol) => {
          val decls = List(
            decl,
            Decl(weSymbol, None),
            Decl(waSymbol, None),
            Decl(wdSymbol, None)
          )
          Thicket(decls) regularize tree.loc
        }
      } getOrElse {
        tree
      }
    }

    //////////////////////////////////////////////////////////////////////////
    // Add _we/_waddr/_wdata = 'b0 fence statements
    //////////////////////////////////////////////////////////////////////////

    case entity: Entity => {
      val fenceStmts = entity.declarations collect {
        case decl @ Decl(symbol, _) if symbol.attr.arr.isSet => {
          val (weSymbol, waSymbol, wdSymbol) = symbol.attr.arr.value
          StmtBlock(
            List(
              StmtAssign(ExprRef(Sym(weSymbol)), makeExprInt(weSymbol, 0)),
              StmtAssign(ExprRef(Sym(waSymbol)), makeExprInt(waSymbol, 0)),
              StmtAssign(ExprRef(Sym(wdSymbol)), makeExprInt(wdSymbol, 0))
            )
          ) regularize decl.loc
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

object LowerArrays extends TreeTransformerPass {
  val name = "lower-arrays"
  def create(implicit cc: CompilerContext) = new LowerArrays
}
