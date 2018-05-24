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
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Math
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.FollowedBy

final class LowerArrays(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy {

  override def skip(tree: Tree): Boolean = tree match {
    case entity: Entity => entity.variant == "network"
    case _              => false
  }

  private[this] def intType(loc: Loc, signed: Boolean, width: Int): TypeUInt = {
    TypeUInt {
      TypeAssigner {
        ExprNum(signed, width) withLoc loc
      }
    }
  }

  override def enter(tree: Tree): Unit = tree match {

    case Decl(symbol, _) if symbol.kind.isInstanceOf[TypeArray] => {
      val loc = tree.loc
      val name = symbol.name
      val TypeArray(kind, size) = symbol.kind
      // Append _q to array name symbol
      symbol rename s"${name}_q"
      // Create we symbol
      val weSymbol = cc.newTermSymbol(s"${name}_we", loc, intType(loc, false, 1))
      // Create waddr symbol
      val abits = Math.clog2(size.value.get) ensuring { _ > 0 }
      val waSymbol = cc.newTermSymbol(s"${name}_waddr", loc, intType(loc, false, abits))
      // Create wdata symbol
      val dbits = kind.width.value.get.toInt
      val wdSymbol = cc.newTermSymbol(s"${name}_wdata", loc, intType(loc, kind.isSigned, dbits))
      // Set attributes
      symbol.attr.memory.set((weSymbol, waSymbol, wdSymbol))
    }

    case _ =>
  }

  private def makeExprInt(symbol: TermSymbol, value: Int): ExprInt = {
    val kind = symbol.kind
    val width = kind.width.value.get.toInt
    ExprInt(kind.isSigned, width, value)
  }

  override def transform(tree: Tree): Tree = tree match {

    //////////////////////////////////////////////////////////////////////////
    // Rewrite .write calls
    //////////////////////////////////////////////////////////////////////////

    case StmtExpr(ExprCall(ExprSelect(ExprRef(symbol: TermSymbol), "write"), List(addr, data))) => {
      // Rewrite assignments to array elements
      symbol.attr.memory.get map {
        case (weSymbol, waSymbol, wdSymbol) => {
          val stmts = List(
            StmtAssign(ExprRef(weSymbol), makeExprInt(weSymbol, 1)),
            StmtAssign(ExprRef(waSymbol), addr),
            StmtAssign(ExprRef(wdSymbol), data)
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
      symbol.attr.memory.get map {
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
        case decl @ Decl(symbol, _) if symbol.attr.memory.isSet => {
          val (weSymbol, waSymbol, wdSymbol) = symbol.attr.memory.value
          StmtBlock(
            List(
              StmtAssign(ExprRef(weSymbol), makeExprInt(weSymbol, 0)),
              StmtAssign(ExprRef(waSymbol), makeExprInt(waSymbol, 0)),
              StmtAssign(ExprRef(wdSymbol), makeExprInt(wdSymbol, 0))
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
