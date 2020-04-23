////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Lower Stall statements and clearOnStall
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.enums.EntityVariant
import com.argondesign.alogic.typer.TypeAssigner

import scala.collection.mutable

final class LowerStalls(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  private var goSymbol: Option[Symbol] = None

  private val clearOnStall = mutable.ListBuffer[Symbol]()

  override def enter(tree: Tree): Option[Tree] = {
    tree match {
      case decl: DeclEntity =>
        goSymbol = decl.go
      case Decl(symbol) if symbol.attr.clearOnStall.isSet =>
        clearOnStall += symbol
      case _ =>
    }
    if (goSymbol.isDefined) {
      None // Proceed
    } else {
      Some(tree) // Skip all
    }
  }

  override def transform(tree: Tree): Tree = tree match {
    //////////////////////////////////////////////////////////////////////////
    // Rewrite StmtStall into clearing go
    //////////////////////////////////////////////////////////////////////////

    case StmtStall(cond) =>
      StmtIf(
        !cond,
        List(StmtAssign(ExprSym(goSymbol.get), ExprInt(false, 1, 0))),
        Nil
      ) regularize tree.loc

    //////////////////////////////////////////////////////////////////////////
    // Add clears
    //////////////////////////////////////////////////////////////////////////

    case EntCombProcess(body) if clearOnStall.nonEmpty =>
      val fini = List(
        StmtComment("Clears"),
        StmtIf(
          !ExprSym(goSymbol.get),
          List from {
            clearOnStall.iterator map { symbol =>
              StmtAssign(ExprSym(symbol), ExprInt(symbol.kind.isSigned, symbol.kind.width.toInt, 0))
            }
          } tap { _ =>
            clearOnStall foreach { _.attr.clearOnStall.clear() }
            clearOnStall.clear()
          },
          Nil
        )
      )
      fini foreach { _ regularize tree.loc }
      TypeAssigner(EntCombProcess(body ::: fini) withLoc tree.loc)

    //
    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = tree visit {
    case stmt: StmtStall => cc.ice(stmt, "StmtStall remains")
  }

}

object LowerStalls extends EntityTransformerPass(declFirst = true) {
  val name = "lower-stalls"

  override def skip(decl: Decl, defn: Defn)(implicit cc: CompilerContext): Boolean =
    super.skip(decl, defn) || defn.asInstanceOf[DefnEntity].variant != EntityVariant.Fsm

  def create(symbol: Symbol)(implicit cc: CompilerContext) = new LowerStalls
}
