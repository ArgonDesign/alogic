////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Lower Stall statements and clearOnStall
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.enums.EntityVariant

import scala.collection.mutable

final class LowerWait extends StatelessTreeTransformer {

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
    // Rewrite StmtWait into clearing go
    //////////////////////////////////////////////////////////////////////////

    case StmtWait(cond) =>
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
    case stmt: StmtWait => throw Ice(stmt, "StmtWait remains")
  }

}

object LowerWait extends EntityTransformerPass(declFirst = true, parallel = true) {
  val name = "lower-wait"

  override def skip(decl: DeclEntity, defn: DefnEntity): Boolean = defn.variant != EntityVariant.Fsm

  def create(symbol: Symbol)(implicit cc: CompilerContext) = new LowerWait
}
