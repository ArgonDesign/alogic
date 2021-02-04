////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Create temporary symbols for intermediate results
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.TypeAssigner

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class CreateTemporaries extends StatelessTreeTransformer {

  // Temporary symbols
  private val tmpSymbols = mutable.ListBuffer[Symbol]()
  // List of statements to emit before a statement
  private val extraStmts = mutable.Stack[mutable.ListBuffer[Stmt]]()

  override def enter(tree: Tree): Option[Tree] = {
    tree match {
      case _: Stmt => extraStmts.push(new ListBuffer)
      case _       =>
    }
    None
  }

  private def mkTmp(expr: Expr): Symbol = {
    require(expr.tpe.underlying.isFund && expr.tpe.isPacked && expr.tpe.width > 0)
    Symbol.temp("_tmp", expr.loc, expr.tpe.underlying) tap { tmpSymbol =>
      tmpSymbol.attr.combSignal set true
      tmpSymbols.append(tmpSymbol)
      extraStmts.top append {
        StmtAssign(ExprSym(tmpSymbol), expr) regularize expr.loc
      }
    }
  }

  override def transform(tree: Tree): Tree = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Add decls/defns to Entities
    ////////////////////////////////////////////////////////////////////////////

    case decl: DeclEntity if tmpSymbols.nonEmpty =>
      val extraDecls = tmpSymbols.iterator map { s => s.mkDecl regularize s.loc }
      TypeAssigner(decl.copy(decls = decl.decls appendedAll extraDecls) withLoc tree.loc)

    case defn: DefnEntity if tmpSymbols.nonEmpty =>
      val extraBody = tmpSymbols.iterator map { s => EntSplice(s.mkDefn) regularize s.loc }
      TypeAssigner(defn.copy(body = defn.body appendedAll extraBody) withLoc defn.loc)

    ////////////////////////////////////////////////////////////////////////////
    // Add necessary temporaries
    ////////////////////////////////////////////////////////////////////////////

    case expr @ ExprIndex(tgt, _) if tgt.tpe.isPacked =>
      tgt match {
        case _: ExprSym => tree
        case tgt =>
          val newTgt = TypeAssigner(ExprSym(mkTmp(tgt)) withLoc tgt.loc)
          TypeAssigner(expr.copy(expr = newTgt) withLoc tree.loc)
      }

    case expr @ ExprSlice(tgt, _, _, _) if tgt.tpe.isPacked =>
      tgt match {
        case _: ExprSym => tree
        case tgt =>
          val newTgt = TypeAssigner(ExprSym(mkTmp(tgt)) withLoc tgt.loc)
          TypeAssigner(expr.copy(expr = newTgt) withLoc tree.loc)
      }

    ////////////////////////////////////////////////////////////////////////////
    // Emit temporary assignments
    ////////////////////////////////////////////////////////////////////////////

    case stmt: Stmt =>
      val stmts = extraStmts.pop()
      if (stmts.nonEmpty) Thicket((stmts append stmt).toList) else tree

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(extraStmts.isEmpty)
  }

}

object CreateTemporaries extends EntityTransformerPass(declFirst = false, parallel = true) {
  val name = "create-temporaries"

  def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer = new CreateTemporaries
}
