////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018-2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Desugar rewrites basic syntactic sugar into their equivalent form.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

final class Desugar(implicit cc: CompilerContext) extends StatelessTreeTransformer {

  override def transform(tree: Tree): Tree = tree match {
    // "a++" rewritten as  "a = a + <width of a>'d1"
    case StmtPost(lhs, op) =>
      val one = TypeAssigner(ExprInt(false, lhs.tpe.width.toInt, 1) withLoc tree.loc)
      val rhs = op match {
        case "++" => lhs + one
        case "--" => lhs - one
        case _    => unreachable
      }
      TypeAssigner(StmtAssign(lhs, rhs) withLoc tree.loc)

    // "a += b" rewritten as "a = a + b"
    case StmtUpdate(lhs, op, expr) =>
      StmtAssign(lhs, ExprBinary(lhs, op, expr)) regularize tree.loc

    // "let(<init>) <stmts>" rewritten as "<init> <stmts>"
    case StmtLet(inits, stmts) => Thicket(inits ::: stmts)

    // "new entity <name> {<body>}" rewritten as "entity <name> {<body>} <name> = new <name>();"
    case DeclSingleton(symbol, decls) =>
      val eSymbol = symbol.dup
      val declEntity = TypeAssigner(DeclEntity(eSymbol, decls) withLoc tree.loc)
      val specInstance = TypeAssigner(ExprSym(eSymbol) withLoc symbol.loc)
      val declInstance = TypeAssigner(DeclInstance(symbol, specInstance) withLoc tree.loc)
      Thicket(List(declEntity, declInstance))

    case DefnSingleton(symbol, variant, body) =>
      val eSymbol = symbol.decl match {
        case DeclInstance(`symbol`, ExprSym(s)) => s
        case _                                  => unreachable
      }
      val defnEntity = TypeAssigner(DefnEntity(eSymbol, variant, body) withLoc tree.loc)
      val defnInstance = TypeAssigner(DefnInstance(symbol) withLoc tree.loc)
      Thicket(List(defnEntity, defnInstance))

    //
    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    // Should have removed all StmtLet, StmtUpdate, StmtPost
    tree visit {
      case node: StmtLet       => cc.ice(node, s"StmtLet remains")
      case node: StmtUpdate    => cc.ice(node, s"StmtUpdate remains")
      case node: StmtPost      => cc.ice(node, s"StmtPost remains")
      case node: DeclSingleton => cc.ice(node, s"DeclSingleton remains")
      case node: DefnSingleton => cc.ice(node, s"DefnSingleton remains")
    }
  }

}

object Desugar extends PairTransformerPass {
  val name = "desugar"
  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) =
    (cc.desugar(decl), cc.desugar(defn))
}
