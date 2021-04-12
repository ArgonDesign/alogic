////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Rewrite basic syntactic sugar into their equivalent normal form.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Types.TypeEntity
import com.argondesign.alogic.util.unreachable

object DesugarTransform extends StatelessTreeTransformer {

  override def transform(tree: Tree): Tree = tree match {
    // "a++" rewritten as  "a = a + <width of a>'(s?)d1"
    case StmtPost(lhs, op) =>
      val rhs = op match {
        case "++" => lhs.inc
        case "--" => lhs.dec
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

    // Convert EntConnects to EntAssigns (note this swap the order of LHS/RHS).
    // Make cardinal port references explicit.
    case EntConnect(lhs, rhss) =>
      val newRhs = lhs.tpe match {
        case _: TypeEntity => TypeAssigner(ExprSel(lhs, "out") withLoc lhs.loc)
        case _             => lhs
      }
      Thicket(
        rhss map { rhs =>
          val newLhs = rhs.tpe match {
            case _: TypeEntity => TypeAssigner(ExprSel(rhs, "in") withLoc rhs.loc)
            case _             => rhs
          }
          val loc = tree.loc.copy(end = rhs.loc.end, point = rhs.loc.start)
          TypeAssigner(EntAssign(newLhs, newRhs) withLoc loc)
        }
      )

    //
    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = tree visit {
    case node: StmtLet       => throw Ice(node, s"StmtLet remains")
    case node: StmtUpdate    => throw Ice(node, s"StmtUpdate remains")
    case node: StmtPost      => throw Ice(node, s"StmtPost remains")
    case node: DeclSingleton => throw Ice(node, s"DeclSingleton remains")
    case node: DefnSingleton => throw Ice(node, s"DefnSingleton remains")
    case node: EntConnect    => throw Ice(node, "EntConnect with remains")
  }

}

object Desugar extends PairTransformerPass {
  val name = "desugar"
  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) =
    (DesugarTransform(decl), DesugarTransform(defn))
}
