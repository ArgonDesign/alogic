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
// Replace references to polymorphic builtins with the resolved symbol
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.typer.TypeAssigner

final class ResolvePolyFunc(implicit cc: CompilerContext) extends StatelessTreeTransformer {

  override def transform(tree: Tree): Tree = tree match {
    case ExprCall(expr, args) if expr.tpe.isPolyFunc =>
      val symbol = expr.tpe.asPolyFunc.resolve(args).get
      val ref = TypeAssigner(ExprSym(symbol) withLoc expr.loc)
      TypeAssigner(ExprCall(ref, args) withLoc tree.loc)

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    // $COVERAGE-OFF$ Debug code
    tree visitAll {
      case node: Tree if node.tpe.isPolyFunc =>
        cc.ice(node, s"ResolvePolyFunc: node of type TypePolyFunc remains")
    }
    // $COVERAGE-ON$
  }

}

object ResolvePolyFunc extends PairTransformerPass {
  val name = "resolve-poly-func"
  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) =
    (cc.resolvePolyFunc(decl), cc.resolvePolyFunc(defn))
}
