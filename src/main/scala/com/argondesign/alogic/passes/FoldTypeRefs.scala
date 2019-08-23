////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Dereference and remove all TypeRef instances
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.TreeInTypeTransformer
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.TreeLike
import com.argondesign.alogic.typer.TypeAssigner

final class FoldTypeRefs(implicit cc: CompilerContext) extends TreeTransformer {

  object TypeFoldTypeRefs extends TreeInTypeTransformer(this) {
    override def transform(kind: Type): Type = kind match {
      case TypeRef(Sym(symbol, idxs)) =>
        assert(idxs.isEmpty)
        walk(symbol.kind)
      case _ => super.transform(kind)
    }
  }

  override def transform(tree: Tree): Tree = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // These should have been removed by now
    ////////////////////////////////////////////////////////////////////////////

    case _: DefnRef                    => cc.ice(tree, "Don't know how")
    case _: DeclRef                    => cc.ice(tree, "Don't know how")
    case Sym(_, idxs) if idxs.nonEmpty => cc.ice(tree, "Don't know how")

    ////////////////////////////////////////////////////////////////////////////
    // Types in trees
    ////////////////////////////////////////////////////////////////////////////

    case ExprType(kind) =>
      val newKind = kind rewrite TypeFoldTypeRefs
      if (newKind ne kind) TypeAssigner(ExprType(newKind) withLoc tree.loc) else tree

    case ExprCast(kind, expr) =>
      val newKind = kind rewrite TypeFoldTypeRefs
      if (newKind ne kind) TypeAssigner(ExprCast(newKind, expr) withLoc tree.loc) else tree

    ////////////////////////////////////////////////////////////////////////////
    // Symbol types
    ////////////////////////////////////////////////////////////////////////////

    case Decl(symbol, _) =>
      symbol.kind = symbol.kind rewrite TypeFoldTypeRefs
      tree

    case Defn(symbol) =>
      symbol.kind = symbol.kind rewrite TypeFoldTypeRefs
      tree

    case Sym(symbol, Nil) =>
      // Need to re-write to allow re-typing of the node
      symbol.kind = symbol.kind rewrite TypeFoldTypeRefs
      TypeAssigner(Sym(symbol, Nil) withLoc tree.loc)

    case ExprSym(symbol) =>
      // Need to re-write to allow re-typing of the node
      symbol.kind = symbol.kind rewrite TypeFoldTypeRefs
      TypeAssigner(ExprSym(symbol) withLoc tree.loc)

    ////////////////////////////////////////////////////////////////////////////
    // Update content dependent types
    ////////////////////////////////////////////////////////////////////////////

    case entity: Entity =>
      entity.symbol.kind = entity.typeBasedOnContents
      tree

    // TODO: remove this once later stages understand how to handle Root nodes
    case Root(_, entity) => entity

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    def check(root: TreeLike): Unit = {
      root visitAll {
        case node: TypeRef   => cc.ice(s"TypeRef remains: ${node.toSource}")
        case Decl(symbol, _) => check(symbol.kind)
        case Defn(symbol)    => check(symbol.kind)
        case Sym(symbol, _)  => check(symbol.kind)
        case ExprSym(symbol) => check(symbol.kind)
      }
    }
    check(tree)
  }
}

object FoldTypeRefs extends TreeTransformerPass {
  val name = "fold-type-refs"
  def create(implicit cc: CompilerContext) = new FoldTypeRefs
}
