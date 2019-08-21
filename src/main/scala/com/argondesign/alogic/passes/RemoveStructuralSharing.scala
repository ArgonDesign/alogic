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
// Any pass relying on Tree.id may fail with structural sharing. This pass
// copies any shared nodes.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext

import scala.collection.mutable

final class RemoveStructuralSharing(implicit cc: CompilerContext) extends TreeTransformer {

  // Set of stack symbols to replace
  private val visited = mutable.Set[Int]()

  override def transform(tree: Tree): Tree = {
    if (!(visited contains tree.id)) {
      visited add tree.id
      tree
    } else {
      val duplicate = tree match {
        case node: Thicket        => node.copy()
        case node: Root           => node.copy()
        case node: Ident          => node.copy()
        case node: Sym            => node.copy()
        case node: DefIdent       => node.copy()
        case node: Def            => node.copy()
        case node: DeclIdent      => node.copy()
        case node: Decl           => node.copy()
        case node: Entity         => node.copy()
        case node: EntDecl        => node.copy()
        case node: EntEntity      => node.copy()
        case node: EntInstance    => node.copy()
        case node: EntConnect     => node.copy()
        case node: EntCombProcess => node.copy()
        case node: EntFunction    => node.copy()
        case node: EntState       => node.copy()
        case node: EntVerbatim    => node.copy()
        case node: EntGen         => node.copy()
        case node: GenDecl        => node.copy()
        case node: GenIf          => node.copy()
        case node: GenFor         => node.copy()
        case node: GenRange       => node.copy()
        case node: StmtBlock      => node.copy()
        case node: StmtIf         => node.copy()
        case node: StmtCase       => node.copy()
        case node: CaseRegular    => node.copy()
        case node: CaseDefault    => node.copy()
        case node: CaseGen        => node.copy()
        case node: StmtLoop       => node.copy()
        case node: StmtWhile      => node.copy()
        case node: StmtFor        => node.copy()
        case node: StmtDo         => node.copy()
        case node: StmtLet        => node.copy()
        case _: StmtFence         => StmtFence()
        case _: StmtBreak         => StmtBreak()
        case _: StmtContinue      => StmtContinue()
        case node: StmtGoto       => node.copy()
        case _: StmtReturn        => StmtReturn()
        case node: StmtAssign     => node.copy()
        case node: StmtUpdate     => node.copy()
        case node: StmtPost       => node.copy()
        case node: StmtExpr       => node.copy()
        case node: StmtDecl       => node.copy()
        case _: StmtRead          => StmtRead()
        case _: StmtWrite         => StmtWrite()
        case node: StmtStall      => node.copy()
        case node: StmtComment    => node.copy()
        case node: StmtGen        => node.copy()
        case _: StmtError         => StmtError()
        case node: ExprCall       => node.copy()
        case node: ExprUnary      => node.copy()
        case node: ExprBinary     => node.copy()
        case node: ExprTernary    => node.copy()
        case node: ExprRep        => node.copy()
        case node: ExprCat        => node.copy()
        case node: ExprIndex      => node.copy()
        case node: ExprSlice      => node.copy()
        case node: ExprSelect     => node.copy()
        case node: ExprIdent      => node.copy()
        case node: ExprRef        => node.copy()
        case node: ExprType       => node.copy()
        case node: ExprCast       => node.copy()
        case node: ExprInt        => node.copy()
        case node: ExprNum        => node.copy()
        case node: ExprStr        => node.copy()
        case _: ExprError         => ExprError()
      }
      duplicate withLoc tree.loc withTpe tree.tpe
    }
  }
}

object RemoveStructuralSharing extends TreeTransformerPass {
  val name = "remove-structural-sharing"
  def create(implicit cc: CompilerContext) = new RemoveStructuralSharing
}
