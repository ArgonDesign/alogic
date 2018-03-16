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
// A Tree transformer used to modify Trees
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.lib.TreeLikeTransformer

import Trees._

// Tree transformers are applied during a post-order traversal of a Tree.
abstract class TreeTransformer(implicit val cc: CompilerContext) extends TreeLikeTransformer[Tree] {

  ///////////////////////////////////////////////////////////////////////////////
  // Public API
  ///////////////////////////////////////////////////////////////////////////////

  final override def apply(tree: Tree): Tree = {
    // Transform Tree
    val result = super.apply(tree)
    // Ensure locations are present
    result visitAll {
      case node: Tree if !node.hasLoc => cc.fatal(s"Lost location for node '${node}'")
    }
    // Yield result
    result
  }

  ///////////////////////////////////////////////////////////////////////////////
  // Internals
  ///////////////////////////////////////////////////////////////////////////////

  // Walk list, flatten Thickets
  protected final override def walk(trees: List[Tree]): List[Tree] = {
    val newTrees = super.walk(trees)
    if (newTrees eq trees) {
      trees
    } else {
      newTrees flatMap {
        case Thicket(trees) => trees
        case tree: Tree     => List(tree)
      }
    }
  }

  protected final override def walk(tree: Tree): Tree = {
    enter(tree)
    tree match {
      case node: Root => {
        val typeDefinitions = walk(node.typeDefinitions)
        val entity = walk(node.entity)
        transform(TreeCopier(node)(typeDefinitions, entity))
      }
      case node: Ident => transform(node)
      case node: Sym   => transform(node)
      case node: TypeDefinitionStruct => {
        val ref = walk(node.ref)
        transform(TreeCopier(node)(ref))
      }
      case node: TypeDefinitionTypedef => {
        val ref = walk(node.ref)
        transform(TreeCopier(node)(ref))
      }
      case node: Entity => {
        val ref = walk(node.ref)
        val declarations = walk(node.declarations)
        val instances = walk(node.instances)
        val connects = walk(node.connects)
        val functions = walk(node.functions)
        val states = walk(node.states)
        val fenceStmts = walk(node.fenceStmts)
        val entities = walk(node.entities)
        transform(
          TreeCopier(node)(
            ref,
            declarations,
            instances,
            connects,
            functions,
            states,
            fenceStmts,
            entities
          ))
      }
      case node: Decl => {
        val ref = walk(node.ref)
        val init = walk(node.init)
        transform(TreeCopier(node)(ref, init))
      }
      case node: Instance => {
        val ref = walk(node.ref)
        val module = walk(node.module)
        val paramExprs = walk(node.paramExprs)
        transform(TreeCopier(node)(ref, module, paramExprs))
      }
      case node: Connect => {
        val lhs = walk(node.lhs)
        val rhs = walk(node.rhs)
        transform(TreeCopier(node)(lhs, rhs))
      }
      case node: Function => {
        val ref = walk(node.ref)
        val body = walk(node.body)
        transform(TreeCopier(node)(ref, body))
      }
      case node: State => {
        val ref = walk(node.ref)
        val body = walk(node.body)
        transform(TreeCopier(node)(ref, body))
      }
      case node: StmtBlock => {
        val body = walk(node.body)
        transform(TreeCopier(node)(body))
      }
      case node: StmtIf => {
        val cond = walk(node.cond)
        val thenStmt = walk(node.thenStmt)
        val elseStmt = walk(node.elseStmt)
        transform(TreeCopier(node)(cond, thenStmt, elseStmt))
      }
      case node: StmtCase => {
        val expr = walk(node.expr)
        val cases = walk(node.cases)
        val default = walk(node.default)
        transform(TreeCopier(node)(expr, cases, default))
      }
      case node: CaseClause => {
        val cond = walk(node.cond)
        val body = walk(node.body)
        transform(TreeCopier(node)(cond, body))
      }
      case node: StmtLoop => {
        val body = walk(node.body)
        transform(TreeCopier(node)(body))
      }
      case node: StmtWhile => {
        val cond = walk(node.cond)
        val body = walk(node.body)
        transform(TreeCopier(node)(cond, body))
      }
      case node: StmtFor => {
        val inits = walk(node.inits)
        val cond = walk(node.cond)
        val incr = walk(node.step)
        val body = walk(node.body)
        transform(TreeCopier(node)(inits, cond, incr, body))
      }
      case node: StmtDo => {
        val cond = walk(node.cond)
        val body = walk(node.body)
        transform(TreeCopier(node)(cond, body))
      }
      case node: StmtLet => {
        val inits = walk(node.inits)
        val body = walk(node.body)
        transform(TreeCopier(node)(inits, body))
      }
      case node: StmtFence => transform(node)
      case node: StmtBreak => transform(node)
      case node: StmtGoto => {
        val ref = walk(node.ref)
        transform(TreeCopier(node)(ref))
      }
      case node: StmtReturn => transform(node)
      case node: StmtAssign => {
        val lhs = walk(node.lhs)
        val rhs = walk(node.rhs)
        transform(TreeCopier(node)(lhs, rhs))
      }
      case node: StmtUpdate => {
        val lhs = walk(node.lhs)
        val rhs = walk(node.rhs)
        transform(TreeCopier(node)(lhs, rhs))
      }
      case node: StmtPost => {
        val expr = walk(node.expr)
        transform(TreeCopier(node)(expr))
      }
      case node: StmtExpr => {
        val expr = walk(node.expr)
        transform(TreeCopier(node)(expr))
      }
      case node: StmtDecl => {
        val decl = walk(node.decl)
        transform(TreeCopier(node)(decl))
      }
      case node: StmtRead          => transform(node)
      case node: StmtWrite         => transform(node)
      case node: StmtDollarComment => transform(node)
      case node: StmtError         => transform(node)
      case node: ExprCall => {
        val expr = walk(node.expr)
        val args = walk(node.args)
        transform(TreeCopier(node)(expr, args))
      }
      case node: ExprUnary => {
        val expr = walk(node.expr)
        transform(TreeCopier(node)(expr))
      }
      case node: ExprBinary => {
        val lhs = walk(node.lhs)
        val rhs = walk(node.rhs)
        transform(TreeCopier(node)(lhs, rhs))
      }
      case node: ExprTernary => {
        val cond = walk(node.cond)
        val thenExpr = walk(node.thenExpr)
        val elseExpr = walk(node.elseExpr)
        transform(TreeCopier(node)(cond, thenExpr, elseExpr))
      }
      case node: ExprRep => {
        val count = walk(node.count)
        val expr = walk(node.expr)
        transform(TreeCopier(node)(count, expr))
      }
      case node: ExprCat => {
        val parts = walk(node.parts)
        transform(TreeCopier(node)(parts))
      }
      case node: ExprIndex => {
        val expr = walk(node.expr)
        val index = walk(node.index)
        transform(TreeCopier(node)(expr, index))
      }
      case node: ExprSlice => {
        val expr = walk(node.expr)
        val lidx = walk(node.lidx)
        val ridx = walk(node.ridx)
        transform(TreeCopier(node)(expr, lidx, ridx))
      }
      case node: ExprSelect => {
        val expr = walk(node.expr)
        transform(TreeCopier(node)(expr))
      }
      case node: ExprInt => transform(node)
      case node: ExprNum => transform(node)
      case node: ExprStr => transform(node)
      case node: ExprRef => {
        val ref = walk(node.ref)
        transform(TreeCopier(node)(ref))
      }
      case node: ExprType  => transform(node)
      case node: ExprError => transform(node)
      case node: Thicket =>
        cc.ice(node, "Thicket should have been flattened in 'walk(_: List[_]): List[_]'")
    }
  }
}
