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

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.TreeLikeTransformer
import com.argondesign.alogic.typer.TypeAssigner

import scala.collection.mutable
import scala.util.ChainingSyntax

// Tree transformers are applied during a post-order traversal of a Tree.
abstract class TreeTransformer(implicit val cc: CompilerContext)
    extends TreeLikeTransformer[Tree]
    with ChainingSyntax {

  val typed: Boolean = true

  ///////////////////////////////////////////////////////////////////////////////
  // Protected API
  ///////////////////////////////////////////////////////////////////////////////

  // The TypeSymbol representing the currently processed entity
  protected[this] def entitySymbol: TypeSymbol = entityStack.top.symbol

  ///////////////////////////////////////////////////////////////////////////////
  // Internals
  ///////////////////////////////////////////////////////////////////////////////

  private[this] val entityStack = mutable.Stack[Entity]()

  // Walk list, flatten Thickets
  final override def walk(trees: List[Tree]): List[Tree] = {
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

  final def doTransform(tree: Tree): Tree = {
    // Nodes with children that have been rewritten and henceforth
    // been copied by TreeCopier need their types assigned
    if (typed && !tree.hasTpe) {
      TypeAssigner(tree)
    }

    // Transform the node
    val result = transform(tree)

    if (!result.hasLoc) {
      cc.ice(s"Lost location of transformed node:", result.toString, this.getClass.getName)
    }

    // Check it has type
    if (typed && !result.hasTpe) {
      cc.ice(result, "Lost type of transformed node", result.toString, this.getClass.getName)
    }

    result
  }

  final override def walk(tree: Tree): Tree = {
    tree match {
      case entity: Entity => entityStack.push(entity)
      case _              => ()
    }
    // Check skip in pre order
    if (skip(tree)) {
      tree match {
        case entity: Entity => entityStack.pop()
        case _              => ()
      }
      tree
    } else {
      // Call enter in pre order
      enter(tree)
      // Call transform in post order
      tree match {
        case node: Root => {
          val typeDefinitions = walk(node.typeDefinitions)
          val entity = walk(node.entity)
          doTransform(TreeCopier(node)(typeDefinitions, entity))
        }
        case node: Ident => doTransform(node)
        case node: Sym   => doTransform(node)
        case node: TypeDefinitionStruct => {
          val ref = walk(node.ref)
          doTransform(TreeCopier(node)(ref))
        }
        case node: TypeDefinitionTypedef => {
          val ref = walk(node.ref)
          doTransform(TreeCopier(node)(ref))
        }
        case node: DeclIdent => {
          val ident = walk(node.ident)
          val init = walk(node.init)
          doTransform(TreeCopier(node)(ident, init))
        }
        case node: Decl => {
          val init = walk(node.init)
          doTransform(TreeCopier(node)(init))
        }
        case node: Entity => {
          val ref = walk(node.ref)
          val body = walk(node.body)
          doTransform(TreeCopier(node)(ref, body))
        } tap { _ =>
          entityStack.pop()
        }
        case node: EntDecl => {
          val decl = walk(node.decl)
          doTransform(TreeCopier(node)(decl))
        }
        case node: EntEntity => {
          val entity = walk(node.entity)
          doTransform(TreeCopier(node)(entity))
        }
        case node: EntInstance => {
          val instance = walk(node.instance)
          val entity = walk(node.entity)
          val paramExprs = walk(node.paramExprs)
          doTransform(TreeCopier(node)(instance, entity, paramExprs))
        }
        case node: EntConnect => {
          val lhs = walk(node.lhs)
          val rhs = walk(node.rhs)
          doTransform(TreeCopier(node)(lhs, rhs))
        }
        case node: EntFunction => {
          val ref = walk(node.ref)
          val stmts = walk(node.stmts)
          doTransform(TreeCopier(node)(ref, stmts))
        }
        case node: EntState => {
          val expr = walk(node.expr)
          val stmts = walk(node.stmts)
          doTransform(TreeCopier(node)(expr, stmts))
        }
        case node: EntCombProcess => {
          val stmts = walk(node.stmts)
          doTransform(TreeCopier(node)(stmts))
        }
        case node: EntVerbatim => doTransform(node)
        case node: GenIf => {
          val cond = walk(node.cond)
          val thenItems = walk(node.thenItems)
          val elseItems = walk(node.elseItems)
          doTransform(TreeCopier(node)(cond, thenItems, elseItems))
        }
        case node: GenFor => {
          val inits = walk(node.inits)
          val cond = walk(node.cond)
          val step = walk(node.step)
          val body = walk(node.body)
          doTransform(TreeCopier(node)(inits, cond, step, body))
        }
        case node: GenRange => {
          val decl = walk(node.decl)
          val end = walk(node.end)
          val body = walk(node.body)
          doTransform(TreeCopier(node)(decl, end, body))
        }
        case node: StmtBlock => {
          val body = walk(node.body)
          doTransform(TreeCopier(node)(body))
        }
        case node: StmtIf => {
          val cond = walk(node.cond)
          val thenStmts = walk(node.thenStmts)
          val elseStmts = walk(node.elseStmts)
          doTransform(TreeCopier(node)(cond, thenStmts, elseStmts))
        }
        case node: StmtCase => {
          val expr = walk(node.expr)
          val cases = walk(node.cases)
          doTransform(TreeCopier(node)(expr, cases))
        }
        case node: CaseRegular => {
          val cond = walk(node.cond)
          val stmts = walk(node.stmts)
          doTransform(TreeCopier(node)(cond, stmts))
        }
        case node: CaseDefault => {
          val stmts = walk(node.stmts)
          doTransform(TreeCopier(node)(stmts))
        }
        case node: CaseGen => {
          val gen = walk(node.gen)
          doTransform(TreeCopier(node)(gen))
        }
        case node: StmtLoop => {
          val body = walk(node.body)
          doTransform(TreeCopier(node)(body))
        }
        case node: StmtWhile => {
          val cond = walk(node.cond)
          val body = walk(node.body)
          doTransform(TreeCopier(node)(cond, body))
        }
        case node: StmtFor => {
          val inits = walk(node.inits)
          val cond = walk(node.cond)
          val incr = walk(node.step)
          val body = walk(node.body)
          doTransform(TreeCopier(node)(inits, cond, incr, body))
        }
        case node: StmtDo => {
          val cond = walk(node.cond)
          val body = walk(node.body)
          doTransform(TreeCopier(node)(cond, body))
        }
        case node: StmtLet => {
          val inits = walk(node.inits)
          val body = walk(node.body)
          doTransform(TreeCopier(node)(inits, body))
        }
        case node: StmtFence    => doTransform(node)
        case node: StmtBreak    => doTransform(node)
        case node: StmtContinue => doTransform(node)
        case node: StmtGoto => {
          val expr = walk(node.expr)
          doTransform(TreeCopier(node)(expr))
        }
        case node: StmtReturn => doTransform(node)
        case node: StmtAssign => {
          val lhs = walk(node.lhs)
          val rhs = walk(node.rhs)
          doTransform(TreeCopier(node)(lhs, rhs))
        }
        case node: StmtUpdate => {
          val lhs = walk(node.lhs)
          val rhs = walk(node.rhs)
          doTransform(TreeCopier(node)(lhs, rhs))
        }
        case node: StmtPost => {
          val expr = walk(node.expr)
          doTransform(TreeCopier(node)(expr))
        }
        case node: StmtExpr => {
          val expr = walk(node.expr)
          doTransform(TreeCopier(node)(expr))
        }
        case node: StmtDecl => {
          val decl = walk(node.decl)
          doTransform(TreeCopier(node)(decl))
        }
        case node: StmtRead    => doTransform(node)
        case node: StmtWrite   => doTransform(node)
        case node: StmtComment => doTransform(node)
        case node: StmtStall => {
          val cond = walk(node.cond)
          doTransform(TreeCopier(node)(cond))
        }
        case node: StmtGen => {
          val gen = walk(node.gen)
          doTransform(TreeCopier(node)(gen))
        }
        case node: StmtError => doTransform(node)
        case node: ExprCall => {
          val expr = walk(node.expr)
          val args = walk(node.args)
          doTransform(TreeCopier(node)(expr, args))
        }
        case node: ExprUnary => {
          val expr = walk(node.expr)
          doTransform(TreeCopier(node)(expr))
        }
        case node: ExprBinary => {
          val lhs = walk(node.lhs)
          val rhs = walk(node.rhs)
          doTransform(TreeCopier(node)(lhs, rhs))
        }
        case node: ExprTernary => {
          val cond = walk(node.cond)
          val thenExpr = walk(node.thenExpr)
          val elseExpr = walk(node.elseExpr)
          doTransform(TreeCopier(node)(cond, thenExpr, elseExpr))
        }
        case node: ExprRep => {
          val count = walk(node.count)
          val expr = walk(node.expr)
          doTransform(TreeCopier(node)(count, expr))
        }
        case node: ExprCat => {
          val parts = walk(node.parts)
          doTransform(TreeCopier(node)(parts))
        }
        case node: ExprIndex => {
          val expr = walk(node.expr)
          val index = walk(node.index)
          doTransform(TreeCopier(node)(expr, index))
        }
        case node: ExprSlice => {
          val expr = walk(node.expr)
          val lidx = walk(node.lidx)
          val ridx = walk(node.ridx)
          doTransform(TreeCopier(node)(expr, lidx, ridx))
        }
        case node: ExprSelect => {
          val expr = walk(node.expr)
          doTransform(TreeCopier(node)(expr))
        }
        case node: ExprCast => {
          val expr = walk(node.expr)
          doTransform(TreeCopier(node)(expr))
        }
        case node: ExprInt   => doTransform(node)
        case node: ExprNum   => doTransform(node)
        case node: ExprStr   => doTransform(node)
        case node: ExprIdent => doTransform(node)
        case node: ExprRef   => doTransform(node)
        case node: ExprType  => doTransform(node)
        case node: ExprError => doTransform(node)
        case node: Thicket => {
          val trees = walk(node.trees)
          doTransform(TreeCopier(node)(trees))
        }
      }
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Default checks to run after each pass
  //////////////////////////////////////////////////////////////////////////////

  val checkRefs = true

  override def defaultCheck(orig: Tree, tree: Tree): Unit = {
    assert(entityStack.isEmpty)

    // Ensure locations are present
    tree visitAll {
      case node: Tree if !node.hasLoc => {
        cc.ice(s"Lost location of node:", node.toString, this.getClass.getName)
      }
    }

    // Ensure types are present and correct at all nodes
    if (typed) {
      tree visit {
        case node: Tree if !node.hasTpe => {
          cc.ice(node, "Lost type of node:", node.toString, this.getClass.getName)
        }
        case node: Tree if !cc.hasError && node.tpe == TypeError => {
          cc.ice(node, "Transformed tree has type error:", node.toString, this.getClass.getName)
        }
        case node @ EntInstance(Sym(iSymbol), Sym(eSymbol: TypeSymbol), _, _)
            if iSymbol.kind != TypeInstance(eSymbol) => {
          cc.ice(node, "Bad type for instance symbol", iSymbol.kind.toString, this.getClass.getName)
        }
        case node @ Entity(Sym(eSymbol), _) if !eSymbol.kind.isInstanceOf[TypeEntity] => {
          cc.ice(node, "Bad type for entity symbol", eSymbol.kind.toString, this.getClass.getName)
        }
      }
    }

    // Ensure references are only present to TermSymbols
    // declared within the root entity scope
    val trees = tree match {
      case Thicket(trees) => trees
      case tree           => List(tree)
    }

    for (tree <- trees) {
      val declaredSymbols = {
        val set = mutable.Set[Symbol]()

        def add(node: Tree, symbol: Symbol): Unit = {
          if (set contains symbol) {
            cc.ice(node, "Symbol declared multiple times")
          }
          set add symbol
        }

        tree visitAll {
          case node @ Decl(symbol, _)                   => add(node, symbol)
          case node @ EntInstance(Sym(symbol), _, _, _) => add(node, symbol)
          case node @ EntFunction(Sym(symbol), _)       => add(node, symbol)
          case node @ EntState(ExprRef(symbol), _)      => add(node, symbol)
          case node @ Entity(Sym(symbol), _)            => add(node, symbol)
        }

        set
      }

      def errIfUndeclared(node: Tree, symbol: Symbol) = {
        if (!(declaredSymbols contains symbol) && !symbol.isBuiltin) {
          cc.ice(
            node,
            s"reference to undeclared symbol '${symbol.name}'",
            s"of type '${symbol.kind.toSource}'",
            this.getClass.getName,
            tree.toSource
          )
        }
      }

      def check(tree: Tree): Unit = {
        tree visitAll {
          case node @ Sym(symbol: TermSymbol)     => errIfUndeclared(node, symbol)
          case node @ ExprRef(symbol: TermSymbol) => errIfUndeclared(node, symbol)
          case Decl(symbol, _)                    => symbol.kind visit { case tree: Tree => check(tree) }
          case Entity(Sym(symbol), _)             => symbol.kind visit { case tree: Tree => check(tree) }
        }
      }

      if (checkRefs && orig.isInstanceOf[Entity] || orig.isInstanceOf[Root]) {
        check(tree)
      }
    }
  }
}
