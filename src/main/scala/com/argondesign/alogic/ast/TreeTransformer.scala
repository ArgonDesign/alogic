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
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.core.Symbols.TypeSymbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Stack
import com.argondesign.alogic.lib.TreeLikeTransformer
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.FollowedBy

// Tree transformers are applied during a post-order traversal of a Tree.
abstract class TreeTransformer(implicit val cc: CompilerContext)
    extends TreeLikeTransformer[Tree]
    with FollowedBy {

  val typed: Boolean = true

  ///////////////////////////////////////////////////////////////////////////////
  // Protected API
  ///////////////////////////////////////////////////////////////////////////////

  // The TypeSymbol representing the currently processed entity
  protected[this] def entitySymbol: TypeSymbol = {
    val Sym(symbol: TypeSymbol) = entityStack.top.ref
    symbol
  }

  ///////////////////////////////////////////////////////////////////////////////
  // Internals
  ///////////////////////////////////////////////////////////////////////////////

  private[this] val entityStack = Stack[Entity]()

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
      cc.ice(s"Lost location of transformed node:", result.toString)
    }

    // Check it has type
    if (typed && !result.hasTpe) {
      cc.ice(result, "Lost type of transformed node", result.toString)
    }

    result
  }

  final override def walk(tree: Tree): Tree = {
    tree match {
      case entity: Entity => entityStack.push(entity)
      case _              => ()
    }
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
        case node: Entity => {
          val ref = walk(node.ref)
          val declarations = walk(node.declarations)
          val instances = walk(node.instances)
          val connects = walk(node.connects)
          val functions = walk(node.functions)
          val states = walk(node.states)
          val fenceStmts = walk(node.fenceStmts)
          val entities = walk(node.entities)
          doTransform(
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
        } followedBy {
          entityStack.pop()
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
        case node: Instance => {
          val ref = walk(node.ref)
          val module = walk(node.module)
          val paramExprs = walk(node.paramExprs)
          doTransform(TreeCopier(node)(ref, module, paramExprs))
        }
        case node: Connect => {
          val lhs = walk(node.lhs)
          val rhs = walk(node.rhs)
          doTransform(TreeCopier(node)(lhs, rhs))
        }
        case node: Function => {
          val ref = walk(node.ref)
          val body = walk(node.body)
          doTransform(TreeCopier(node)(ref, body))
        }
        case node: State => {
          val expr = walk(node.expr)
          val body = walk(node.body)
          doTransform(TreeCopier(node)(expr, body))
        }
        case node: StmtBlock => {
          val body = walk(node.body)
          doTransform(TreeCopier(node)(body))
        }
        case node: StmtIf => {
          val cond = walk(node.cond)
          val thenStmt = walk(node.thenStmt)
          val elseStmt = walk(node.elseStmt)
          doTransform(TreeCopier(node)(cond, thenStmt, elseStmt))
        }
        case node: StmtCase => {
          val expr = walk(node.expr)
          val cases = walk(node.cases)
          val default = walk(node.default)
          doTransform(TreeCopier(node)(expr, cases, default))
        }
        case node: CaseClause => {
          val cond = walk(node.cond)
          val body = walk(node.body)
          doTransform(TreeCopier(node)(cond, body))
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
        case node: StmtFence => doTransform(node)
        case node: StmtBreak => doTransform(node)
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
        case node: StmtRead          => doTransform(node)
        case node: StmtWrite         => doTransform(node)
        case node: StmtDollarComment => doTransform(node)
        case node: StmtStall         => doTransform(node)
        case node: StmtError         => doTransform(node)
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
        case node: ExprInt => doTransform(node)
        case node: ExprNum => doTransform(node)
        case node: ExprStr => doTransform(node)
        case node: ExprRef => {
          val ref = walk(node.ref)
          doTransform(TreeCopier(node)(ref))
        }
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

  override def defaultCheck(orig: Tree, tree: Tree): Unit = {
    assert(entityStack.isEmpty)

    // Ensure locations are present
    tree visitAll {
      case node: Tree if !node.hasLoc => {
        cc.ice(s"Lost location of node:", node.toString)
      }
    }

    // Ensure types are present and correct at all nodes
    if (typed) {
      tree visit {
        case node: Tree if !node.hasTpe => {
          cc.ice(node, "Lost type of node:", node.toString)
        }
        case node: Tree if node.tpe == TypeError => {
          cc.ice(node, "Transformed tree has type error:", node.toString)
        }
        case node @ Instance(Sym(iSymbol), Sym(eSymbol: TypeSymbol), _, _)
            if iSymbol.kind != TypeInstance(eSymbol) => {
          cc.ice(node, "Bad type for instance symbol", iSymbol.kind.toString)
        }
        case node @ Entity(Sym(eSymbol), _, _, _, _, _, _, _, _)
            if !eSymbol.kind.isInstanceOf[TypeEntity] => {
          cc.ice(node, "Bad type for entity symbol", eSymbol.kind.toString)
        }
      }
    }

    // Ensure references are only present to TermSymbols
    // declared within the root entity scope
    if (orig.isInstanceOf[Entity] || orig.isInstanceOf[Root]) {
      val trees = tree match {
        case Thicket(trees) => trees
        case tree           => List(tree)
      }

      for (tree <- trees) {
        val declaredSymbols = {
          val it = tree collectAll {
            case Decl(symbol, _)                            => symbol
            case Instance(Sym(symbol: TermSymbol), _, _, _) => symbol
            case Function(Sym(symbol: TermSymbol), _)       => symbol
            case State(ExprRef(Sym(symbol: TermSymbol)), _) => symbol
          }
          it.toSet
        }
        tree visit {
          case node @ Sym(symbol: TermSymbol)
              if !(declaredSymbols contains symbol) && !symbol.isBuiltin => {
            println(tree.toSource)
            cc.ice(
              node,
              s"reference to undeclared symbol '${symbol.name}'",
              s"of type '${symbol.kind.toSource}'"
            )
          }
        }
      }
    }
  }
}
