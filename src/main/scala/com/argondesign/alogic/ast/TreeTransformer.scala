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

import com.argondesign.alogic.Config
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types.TypeEntity
import com.argondesign.alogic.core.Types.TypeType
import com.argondesign.alogic.typer.TypeAssigner

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.ChainingSyntax

// Tree transformers are applied during a traversal of a Tree.
abstract class TreeTransformer(
    implicit val cc: CompilerContext
) extends (Tree => Tree)
    with ChainingSyntax {

  //////////////////////////////////////////////////////////////////////////////
  // Public API
  //////////////////////////////////////////////////////////////////////////////

  // Apply transform to tree
  final def apply(tree: Tree): Tree = {
    // Call start
    start(tree)
    // Walk the tree
    val walked = walk(tree)
    // Call finish
    val result = finish(walked)
    // Call checks if appropriate
    if (Config.applyTransformChecks && !cc.hasError) {
      // Apply default check
      defaultCheck(tree, result)
      // Apply final check
      finalCheck(result)
    }
    // Yield result
    result
  }

  //////////////////////////////////////////////////////////////////////////////
  // Transform specific interface to be filled in by sub-classes
  //////////////////////////////////////////////////////////////////////////////

  // 'start' is called with the root of the input tree, before the first
  // call to 'skip'.
  protected def start(tree: Tree): Unit = ()

  // 'skip' is a predicate that can be used to mark subtrees that should not be
  // visited. If 'skip' returns true for a node, that node will not be visited,
  // i.e.: enter and transform will not be called on that node, or any of their
  // children, leaving the subtree unmodified
  protected def skip(tree: Tree): Boolean = false

  // 'replace' is a predicate indicating which symbols need to be replaced with
  // a new symbol. This is useful for changing the types of symbols in a
  // consistent manner. Symbols which are replaced will have any containing
  // Decl/Defn/Sym/ExprSym nodes copied to use a fresh symbol and this
  // replacement node is walked instead of the original node. For typed
  // transformers, the first time a symbol is encountered, it's replaced Decl
  // is walked first in order to ensure the type of the symbol is known. Note
  // That this can cause the Decls of replaced symbols to be walked out of
  // order.
  protected def replace(symbol: Symbol): Boolean = false

  // 'enter' is called on each non-skipped node, in pre-order (before visiting
  // any of their children) when the node is first encountered. 'enter' is used
  // to modify the state of the TreeTransformer or the context before
  // transforming children. When 'enter' returns Some(node), the node 'enter'
  // is called with is immediately replaced with the returned node, without
  // visiting the children or calling transform on the node, effectively
  // preempting traversal of the subtree below the given tree. If 'enter'
  // returns None, traversal continues by transforming all children and the
  // entered node.
  protected def enter(tree: Tree): Option[Tree] = None

  // 'transform' is called on each non-skipped node, in post-order (after all
  // children have been transformed). 'transform' us used to modify tree nodes.
  // When 'transform' is called, all child nodes have already been transformed
  // using the same function.
  protected def transform(tree: Tree): Tree = tree

  // 'finish' is called with the root of the transformed tree, after the last
  // call to 'transform'.
  protected def finish(tree: Tree): Tree = tree

  // 'finalCheck' is invoked with the root of the transformed tree.
  // This can be used to verify invariants introduced by this transform
  protected def finalCheck(tree: Tree): Unit = ()

  // Whether this transform operates on typed or untyped trees
  val typed: Boolean = true

  //////////////////////////////////////////////////////////////////////////////
  // Enclosing context tracker
  //////////////////////////////////////////////////////////////////////////////

  private[this] final val _enclosingSymbols = mutable.Stack[Symbol]()

  protected[this] final def enclosingSymbols: collection.Seq[Symbol] = _enclosingSymbols

  protected[this] final def withEnclosingSymbol[R](symbol: Symbol)(f: => R): R = {
    _enclosingSymbols push symbol
    f
  } tap { _ =>
    _enclosingSymbols.pop
  }

  // The Symbol of the closest enclosing entity
  protected[this] final def entitySymbol: Symbol =
    (_enclosingSymbols find {
      _.kind match {
        case TypeType(_: TypeEntity) => true
        case _                       => false
      }
    }).get

  //////////////////////////////////////////////////////////////////////////////
  // Symbol replacement implementation
  //////////////////////////////////////////////////////////////////////////////

  // Map from replacement symbol to original symbol
  protected final val orig: mutable.Map[Symbol, Symbol] = mutable.Map()

  // Replacement symbol for original symbol, if exists
  protected final def repl(symbol: Symbol): Option[Symbol] = _replacementDecl.get(symbol) map {
    _.symbol
  }

  // Map from replacement symbol to its declaration
  private final val _replacementDecl: mutable.Map[Symbol, Decl] = mutable.Map()

  private[this] final def replacementDecl(symbol: Symbol): Decl = {
    // First time we encounter a symbol that is replaced, we create a new
    // symbol and process it's declaration so it's type is known.
    _replacementDecl.getOrElseUpdate(
      symbol, {
        // Create the fresh symbol
        val newSymbol = symbol.dup
        // Add it to the reverse map
        orig(newSymbol) = symbol
        // Copy the decl replacing the symbol with the fresh symbol
        val cpyDecl = TypeAssigner(symbol.decl.cpy(symbol = newSymbol) withLoc symbol.decl.loc)
        // Transform the replaced decl immediately to ensure types are known
        descend(cpyDecl).asInstanceOf[Decl]
      }
    )
  }

  private[this] final def replacementSymbol(symbol: Symbol): Symbol = replacementDecl(symbol).symbol

  private[this] final def mustBeReplaced(symbol: Symbol): Boolean = {
    !(orig contains symbol) && ((_replacementDecl contains symbol) || replace(symbol))
  }

  //////////////////////////////////////////////////////////////////////////////
  // Internals
  //////////////////////////////////////////////////////////////////////////////

  // Walk list, but return the original list if nothing is transformed
  final def walk(trees: List[Tree]): List[Tree] = trees match {
    case Nil => Nil
    case _   =>
      // Using a ListBuilder without recursion here as these lists can grow
      // very long with liberal use of 'gen' constructs.
      var same = true
      val results = new ListBuffer[Tree]()
      trees foreach { tree =>
        walk(tree) match {
          case Thicket(ts)          => same = false; results ++= ts
          case Stump                => same = false
          case same if same eq tree => results += same
          case t                    => same = false; results += t
        }
      }
      if (same) trees else results.toList
  }

  // Walk option, but return the original option if value is not transformed
  final def walk(treeOpt: Option[Tree]): Option[Tree] = treeOpt match {
    case None => None
    case Some(tree) =>
      val newTree = walk(tree)
      if (newTree eq tree) treeOpt else Some(newTree)
  }

  // Walk single node
  final def walk(tree: Tree): Tree = {
    if (skip(tree)) {
      tree
    } else {
      tree match {
        case sym @ Sym(symbol, _) if mustBeReplaced(symbol) =>
          assert(typed)
          descend {
            TypeAssigner(sym.copy(symbol = replacementSymbol(symbol)) withLoc tree.loc)
          }
        case Decl(symbol) if mustBeReplaced(symbol) =>
          assert(typed)
          replacementDecl(symbol)
        case defn @ Defn(symbol) if mustBeReplaced(symbol) =>
          assert(typed)
          descend {
            TypeAssigner(defn.cpy(symbol = replacementSymbol(symbol)) withLoc tree.loc)
          }
        case ExprSym(symbol) if mustBeReplaced(symbol) =>
          assert(typed)
          descend {
            TypeAssigner(ExprSym(replacementSymbol(symbol)) withLoc tree.loc)
          }
        case other => descend(other)
      }
    }
  }

  // Walk child, propagate Thicket/Stump
  private[this] final def splice(child: Tree, treeCopier: Tree => Tree): Tree = walk(child) match {
    case Stump       => Stump
    case Thicket(ts) => Thicket(ts map (treeCopier andThen doTransform))
    case tree        => treeCopier(tree)
  }

  private[this] final def descend(tree: Tree): Tree = {
    // Call enter in pre order
    enter(tree) match {
      // 'enter' provided replacement, use it
      case Some(replacement) => replacement
      // 'enter' did not provide replacement, traverse the tree.
      case None =>
        // Work out introduced symbol
        val introducedSymbol = tree match {
          case desc: Desc =>
            desc.ref match {
              case Sym(symbol, _) => Some(symbol)
              case _              => None
            }
          case Decl(symbol) => Some(symbol)
          case Defn(symbol) => Some(symbol)
          case _            => None
        }

        // Push the introduced symbol to the enclosing symbol stack
        introducedSymbol foreach { symbol =>
          _enclosingSymbols.push(symbol)
        }

        // Walk children of node
        val traversed = tree match {
          ////////////////////////////////////////////////////////////////////////
          // Root
          ////////////////////////////////////////////////////////////////////////
          case node: Root =>
            val body = walk(node.body)
            TreeCopier(node)(body)
          ////////////////////////////////////////////////////////////////////////
          // Ref
          ////////////////////////////////////////////////////////////////////////
          case node: Ident =>
            val indices = walk(node.idxs)
            TreeCopier(node)(indices)
          case node: Sym =>
            val indices = walk(node.idxs)
            TreeCopier(node)(indices)
          ////////////////////////////////////////////////////////////////////////
          // Desc
          ////////////////////////////////////////////////////////////////////////
          case node: DescVar =>
            val ref = walk(node.ref)
            val spec = walk(node.spec)
            val initOpt = walk(node.initOpt)
            TreeCopier(node)(ref, spec, initOpt)
          case node: DescIn =>
            val ref = walk(node.ref)
            val spec = walk(node.spec)
            TreeCopier(node)(ref, spec)
          case node: DescOut =>
            val ref = walk(node.ref)
            val spec = walk(node.spec)
            val initOpt = walk(node.initOpt)
            TreeCopier(node)(ref, spec, initOpt)
          case node: DescPipeline =>
            val ref = walk(node.ref)
            val spec = walk(node.spec)
            TreeCopier(node)(ref, spec)
          case node: DescParam =>
            val ref = walk(node.ref)
            val spec = walk(node.spec)
            val initOpt = walk(node.initOpt)
            TreeCopier(node)(ref, spec, initOpt)
          case node: DescConst =>
            val ref = walk(node.ref)
            val spec = walk(node.spec)
            val init = walk(node.init)
            TreeCopier(node)(ref, spec, init)
          case node: DescGen =>
            val ref = walk(node.ref)
            val spec = walk(node.spec)
            val init = walk(node.init)
            TreeCopier(node)(ref, spec, init)
          case node: DescArray =>
            val ref = walk(node.ref)
            val elem = walk(node.elem)
            val size = walk(node.size)
            TreeCopier(node)(ref, elem, size)
          case node: DescSram =>
            val ref = walk(node.ref)
            val elem = walk(node.elem)
            val size = walk(node.size)
            TreeCopier(node)(ref, elem, size)
          case node: DescType =>
            val ref = walk(node.ref)
            val spec = walk(node.spec)
            TreeCopier(node)(ref, spec)
          case node: DescEntity =>
            val ref = walk(node.ref)
            val body = walk(node.body)
            TreeCopier(node)(ref, body)
          case node: DescRecord =>
            val ref = walk(node.ref)
            val body = walk(node.body)
            TreeCopier(node)(ref, body)
          case node: DescInstance =>
            val ref = walk(node.ref)
            val spec = walk(node.spec)
            TreeCopier(node)(ref, spec)
          case node: DescSingleton =>
            val ref = walk(node.ref)
            val body = walk(node.body)
            TreeCopier(node)(ref, body)
          case node: DescFunc =>
            val ref = walk(node.ref)
            val ret = walk(node.ret)
            val args = walk(node.args)
            val body = walk(node.body)
            TreeCopier(node)(ref, ret, args, body)
          case node: DescChoice =>
            val ref = walk(node.ref)
            val choices = walk(node.choices)
            TreeCopier(node)(ref, choices)
          ////////////////////////////////////////////////////////////////////////
          // Decl
          ////////////////////////////////////////////////////////////////////////
          case node: DeclVar =>
            val spec = walk(node.spec)
            TreeCopier(node)(spec)
          case node: DeclIn =>
            val spec = walk(node.spec)
            TreeCopier(node)(spec)
          case node: DeclOut =>
            val spec = walk(node.spec)
            TreeCopier(node)(spec)
          case node: DeclPipeline =>
            val spec = walk(node.spec)
            TreeCopier(node)(spec)
          case node: DeclConst =>
            val spec = walk(node.spec)
            TreeCopier(node)(spec)
          case node: DeclGen =>
            val spec = walk(node.spec)
            TreeCopier(node)(spec)
          case node: DeclArray =>
            val elem = walk(node.elem)
            val size = walk(node.size)
            TreeCopier(node)(elem, size)
          case node: DeclSram =>
            val elem = walk(node.elem)
            val size = walk(node.size)
            TreeCopier(node)(elem, size)
          case node: DeclStack =>
            val elem = walk(node.elem)
            val size = walk(node.size)
            TreeCopier(node)(elem, size)
          case node: DeclType =>
            val spec = walk(node.spec)
            TreeCopier(node)(spec)
          case node: DeclEntity =>
            val decls = walk(node.decls)
            TreeCopier(node)(decls)
          case node: DeclRecord =>
            val decls = walk(node.decls)
            TreeCopier(node)(decls)
          case node: DeclInstance =>
            val spec = walk(node.spec)
            TreeCopier(node)(spec)
          case node: DeclSingleton =>
            val decls = walk(node.decls)
            TreeCopier(node)(decls)
          case node: DeclFunc =>
            val ret = walk(node.ret)
            val args = walk(node.args)
            TreeCopier(node)(ret, args)
          case node: DeclState => node
          ////////////////////////////////////////////////////////////////////////
          // Defn
          ////////////////////////////////////////////////////////////////////////
          case node: DefnVar =>
            val initOpt = walk(node.initOpt)
            TreeCopier(node)(initOpt)
          case node: DefnIn => node
          case node: DefnOut =>
            val initOpt = walk(node.initOpt)
            TreeCopier(node)(initOpt)
          case node: DefnPipeline => node
          case node: DefnConst =>
            val init = walk(node.init)
            TreeCopier(node)(init)
          case node: DefnGen =>
            val init = walk(node.init)
            TreeCopier(node)(init)
          case node: DefnArray => node
          case node: DefnSram  => node
          case node: DefnStack => node
          case node: DefnType  => node
          case node: DefnEntity =>
            val body = walk(node.body)
            TreeCopier(node)(body)
          case node: DefnRecord =>
            val body = walk(node.body)
            TreeCopier(node)(body)
          case node: DefnInstance => node
          case node: DefnSingleton =>
            val body = walk(node.body)
            TreeCopier(node)(body)
          case node: DefnFunc =>
            val args = walk(node.args)
            val body = walk(node.body)
            TreeCopier(node)(args, body)
          case node: DefnState =>
            val expr = walk(node.expr)
            val body = walk(node.body)
            TreeCopier(node)(expr, body)
          ////////////////////////////////////////////////////////////////////////
          // Gen
          ////////////////////////////////////////////////////////////////////////
          case node: GenIf =>
            val cond = walk(node.cond)
            val thenItems = walk(node.thenItems)
            val elseItems = walk(node.elseItems)
            TreeCopier(node)(cond, thenItems, elseItems)
          case node: GenFor =>
            val inits = walk(node.inits)
            val cond = walk(node.cond)
            val step = walk(node.steps)
            val body = walk(node.body)
            TreeCopier(node)(inits, cond, step, body)
          case node: GenRange =>
            val inits = walk(node.inits)
            val end = walk(node.end)
            val body = walk(node.body)
            TreeCopier(node)(inits, end, body)
          ////////////////////////////////////////////////////////////////////////
          // Riz
          ////////////////////////////////////////////////////////////////////////
          case node: RizDesc => splice(node.desc, TreeCopier(node))
          case node: RizDecl => splice(node.decl, TreeCopier(node))
          case node: RizDefn => splice(node.defn, TreeCopier(node))
          ////////////////////////////////////////////////////////////////////////
          // Ent
          ////////////////////////////////////////////////////////////////////////
          case node: EntDesc => splice(node.desc, TreeCopier(node))
          case node: EntDecl => splice(node.decl, TreeCopier(node))
          case node: EntDefn => splice(node.defn, TreeCopier(node))
          case node: EntGen =>
            val gen = walk(node.gen)
            TreeCopier(node)(gen)
          case node: EntConnect =>
            val lhs = walk(node.lhs)
            val rhs = walk(node.rhs)
            TreeCopier(node)(lhs, rhs)
          case node: EntCombProcess =>
            val stmts = walk(node.stmts)
            TreeCopier(node)(stmts)
          case node: EntVerbatim => node
          case node: EntComment  => node
          ////////////////////////////////////////////////////////////////////////
          // Rec
          ////////////////////////////////////////////////////////////////////////
          case node: RecDesc => splice(node.desc, TreeCopier(node))
          case node: RecDecl => splice(node.decl, TreeCopier(node))
          case node: RecDefn => splice(node.defn, TreeCopier(node))
          case node: RecGen =>
            val gen = walk(node.gen)
            TreeCopier(node)(gen)
          case node: RecComment => node
          ////////////////////////////////////////////////////////////////////////
          // Stmt
          ////////////////////////////////////////////////////////////////////////
          case node: StmtDesc => splice(node.desc, TreeCopier(node))
          case node: StmtDecl => splice(node.decl, TreeCopier(node))
          case node: StmtDefn => splice(node.defn, TreeCopier(node))
          case node: StmtGen =>
            val gen = walk(node.gen)
            TreeCopier(node)(gen)
          case node: StmtBlock =>
            val body = walk(node.body)
            TreeCopier(node)(body)
          case node: StmtIf =>
            val cond = walk(node.cond)
            val thenStmts = walk(node.thenStmts)
            val elseStmts = walk(node.elseStmts)
            TreeCopier(node)(cond, thenStmts, elseStmts)
          case node: StmtCase =>
            val expr = walk(node.expr)
            val cases = walk(node.cases)
            TreeCopier(node)(expr, cases)
          case node: StmtLoop =>
            val body = walk(node.body)
            TreeCopier(node)(body)
          case node: StmtWhile =>
            val cond = walk(node.cond)
            val body = walk(node.body)
            TreeCopier(node)(cond, body)
          case node: StmtFor =>
            val inits = walk(node.inits)
            val cond = walk(node.cond)
            val incr = walk(node.steps)
            val body = walk(node.body)
            TreeCopier(node)(inits, cond, incr, body)
          case node: StmtDo =>
            val cond = walk(node.cond)
            val body = walk(node.body)
            TreeCopier(node)(cond, body)
          case node: StmtLet =>
            val inits = walk(node.inits)
            val body = walk(node.body)
            TreeCopier(node)(inits, body)
          case node: StmtFence    => node
          case node: StmtBreak    => node
          case node: StmtContinue => node
          case node: StmtGoto =>
            val expr = walk(node.expr)
            TreeCopier(node)(expr)
          case node: StmtReturn => node
          case node: StmtAssign =>
            val lhs = walk(node.lhs)
            val rhs = walk(node.rhs)
            TreeCopier(node)(lhs, rhs)
          case node: StmtUpdate =>
            val lhs = walk(node.lhs)
            val rhs = walk(node.rhs)
            TreeCopier(node)(lhs, rhs)
          case node: StmtPost =>
            val expr = walk(node.expr)
            TreeCopier(node)(expr)
          case node: StmtRead  => node
          case node: StmtWrite => node
          case node: StmtExpr =>
            val expr = walk(node.expr)
            TreeCopier(node)(expr)
          case node: StmtStall =>
            val cond = walk(node.cond)
            TreeCopier(node)(cond)
          case node: StmtError   => node
          case node: StmtComment => node
          ////////////////////////////////////////////////////////////////////////
          // Case
          ////////////////////////////////////////////////////////////////////////
          case node: CaseGen =>
            val gen = walk(node.gen)
            TreeCopier(node)(gen)
          case node: CaseRegular =>
            val cond = walk(node.cond)
            val stmts = walk(node.stmts)
            TreeCopier(node)(cond, stmts)
          case node: CaseDefault =>
            val stmts = walk(node.stmts)
            TreeCopier(node)(stmts)
          ////////////////////////////////////////////////////////////////////////
          // Expr
          ////////////////////////////////////////////////////////////////////////
          case node: ExprCall =>
            val expr = walk(node.expr)
            val args = walk(node.args)
            TreeCopier(node)(expr, args)
          case node: ExprUnary =>
            val expr = walk(node.expr)
            TreeCopier(node)(expr)
          case node: ExprBinary =>
            val lhs = walk(node.lhs)
            val rhs = walk(node.rhs)
            TreeCopier(node)(lhs, rhs)
          case node: ExprTernary =>
            val cond = walk(node.cond)
            val thenExpr = walk(node.thenExpr)
            val elseExpr = walk(node.elseExpr)
            TreeCopier(node)(cond, thenExpr, elseExpr)
          case node: ExprRep =>
            val count = walk(node.count)
            val expr = walk(node.expr)
            TreeCopier(node)(count, expr)
          case node: ExprCat =>
            val parts = walk(node.parts)
            TreeCopier(node)(parts)
          case node: ExprIndex =>
            val expr = walk(node.expr)
            val index = walk(node.index)
            TreeCopier(node)(expr, index)
          case node: ExprSlice =>
            val expr = walk(node.expr)
            val lidx = walk(node.lIdx)
            val ridx = walk(node.rIdx)
            TreeCopier(node)(expr, lidx, ridx)
          case node: ExprSelect =>
            val expr = walk(node.expr)
            val idxs = walk(node.idxs)
            TreeCopier(node)(expr, idxs)
          case node: ExprRef =>
            val ref = walk(node.ref)
            TreeCopier(node)(ref)
          case node: ExprSym  => node
          case node: ExprType => node
          case node: ExprCast =>
            val expr = walk(node.expr)
            TreeCopier(node)(expr)
          case node: ExprInt   => node
          case node: ExprNum   => node
          case node: ExprStr   => node
          case node: ExprError => node
          ////////////////////////////////////////////////////////////////////////
          // Arg
          ////////////////////////////////////////////////////////////////////////
          case node: ArgP =>
            val expr = walk(node.expr)
            TreeCopier(node)(expr)
          case node: ArgN =>
            val expr = walk(node.expr)
            TreeCopier(node)(expr)
          ////////////////////////////////////////////////////////////////////////
          // Thicket/Stump TODO: these should be unreachable
          ////////////////////////////////////////////////////////////////////////
          case node: Thicket =>
            val trees = walk(node.trees)
            TreeCopier(node)(trees)
          case Stump => Stump
        }

        // Pop the introduced symbol from the enclosing symbol stack
        introducedSymbol foreach { _ =>
          _enclosingSymbols.pop()
        }

        // Apply transform, unless it's a Thicket or Stump
        traversed match {
          case _: Thicket => traversed
          case Stump      => traversed
          case other      => doTransform(other)
        }
    }
  }

  private[this] final def doTransform(tree: Tree): Tree = {
    // Nodes with children that have been rewritten and henceforth
    // been copied by TreeCopier need their types assigned
    if (typed && !tree.hasTpe) {
      TypeAssigner(tree)
    }

    // Transform the node
    val result = transform(tree)

    // Check it has location
    if (!result.hasLoc) {
      cc.ice(
        s"Pass '${this.getClass.getName}' lost location of transformed node:",
        result.toString,
        "original at:",
        if (tree.hasLoc) tree.loc.prefix else "UNKNOWN",
        tree.toString
      )
    }

    // Check it has type
    if (typed && !result.hasTpe) {
      cc.ice(
        s"Pass '${this.getClass.getName}' lost type of transformed node:",
        result.toString,
        "original at:",
        if (tree.hasLoc) tree.loc.prefix else "UNKNOWN",
        tree.toString
      )
    }

    result
  }

  //////////////////////////////////////////////////////////////////////////////
  // Default checks to run after each pass
  //////////////////////////////////////////////////////////////////////////////

  final def defaultCheck(orig: Tree, tree: Tree): Unit = {
    assert(enclosingSymbols.isEmpty, this.getClass.getName + " " + enclosingSymbols)

    assert(!typed || !tree.tpe.isError, this.getClass.getName + "\n" + tree.toSource)

    // TODO: Add back referencing checks
  }
}
