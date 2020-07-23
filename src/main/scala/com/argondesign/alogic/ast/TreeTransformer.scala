////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Base class of a Tree transformer used to modify Trees
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.Config
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

import scala.util.ChainingSyntax

// Tree transformers are applied during a traversal of a Tree.
abstract class TreeTransformer(implicit val cc: CompilerContext)
    extends (Tree => Tree)
    with ChainingSyntax {

  //////////////////////////////////////////////////////////////////////////////
  // Public API
  //////////////////////////////////////////////////////////////////////////////

  // Apply transform to tree
  final def apply(tree: Tree): Tree = {
    // Call start
    start(tree)
    // Walk the tree
    val walked = walkTree(tree)
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
  // Transform specific interface overridable by sub-classes
  //////////////////////////////////////////////////////////////////////////////

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

  // 'skip' is a predicate that can be used to mark subtrees that should not be
  // visited. If 'skip' returns true for a node, that node will not be visited,
  // i.e.: enter and transform will not be called on that node, or any of their
  // children, leaving the subtree unmodified
  protected def skip(tree: Tree): Boolean = false

  // 'start' is called with the root of the input tree, at the beginning of the
  // walk.
  protected def start(tree: Tree): Unit = ()

  // 'finish' is called with the root of the transformed tree, at the end of the
  // walk. It returns the final result tree.
  protected def finish(tree: Tree): Tree = tree

  // 'finalCheck' is invoked with the root of the transformed tree.
  // This can be used to verify invariants introduced by this transform
  protected def finalCheck(tree: Tree): Unit = ()

  // Whether this transform operates on typed or untyped trees
  protected val typed: Boolean = true

  //////////////////////////////////////////////////////////////////////////////
  // Protected API
  //////////////////////////////////////////////////////////////////////////////

  // Walk list, but return the original list if nothing is transformed
  final protected def walk(trees: List[Tree]): List[Tree] = trees match {
    case Nil => Nil
    case _   =>
      // Using a ListBuilder without recursion here as these lists can grow
      // very long with liberal use of 'gen' constructs.
      var same = true
      val results = new collection.mutable.ListBuffer[Tree]()
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
  final protected def walk(treeOpt: Option[Tree]): Option[Tree] = treeOpt match {
    case None => None
    case Some(tree) =>
      val newTree = walk(tree)
      if (newTree eq tree) treeOpt else Some(newTree)
  }

  // Walk single node and check post conditions
  final protected def walk(tree: Tree): Tree = walkTree(tree) tap {
    case Thicket(ts) =>
      ts foreach { result =>
        checkResult(tree, result)
      }
    case Stump  =>
    case result => checkResult(tree, result)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Implementation of transformation of single node
  //////////////////////////////////////////////////////////////////////////////

  // Walk single node, to be implemented by generic subclasses
  protected[ast] def walkTree(tree: Tree): Tree

  //////////////////////////////////////////////////////////////////////////////
  // Private internals
  //////////////////////////////////////////////////////////////////////////////

  // Check result of walk
  final private def checkResult(tree: Tree, result: Tree): Unit = {
    if (!result.hasLoc) {
      throw Ice(
        s"TreeTransformer '${this.getClass.getName}' lost location of transformed node:",
        result.toString,
        "original at:",
        if (tree.hasLoc) tree.loc.prefix else "UNKNOWN",
        tree.toString
      )
    }

    // Check it has type
    if (typed && !result.hasTpe) {
      throw Ice(
        s"TreeTransformer '${this.getClass.getName}' lost type of transformed node:",
        result.toString,
        "original at:",
        if (tree.hasLoc) tree.loc.prefix else "UNKNOWN",
        tree.toString
      )
    }
  }

  // Nodes with children that have been rewritten and therefore copied by
  // TreeCopier need their types assigned
  final private def assignType(tree: Tree): Tree =
    if (typed && !tree.hasTpe) TypeAssigner(tree) else tree

  // Walk child, propagate Thicket/Stump
  final private def splice(child: Tree, treeCopier: Tree => Tree): Tree = walk(child) match {
    case Stump       => Stump
    case Thicket(ts) => Thicket(ts map (treeCopier andThen assignType))
    case tree        => treeCopier(tree)
  }

  final private def walkChildrenRef(tree: Ref): Tree = tree match {
    case node: Ident =>
      val indices = walk(node.idxs)
      TreeCopier(node)(indices)
    case node: Sym =>
      val indices = walk(node.idxs)
      TreeCopier(node)(indices)
  }

  final private def walkChildrenDesc(tree: Desc): Tree = tree match {
    case node: DescVar =>
      val ref = walk(node.ref)
      val spec = walk(node.spec)
      val initOpt = walk(node.initOpt)
      TreeCopier(node)(ref, spec, initOpt)
    case node: DescVal =>
      val ref = walk(node.ref)
      val spec = walk(node.spec)
      val init = walk(node.init)
      TreeCopier(node)(ref, spec, init)
    case node: DescStatic =>
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
    case node: DescPipeVar =>
      val ref = walk(node.ref)
      val spec = walk(node.spec)
      TreeCopier(node)(ref, spec)
    case node: DescPipeIn =>
      val ref = walk(node.ref)
      TreeCopier(node)(ref)
    case node: DescPipeOut =>
      val ref = walk(node.ref)
      TreeCopier(node)(ref)
    case node: DescParam =>
      val ref = walk(node.ref)
      val spec = walk(node.spec)
      val initOpt = walk(node.initOpt)
      TreeCopier(node)(ref, spec, initOpt)
    case node: DescParamType =>
      val ref = walk(node.ref)
      val initOpt = walk(node.initOpt)
      TreeCopier(node)(ref, initOpt)
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
  }

  final private def walkChildrenDecl(tree: Decl): Tree = tree match {
    case node: DeclVar =>
      val spec = walk(node.spec)
      TreeCopier(node)(spec)
    case node: DeclVal =>
      val spec = walk(node.spec)
      TreeCopier(node)(spec)
    case node: DeclStatic =>
      val spec = walk(node.spec)
      TreeCopier(node)(spec)
    case node: DeclIn =>
      val spec = walk(node.spec)
      TreeCopier(node)(spec)
    case node: DeclOut =>
      val spec = walk(node.spec)
      TreeCopier(node)(spec)
    case node: DeclPipeVar =>
      val spec = walk(node.spec)
      TreeCopier(node)(spec)
    case node: DeclPipeIn  => node
    case node: DeclPipeOut => node
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
  }

  final private def walkChildrenDefn(tree: Defn): Tree = tree match {
    case node: DefnVar =>
      val initOpt = walk(node.initOpt)
      TreeCopier(node)(initOpt)
    case node: DefnVal =>
      val init = walk(node.init)
      TreeCopier(node)(init)
    case node: DefnStatic =>
      val initOpt = walk(node.initOpt)
      TreeCopier(node)(initOpt)
    case node: DefnIn => node
    case node: DefnOut =>
      val initOpt = walk(node.initOpt)
      TreeCopier(node)(initOpt)
    case node: DefnPipeVar => node
    case node: DefnPipeIn  => node
    case node: DefnPipeOut => node
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
      val body = walk(node.body)
      TreeCopier(node)(body)
  }

  final private def walkChildrenGen(tree: Gen): Tree = tree match {
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
  }

  final private def walkChildrenAssertion(tree: Assertion): Tree = tree match {
    case node: AssertionAssert =>
      val cond = walk(node.cond)
      TreeCopier(node)(cond)
    case node: AssertionAssume =>
      val cond = walk(node.cond)
      TreeCopier(node)(cond)
    case node: AssertionStatic =>
      val cond = walk(node.cond)
      TreeCopier(node)(cond)
  }

  final private def walkChildrenRiz(tree: Riz): Tree = tree match {
    case node: RizDesc => splice(node.desc, TreeCopier(node))
    case node: RizDecl => splice(node.decl, TreeCopier(node))
    case node: RizDefn => splice(node.defn, TreeCopier(node))
  }

  final private def walkChildrenEnt(tree: Ent): Tree = tree match {
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
    case node: EntAssign =>
      val lhs = walk(node.lhs)
      val rhs = walk(node.rhs)
      TreeCopier(node)(lhs, rhs)
    case node: EntCombProcess =>
      val stmts = walk(node.stmts)
      TreeCopier(node)(stmts)
    case node: EntClockedProcess =>
      val clk = walk(node.clk)
      val rstOpt = walk(node.rstOpt)
      val stmts = walk(node.stmts)
      TreeCopier(node)(clk, rstOpt, stmts)
    case node: EntAssertion => splice(node.assertion, TreeCopier(node))
    case node: EntVerbatim  => node
    case node: EntComment   => node
  }

  final private def walkChildrenRec(tree: Rec): Tree = tree match {
    case node: RecDesc => splice(node.desc, TreeCopier(node))
    case node: RecDecl => splice(node.decl, TreeCopier(node))
    case node: RecDefn => splice(node.defn, TreeCopier(node))
    case node: RecGen =>
      val gen = walk(node.gen)
      TreeCopier(node)(gen)
    case node: RecAssertion => splice(node.assertion, TreeCopier(node))
    case node: RecComment   => node
  }

  final private def walkChildrenStmt(tree: Stmt): Tree = tree match {
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
    case node: StmtReturn =>
      val exprOpt = walk(node.exprOpt)
      TreeCopier(node)(exprOpt)
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
    case node: StmtDelayed =>
      val lhs = walk(node.lhs)
      val rhs = walk(node.rhs)
      TreeCopier(node)(lhs, rhs)
    case node: StmtOutcall =>
      val output = walk(node.output)
      val func = walk(node.func)
      val inputs = walk(node.inputs)
      TreeCopier(node)(output, func, inputs)
    case node: StmtExpr =>
      val expr = walk(node.expr)
      TreeCopier(node)(expr)
    case node: StmtWait =>
      val cond = walk(node.cond)
      TreeCopier(node)(cond)
    case node: StmtAssertion => splice(node.assertion, TreeCopier(node))
    case node: StmtError     => node
    case node: StmtComment   => node
  }

  final private def walkChildrenCase(tree: Case): Tree = tree match {
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
  }

  final private def walkChildrenExpr(tree: Expr): Tree = tree match {
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
    case node: ExprCond =>
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
    case node: ExprSel =>
      val expr = walk(node.expr)
      val idxs = walk(node.idxs)
      TreeCopier(node)(expr, idxs)
    case node: ExprRef =>
      val ref = walk(node.ref)
      TreeCopier(node)(ref)
    case node: ExprSym => node
    case node: ExprOld =>
      val expr = walk(node.expr)
      TreeCopier(node)(expr)
    case node: ExprThis =>
      val expr = walk(node.expr)
      TreeCopier(node)(expr)
    case node: ExprType => node
    case node: ExprCast =>
      val expr = walk(node.expr)
      TreeCopier(node)(expr)
    case node: ExprInt   => node
    case node: ExprNum   => node
    case node: ExprStr   => node
    case node: ExprError => node
  }

  final private def walkChildrenArg(tree: Arg): Tree = tree match {
    case node: ArgP =>
      val expr = walk(node.expr)
      TreeCopier(node)(expr)
    case node: ArgN =>
      val expr = walk(node.expr)
      TreeCopier(node)(expr)
    case node: ArgD =>
      val idxs = walk(node.idxs)
      val expr = walk(node.expr)
      TreeCopier(node)(idxs, expr)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Protected internals
  //////////////////////////////////////////////////////////////////////////////

  // Walk children of node
  final protected[ast] def walkChildren(tree: Tree): Tree = assignType {
    tree match {
      ////////////////////////////////////////////////////////////////////////
      // Dispatch based on group
      ////////////////////////////////////////////////////////////////////////
      case node: Expr      => walkChildrenExpr(node)
      case node: Stmt      => walkChildrenStmt(node)
      case node: Case      => walkChildrenCase(node)
      case node: Decl      => walkChildrenDecl(node)
      case node: Defn      => walkChildrenDefn(node)
      case node: Ent       => walkChildrenEnt(node)
      case node: Rec       => walkChildrenRec(node)
      case node: Arg       => walkChildrenArg(node)
      case node: Assertion => walkChildrenAssertion(node)
      case node: Desc      => walkChildrenDesc(node)
      case node: Gen       => walkChildrenGen(node)
      case node: Ref       => walkChildrenRef(node)
      case node: Riz       => walkChildrenRiz(node)
      ////////////////////////////////////////////////////////////////////////
      // Root
      ////////////////////////////////////////////////////////////////////////
      case node: Root =>
        val body = walk(node.body)
        TreeCopier(node)(body)
      ////////////////////////////////////////////////////////////////////////
      // Thicket/Stump TODO: these should be unreachable
      ////////////////////////////////////////////////////////////////////////
      case node: Thicket =>
        val trees = walk(node.trees)
        TreeCopier(node)(trees)
      case Stump => unreachable
    }
  }

  // Apply transform to list of trees, flatten Thickets
  final protected[ast] def transform(trees: List[Tree]): List[Tree] = trees flatMap { t =>
    transform(t) match {
      case Thicket(results) => results
      case result           => List(result)
    }
  }

  // Default checks to run after each transformation
  protected[ast] def defaultCheck(orig: Tree, tree: Tree): Unit = {
    assert(!typed || !tree.tpe.isError, this.getClass.getName + "\n" + tree.toSource)
    // TODO: Add back referencing checks
  }

}
