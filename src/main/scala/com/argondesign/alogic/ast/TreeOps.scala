////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Common members of ast.Trees.Tree
// These are factored out into a separate file to keep ast.Trees readable
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import Trees.Tree
import scala.collection.GenTraversableOnce

trait TreeOps { this: Tree =>

  // Iterate all children of this node, including ones that are held through a list or option member
  def children: Iterator[Tree] = {
    val childLists = productIterator collect {
      case tree: Tree       => tree :: Nil
      case xs: List[_]      => xs collect { case tree: Tree => tree }
      case Some(tree: Tree) => tree :: Nil
    }
    childLists.flatten
  }

  ////////////////////////////////////////////////////////////////////////////////
  // visit methods
  ////////////////////////////////////////////////////////////////////////////////

  // Walk the tree with partial function
  // Wherever the partial function is not defined, we recurse.
  // Wherever the partial function is defined, we apply it and stop recursion.
  // To continue recursing after application, the client can invoke visit
  // selectively, or visitChildren to visit all children.
  def visit(visitor: PartialFunction[Tree, Unit]): Unit = {
    def v(tree: Tree): Unit = {
      if (visitor.isDefinedAt(tree)) {
        visitor(tree)
      } else {
        tree.children foreach v
      }
    }
    v(this)
  }

  // Visit all children of this tree
  def visitChildren(visitor: PartialFunction[Tree, Unit]): Unit = {
    children foreach { _ visit visitor }
  }

  // Same as visit but always recurse through the whole tree
  def visitAll(visitor: PartialFunction[Tree, Unit]): Unit = {
    def v(tree: Tree): Unit = {
      if (visitor.isDefinedAt(tree)) {
        visitor(tree)
      }
      tree.children foreach v
    }
    v(this)
  }

  ////////////////////////////////////////////////////////////////////////////////
  // collect methods
  ////////////////////////////////////////////////////////////////////////////////

  // Collect results of walking the tree with partial function
  // Wherever the partial function is not defined, we recurse.
  // Wherever the partial function is defined, we apply it, collect the result and stop recursion.
  def collect[E](pf: PartialFunction[Tree, E]): List[E] = {
    def c(tree: Tree): Iterator[E] = {
      if (pf.isDefinedAt(tree)) {
        Iterator.single(pf(tree))
      } else {
        tree.children flatMap c
      }
    }
    c(this).toList
  }

  // Collect all children of this tree
  def collectChildren[E](pf: PartialFunction[Tree, E]): List[E] = {
    (children flatMap { _ collect pf }).toList
  }

  // Same as collect but always recurse through the whole tree
  def collectAll[E](pf: PartialFunction[Tree, E]): List[E] = {
    def c(tree: Tree): Iterator[E] = {
      if (pf.isDefinedAt(tree)) {
        Iterator.single(pf(tree)) ++ (tree.children flatMap c)
      } else {
        tree.children flatMap c
      }
    }
    c(this).toList
  }

  ////////////////////////////////////////////////////////////////////////////////
  // flatCollect methods
  ////////////////////////////////////////////////////////////////////////////////

  def flatCollect[E](pf: PartialFunction[Tree, GenTraversableOnce[E]]): List[E] = {
    def c(tree: Tree): Iterator[E] = {
      if (pf.isDefinedAt(tree)) {
        pf(tree).toIterator
      } else {
        tree.children flatMap c
      }
    }
    c(this).toList
  }

  def flatCollectChildren[E](pf: PartialFunction[Tree, GenTraversableOnce[E]]): List[E] = {
    (children flatMap { _ flatCollect pf }).toList
  }

  def flatCollectAll[E](pf: PartialFunction[Tree, GenTraversableOnce[E]]): List[E] = {
    def c(tree: Tree): Iterator[E] = {
      if (pf.isDefinedAt(tree)) {
        pf(tree).toIterator ++ (tree.children flatMap c)
      } else {
        tree.children flatMap c
      }
    }
    c(this).toList
  }

  //
  //  // Recurse through the tree and apply partial function to all nodes in pre-order
  //  // Wherever the partial function is defined, the tree is rewritten, otherwise the
  //  // recursion continues
  //  def _rewrite[T <: Node](callback: PartialFunction[Node, Node]): T = {
  //
  //    val cb = callback.lift
  //
  //    def r[E <: Node](node: Node): E = {
  //      val v = cb(node) match {
  //        case Some(x) => x
  //        case None => node match {
  //          case x: Instantiate                                => x
  //          case Connect(a, start, end)                        => Connect(a, r[DottedName](start), end map r[DottedName])
  //          case Function(a, name, body)                       => Function(a, name, r[CtrlStmt](body))
  //          case FenceFunction(a, body)                        => FenceFunction(a, r[CombStmt](body))
  //          case FsmTask(a, name, decls, fns, fencefn, vfns)   => FsmTask(a, name, decls, fns map r[Function], fencefn map r[FenceFunction], vfns)
  //          case StateTask(a, name, decls, sbs, fencefn, vfns) => StateTask(a, name, decls, sbs map r[StateBlock], fencefn map r[FenceFunction], vfns)
  //          case NetworkTask(a, name, decls, inst, conn, vfns, fsms) => {
  //            NetworkTask(a, name, decls, inst map r[Instantiate], conn map r[Connect], vfns map r[VerilogFunction], fsms map r[FsmTask])
  //          }
  //          case x: VerilogTask                        => x
  //          case Assign(a, lhs, rhs)                   => Assign(a, r[LVal](lhs), r[Expr](rhs))
  //          case Update(a, lhs, op, rhs)               => Update(a, r[LVal](lhs), op, r[Expr](rhs))
  //          case Plusplus(a, lhs)                      => Plusplus(a, r[LVal](lhs))
  //          case Minusminus(a, lhs)                    => Minusminus(a, r[LVal](lhs))
  //          case CombinatorialBlock(a, cmds)           => CombinatorialBlock(a, cmds map r[CombStmt])
  //          case StateBlock(a, state, cmds)            => StateBlock(a, state, cmds map r[CombStmt])
  //          case DeclarationStmt(a, decl: DeclVar)     => DeclarationStmt(a, decl.copy(init = (decl.init map r[Expr])))
  //          case CombinatorialIf(a, cond, body, e)     => CombinatorialIf(a, r[Expr](cond), r[CombStmt](body), e map r[CombStmt])
  //          case x: AlogicComment                      => x
  //          case CombinatorialCaseStmt(a, value, c, d) => CombinatorialCaseStmt(a, r[Expr](value), c map r[CombinatorialCaseLabel], d map r[CombStmt])
  //          case ControlCaseStmt(a, value, c, d)       => ControlCaseStmt(a, r[Expr](value), c map r[ControlCaseLabel], d map r[CtrlStmt])
  //          case ControlIf(a, cond, body, e)           => ControlIf(a, cond, r[CtrlStmt](body), e map r[CtrlStmt])
  //          case ControlBlock(a, cmds)                 => ControlBlock(a, cmds map r[Stmt])
  //          case ControlLoop(a, body)                  => ControlLoop(a, r[ControlBlock](body))
  //          case ControlWhile(a, cond, body)           => ControlWhile(a, r[Expr](cond), body map r[Stmt])
  //          case ControlFor(a, init, cond, incr, body) => ControlFor(a, r[CombStmt](init), r[Expr](cond), r[CombStmt](incr), body map r[Stmt])
  //          case ControlDo(a, cond, body)              => ControlDo(a, r[Expr](cond), body map r[Stmt])
  //          case x: FenceStmt                          => x
  //          case x: BreakStmt                          => x
  //          case x: ReturnStmt                         => x
  //          case x: GotoStmt                           => x
  //          case x: GotoState                          => x
  //          case x: VerilogFunction                    => x
  //          case ControlCaseLabel(a, cond, body)       => ControlCaseLabel(a, cond map r[Expr], r[CtrlStmt](body))
  //          case CombinatorialCaseLabel(a, cond, body) => CombinatorialCaseLabel(a, cond map r[Expr], r[CombStmt](body))
  //          case ExprStmt(a, expr)                     => ExprStmt(a, r[Expr](expr))
  //          case x: CallStmt                           => x
  //          case x: CallState                          => x
  //          case x: ReturnState                        => x
  //          case x: ErrorStmt                          => x
  //
  //          // Expressions
  //          case ExprArrIndex(a, name, index)          => ExprArrIndex(a, r[DottedName](name), index map r[Expr])
  //          case ExprVecIndex(a, ref, index)           => ExprVecIndex(a, r[Expr](ref), index map r[Expr])
  //          case Slice(a, ref, lidx, op, ridx)         => Slice(a, r[Expr](ref), r[Expr](lidx), op, r[Expr](ridx))
  //          case CallExpr(a, name, args)               => CallExpr(a, r[DottedName](name), args map r[Expr])
  //          case Zxt(a, numbits, expr)                 => Zxt(a, r[Expr](numbits), r[Expr](expr))
  //          case Sxt(a, numbits, expr)                 => Sxt(a, r[Expr](numbits), r[Expr](expr))
  //          case DollarCall(a, name, args)             => DollarCall(a, name, args map r[Expr])
  //          case ReadCall(a, name)                     => ReadCall(a, r[DottedName](name))
  //          case x: PipelineRead                       => x
  //          case x: PipelineWrite                      => x
  //          case WaitCall(a, name)                     => WaitCall(a, r[DottedName](name))
  //          case ValidCall(a, name)                    => ValidCall(a, r[DottedName](name))
  //          case WriteCall(a, name, args)              => WriteCall(a, r[DottedName](name), args map r[Expr])
  //          case BinaryOp(a, lhs, op, rhs)             => BinaryOp(a, r[Expr](lhs), op, r[Expr](rhs))
  //          case UnaryOp(a, op, lhs)                   => UnaryOp(a, op, r[Expr](lhs))
  //          case Bracket(a, content)                   => Bracket(a, r[Expr](content))
  //          case TernaryOp(a, cond, lhs, rhs)          => TernaryOp(a, r[Expr](cond), r[Expr](lhs), r[Expr](rhs))
  //          case BitRep(a, count, value)               => BitRep(a, r[Expr](count), r[Expr](value))
  //          case BitCat(a, parts)                      => BitCat(a, parts map r[Expr])
  //          case x: DottedName                         => x
  //          case x: Literal                            => x
  //          case x: Num                                => x
  //          case x: ErrorExpr                          => x
  //
  //          // LVals
  //          case x: LValName                           => x
  //          case LValArrayLookup(a, name, index)       => LValArrayLookup(a, r[LValName](name), index map r[Expr])
  //          case LValSlice(a, ref, lidx, op, ridx)     => LValSlice(a, r[LVal](ref), r[Expr](lidx), op, r[Expr](ridx))
  //          case LValCat(a, parts)                     => LValCat(a, parts map r[LVal])
  //        }
  //      }
  //      v.asInstanceOf[E]
  //    }
  //
  //    r[T](this)
  //  }
  //
}

// object NodeOps {
//  // NodeOpsDispatcher is used for methods that need to return the same
//  // Node subtype as they are called on.
//  implicit class NodeOpsDispatcher[T <: Node](val node: T) extends AnyVal {
//    def rewrite(callback: PartialFunction[Node, Node]): T = node._rewrite[T](callback)
//  }
// }
