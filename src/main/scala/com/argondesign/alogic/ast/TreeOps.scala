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

  // Recurse through the tree with partial function
  // Wherever the partial function is not defined, we recurse.
  // Wherever the partialfunction is defined, we apply it and stop recursion.
  // To continue recursing after application, the client can invoke visit
  // selectively, or visitChildren to visit all children
  def visit(callback: PartialFunction[Tree, Unit]): Unit = {
    def v(tree: Tree): Unit = {
      if (callback.isDefinedAt(tree)) {
        callback(tree)
      } else {
        tree.children foreach v
      }
    }
    v(this)
  }

  // Visit all children of this tree
  def visitChildren(callback: PartialFunction[Tree, Unit]): Unit = {
    children foreach { _ visit callback }
  }

  // Same as visit but always recurse through the whole tree
  def visitAll(callback: PartialFunction[Tree, Unit]): Unit = {
    def v(tree: Tree): Unit = {
      if (callback.isDefinedAt(tree)) {
        callback(tree)
      }
      tree.children foreach v
    }
    v(this)
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
  //  // Recurse through the tree and collect results of pf for all nodes where it is defined.
  //  // This function is analogous to List.collect in the standard library
  //  def collect[E](pf: PartialFunction[Node, E]): List[E] = {
  //
  //    val f = pf.lift
  //
  //    def c(node: Node): List[E] = {
  //      val headOption: Option[E] = f(node)
  //      val tail: List[E] = node match {
  //        case Instantiate(_, _, _, args)                      => args.values.toList flatMap c
  //        case Connect(_, start, end)                          => (start :: end) flatMap c
  //        case Function(_, _, body)                            => c(body)
  //        case FenceFunction(_, body)                          => c(body)
  //        case FsmTask(_, _, _, fns, fencefn, vfns)            => (fns flatMap c) ::: (fencefn map c getOrElse Nil) ::: (vfns flatMap c)
  //        case StateTask(_, _, _, sbs, fencefn, vfns)          => (sbs flatMap c) ::: (fencefn map c getOrElse Nil) ::: (vfns flatMap c)
  //        case NetworkTask(_, _, _, inst, conn, vfns, fsms)    => (inst flatMap c) ::: (conn flatMap c) ::: (vfns flatMap c) ::: (fsms flatMap c)
  //        case VerilogTask(_, _, _, vfns)                      => vfns flatMap c
  //        case ExprArrIndex(_, name, index)                    => c(name) ::: (index flatMap c)
  //        case ExprVecIndex(_, ref, index)                     => c(ref) ::: (index flatMap c)
  //        case Slice(_, ref, l, _, r)                          => c(ref) ::: c(l) ::: c(r)
  //        case CallExpr(_, name, args)                         => c(name) ::: (args flatMap c)
  //        case Zxt(_, numbits, expr)                           => c(numbits) ::: c(expr)
  //        case Sxt(_, numbits, expr)                           => c(numbits) ::: c(expr)
  //        case DollarCall(_, name, args)                       => args flatMap c
  //        case ReadCall(_, name)                               => c(name)
  //        case WaitCall(_, name)                               => c(name)
  //        case ValidCall(_, name)                              => c(name)
  //        case WriteCall(_, name, args)                        => c(name) ::: (args flatMap c)
  //        case Assign(_, lhs, rhs)                             => c(lhs) ::: c(rhs)
  //        case Update(_, lhs, op, rhs)                         => c(lhs) ::: c(rhs)
  //        case Plusplus(_, lhs)                                => c(lhs)
  //        case Minusminus(_, lhs)                              => c(lhs)
  //        case BinaryOp(_, lhs, op, rhs)                       => c(lhs) ::: c(rhs)
  //        case UnaryOp(_, op, lhs)                             => c(lhs)
  //        case Bracket(_, content)                             => c(content)
  //        case TernaryOp(_, cond, lhs, rhs)                    => c(cond) ::: c(lhs) ::: c(rhs)
  //        case CombinatorialBlock(_, cmds)                     => cmds flatMap c
  //        case StateBlock(_, state, cmds)                      => cmds flatMap c
  //        case DeclarationStmt(_, DeclVar(_, _, init))         => init map c getOrElse Nil
  //        case CombinatorialIf(_, cond, t, e)                  => c(cond) ::: c(t) ::: (e map c getOrElse Nil)
  //        case BitRep(_, count, value)                         => c(count) ::: c(value)
  //        case BitCat(_, parts)                                => parts flatMap c
  //        case _: AlogicComment                                => Nil
  //        case CombinatorialCaseStmt(_, value, cases, default) => c(value) ::: (cases flatMap c) ::: (default map c getOrElse Nil)
  //        case CombinatorialCaseLabel(_, cond, body)           => (cond flatMap c) ::: c(body)
  //        case ControlCaseStmt(_, value, cases, default)       => c(value) ::: (cases flatMap c) ::: (default map c getOrElse Nil)
  //        case ControlCaseLabel(_, cond, body)                 => (cond flatMap c) ::: c(body)
  //        case ControlIf(_, cond, t, e)                        => c(cond) ::: c(t) ::: (e map c getOrElse Nil)
  //        case ControlBlock(_, cmds)                           => cmds flatMap c
  //        case ControlLoop(_, body)                            => c(body)
  //        case ControlWhile(_, cond, body)                     => c(cond) ::: (body flatMap c)
  //        case ControlFor(_, init, cond, incr, body)           => c(init) ::: c(cond) ::: c(incr) ::: (body flatMap c)
  //        case ControlDo(_, cond, body)                        => c(cond) ::: (body flatMap c)
  //        case _: FenceStmt                                    => Nil
  //        case _: BreakStmt                                    => Nil
  //        case _: ReturnStmt                                   => Nil
  //        case _: PipelineRead                                 => Nil
  //        case _: PipelineWrite                                => Nil
  //        case _: GotoStmt                                     => Nil
  //        case _: GotoState                                    => Nil
  //        case DottedName(_, names)                            => Nil
  //        case Literal(_, _)                                   => Nil
  //        case Num(_, _, _, _)                                 => Nil
  //        case VerilogFunction(_, _)                           => Nil
  //        case ExprStmt(_, expr)                               => c(expr)
  //        case _: CallStmt                                     => Nil
  //        case _: CallState                                    => Nil
  //        case _: ReturnState                                  => Nil
  //        case _: ErrorExpr                                    => Nil
  //        case _: ErrorStmt                                    => Nil
  //
  //        case _: LValName                                     => Nil
  //        case LValArrayLookup(_, name, index)                 => c(name) ::: (index flatMap c)
  //        case LValSlice(_, ref, l, _, r)                      => c(ref) ::: c(l) ::: c(r)
  //        case LValCat(_, parts)                               => parts flatMap c
  //      }
  //
  //      headOption match {
  //        case Some(head) => head :: tail
  //        case None       => tail
  //      }
  //    }
  //
  //    c(this)
  //  }
}

// object NodeOps {
//  // NodeOpsDispatcher is used for methods that need to return the same
//  // Node subtype as they are called on.
//  implicit class NodeOpsDispatcher[T <: Node](val node: T) extends AnyVal {
//    def rewrite(callback: PartialFunction[Node, Node]): T = node._rewrite[T](callback)
//  }
// }
