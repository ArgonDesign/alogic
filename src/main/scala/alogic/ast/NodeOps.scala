////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017 Argon Design Ltd.
// All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

// This file contains some useful functions for manipulating the abstract syntax tree

package alogic.ast

trait NodeOps extends NodePrettyPrintOps { this: Node =>

  // Recurse through the tree and apply function to all nodes in pre-order
  // Callback returns true to continue recursing, or false to stop processing children
  def visit(callback: Node => Boolean): Unit = {

    def v(node: Node): Unit = {
      if (callback(node))
        node match {
          case Instantiate(_, _, _)                             =>
          case Connect(start, end)                              => { v(start); end foreach v }
          case Function(name, body)                             => v(body)
          case FenceFunction(body)                              => v(body)
          case FsmTask(name, decls, fns, fencefn, vfns, hasnew) => { fns foreach v; fencefn foreach v; vfns foreach v }
          case StateTask(name, decls, sbs, fencefn, vfns)       => { sbs foreach v; fencefn foreach v; vfns foreach v }
          case NetworkTask(name, decls, inst, conn, vfns, fsms) => { inst foreach v; conn foreach v; vfns foreach v; fsms foreach v }
          case VerilogTask(name, decls, fns)                    => fns foreach v
          case ArrayLookup(name, index)                         => { v(name); index foreach v }
          case Slice(ref, l, op, r)                             => { v(ref); v(l); v(r) }
          case CallExpr(name, args)                             => { v(name); args foreach v }
          case Zxt(numbits, expr)                               => { v(numbits); v(expr) }
          case Sxt(numbits, expr)                               => { v(numbits); v(expr) }
          case DollarCall(name, args)                           => args foreach v
          case ReadCall(name)                                   =>
          case PipelineRead                                     =>
          case PipelineWrite                                    =>
          case LockCall(name)                                   =>
          case UnlockCall(_) | ValidCall(_)                     =>
          case WriteCall(name, args)                            => args foreach v
          case Assign(lhs, rhs)                                 => { v(lhs); v(rhs) }
          case Update(lhs, op, rhs)                             => { v(lhs); v(rhs) }
          case Plusplus(lhs)                                    => v(lhs)
          case Minusminus(lhs)                                  => v(lhs)
          case BinaryOp(lhs, op, rhs)                           => { v(lhs); v(rhs) }
          case UnaryOp(op, lhs)                                 => v(lhs)
          case Bracket(content)                                 => v(content)
          case TernaryOp(cond, lhs, rhs)                        => { v(cond); v(lhs); v(rhs) }
          case CombinatorialBlock(cmds)                         => cmds foreach v
          case StateBlock(state, cmds)                          => cmds foreach v
          case DeclarationStmt(decl: VarDeclaration)            => decl.init foreach v
          case CombinatorialIf(cond, body, Some(e))             => { v(cond); v(body); v(e) }
          case CombinatorialIf(cond, body, None)                => { v(cond); v(body) }
          case BitRep(count, value)                             => { v(count); v(value) }
          case BitCat(parts)                                    => parts foreach v
          case AlogicComment(str)                               =>
          case CombinatorialCaseStmt(value, c, d)               => { v(value); c foreach v; d foreach v }
          case ControlCaseStmt(value, c, d)                     => { v(value); c foreach v; d foreach v }
          case ControlIf(cond, body, Some(e))                   => { v(cond); v(body); v(e) }
          case ControlIf(cond, body, None)                      => { v(cond); v(body) }
          case ControlBlock(cmds)                               => cmds foreach v
          case ControlLoop(body)                                => v(body)
          case ControlWhile(cond, body)                         => { v(cond); body foreach v }
          case ControlFor(init, cond, incr, body)               => { v(init); v(cond); v(incr); body foreach v }
          case ControlDo(cond, body)                            => { v(cond); body foreach v }
          case FenceStmt                                        =>
          case BreakStmt                                        =>
          case ReturnStmt                                       =>
          case GotoStmt(target: String)                         =>
          case GotoState(state: Int)                            =>
          case DottedName(names)                                =>
          case Literal(_)                                       =>
          case Num(_, _, _)                                     =>
          case VerilogFunction(_)                               =>
          case ControlCaseLabel(cond, body)                     => { cond foreach v; v(body) }
          case CombinatorialCaseLabel(cond, body)               => { cond foreach v; v(body) }
          case ExprStmt(expr)                                   => v(expr)
          case CallStmt(_)                                      =>
          case CallState(_, _)                                  =>
          case ReturnState                                      =>
        }
    }

    v(this)
  }

  // Recurse through the tree and apply partial function to all nodes in pre-order
  // Wherever the partial function is defined, the tree is rewritten, otherwise the
  // recursion continues
  def _rewrite[T <: Node](callback: PartialFunction[Node, Node]): T = {

    val cb = callback.lift

    def r[E <: Node](node: Node): E = {
      val v = cb(node) match {
        case Some(x) => x
        case None => node match {
          case x: Instantiate                                   => x
          case Connect(start, end)                              => Connect(r[DottedName](start), end map r[DottedName])
          case Function(name, body)                             => Function(name, r[CtrlStmt](body))
          case FenceFunction(body)                              => FenceFunction(r[CombStmt](body))
          case FsmTask(name, decls, fns, fencefn, vfns, hasnew) => FsmTask(name, decls, fns map r[Function], fencefn map r[FenceFunction], vfns, hasnew)
          case StateTask(name, decls, sbs, fencefn, vfns)       => StateTask(name, decls, sbs map r[StateBlock], fencefn map r[FenceFunction], vfns)
          case NetworkTask(name, decls, inst, conn, vfns, fsms) => {
            NetworkTask(name, decls, inst map r[Instantiate], conn map r[Connect], vfns map r[VerilogFunction], fsms map r[FsmTask])
          }
          case x: VerilogTask                        => x
          case Assign(lhs, rhs)                      => Assign(r[Expr](lhs), r[Expr](rhs))
          case Update(lhs, op, rhs)                  => Update(r[Expr](lhs), op, r[Expr](rhs))
          case Plusplus(lhs)                         => Plusplus(r[Expr](lhs))
          case Minusminus(lhs)                       => Minusminus(r[Expr](lhs))
          case CombinatorialBlock(cmds)              => CombinatorialBlock(cmds map r[CombStmt])
          case StateBlock(state, cmds)               => StateBlock(state, cmds map r[CombStmt])
          case DeclarationStmt(decl: VarDeclaration) => DeclarationStmt(decl.copy(init = (decl.init map r[Expr])))
          case CombinatorialIf(cond, body, e)        => CombinatorialIf(r[Expr](cond), r[CombStmt](body), e map r[CombStmt])
          case x: AlogicComment                      => x
          case CombinatorialCaseStmt(value, c, d)    => CombinatorialCaseStmt(r[Expr](value), c map r[CombinatorialCaseLabel], d map r[CombStmt])
          case ControlCaseStmt(value, c, d)          => ControlCaseStmt(r[Expr](value), c map r[ControlCaseLabel], d map r[CtrlStmt])
          case ControlIf(cond, body, e)              => ControlIf(cond, r[CtrlStmt](body), e map r[CtrlStmt])
          case ControlBlock(cmds)                    => ControlBlock(cmds map r[Stmt])
          case ControlLoop(body)                     => ControlLoop(r[ControlBlock](body))
          case ControlWhile(cond, body)              => ControlWhile(r[Expr](cond), body map r[Stmt])
          case ControlFor(init, cond, incr, body)    => ControlFor(r[CombStmt](init), r[Expr](cond), r[CombStmt](incr), body map r[Stmt])
          case ControlDo(cond, body)                 => ControlDo(r[Expr](cond), body map r[Stmt])
          case FenceStmt                             => FenceStmt
          case BreakStmt                             => BreakStmt
          case ReturnStmt                            => ReturnStmt
          case x: GotoStmt                           => x
          case x: GotoState                          => x
          case x: VerilogFunction                    => x
          case ControlCaseLabel(cond, body)          => ControlCaseLabel(cond map r[Expr], r[CtrlStmt](body))
          case CombinatorialCaseLabel(cond, body)    => CombinatorialCaseLabel(cond map r[Expr], r[CombStmt](body))
          case ExprStmt(expr)                        => ExprStmt(r[Expr](expr))
          case x: CallStmt                           => x
          case x: CallState                          => x
          case ReturnState                           => ReturnState

          // Expressions
          case ArrayLookup(name, index)              => ArrayLookup(r[DottedName](name), index map r[Expr])
          case Slice(ref, lidx, op, ridx)            => Slice(r[VarRef](ref), r[Expr](lidx), op, r[Expr](ridx))
          case CallExpr(name, args)                  => CallExpr(r[DottedName](name), args map r[Expr])
          case Zxt(numbits, expr)                    => Zxt(r[Expr](numbits), r[Expr](expr))
          case Sxt(numbits, expr)                    => Sxt(r[Expr](numbits), r[Expr](expr))
          case DollarCall(name, args)                => DollarCall(name, args map r[Expr])
          case ReadCall(name)                        => ReadCall(r[DottedName](name))
          case PipelineRead                          => PipelineRead
          case PipelineWrite                         => PipelineWrite
          case LockCall(name)                        => LockCall(r[DottedName](name))
          case UnlockCall(name)                      => UnlockCall(r[DottedName](name))
          case ValidCall(name)                       => ValidCall(r[DottedName](name))
          case WriteCall(name, args)                 => WriteCall(r[DottedName](name), args map r[Expr])
          case BinaryOp(lhs, op, rhs)                => BinaryOp(r[Expr](lhs), op, r[Expr](rhs))
          case UnaryOp(op, lhs)                      => UnaryOp(op, r[Expr](lhs))
          case Bracket(content)                      => Bracket(r[Expr](content))
          case TernaryOp(cond, lhs, rhs)             => TernaryOp(r[Expr](cond), r[Expr](lhs), r[Expr](rhs))
          case BitRep(count, value)                  => BitRep(r[Expr](count), r[Expr](value))
          case BitCat(parts)                         => BitCat(parts map r[Expr])
          case x: DottedName                         => x
          case x: Literal                            => x
          case x: Num                                => x
        }
      }
      v.asInstanceOf[E]
    }

    r[T](this)
  }

  // Recurse through the tree and collect results of pf for all nodes where it is defined.
  // This function is analogous to List.collect in the standard library
  def collect[E](pf: PartialFunction[Node, E]): List[E] = {

    val f = pf.lift

    def c(node: Node): List[E] = {
      val headOption: Option[E] = f(node)
      val tail: List[E] = node match {
        case Instantiate(_, _, args)                      => args.values.toList flatMap c
        case Connect(start, end)                          => (start :: end) flatMap c
        case Function(_, body)                            => c(body)
        case FenceFunction(body)                          => c(body)
        case FsmTask(_, _, fns, fencefn, vfns, hasnew)    => (fns flatMap c) ::: (fencefn map c getOrElse Nil) ::: (vfns flatMap c)
        case StateTask(_, _, sbs, fencefn, vfns)          => (sbs flatMap c) ::: (fencefn map c getOrElse Nil) ::: (vfns flatMap c)
        case NetworkTask(_, _, inst, conn, vfns, fsms)    => (inst flatMap c) ::: (conn flatMap c) ::: (vfns flatMap c) ::: (fsms flatMap c)
        case VerilogTask(_, _, vfns)                      => vfns flatMap c
        case ArrayLookup(name, index)                     => c(name) ::: (index flatMap c)
        case Slice(ref, l, _, r)                          => c(ref) ::: c(l) ::: c(r)
        case CallExpr(name, args)                         => c(name) ::: (args flatMap c)
        case Zxt(numbits, expr)                           => c(numbits) ::: c(expr)
        case Sxt(numbits, expr)                           => c(numbits) ::: c(expr)
        case DollarCall(name, args)                       => args flatMap c
        case ReadCall(name)                               => c(name)
        case LockCall(name)                               => c(name)
        case UnlockCall(name)                             => c(name)
        case ValidCall(name)                              => c(name)
        case WriteCall(name, args)                        => c(name) ::: (args flatMap c)
        case Assign(lhs, rhs)                             => c(lhs) ::: c(rhs)
        case Update(lhs, op, rhs)                         => c(lhs) ::: c(rhs)
        case Plusplus(lhs)                                => c(lhs)
        case Minusminus(lhs)                              => c(lhs)
        case BinaryOp(lhs, op, rhs)                       => c(lhs) ::: c(rhs)
        case UnaryOp(op, lhs)                             => c(lhs)
        case Bracket(content)                             => c(content)
        case TernaryOp(cond, lhs, rhs)                    => c(cond) ::: c(lhs) ::: c(rhs)
        case CombinatorialBlock(cmds)                     => cmds flatMap c
        case StateBlock(state, cmds)                      => cmds flatMap c
        case DeclarationStmt(VarDeclaration(_, _, init))  => init map c getOrElse Nil
        case CombinatorialIf(cond, t, e)                  => c(cond) ::: c(t) ::: (e map c getOrElse Nil)
        case BitRep(count, value)                         => c(count) ::: c(value)
        case BitCat(parts)                                => parts flatMap c
        case _: AlogicComment                             => Nil
        case CombinatorialCaseStmt(value, cases, default) => c(value) ::: (cases flatMap c) ::: (default map c getOrElse Nil)
        case CombinatorialCaseLabel(cond, body)           => (cond flatMap c) ::: c(body)
        case ControlCaseStmt(value, cases, default)       => c(value) ::: (cases flatMap c) ::: (default map c getOrElse Nil)
        case ControlCaseLabel(cond, body)                 => (cond flatMap c) ::: c(body)
        case ControlIf(cond, t, e)                        => c(cond) ::: c(t) ::: (e map c getOrElse Nil)
        case ControlBlock(cmds)                           => cmds flatMap c
        case ControlLoop(body)                            => c(body)
        case ControlWhile(cond, body)                     => c(cond) ::: (body flatMap c)
        case ControlFor(init, cond, incr, body)           => c(init) ::: c(cond) ::: c(incr) ::: (body flatMap c)
        case ControlDo(cond, body)                        => c(cond) ::: (body flatMap c)
        case FenceStmt                                    => Nil
        case BreakStmt                                    => Nil
        case ReturnStmt                                   => Nil
        case PipelineRead                                 => Nil
        case PipelineWrite                                => Nil
        case _: GotoStmt                                  => Nil
        case _: GotoState                                 => Nil
        case DottedName(names)                            => Nil
        case Literal(_)                                   => Nil
        case Num(_, _, _)                                 => Nil
        case VerilogFunction(_)                           => Nil
        case ExprStmt(expr)                               => c(expr)
        case _: CallStmt                                  => Nil
        case _: CallState                                 => Nil
        case ReturnState                                  => Nil
      }

      headOption match {
        case Some(head) => head :: tail
        case None       => tail
      }
    }

    c(this)
  }
}

object NodeOps {
  // NodeOpsDispatcher is used for methods that need to return the same
  // Node subtype as they are called on.
  implicit class NodeOpsDispatcher[T <: Node](val node: T) extends AnyVal {
    def rewrite(callback: PartialFunction[Node, Node]): T = node._rewrite[T](callback)
  }
}
