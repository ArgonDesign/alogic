// This file contains some useful functions for manipulating the abstract syntax tree

package alogic.ast

import alogic.Str
import alogic.StrList
import alogic.StrTree

object AstOps {
  def ExtractName(tree: Node): String = tree match {
    case DottedName(ns)    => ns.head
    case ArrayLookup(a, _) => ExtractName(a)
    case _                 => "Unknown"
  }

  def ExtractName(tree: Declaration): String = tree match {
    case VarDeclaration(_, id, _)   => ExtractName(id)
    case ConstDeclaration(_, id, _) => id
    case ParamDeclaration(_, id, _) => id
    case VerilogDeclaration(_, id)  => ExtractName(id)
    case OutDeclaration(_, _, name) => name
    case InDeclaration(_, _, name)  => name
    case _                          => "Unknown"
  }

  // Does this statement translate to a goto?
  def is_control_stmt(cmd: Stmt): Boolean = cmd match {
    case FenceStmt                       => true
    case BreakStmt                       => true
    case ReturnStmt                      => true
    case GotoStmt(target)                => true
    case ControlBlock(s)                 => true
    case ControlIf(cond, body, elsebody) => true
    case ControlWhile(cond, body)        => true
    case ControlFor(_, _, _, _)          => true
    case ControlDo(_, _)                 => true
    case ControlCaseStmt(_, _)           => true
    case CallStmt(_)                     => true
    case _                               => false
  }

  // Does this sync type contain a valid line?
  def HasValid(s: SyncType): Boolean = s match {
    case Wire => false
    case _    => true
  }

  // Does this sync type contain a ready line?
  def HasReady(s: SyncType): Boolean = s match {
    case SyncReadyBubble => true
    case SyncReady       => true
    case _               => false
  }

  // Does this sync type contain an accept line?
  def HasAccept(s: SyncType): Boolean = s match {
    case SyncAccept     => true
    case WireSyncAccept => true
    case _              => false
  }

  // Call VisitType to get callback for each node
  def VisitType(typ: AlogicType, name: StrTree)(callback: (AlogicType, StrTree) => Unit): Unit = {

    def visit(typ: AlogicType, name: StrTree): Unit = {
      callback(typ, name)
      typ match {
        case Struct(fields) => for ((n, t) <- fields) {
          visit(t, StrList(name :: Str("_") :: Str(n) :: Nil))
        }
        case _ => ()
      }
    }
    visit(typ, name)
  }

  implicit class AlogicAstWrapper[T <: Node](val tree: T) extends AnyVal {
    // Recurse through the tree and apply function to all nodes in pre-order
    // Callback returns true to continue recursing, or false to stop processing children
    def visit(callback: Node => Boolean): Unit = {

      def v(node: Node): Unit = {
        if (callback(node))
          node match {
            case Instantiate(_, _, args)                    => args foreach v
            case Connect(start, end)                        => { v(start); end foreach v }
            case Function(name, body)                       => v(body)
            case FenceFunction(body)                        => v(body)
            case FsmTask(name, decls, fns, fencefn, vfns)   => { fns foreach v; fencefn foreach v; vfns foreach v }
            case StateTask(name, decls, sbs, fencefn, vfns) => { sbs foreach v; fencefn foreach v; vfns foreach v }
            case NetworkTask(name, decls, fns)              => fns foreach v
            case VerilogTask(name, decls, fns)              => fns foreach v
            case ArrayLookup(name, index)                   => { v(name); index foreach v }
            case Slice(ref, l, op, r)                       => { v(ref); v(l); v(r) }
            case CallExpr(name, args)                       => { v(name); args foreach v }
            case Zxt(numbits, expr)                         => { v(numbits); v(expr) }
            case Sxt(numbits, expr)                         => { v(numbits); v(expr) }
            case DollarCall(name, args)                     => args foreach v
            case ReadCall(name)                             =>
            case LockCall(name)                             =>
            case UnlockCall(_) | ValidCall(_)               =>
            case WriteCall(name, args)                      => args foreach v
            case Assign(lhs, rhs)                           => { v(lhs); v(rhs) }
            case Update(lhs, op, rhs)                       => { v(lhs); v(rhs) }
            case Plusplus(lhs)                              => v(lhs)
            case Minusminus(lhs)                            => v(lhs)
            case BinaryOp(lhs, op, rhs)                     => { v(lhs); v(rhs) }
            case UnaryOp(op, lhs)                           => v(lhs)
            case Bracket(content)                           => v(content)
            case TernaryOp(cond, lhs, rhs)                  => { v(cond); v(lhs); v(rhs) }
            case CombinatorialBlock(cmds)                   => cmds foreach v
            case StateBlock(state, cmds)                    => cmds foreach v
            case DeclarationStmt(decl: VarDeclaration)      => () // TODO should we recurse here?
            case CombinatorialIf(cond, body, Some(e))       => { v(cond); v(body); v(e) }
            case CombinatorialIf(cond, body, None)          => { v(cond); v(body) }
            case BitRep(count, value)                       => { v(count); v(value) }
            case BitCat(parts)                              => parts foreach v
            case AlogicComment(str)                         =>
            case CombinatorialCaseStmt(value, cases)        => { v(value); cases foreach v }
            case ControlCaseStmt(value, cases)              => { v(value); cases foreach v }
            case ControlIf(cond, body, Some(e))             => { v(cond); v(body); v(e) }
            case ControlIf(cond, body, None)                => { v(cond); v(body) }
            case ControlBlock(cmds)                         => cmds foreach v
            case ControlWhile(cond, body)                   => { v(cond); body foreach v }
            case ControlFor(init, cond, incr, body)         => { v(init); v(cond); v(incr); body foreach v }
            case ControlDo(cond, body)                      => { v(cond); body foreach v }
            case FenceStmt                                  =>
            case BreakStmt                                  =>
            case ReturnStmt                                 =>
            case GotoStmt(target: String)                   =>
            case GotoState(state: Int)                      =>
            case DottedName(names)                          =>
            case Literal(_)                                 =>
            case Num(_)                                     =>
            case VerilogFunction(_)                         =>
            case ControlCaseLabel(cond, body)               => { cond foreach v; v(body) }
            case CombinatorialCaseLabel(cond, body)         => { cond foreach v; v(body) }
            case ExprStmt(expr)                             => v(expr)
            case CallStmt(_)                                =>
            case CallState(_, _)                            =>
            case ReturnState                                =>

          }
      }

      v(tree)
    }

    // Recurse through the tree and apply partial function to all nodes in pre-order
    // Wherever the partial function is defined, the tree is rewritten, otherwise the
    // recursion continues
    def rewrite(callback: PartialFunction[Node, Node]): T = {

      val cb = callback.lift

      def r[E <: Node](node: Node): E = {
        val v = cb(node) match {
          case Some(x) => x
          case None => node match {
            case Instantiate(a, b, args)                    => Instantiate(a, b, args map r[Node])
            case Connect(start, end)                        => Connect(r[Node](start), end map r[Node])
            case Function(name, body)                       => Function(name, r[Stmt](body))
            case FenceFunction(body)                        => FenceFunction(r[Stmt](body))
            case FsmTask(name, decls, fns, fencefn, vfns)   => FsmTask(name, decls, fns map r[Function], fencefn map r[FenceFunction], vfns)
            case StateTask(name, decls, sbs, fencefn, vfns) => StateTask(name, decls, sbs map r[StateBlock], fencefn map r[FenceFunction], vfns)
            case NetworkTask(name, decls, fns)              => NetworkTask(name, decls, fns map r[Node])
            case x: VerilogTask                             => x
            case Assign(lhs, rhs)                           => Assign(r[Node](lhs), r[Expr](rhs))
            case Update(lhs, op, rhs)                       => Update(r[Expr](lhs), op, r[Expr](rhs))
            case Plusplus(lhs)                              => Plusplus(r[Expr](lhs))
            case Minusminus(lhs)                            => Minusminus(r[Expr](lhs))
            case CombinatorialBlock(cmds)                   => CombinatorialBlock(cmds map r[Stmt])
            case StateBlock(state, cmds)                    => StateBlock(state, cmds map r[Stmt])
            case x: DeclarationStmt                         => x
            case CombinatorialIf(cond, body, e)             => CombinatorialIf(r[Node](cond), r[Stmt](body), e map r[Stmt])
            case x: AlogicComment                           => x
            case CombinatorialCaseStmt(value, cases)        => CombinatorialCaseStmt(r[Node](value), cases map r[Node])
            case ControlCaseStmt(value, cases)              => ControlCaseStmt(r[Node](value), cases map r[ControlCaseLabel])
            case ControlIf(cond, body, e)                   => ControlIf(cond, r[Stmt](body), e map r[Stmt])
            case ControlBlock(cmds)                         => ControlBlock(cmds map r[Stmt])
            case ControlWhile(cond, body)                   => ControlWhile(r[Expr](cond), body map r[Stmt])
            case ControlFor(init, cond, incr, body)         => ControlFor(r[Stmt](init), r[Expr](cond), r[Stmt](incr), body map r[Stmt])
            case ControlDo(cond, body)                      => ControlDo(r[Expr](cond), body map r[Stmt])
            case FenceStmt                                  => FenceStmt
            case BreakStmt                                  => BreakStmt
            case ReturnStmt                                 => ReturnStmt
            case x: GotoStmt                                => x
            case x: GotoState                               => x
            case x: VerilogFunction                         => x
            case ControlCaseLabel(cond, body)               => ControlCaseLabel(cond map r[Expr], r[Stmt](body))
            case CombinatorialCaseLabel(cond, body)         => CombinatorialCaseLabel(cond map r[Expr], r[Stmt](body))
            case ExprStmt(expr)                             => ExprStmt(r[Expr](expr))
            case x: CallStmt                                => x
            case x: CallState                               => x
            case ReturnState                                => ReturnState

            // Expressions
            case ArrayLookup(name, index)                   => ArrayLookup(r[DottedName](name), index map r[Expr])
            case Slice(ref, lidx, op, ridx)                 => Slice(r[VarRef](ref), r[Expr](lidx), op, r[Expr](ridx))
            case CallExpr(name, args)                       => CallExpr(r[DottedName](name), args map r[Expr])
            case Zxt(numbits, expr)                         => Zxt(r[Expr](numbits), r[Expr](expr))
            case Sxt(numbits, expr)                         => Sxt(r[Expr](numbits), r[Expr](expr))
            case DollarCall(name, args)                     => DollarCall(name, args map r[Expr])
            case ReadCall(name)                             => ReadCall(name)
            case LockCall(name)                             => LockCall(name)
            case UnlockCall(name)                           => UnlockCall(name)
            case ValidCall(name)                            => ValidCall(name)
            case WriteCall(name, args)                      => WriteCall(name, args map r[Expr])
            case BinaryOp(lhs, op, rhs)                     => BinaryOp(r[Expr](lhs), op, r[Expr](rhs))
            case UnaryOp(op, lhs)                           => UnaryOp(op, r[Expr](lhs))
            case Bracket(content)                           => Bracket(r[Expr](content))
            case TernaryOp(cond, lhs, rhs)                  => TernaryOp(r[Expr](cond), r[Expr](lhs), r[Expr](rhs))
            case BitRep(count, value)                       => BitRep(r[Expr](count), r[Expr](value))
            case BitCat(parts)                              => BitCat(parts map r[Expr])
            case x: DottedName                              => x
            case x: Literal                                 => x
            case x: Num                                     => x
          }
        }
        v.asInstanceOf[E]
      }

      r[T](tree)
    }
  }
}
