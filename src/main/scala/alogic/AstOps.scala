// This file contains some useful functions for manipulating the abstract syntax tree

package alogic

object AstOps {
  def ExtractName(tree: AlogicAST): String = tree match {
    case DottedName(ns)                => ns.head
    case ArrayLookup(a, _)             => ExtractName(a)
    case BinaryArrayLookup(a, _, _, _) => ExtractName(a)
    case _                             => "Unknown"
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
  def is_control_stmt(cmd: AlogicAST): Boolean = cmd match {
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
    case FunCall(_, _)                   => true
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
        case Struct(fields) => fields foreach {
          case Field(t, n) =>
            visit(t, StrList(name :: Str("_") :: Str(n) :: Nil))
        }
        case _ => ()
      }
    }
    visit(typ, name)
  }

  implicit class AlogicAstWrapper[T <: AlogicAST](val tree: T) extends AnyVal {
    // Recurse through the tree and apply function to all nodes in pre-order
    // Callback returns true to continue recursing, or false to stop processing children
    def visit(callback: AlogicAST => Boolean): Unit = {

      def v(node: AlogicAST): Unit = {
        if (callback(node))
          node match {
            case Instantiate(_, _, args)                       => args foreach v
            case Connect(start, end)                           => { v(start); end foreach v }
            case Function(name, body)                          => v(body)
            case FenceFunction(body)                           => v(body)
            case FsmTask(name, decls, fns, fencefn, vfns)      => { fns foreach v; fencefn foreach v; vfns foreach v }
            case StateTask(name, decls, fns, fencefn, vfns, n) => { fns foreach v; fencefn foreach v; vfns foreach v }
            case NetworkTask(name, decls, fns)                 => fns foreach v
            case VerilogTask(name, decls, fns)                 => fns foreach v
            case ArrayLookup(name, index)                      => { v(name); v(index) }
            case BinaryArrayLookup(name, lhs, op, rhs)         => { v(name); v(lhs); v(rhs) }
            case FunCall(name, args)                           => { v(name); args foreach v }
            case Zxt(numbits, expr)                            => { v(numbits); v(expr) }
            case Sxt(numbits, expr)                            => { v(numbits); v(expr) }
            case DollarCall(name, args)                        => args foreach v
            case ReadCall(name)                                =>
            case LockCall(name)                                =>
            case UnlockCall(_) | ValidCall(_)                  =>
            case WriteCall(name, args)                         => args foreach v
            case Assign(lhs, rhs)                              => { v(lhs); v(rhs) }
            case Update(lhs, op, rhs)                          => { v(lhs); v(rhs) }
            case Plusplus(lhs)                                 => v(lhs)
            case Minusminus(lhs)                               => v(lhs)
            case BinaryOp(lhs, op, rhs)                        => { v(lhs); v(rhs) }
            case UnaryOp(op, lhs)                              => v(lhs)
            case Bracket(content)                              => v(content)
            case TernaryOp(cond, lhs, rhs)                     => { v(cond); v(lhs); v(rhs) }
            case CombinatorialBlock(cmds)                      => cmds foreach v
            case StateBlock(state, cmds)                       => cmds foreach v
            case DeclarationStmt(decl: VarDeclaration)         => () // TODO should we recurse here?
            case CombinatorialIf(cond, body, Some(e))          => { v(cond); v(body); v(e) }
            case CombinatorialIf(cond, body, None)             => { v(cond); v(body) }
            case BitRep(count, value)                          => { v(count); v(value) }
            case BitCat(parts)                                 => parts foreach v
            case AlogicComment(str)                            =>
            case CombinatorialCaseStmt(value, cases)           => { v(value); cases foreach v }
            case ControlCaseStmt(value, cases)                 => { v(value); cases foreach v }
            case ControlIf(cond, body, Some(e))                => { v(cond); v(body); v(e) }
            case ControlIf(cond, body, None)                   => { v(cond); v(body) }
            case ControlBlock(cmds)                            => cmds foreach v
            case ControlWhile(cond, body)                      => { v(cond); body foreach v }
            case ControlFor(init, cond, incr, body)            => { v(init); v(cond); v(incr); body foreach v }
            case ControlDo(cond, body)                         => { v(cond); body foreach v }
            case FenceStmt                                     =>
            case BreakStmt                                     =>
            case ReturnStmt                                    =>
            case GotoStmt(target: String)                      =>
            case StateStmt(state: Int)                         =>
            case GotoState(state: Int)                         =>
            case DottedName(names)                             =>
            case Literal(_)                                    =>
            case Num(_)                                        =>
            case VerilogFunction(_)                            =>
            case ControlCaseLabel(cond, body)                  => { cond foreach v; v(body) }
            case CombinatorialCaseLabel(cond, body)            => { cond foreach v; v(body) }
          }
      }

      v(tree)
    }

    // Recurse through the tree and apply partial function to all nodes in pre-order
    // Wherever the partial function is defined, the tree is rewritten, otherwise the
    // recursion continues
    def rewrite(callback: PartialFunction[AlogicAST, AlogicAST]): T = {

      val cb = callback.lift

      def r[E <: AlogicAST](node: AlogicAST): E = {
        val v = cb(node) match {
          case Some(x) => x
          case None => node match {
            case Instantiate(a, b, args)                       => Instantiate(a, b, args map r[AlogicAST])
            case Connect(start, end)                           => Connect(r[AlogicAST](start), end map r[AlogicAST])
            case Function(name, body)                          => Function(name, r[AlogicAST](body))
            case FenceFunction(body)                           => FenceFunction(r[AlogicAST](body))
            case FsmTask(name, decls, fns, fencefn, vfns)      => FsmTask(name, decls, fns map r[Function], fencefn map r[FenceFunction], vfns)
            case StateTask(name, decls, fns, fencefn, vfns, n) => StateTask(name, decls, fns map r[Function], fencefn map r[FenceFunction], vfns, n)
            case NetworkTask(name, decls, fns)                 => NetworkTask(name, decls, fns map r[AlogicAST])
            case x: VerilogTask                                => x
            case Assign(lhs, rhs)                              => Assign(r[AlogicAST](lhs), r[AlogicExpr](rhs))
            case Update(lhs, op, rhs)                          => Update(r[AlogicExpr](lhs), op, r[AlogicExpr](rhs))
            case Plusplus(lhs)                                 => Plusplus(r[AlogicExpr](lhs))
            case Minusminus(lhs)                               => Minusminus(r[AlogicExpr](lhs))
            case CombinatorialBlock(cmds)                      => CombinatorialBlock(cmds map r[AlogicAST])
            case StateBlock(state, cmds)                       => StateBlock(state, cmds map r[AlogicAST])
            case x: DeclarationStmt                            => x
            case CombinatorialIf(cond, body, e)                => CombinatorialIf(r[AlogicAST](cond), r[AlogicAST](body), e map r[AlogicAST])
            case x: AlogicComment                              => x
            case CombinatorialCaseStmt(value, cases)           => CombinatorialCaseStmt(r[AlogicAST](value), cases map r[AlogicAST])
            case ControlCaseStmt(value, cases)                 => ControlCaseStmt(r[AlogicAST](value), cases map r[ControlCaseLabel])
            case ControlIf(cond, body, e)                      => ControlIf(cond, r[AlogicAST](body), e map r[AlogicAST])
            case ControlBlock(cmds)                            => ControlBlock(cmds map r[AlogicAST])
            case ControlWhile(cond, body)                      => ControlWhile(r[AlogicExpr](cond), body map r[AlogicAST])
            case ControlFor(init, cond, incr, body)            => ControlFor(r[AlogicAST](init), r[AlogicExpr](cond), r[AlogicAST](incr), body map r[AlogicAST])
            case ControlDo(cond, body)                         => ControlDo(r[AlogicExpr](cond), body map r[AlogicAST])
            case FenceStmt                                     => FenceStmt
            case BreakStmt                                     => BreakStmt
            case ReturnStmt                                    => ReturnStmt
            case x: GotoStmt                                   => x
            case x: StateStmt                                  => x
            case x: GotoState                                  => x
            case x: VerilogFunction                            => x
            case ControlCaseLabel(cond, body)                  => ControlCaseLabel(cond map r[AlogicExpr], r[AlogicAST](body))
            case CombinatorialCaseLabel(cond, body)            => CombinatorialCaseLabel(cond map r[AlogicExpr], r[AlogicAST](body))

            // Expressions
            case ArrayLookup(name, index)                      => ArrayLookup(r[DottedName](name), r[AlogicExpr](index))
            case BinaryArrayLookup(name, lhs, op, rhs)         => BinaryArrayLookup(r[DottedName](name), r[AlogicExpr](lhs), op, r[AlogicExpr](rhs))
            case FunCall(name, args)                           => FunCall(r[DottedName](name), args map r[AlogicExpr])
            case Zxt(numbits, expr)                            => Zxt(r[AlogicExpr](numbits), r[AlogicExpr](expr))
            case Sxt(numbits, expr)                            => Sxt(r[AlogicExpr](numbits), r[AlogicExpr](expr))
            case DollarCall(name, args)                        => DollarCall(name, args map r[AlogicExpr])
            case ReadCall(name)                                => ReadCall(name)
            case LockCall(name)                                => LockCall(name)
            case UnlockCall(name)                              => UnlockCall(name)
            case ValidCall(name)                               => ValidCall(name)
            case WriteCall(name, args)                         => WriteCall(name, args map r[AlogicExpr])
            case BinaryOp(lhs, op, rhs)                        => BinaryOp(r[AlogicExpr](lhs), op, r[AlogicExpr](rhs))
            case UnaryOp(op, lhs)                              => UnaryOp(op, r[AlogicExpr](lhs))
            case Bracket(content)                              => Bracket(r[AlogicExpr](content))
            case TernaryOp(cond, lhs, rhs)                     => TernaryOp(r[AlogicExpr](cond), r[AlogicExpr](lhs), r[AlogicExpr](rhs))
            case BitRep(count, value)                          => BitRep(r[AlogicExpr](count), r[AlogicExpr](value))
            case BitCat(parts)                                 => BitCat(parts map r[AlogicExpr])
            case x: DottedName                                 => x
            case x: Literal                                    => x
            case x: Num                                        => x
          }
        }
        v.asInstanceOf[E]
      }

      r[T](tree)
    }
  }
}
