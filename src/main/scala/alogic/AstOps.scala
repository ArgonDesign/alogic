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
    case ParamDeclaration(_, id, _) => id
    case VerilogDeclaration(_, id)  => ExtractName(id)
    case OutDeclaration(_, _, name) => name
    case InDeclaration(_, _, name)  => name
    case _                          => "Unknown"
  }

  // Does this statement translate to a goto?
  def is_control_stmt(cmd: AlogicAST): Boolean = cmd match {
    case FenceStmt()                     => true
    case BreakStmt()                     => true
    case ReturnStmt()                    => true
    case GotoStmt(target)                => true
    case ControlBlock(s)                 => true
    case ControlIf(cond, body, elsebody) => true
    case WhileLoop(cond, body)           => true
    case ControlFor(_, _, _, _)          => true
    case ControlDo(_, _)                 => true
    case ControlCaseStmt(_, _)           => true
    case FunCall(_, _)                   => true
    case _                               => false
  }

  // Does this sync type contain a valid line?
  def HasValid(s: SyncType): Boolean = s match {
    case Wire() => false
    case _      => true
  }

  // Does this sync type contain a ready line?
  def HasReady(s: SyncType): Boolean = s match {
    case SyncReadyBubble() => true
    case SyncReady()       => true
    case _                 => false
  }

  // Does this sync type contain an accept line?
  def HasAccept(s: SyncType): Boolean = s match {
    case SyncAccept()     => true
    case WireSyncAccept() => true
    case _                => false
  }

  // Recurse through the tree and apply function to all nodes in pre-order
  // Callback returns true to continue recursing, or false to stop processing children
  def VisitAST(tree: AlogicAST)(callback: AlogicAST => Boolean): Unit = {

    def visit(tree: AlogicAST): Unit = {
      if (callback(tree))
        tree match {
          case Instantiate(_, _, args)               => args foreach visit
          case Connect(start, end)                   => { visit(start); visit(end); }
          case Function(name, body)                  => visit(body)
          case FenceFunction(body)                   => visit(body)
          case Task(tasktype, name, decls, fns)      => fns foreach visit
          case ArrayLookup(name, index)              => { visit(name); visit(index) }
          case BinaryArrayLookup(name, lhs, op, rhs) => { visit(name); visit(lhs); visit(rhs) }
          case FunCall(name, args)                   => { visit(name); args foreach visit }
          case Zxt(numbits, expr)                    => { visit(numbits); visit(expr) }
          case Sxt(numbits, expr)                    => { visit(numbits); visit(expr) }
          case DollarCall(name, args)                => args foreach visit
          case ReadCall(name)                        =>
          case LockCall(name)                        =>
          case UnlockCall(name)                      =>
          case WriteCall(name, args)                 => args foreach visit
          case Assign(lhs, op, rhs)                  => { visit(lhs); visit(rhs) }
          case Plusplus(lhs)                         => visit(lhs)
          case Minusminus(lhs)                       => visit(lhs)
          case BinaryOp(lhs, op, rhs)                => { visit(lhs); visit(rhs) }
          case UnaryOp(op, lhs)                      => visit(lhs)
          case Bracket(content)                      => visit(content)
          case TernaryOp(cond, lhs, rhs)             => { visit(cond); visit(lhs); visit(rhs) }
          case CombinatorialBlock(cmds)              => cmds foreach visit
          case DeclarationStmt(decl: VarDeclaration) => () // TODO should we recurse here?
          case CombinatorialIf(cond, body, Some(e))  => { visit(cond); visit(body); visit(e) }
          case CombinatorialIf(cond, body, None)     => { visit(cond); visit(body) }
          case BitRep(count, value)                  => { visit(count); visit(value) }
          case BitCat(parts)                         => parts foreach visit
          case AlogicComment(str)                    =>
          case CombinatorialCaseStmt(value, cases)   => { visit(value); cases foreach visit }
          case Define()                              =>
          case Typedef()                             =>
          case Program(cmds)                         => cmds foreach visit
          case ControlCaseStmt(value, cases)         => { visit(value); cases foreach visit }
          case ControlIf(cond, body, Some(e))        => { visit(cond); visit(body); visit(e) }
          case ControlIf(cond, body, None)           => { visit(cond); visit(body) }
          case ControlBlock(cmds)                    => cmds foreach visit
          case WhileLoop(cond, body)                 => { visit(cond); visit(body) }
          case ControlFor(init, cond, incr, body)    => { visit(init); visit(cond); visit(incr); body foreach visit }
          case ControlDo(cond, body)                 => { visit(cond); body foreach visit }
          case FenceStmt()                           =>
          case BreakStmt()                           =>
          case ReturnStmt()                          =>
          case GotoStmt(target: String)              =>
          case StateProgram(cmds, numStates)         => cmds foreach visit
          case StateStmt(state: Int)                 =>
          case GotoState(state: Int)                 =>
          case DottedName(names)                     =>
          case Literal(_)                            =>
          case Num(_)                                =>
          case VerilogFunction(_)                    =>
          case ControlCaseLabel(cond, body)          => { cond foreach visit; visit(body) }
          case CombinatorialCaseLabel(cond, body)    => { cond foreach visit; visit(body) }
        }
    }

    visit(tree)
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

  // Recurse through the tree and apply function to all nodes in pre-order
  // Callback returns None to continue recursing, or Some to change the tree
  def RewriteAST(tree: AlogicAST)(callback: AlogicAST => Option[AlogicAST]): AlogicAST = {

    def rewrite(tree: AlogicAST): AlogicAST = {
      callback(tree) match {
        case Some(x) => x
        case None => tree match {
          case Instantiate(a, b, args)                   => Instantiate(a, b, args map rewrite)
          case Connect(start, end)                       => Connect(rewrite(start), rewrite(end))
          case Function(name, body)                      => Function(name, rewrite(body))
          case FenceFunction(body)                       => FenceFunction(rewrite(body))
          case Task(tasktype, name, decls, fns)          => Task(tasktype, name, decls, fns map rewrite)
          case ArrayLookup(name, index)                  => ArrayLookup(rewrite(name), rewrite(index))
          case BinaryArrayLookup(name, lhs, op, rhs)     => BinaryArrayLookup(rewrite(name), rewrite(lhs), op, rewrite(rhs))
          case FunCall(name, args)                       => FunCall(rewrite(name), args map rewrite)
          case Zxt(numbits, expr)                        => Zxt(rewrite(numbits), rewrite(expr))
          case Sxt(numbits, expr)                        => Sxt(rewrite(numbits), rewrite(expr))
          case DollarCall(name, args)                    => DollarCall(name, args map rewrite)
          case ReadCall(name)                            => ReadCall(name)
          case LockCall(name)                            => LockCall(name)
          case UnlockCall(name)                          => UnlockCall(name)
          case WriteCall(name, args)                     => WriteCall(name, args map rewrite)
          case Assign(lhs, op, rhs)                      => Assign(rewrite(lhs), op, rewrite(rhs))
          case Plusplus(lhs)                             => Plusplus(rewrite(lhs))
          case Minusminus(lhs)                           => Minusminus(rewrite(lhs))
          case BinaryOp(lhs, op, rhs)                    => BinaryOp(rewrite(lhs), op, rewrite(rhs))
          case UnaryOp(op, lhs)                          => UnaryOp(op, rewrite(lhs))
          case Bracket(content)                          => Bracket(rewrite(content))
          case TernaryOp(cond, lhs, rhs)                 => TernaryOp(rewrite(cond), rewrite(lhs), rewrite(rhs))
          case CombinatorialBlock(cmds)                  => CombinatorialBlock(cmds map rewrite)
          case x @ DeclarationStmt(decl: VarDeclaration) => x
          case CombinatorialIf(cond, body, Some(e))      => CombinatorialIf(rewrite(cond), rewrite(body), Some(rewrite(e)))
          case CombinatorialIf(cond, body, None)         => CombinatorialIf(rewrite(cond), rewrite(body), None)
          case BitRep(count, value)                      => BitRep(rewrite(count), rewrite(value))
          case BitCat(parts)                             => BitCat(parts map rewrite)
          case x @ AlogicComment(str)                    => x
          case CombinatorialCaseStmt(value, cases)       => CombinatorialCaseStmt(rewrite(value), cases map rewrite)
          case x @ Define()                              => x
          case x @ Typedef()                             => x
          case Program(cmds)                             => Program(cmds map rewrite)
          case ControlCaseStmt(value, cases)             => ControlCaseStmt(rewrite(value), cases map rewrite)
          case ControlIf(cond, body, Some(e))            => ControlIf(rewrite(cond), rewrite(body), Some(rewrite(e)))
          case ControlIf(cond, body, None)               => ControlIf(rewrite(cond), rewrite(body), None)
          case ControlBlock(cmds)                        => ControlBlock(cmds map rewrite)
          case WhileLoop(cond, body)                     => WhileLoop(rewrite(cond), rewrite(body))
          case ControlFor(init, cond, incr, body)        => ControlFor(rewrite(init), rewrite(cond), rewrite(incr), body map rewrite)
          case ControlDo(cond, body)                     => ControlDo(rewrite(cond), body map rewrite)
          case x @ FenceStmt()                           => x
          case x @ BreakStmt()                           => x
          case x @ ReturnStmt()                          => x
          case x @ GotoStmt(target: String)              => x
          case StateProgram(cmds, numStates)             => StateProgram(cmds map rewrite, numStates)
          case x @ StateStmt(state: Int)                 => x
          case x @ GotoState(state: Int)                 => x
          case x @ DottedName(names)                     => x
          case x @ Literal(_)                            => x
          case x @ Num(_)                                => x
          case x @ VerilogFunction(_)                    => x
          case ControlCaseLabel(cond, body)              => ControlCaseLabel(cond map rewrite, rewrite(body))
          case CombinatorialCaseLabel(cond, body)        => CombinatorialCaseLabel(cond map rewrite, rewrite(body))
        }
      }
    }

    rewrite(tree)
  }
}
