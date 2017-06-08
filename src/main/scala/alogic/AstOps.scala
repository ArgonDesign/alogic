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

  // Recurse through the tree and apply function to all nodes in pre-order
  // Callback returns true to continue recursing, or false to stop processing children
  def VisitAST(tree: AlogicAST)(callback: AlogicAST => Boolean): Unit = {

    def visit(tree: AlogicAST): Unit = {
      if (callback(tree))
        tree match {
          case Instantiate(_, _, args)               => args foreach visit
          case Connect(start, end)                   => { visit(start); end foreach visit }
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
          case UnlockCall(_) | ValidCall(_)          =>
          case WriteCall(name, args)                 => args foreach visit
          case Assign(lhs, rhs)                      => { visit(lhs); visit(rhs) }
          case Update(lhs, op, rhs)                  => { visit(lhs); visit(rhs) }
          case Plusplus(lhs)                         => visit(lhs)
          case Minusminus(lhs)                       => visit(lhs)
          case BinaryOp(lhs, op, rhs)                => { visit(lhs); visit(rhs) }
          case UnaryOp(op, lhs)                      => visit(lhs)
          case Bracket(content)                      => visit(content)
          case TernaryOp(cond, lhs, rhs)             => { visit(cond); visit(lhs); visit(rhs) }
          case CombinatorialBlock(cmds)              => cmds foreach visit
          case StateBlock(state, cmds)               => cmds foreach visit
          case DeclarationStmt(decl: VarDeclaration) => () // TODO should we recurse here?
          case CombinatorialIf(cond, body, Some(e))  => { visit(cond); visit(body); visit(e) }
          case CombinatorialIf(cond, body, None)     => { visit(cond); visit(body) }
          case BitRep(count, value)                  => { visit(count); visit(value) }
          case BitCat(parts)                         => parts foreach visit
          case AlogicComment(str)                    =>
          case CombinatorialCaseStmt(value, cases)   => { visit(value); cases foreach visit }
          case Program(cmds)                         => cmds foreach visit
          case ControlCaseStmt(value, cases)         => { visit(value); cases foreach visit }
          case ControlIf(cond, body, Some(e))        => { visit(cond); visit(body); visit(e) }
          case ControlIf(cond, body, None)           => { visit(cond); visit(body) }
          case ControlBlock(cmds)                    => cmds foreach visit
          case ControlWhile(cond, body)              => { visit(cond); body foreach visit }
          case ControlFor(init, cond, incr, body)    => { visit(init); visit(cond); visit(incr); body foreach visit }
          case ControlDo(cond, body)                 => { visit(cond); body foreach visit }
          case FenceStmt                             =>
          case BreakStmt                             =>
          case ReturnStmt                            =>
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
  def RewriteAST[T <: AlogicAST](tree: T)(callback: PartialFunction[AlogicAST, AlogicAST]): T = {

    val cb = callback.lift

    def rewrite[E <: AlogicAST](tree: AlogicAST): E = {
      val v = cb(tree) match {
        case Some(x) => x
        case None => tree match {
          case Instantiate(a, b, args)               => Instantiate(a, b, args map rewrite[AlogicAST])
          case Connect(start, end)                   => Connect(rewrite[AlogicAST](start), end map rewrite[AlogicAST])
          case Function(name, body)                  => Function(name, rewrite[AlogicAST](body))
          case FenceFunction(body)                   => FenceFunction(rewrite[AlogicAST](body))
          case Task(tasktype, name, decls, fns)      => Task(tasktype, name, decls, fns map rewrite[AlogicAST])
          case Assign(lhs, rhs)                      => Assign(rewrite[AlogicAST](lhs), rewrite[AlogicExpr](rhs))
          case Update(lhs, op, rhs)                  => Update(rewrite[AlogicExpr](lhs), op, rewrite[AlogicExpr](rhs))
          case Plusplus(lhs)                         => Plusplus(rewrite[AlogicExpr](lhs))
          case Minusminus(lhs)                       => Minusminus(rewrite[AlogicExpr](lhs))
          case CombinatorialBlock(cmds)              => CombinatorialBlock(cmds map rewrite[AlogicAST])
          case StateBlock(state, cmds)               => StateBlock(state, cmds map rewrite[AlogicAST])
          case x: DeclarationStmt                    => x
          case CombinatorialIf(cond, body, e)        => CombinatorialIf(rewrite[AlogicAST](cond), rewrite[AlogicAST](body), e map rewrite[AlogicAST])
          case x: AlogicComment                      => x
          case CombinatorialCaseStmt(value, cases)   => CombinatorialCaseStmt(rewrite[AlogicAST](value), cases map rewrite[AlogicAST])
          case Program(cmds)                         => Program(cmds map rewrite[AlogicAST])
          case ControlCaseStmt(value, cases)         => ControlCaseStmt(rewrite[AlogicAST](value), cases map rewrite[AlogicAST])
          case ControlIf(cond, body, e)              => ControlIf(cond, rewrite[AlogicAST](body), e map rewrite[AlogicAST])
          case ControlBlock(cmds)                    => ControlBlock(cmds map rewrite[AlogicAST])
          case ControlWhile(cond, body)              => ControlWhile(rewrite[AlogicExpr](cond), body map rewrite[AlogicAST])
          case ControlFor(init, cond, incr, body)    => ControlFor(rewrite[AlogicAST](init), rewrite[AlogicExpr](cond), rewrite[AlogicAST](incr), body map rewrite[AlogicAST])
          case ControlDo(cond, body)                 => ControlDo(rewrite[AlogicExpr](cond), body map rewrite[AlogicAST])
          case FenceStmt                             => FenceStmt
          case BreakStmt                             => BreakStmt
          case ReturnStmt                            => ReturnStmt
          case x: GotoStmt                           => x
          case StateProgram(cmds, numStates)         => StateProgram(cmds map rewrite[AlogicAST], numStates)
          case x: StateStmt                          => x
          case x: GotoState                          => x
          case x: VerilogFunction                    => x
          case ControlCaseLabel(cond, body)          => ControlCaseLabel(cond map rewrite[AlogicExpr], rewrite[AlogicAST](body))
          case CombinatorialCaseLabel(cond, body)    => CombinatorialCaseLabel(cond map rewrite[AlogicExpr], rewrite[AlogicAST](body))

          case ArrayLookup(name, index)              => ArrayLookup(rewrite[AlogicAST](name), rewrite[AlogicExpr](index))
          case BinaryArrayLookup(name, lhs, op, rhs) => BinaryArrayLookup(rewrite[AlogicAST](name), rewrite[AlogicExpr](lhs), op, rewrite[AlogicExpr](rhs))
          case FunCall(name, args)                   => FunCall(rewrite[AlogicAST](name), args map rewrite[AlogicExpr])
          case Zxt(numbits, expr)                    => Zxt(rewrite[AlogicExpr](numbits), rewrite[AlogicExpr](expr))
          case Sxt(numbits, expr)                    => Sxt(rewrite[AlogicExpr](numbits), rewrite[AlogicExpr](expr))
          case DollarCall(name, args)                => DollarCall(name, args map rewrite[AlogicExpr])
          case ReadCall(name)                        => ReadCall(name)
          case LockCall(name)                        => LockCall(name)
          case UnlockCall(name)                      => UnlockCall(name)
          case ValidCall(name)                       => ValidCall(name)
          case WriteCall(name, args)                 => WriteCall(name, args map rewrite[AlogicExpr])
          case BinaryOp(lhs, op, rhs)                => BinaryOp(rewrite[AlogicExpr](lhs), op, rewrite[AlogicExpr](rhs))
          case UnaryOp(op, lhs)                      => UnaryOp(op, rewrite[AlogicExpr](lhs))
          case Bracket(content)                      => Bracket(rewrite[AlogicExpr](content))
          case TernaryOp(cond, lhs, rhs)             => TernaryOp(rewrite[AlogicExpr](cond), rewrite[AlogicExpr](lhs), rewrite[AlogicExpr](rhs))
          case BitRep(count, value)                  => BitRep(rewrite[AlogicExpr](count), rewrite[AlogicExpr](value))
          case BitCat(parts)                         => BitCat(parts map rewrite[AlogicExpr])
          case x: DottedName                         => x
          case x: Literal                            => x
          case x: Num                                => x
        }
      }
      v.asInstanceOf[E]
    }

    rewrite[T](tree)
  }
}
