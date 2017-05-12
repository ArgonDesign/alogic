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

  // Recurse through the tree and apply function to all nodes in pre-order
  def VisitAST(tree: AlogicAST)(callback: AlogicAST => Unit): Unit = {
    def visitCaseLabel(label: CaseLabel): Unit = VisitAST(label)(callback)

    def visit(tree: AlogicAST): Unit = {
      callback(tree)
      tree match {
        case Function(name, body)                  => visit(body)
        case FenceFunction(body)                   => visit(body)
        case Task(tasktype, name, decls, fns)      => fns foreach visit
        case ArrayLookup(name, index)              => { visit(name); visit(index) }
        case BinaryArrayLookup(name, lhs, op, rhs) => { visit(name); visit(lhs); visit(rhs) }
        case FunCall(name, args)                   => { visit(name); args foreach visit }
        case Zxt(numbits, expr)                    => { visit(numbits); visit(expr) }
        case Sxt(numbits, expr)                    => { visit(numbits); visit(expr) }
        case DollarCall(name, args)                => args foreach visit
        case ReadCall(name, args)                  => args foreach visit
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
        case CombinatorialCaseStmt(value, cases)   => { visit(value); cases foreach visitCaseLabel }
        case Define()                              =>
        case Typedef()                             =>
        case Program(cmds)                         => cmds foreach visit
        case ControlCaseStmt(value, cases)         => { visit(value); cases foreach visitCaseLabel }
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
      }
    }

    visit(tree)
  }

  def VisitAST(tree: CaseLabel)(callback: AlogicAST => Unit): Unit = {
    def visit(node: AlogicAST): Unit = VisitAST(node)(callback)
    tree match {
      case ControlCaseLabel(cond, body)       => { cond foreach visit; visit(body) }
      case CombinatorialCaseLabel(cond, body) => { cond foreach visit; visit(body) }
    }
  }
}
