////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Peter de Rivaz/Geza Lore
//
// DESCRIPTION:
//
// Pretty printers for Tree nodes
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.ast.Trees._

//trait DeclarationPrettyPrintOps { this: Decl =>
//
//  def toSource: String = this match {
//    case DeclVar(kind, id, Some(init)) => s"${kind.toSource} ${id} = ${init.toSource}"
//    case DeclVar(kind, id, None)       => s"${kind.toSource} ${id}"
//    case DeclArr(kind, id, dims) =>
//      s"${kind.toSource} ${id}${dims map { _.toSource } mkString ("[", "][", "]")}"
//    case DeclParam(kind, id, init) => s"param ${kind.toSource} ${id} = ${init.toSource}"
//    case DeclConst(kind, id, init) => s"const ${kind.toSource} ${id} = ${init.toSource}"
//    case DeclVerilogVar(kind, id)  => s"verilog ${kind.toSource} ${id}"
//    case DeclVerilogArr(kind, id, dims) =>
//      s"verilog ${kind.toSource} ${id}${id}${dims map { _.toSource } mkString ("[", "][", "]")}"
//    case DeclOut(kind, id, fctype, stype) =>
//      s"out ${fctype.toSource} ${stype.toSource} ${kind.toSource} ${id}"
//    case DeclIn(kind, id, fctype) => s"in ${fctype.toSource} ${kind.toSource} ${id}"
//    case DeclPippeVar(kind, id)   => s"pipeline ${kind.toSource} ${id}"
//  }
//}
//
//trait FlowControlTypePrettyPrintOps { this: FlowControlType =>
//
//  def toSource: String = this match {
//    case FlowControlTypeNone   => ""
//    case FlowControlTypeValid  => "sync"
//    case FlowControlTypeReady  => "sync ready"
//    case FlowControlTypeAccept => "sync accept"
//  }
//}
//
//trait StorageTypePrettyPrintOps { this: StorageType =>
//
//  def toSource: String = this match {
//    case StorageTypeWire   => "wire"
//    case StorageTypeBubble => "bubble"
//    case StorageTypeReg    => ""
//  }
//}
//
//trait TypePrettyPrintOps { this: Type =>
//
//  def toSource: String = this match {
//    case IntType(true, size)   => s"i${size}"
//    case IntType(false, size)  => s"u${size}"
//    case IntVType(true, args)  => s"int(${args map (_.toSource) mkString ", "})"
//    case IntVType(false, args) => s"uint(${args map (_.toSource) mkString ", "})"
//    case Struct(name, _)       => s"struct $name"
//    case VoidType              => "void"
//  }
//}

trait TreePrintOps { this: Tree =>

  private[this] final def toSource(expr: Expr): String = expr match {

    case ExprCall(expr, args)     => s"${toSource(expr)}(${args map toSource mkString ", "})"
    case ExprUnary(op, expr)      => s"${op}${toSource(expr)}"
    case ExprBinary(lhs, op, rhs) => s"${toSource(lhs)} $op ${toSource(rhs)}"
    case ExprTernary(cond, thenExpr, elseExpr) =>
      s"${toSource(cond)} ? ${toSource(thenExpr)} : ${toSource(elseExpr)}"
    case ExprRep(count, expr)   => s"{${toSource(count)}{${toSource(expr)}}}"
    case ExprCat(parts)         => s"{${parts map toSource mkString ", "}}"
    case ExprIndex(expr, index) => s"${toSource(expr)}[${toSource(index)}]"
    case ExprSlice(expr, lidx, op, ridx) =>
      s"${toSource(expr)}[${toSource(lidx)}${op}${toSource(ridx)}]"
    case ExprSelect(expr, selector)   => s"${toSource(expr)}.${selector}"
    case ExprRef(Ident(name))         => name
    case ExprRef(Sym(symbol))         => symbol.denot.name.str
    case ExprType(kind)               => s"type(${kind.toSource})"
    case ExprInt(true, width, value)  => s"${width}'sd${value}"
    case ExprInt(false, width, value) => s"${width}'d${value}"
    case ExprNum(true, value)         => s"${value}"
    case ExprNum(false, value)        => s"'d${value}"
    case ExprStr(value)               => s""""${value}""""
    case ExprError()                  => "/* Error expression */"
  }

  private[this] final def toSource(stmt: Stmt): String = stmt match {
    case StmtBlock(body)                  => ???
    case StmtIf(cond, thenStmt, elseStmt) => ???
    case StmtCase(expr, cases, default)   => ???
    case StmtLoop(body)                   => ???
    case StmtWhile(cond, body)            => ???
    case StmtFor(inits, cond, step, body) => ???
    case StmtDo(cond, body)               => ???
    case StmtLet(inits, body)             => ???
    case StmtFence()                      => "fence;"
    case StmtBreak()                      => "break;"
    case StmtGoto(ref)                    => s"goto ${ref.toSource};"
    case StmtReturn()                     => "return;"
    case StmtAssign(lhs, rhs)             => s"${lhs.toSource} = ${rhs.toSource};"
    case StmtUpdate(lhs, op, rhs)         => s"${lhs.toSource} ${op}= ${rhs.toSource};"
    case StmtPost(expr, op)               => s"${expr.toSource}${op};"
    case StmtExpr(expr)                   => s"${expr.toSource};"
    case StmtDecl(decl) => {
      val init = decl.init map { "= " + _.toSource } getOrElse ""
      s"${decl.kind.toSource} ${decl.ref.toSource}${init};"
    }
    case StmtRead()             => "read;"
    case StmtWrite()            => "write;"
    case StmtDollarComment(str) => "$" + s"""("${str}")"""
    case StmtError()            => "/* Error statement */"
  }

  //        case DeclarationStmt(_, decl)         => s"${decl.toSource};"
  //
  //        case CombinatorialBlock(_, cmds) =>
  //          s"""|{
  //              |${i}  ${cmds map v(indent + 1) mkString s"\n${i}  "}
  //              |${i}}""".stripMargin
  //        case ControlBlock(_, cmds) =>
  //          s"""|{
  //              |${i}  ${cmds map v(indent + 1) mkString s"\n${i}  "}
  //              |${i}}""".stripMargin
  //
  //        case StateBlock(_, state, cmds) =>
  //          s"""|@state ${state} {
  //              |${i}  ${cmds map v(indent + 1) mkString s"\n${i}  "}
  //              |${i}}""".stripMargin
  //
  //        case CombinatorialIf(_, cond, body, Some(e)) =>
  //          s"if (${v(indent)(cond)}) ${v(indent)(body)} else ${v(indent)(e)}"
  //        case CombinatorialIf(_, cond, body, None) => s"if (${v(indent)(cond)}) ${v(indent)(body)}"
  //        case ControlIf(_, cond, body, Some(e)) =>
  //          s"if (${v(indent)(cond)}) ${v(indent)(body)} else ${v(indent)(e)}"
  //        case ControlIf(_, cond, body, None) => s"if (${v(indent)(cond)}) ${v(indent)(body)}"
  //
  //        case CombinatorialCaseStmt(_, value, cases, None) =>
  //          s"""|case (${v(indent)(value)}) {
  //              |${i}  ${cases map v(indent + 1) mkString s"\n${i}  "}
  //              |${i}}""".stripMargin
  //        case CombinatorialCaseStmt(_, value, cases, Some(default)) =>
  //          s"""|case (${v(indent)(value)}) {
  //              |${i}  ${cases map v(indent + 1) mkString s"\n${i}  "}
  //              |${i}  default: ${v(indent + 1)(default)}
  //              |${i}}""".stripMargin
  //        case ControlCaseStmt(_, value, cases, None) =>
  //          s"""|case (${v(indent)(value)}) {
  //              |${i}  ${cases map v(indent + 1) mkString s"\n${i}  "}
  //              |${i}}""".stripMargin
  //        case ControlCaseStmt(_, value, cases, Some(default)) =>
  //          s"""|case (${v(indent)(value)}) {
  //              |${i}  ${cases map v(indent + 1) mkString s"\n${i}  "}
  //              |${i}  default: ${v(indent + 1)(default)}
  //              |${i}}""".stripMargin
  //
  //        case ControlCaseLabel(_, Nil, body)       => s"default : ${v(indent)(body)}"
  //        case CombinatorialCaseLabel(_, Nil, body) => s"default : ${v(indent)(body)}"
  //        case ControlCaseLabel(_, cond, body) =>
  //          s"${cond map v(indent) mkString ", "} : ${v(indent)(body)}"
  //        case CombinatorialCaseLabel(_, cond, body) =>
  //          s"${cond map v(indent) mkString ", "} : ${v(indent)(body)}"
  //
  //        case ControlLoop(_, ControlBlock(_, body)) =>
  //          s"""|loop {
  //              |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
  //              |${i}}""".stripMargin
  //        case ControlWhile(_, cond, body) =>
  //          s"""|while (${v(indent)(cond)}) {
  //              |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
  //              |${i}}""".stripMargin
  //        case ControlFor(_, init, cond, incr, body) =>
  //          s"""|for (${v(indent)(init)} ; ${v(indent)(cond)} ; ${v(indent)(incr)}) {
  //              |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
  //              |${i}}""".stripMargin
  //        case ControlDo(_, cond, body) =>
  //          s"""|do {
  //              |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
  //              |${i}} while (${v(indent)(cond)});""".stripMargin
  //
  //        case ExprStmt(_, expr) => s"${expr.toSource};"
  //        case CallStmt(_, name) => s"$name();"
  //
  //        case _: FenceStmt           => "fence;"
  //        case _: BreakStmt           => "break;"
  //        case _: ReturnStmt          => "return;"
  //        case GotoStmt(_, target)    => s"goto $target;"
  //        case GotoState(_, tgt)      => s"goto state ${tgt};"
  //        case CallState(_, tgt, ret) => s"@push state ${tgt} ${ret}"
  //        case _: ReturnState         => "@pop state"
  //        case AlogicComment(_, str)  => s"TODO: AlogicComment(str)"
  //
  //        case _: ErrorStmt => "/*Error statement*/"

  def toSource: String = this match {
    case expr: Expr => toSource(expr)
    case stmt: Stmt => toSource(stmt)
    case other      => ???
  }
//
//    def v(indent: Int)(node: Node): String = {
//      val i = "  " * indent
//      node match {
//        case expr: Expr => visitExpr(expr)
//        case lval: LVal => visitLVal(lval)
//
//        case Instantiate(_, id, module, args) => {
//          val pas = for ((lhs, rhs) <- args.toList) yield { s"${lhs} = ${v(indent)(rhs)}" }
//          s"${i}new $id  = ${module}(${pas mkString ", "});\n"
//        }
//        case Connect(_, lhs, rhs)     => s"$lhs -> ${rhs map v(indent) mkString ", "}\n"
//        case Function(_, name, body)  => s"void $name() ${v(indent)(body)}"
//        case FenceFunction(_, body)   => s"void fence() ${v(indent)(body)}"
//        case VerilogFunction(_, body) => s"void verilog() {$body}"
//        case FsmTask(_, name, decls, fns, fencefn, vfns) =>
//          s"""|fsm $name {
//              |${i}  /////////////////////////////////
//              |${i}  // Declarations
//              |${i}  /////////////////////////////////
//              |
//              |${i}  ${decls map (_.toSource + ";") mkString s"\n${i}  "}
//              |
//              |${i}  /////////////////////////////////
//              |${i}  // Fence function
//              |${i}  /////////////////////////////////
//              |
//              |${i}  ${if (fencefn.isDefined) v(indent + 1)(fencefn.get) else "// None"}
//              |
//              |${i}  /////////////////////////////////
//              |${i}  // Functions
//              |${i}  /////////////////////////////////
//              |
//              |${i}  ${fns map v(indent + 1) mkString s"\n\n${i}  "}
//              |
//              |${i}  /////////////////////////////////
//              |${i}  // Verilog functions
//              |${i}  /////////////////////////////////
//              |
//              |${i}  ${vfns map v(indent + 1) mkString s"\n\n${i}  "}
//              |
//              |${i}}""".stripMargin
//
//        case StateTask(_, name, decls, sbs, fencefn, vfns) =>
//          s"""|@StateTask $name {
//              |${i}  /////////////////////////////////
//              |${i}  // Declarations
//              |${i}  /////////////////////////////////
//              |
//              |${i}  ${decls map (_.toSource + ";") mkString s"\n${i}  "}
//              |
//              |${i}  /////////////////////////////////
//              |${i}  // Fence function
//              |${i}  /////////////////////////////////
//              |
//              |${i}  ${if (fencefn.isDefined) v(indent + 1)(fencefn.get) else "// None"}
//              |
//              |${i}  /////////////////////////////////
//              |${i}  // States
//              |${i}  /////////////////////////////////
//              |
//              |${i}  ${sbs map v(indent + 1) mkString s"\n\n${i}  "}
//              |
//              |${i}  /////////////////////////////////
//              |${i}  // Verilog functions
//              |${i}  /////////////////////////////////
//              |
//              |${i}  ${vfns map v(indent + 1) mkString s"\n${i}  "}
//              |
//              |${i}}""".stripMargin
//
//        case NetworkTask(_, name, decls, inst, conn, vfns, fsms) =>
//          s"TODO: NetworkTask(name, decls, fns)"
//        case VerilogTask(_, name, decls, fns) => s"TODO: VerilogTask(name, decls, fns)"

//
//      }
//    }
//
//    v(0)(this)
//  }
}
