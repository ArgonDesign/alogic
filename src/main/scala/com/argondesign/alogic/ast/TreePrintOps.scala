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
import com.argondesign.alogic.core.CompilerContext

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

  private[this] final def v(expr: Expr)(implicit cc: CompilerContext): String = expr match {
    case ExprCall(expr, args)                  => s"${v(expr)}(${args map v mkString ", "})"
    case ExprUnary(op, expr)                   => s"${op}${v(expr)}"
    case ExprBinary(lhs, op, rhs)              => s"${v(lhs)} $op ${v(rhs)}"
    case ExprTernary(cond, thenExpr, elseExpr) => s"${v(cond)} ? ${v(thenExpr)} : ${v(elseExpr)}"
    case ExprRep(count, expr)                  => s"{${v(count)}{${v(expr)}}}"
    case ExprCat(parts)                        => s"{${parts map v mkString ", "}}"
    case ExprIndex(expr, index)                => s"${v(expr)}[${v(index)}]"
    case ExprSlice(expr, lidx, op, ridx)       => s"${v(expr)}[${v(lidx)}${op}${v(ridx)}]"
    case ExprSelect(expr, selector)            => s"${v(expr)}.${selector}"
    case ExprRef(ref)                          => ref.toSource
    case ExprType(kind)                        => s"type(${kind.toSource})"
    case ExprInt(true, width, value)           => s"${width}'sd${value}"
    case ExprInt(false, width, value)          => s"${width}'d${value}"
    case ExprNum(true, value)                  => s"'sd${value}"
    case ExprNum(false, value)                 => s"${value}"
    case ExprStr(value)                        => s""""${value}""""
    case ExprError()                           => "/* Error expression */"
  }

  private[this] def v(indent: Int)(tree: Tree)(implicit cc: CompilerContext): String = {
    val i = "  " * indent
    tree match {
      case expr: Expr => v(expr)

      case Root(typeDefinitions, entity) => {
        s"""|${typeDefinitions map v(indent) mkString s"\n\n${i}"}
            |
            |${i}${v(indent)(entity)}
            |""".stripMargin
      }

      case TypeDefinitionTypedef(ref, kind) => {
        s"typedef ${kind.toSource} ${v(indent)(ref)}"
      }

      case TypeDefinitionStruct(ref, fieldNames, fieldTypes) => {
        val fields = for ((fn, ft) <- fieldNames zip fieldTypes) yield {
          s"${ft.toSource} ${fn};"
        }
        s"""|struct ${v(indent)(ref)} {
            |${i}  ${fields mkString s"\n${i}  "}
            |${i}};""".stripMargin
      }

      case entity @ Entity(ref,
                           declarations,
                           instances,
                           connects,
                           functions,
                           states,
                           fenceStmts,
                           entities,
                           verbatim) => {
        s"""|${entity.variant} ${v(indent)(ref)} {
            |${i}  /////////////////////////////////
            |${i}  // Declarations
            |${i}  /////////////////////////////////
            |
            |${i}  ${declarations map v(indent + 1) mkString s"\n${i}  "}
            |
            |${i}  /////////////////////////////////
            |${i}  // Fence block
            |${i}  /////////////////////////////////
            |
            |${i}  fence {
            |${i}    ${fenceStmts map v(indent + 2) mkString s"\n${i}    "}
            |${i}  }
            |
            |${i}  /////////////////////////////////
            |${i}  // Functions
            |${i}  /////////////////////////////////
            |
            |${i}  ${functions map v(indent + 1) mkString s"\n\n${i}  "}
            |
            |${i}  /////////////////////////////////
            |${i}  // States
            |${i}  /////////////////////////////////
            |
            |${i}  ${states map v(indent + 1) mkString s"\n\n${i}  "}
            |
            |${i}  /////////////////////////////////
            |${i}  // Entities
            |${i}  /////////////////////////////////
            |
            |${i}  ${entities map v(indent + 1) mkString s"\n\n${i}  "}
            |
            |${i}  /////////////////////////////////
            |${i}  // Instances
            |${i}  /////////////////////////////////
            |
            |${i}  ${instances map v(indent + 1) mkString s"\n\n${i}  "}
            |
            |${i}  /////////////////////////////////
            |${i}  // Connections
            |${i}  /////////////////////////////////
            |
            |${i}  ${connects map v(indent + 1) mkString s"\n${i}  "}
            |
            |${i}  /////////////////////////////////
            |${i}  // verbatim blocks
            |${i}  /////////////////////////////////
            |
            |${i}  ${verbatim map {
             case (lang, body) => s"verbatim ${lang} {${body}}"
           } mkString s"\n\n${i}  "}
            |
            |${i}}""".stripMargin

      }

      case Ident(name) => name
      case Sym(symbol) => symbol.denot.name.str

      case Decl(ref, kind, None)       => s"${kind.toSource} ${v(indent)(ref)};"
      case Decl(ref, kind, Some(init)) => s"${kind.toSource} ${v(indent)(ref)} = ${v(init)};"

      case Instance(ref, module, paramNames, paramArgs) => {
        val pas = for ((pn, pa) <- paramNames zip paramArgs) yield { s"${pn} = ${v(indent)(pa)}" }
        s"new ${v(indent)(ref)} = ${v(indent)(module)}(${pas mkString ", "});"
      }
      case Connect(lhs, rhs) => s"${v(lhs)} -> ${rhs map v mkString ", "};"
      case Function(ref, body) => {
        s"""|void ${v(indent)(ref)}() {
            |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }

      case State(ref, body) => {
        s"""|@state ${v(indent)(ref)} {
            |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }

      case Thicket(trees) => "thicket(...)"

      case StmtBlock(body) => {
        s"""|{
            |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }
      case StmtIf(cond, thenStmt, Some(elseStmt)) => {
        s"if (${v(cond)}) ${v(indent)(thenStmt)} else ${v(indent)(elseStmt)}"
      }
      case StmtIf(cond, thenStmt, None) => {
        s"if (${v(cond)}) ${v(indent)(thenStmt)}"
      }
      case StmtCase(expr, cases, Nil) => {
        s"""|case (${v(expr)}) {
            |${i}  ${cases map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }
      case StmtCase(expr, cases, default) => {
        s"""|case (${v(expr)}) {
            |${i}  ${cases map v(indent + 1) mkString s"\n${i}  "}
            |${i}  default: {
            |${i}    ${default map v(indent + 2) mkString s"\n${i}    "}
            |${i}  }
            |${i}}""".stripMargin
      }

      case CaseClause(cond, body) => s"${cond map v mkString ", "} : ${v(indent)(body)}"

      case StmtLoop(body) => {
        s"""|loop {
            |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }
      case StmtWhile(cond, body) => {
        s"""|while (${v(indent)(cond)}) {
            |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }
      case StmtFor(inits, cond, steps, body) => {
        val initsStr = inits map v(indent) mkString s", "
        val condStr = cond map v getOrElse ""
        val stepsStr = steps map v(indent) mkString s", "
        s"""|for (${initsStr}; ${condStr} ; ${stepsStr}) {
            |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }
      case StmtDo(cond, body) => {
        s"""|do {
            |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
            |${i}} while (${v(indent)(cond)});""".stripMargin
      }
      case StmtLet(inits, body) => {
        s"let (${inits map v(indent) mkString s", "}) ${v(indent)(body)}"
      }

      case StmtFence()              => "fence;"
      case StmtBreak()              => "break;"
      case StmtGoto(ref)            => s"goto ${v(indent)(ref)};"
      case StmtReturn()             => "return;"
      case StmtAssign(lhs, rhs)     => s"${v(lhs)} = ${v(rhs)};"
      case StmtUpdate(lhs, op, rhs) => s"${v(lhs)} ${op}= ${v(rhs)};"
      case StmtPost(expr, op)       => s"${v(expr)}${op};"
      case StmtExpr(expr)           => s"${v(expr)};"
      case StmtDecl(decl)           => s"${v(indent)(decl)};"
      case StmtRead()               => "read;"
      case StmtWrite()              => "write;"
      case StmtDollarComment(str)   => "$" + s"""("${str}")"""
      case StmtError()              => "/* Error statement */"
    }
  }

  def toSource(implicit cc: CompilerContext): String = v(0)(this)
//
//    def v(indent: Int)(node: Node): String = {
//      val i = "  " * indent
//      node match {
//        case expr: Expr => visitExpr(expr)
//        case lval: LVal => visitLVal(lval)
//
//        case FsmTask(_, name, decls, fns, fencefn, vfns) =>
//
}
