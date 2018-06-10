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
import com.argondesign.alogic.core.Symbols.Symbol

trait TreePrintOps { this: Tree =>

  private[this] final def attrStr(indent: Int, symbol: Symbol)(
      implicit cc: CompilerContext): String = {
    val str = symbol.attr.toSource
    if (str.isEmpty) str else str + "\n" + "  " * indent
  }

  private[this] final def attrStr(indent: Int, ref: Ref)(implicit cc: CompilerContext): String = {
    ref match {
      case Sym(symbol) => attrStr(indent, symbol)
      case _           => ""
    }
  }

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
    case ExprIdent(name)                       => name
    case ExprRef(symbol)                       => symbol.name
    case ExprType(kind)                        => s"type(${kind.toSource})"
    case ExprInt(true, width, value)           => s"${width}'sd${value}"
    case ExprInt(false, width, value)          => s"${width}'d${value}"
    case ExprNum(true, value)                  => s"'sd${value}"
    case ExprNum(false, value)                 => s"${value}"
    case ExprStr(value)                        => s""""${value}""""
    case ExprError()                           => "/* Error expression */"
  }

  private[this] def ensureBlock(indent: Int, stmt: Stmt)(implicit cc: CompilerContext) = {
    stmt match {
      case block: StmtBlock => v(indent)(block)
      case other => {
        val i = "  " * indent
        s"""|{
            |${i}  ${v(indent + 1)(other)}
            |${i}}""".stripMargin
      }
    }
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
        s"${attrStr(indent, ref)}typedef ${kind.toSource} ${v(indent)(ref)}"
      }

      case TypeDefinitionStruct(ref, fieldNames, fieldTypes) => {
        val fields = for ((fn, ft) <- fieldNames zip fieldTypes) yield {
          s"${ft.toSource} ${fn};"
        }
        s"""|${attrStr(indent, ref)}struct ${v(indent)(ref)} {
            |${i}  ${fields mkString s"\n${i}  "}
            |${i}};""".stripMargin
      }

      case _: EntityIdent => ???

      case entity @ EntityNamed(
            symbol,
            declarations,
            instances,
            connects,
            fenceStmts,
            functions,
            states,
            entities,
            verbatim
          ) => {
        s"""|${attrStr(indent, symbol)}${symbol.attr.variant.value} ${symbol.name} {
            |${i}  /////////////////////////////////
            |${i}  // Declarations
            |${i}  /////////////////////////////////
            |
            |${i}  ${declarations map v(indent + 1) mkString s"\n${i}  "}
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
            |${i}  // verbatim blocks
            |${i}  /////////////////////////////////
            |
            |${i}  ${verbatim map {
             case (lang, body) => s"verbatim ${lang} {${body}}"
           } mkString s"\n\n${i}  "}
            |
            |${i}}""".stripMargin
      }

      case entity @ EntityLowered(
            symbol,
            declarations,
            instances,
            connects,
            stateSystems,
            verbatim
          ) => {
        s"""|${attrStr(indent, symbol)}${symbol.attr.variant.value} ${symbol.name} {
            |${i}  /////////////////////////////////
            |${i}  // Declarations
            |${i}  /////////////////////////////////
            |
            |${i}  ${declarations map v(indent + 1) mkString s"\n${i}  "}
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
            |${i}  // State systems
            |${i}  /////////////////////////////////
            |
            |${i}  ${stateSystems map v(indent + 1) mkString s"\n${i}  "}
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
      case Sym(symbol) => symbol.name

      case DeclIdent(ident, kind, None) => {
        s"${kind.toSource} ${v(indent)(ident)};"
      }
      case DeclIdent(ident, kind, Some(init)) => {
        s"${kind.toSource} ${v(indent)(ident)} = ${v(init)};"
      }
      case Decl(symbol, None) => {
        s"${attrStr(indent, symbol)}${symbol.kind.toSource} ${symbol.name};"
      }
      case Decl(symbol, Some(init)) => {
        s"${attrStr(indent, symbol)}${symbol.kind.toSource} ${symbol.name} = ${v(init)};"
      }

      case Instance(ref, module, paramNames, paramArgs) => {
        val pas = for ((pn, pa) <- paramNames zip paramArgs) yield {
          s"${pn} = ${v(indent)(pa)}"
        }
        s"${attrStr(indent, ref)}new ${v(indent)(ref)} = ${v(indent)(module)}(${pas mkString ", "});"
      }
      case Connect(lhs, rhs) => s"${v(lhs)} -> ${rhs map v mkString ", "};"
      case Function(ref, body) => {
        s"""|${attrStr(indent, ref)}void ${v(indent)(ref)}() {
            |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }

      case State(ExprRef(symbol), body) => {
        s"""|${attrStr(indent, symbol)}state ${symbol.name} {
            |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }

      case State(expr, body) => {
        s"""|state ${v(indent)(expr)} {
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
        s"if (${v(cond)}) ${ensureBlock(indent, thenStmt)} else ${ensureBlock(indent, elseStmt)}"
      }
      case StmtIf(cond, thenStmt, None) => {
        s"if (${v(cond)}) ${ensureBlock(indent, thenStmt)}"
      }

      case StmtCase(expr, cases) => {
        s"""|case (${v(expr)}) {
            |${i}  ${cases map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }

      case RegularCase(cond, stmt) => s"${cond map v mkString ", "} : ${v(indent)(stmt)}"
      case DefaultCase(stmt)       => s"default : ${v(indent)(stmt)}"

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
        s"""|for (${initsStr} ; ${condStr} ; ${stepsStr}) {
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
      case StmtComment(str)         => "$" + s"""("${str}")"""
      case StmtStall(cond)          => s"stall(${v(cond)});"
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
