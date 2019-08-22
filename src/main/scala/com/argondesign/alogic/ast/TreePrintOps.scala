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
import com.argondesign.alogic.core.Types.TypeStruct

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

  private[this] final def entityStr(indent: Int)(entity: Entity)(
      implicit cc: CompilerContext): String = {
    val i = "  " * indent
    val sb = new StringBuilder()
    val variant = entity.ref match {
      case ident: Ident => ident.attr("//variant").asInstanceOf[ExprStr].value
      case Sym(symbol)  => symbol.attr.variant.value
    }

    sb append s"${attrStr(indent, entity.ref)}${variant} ${v(indent)(entity.ref)} {\n"

    if (entity.declarations.nonEmpty) {
      sb append s"""|
                    |${i}  /////////////////////////////////
                    |${i}  // Declarations
                    |${i}  /////////////////////////////////
                    |
                    |""".stripMargin
      sb append s"${i}  ${entity.declarations map v(indent + 1) mkString ("", s";\n${i}  ", ";")}"
      sb append "\n"
    }

    if (entity.entities.nonEmpty) {
      sb append s"""|
                    |${i}  /////////////////////////////////
                    |${i}  // Entities
                    |${i}  /////////////////////////////////
                    |
                    |""".stripMargin
      sb append s"${i}  ${entity.entities map v(indent + 1) mkString s"\n\n${i}  "}"
      sb append "\n"
    }

    if (entity.instances.nonEmpty) {
      sb append s"""|
                    |${i}  /////////////////////////////////
                    |${i}  // Instances
                    |${i}  /////////////////////////////////
                    |
                    |""".stripMargin
      sb append s"${i}  ${entity.instances map v(indent + 1) mkString s"\n\n${i}  "}"
      sb append "\n"
    }

    if (entity.connects.nonEmpty) {
      sb append s"""|
                    |${i}  /////////////////////////////////
                    |${i}  // Connections
                    |${i}  /////////////////////////////////
                    |
                    |""".stripMargin
      sb append s"${i}  ${entity.connects map v(indent + 1) mkString s"\n${i}  "}"
      sb append "\n"
    }

    if (entity.combProcesses.nonEmpty) {
      sb append s"""|
                    |${i}  /////////////////////////////////
                    |${i}  // Combinational Blocks
                    |${i}  /////////////////////////////////
                    |
                    |${i}  always_comb {
                    |""".stripMargin
      sb append s"${i}    ${entity.combProcesses map v(indent + 2) mkString s"\n${i}    "}"
      sb append "\n"
    }

    if (entity.functions.nonEmpty) {
      sb append s"""|
                    |${i}  /////////////////////////////////
                    |${i}  // Functions
                    |${i}  /////////////////////////////////
                    |
                    |""".stripMargin
      sb append s"${i}  ${entity.functions map v(indent + 1) mkString s"\n\n${i}  "}"
      sb append "\n"
    }

    if (entity.states.nonEmpty) {
      sb append s"""|
                    |${i}  /////////////////////////////////
                    |${i}  // States
                    |${i}  /////////////////////////////////
                    |
                    |""".stripMargin
      sb append s"${i}  ${entity.states map v(indent + 1) mkString s"\n\n${i}  "}"
      sb append "\n"
    }

    if (entity.verbatims.nonEmpty) {
      sb append s"""|
                    |${i}  /////////////////////////////////
                    |${i}  // verbatim blocks
                    |${i}  /////////////////////////////////
                    |
                    |""".stripMargin
      sb append s"${i}  ${entity.verbatims map v(indent + 1) mkString s"\n\n${i}  "}"
      sb append "\n"
    }

    entity.body foreach {
      case EntGen(gen) =>
        sb append "\n"
        sb append s"${i}  ${v(indent + 1)(gen)}"
        sb append "\n"
      case _ =>
    }

    sb append s"\n${i}}"
    sb.toString
  }

  private[this] final def structStr(indent: Int)(kind: TypeStruct)(
      implicit cc: CompilerContext): String = {
    val i = "  " * indent
    val fields = for ((fn, ft) <- kind.fieldNames zip kind.fieldTypes) yield {
      s"${ft.toSource} ${fn};"
    }
    s"""|struct ${kind.name} {
        |${i}  ${fields mkString s"\n${i}  "}
        |${i}};""".stripMargin
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
    case ExprCast(kind, expr)                  => s"(${kind.toSource})(${v(expr)})"
    case ExprError()                           => "/* Error expression */"
  }

  private[this] def v(indent: Int)(tree: Tree)(implicit cc: CompilerContext): String = {
    val i = "  " * indent
    tree match {
      case expr: Expr => v(expr)

      case _: Thicket => "thicket(...)"

      case Root(typeDefinitions, entity) => {
        s"""|${typeDefinitions map v(indent) mkString s"\n\n${i}"}
            |
            |${i}${v(indent)(entity)}
            |""".stripMargin
      }
      case Ident(name) => name
      case Sym(symbol) => symbol.name

      case DefnIdent(ident, kind) =>
        if (kind.isStruct) {
          s"${attrStr(indent, ident)}${structStr(indent)(kind.asStruct)}"
        } else {
          s"${attrStr(indent, ident)}typedef ${kind.toSource} ${v(indent)(ident)};"
        }
      case Defn(symbol) =>
        if (symbol.kind.isStruct) {
          s"${attrStr(indent, symbol)}${structStr(indent)(symbol.kind.asStruct)}"
        } else {
          s"${attrStr(indent, symbol)}typedef ${symbol.kind.toSource} ${symbol.name};"
        }

      case DeclIdent(ident, kind, None) => {
        s"${kind.toSource} ${v(indent)(ident)}"
      }
      case DeclIdent(ident, kind, Some(init)) => {
        s"${kind.toSource} ${v(indent)(ident)} = ${v(init)}"
      }
      case Decl(symbol, None) => {
        s"${attrStr(indent, symbol)}${symbol.kind.toSource} ${symbol.name}"
      }
      case Decl(symbol, Some(init)) => {
        s"${attrStr(indent, symbol)}${symbol.kind.toSource} ${symbol.name} = ${v(init)}"
      }

      case entity: Entity    => entityStr(indent)(entity)
      case EntDefn(defn)     => s"${v(indent)(defn)};"
      case EntDecl(decl)     => s"${v(indent)(decl)};"
      case EntEntity(entity) => v(indent)(entity)
      case EntInstance(ref, module, paramNames, paramArgs) => {
        val pas = for ((pn, pa) <- paramNames zip paramArgs) yield {
          s"${pn} = ${v(indent)(pa)}"
        }
        s"${attrStr(indent, ref)}${v(indent)(ref)} = new ${v(indent)(module)}(${pas mkString ", "});"
      }
      case EntConnect(lhs, rhs) => s"${v(lhs)} -> ${rhs map v mkString ", "};"
      case EntCombProcess(stmts) => {
        s"""|always_comb {
            |${i}  ${stmts map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }
      case EntFunction(ref, body) => {
        s"""|${attrStr(indent, ref)}void ${v(indent)(ref)}() {
            |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }
      case EntState(ExprRef(symbol), body) => {
        s"""|${attrStr(indent, symbol)}state ${symbol.name} {
            |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }
      case EntState(expr, body) => {
        s"""|state ${v(indent)(expr)} {
            |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }
      case EntVerbatim(lang, body) => s"verbatim ${lang} {${body}}"
      case EntGen(gen)             => v(indent)(gen)

      case GenDecl(decl) => s"${v(indent)(decl)};"
      case GenDefn(defn) => s"${v(indent)(defn)};"
      case GenIf(cond, thenItems, Nil) => {
        s"""|gen if (${v(cond)}) {
            |${i}  ${thenItems map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }
      case GenIf(cond, thenItems, elseItems) => {
        s"""|gen if (${v(cond)}) {
            |${i}  ${thenItems map v(indent + 1) mkString s"\n${i}  "}
            |${i}} else {
            |${i}  ${elseItems map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }
      case GenFor(inits, cond, steps, body) => {
        val initsStr = inits map v(indent) mkString s", "
        val condStr = cond map v getOrElse ""
        val stepsStr = steps map v(indent) mkString s", "
        s"""|gen for (${initsStr} ; ${condStr} ; ${stepsStr}) {
            |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }
      case GenRange(decl, op, end, body) => {
        s"""|gen for (${v(indent)(decl)} ${op}  ${v(end)}) {
            |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }

      case StmtBlock(body) => {
        s"""|{
            |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }
      case StmtIf(cond, thenStmts, Nil) => {
        s"""|if (${v(cond)}) {
            |${i}  ${thenStmts map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }
      case StmtIf(cond, Nil, elseStmts) => {
        s"""|if (${v(cond)}) {} else {
            |${i}  ${elseStmts map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }

      case StmtIf(cond, thenStmts, elseStmts) => {
        s"""|if (${v(cond)}) {
            |${i}  ${thenStmts map v(indent + 1) mkString s"\n${i}  "}
            |${i}} else {
            |${i}  ${elseStmts map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }

      case StmtCase(expr, cases) => {
        s"""|case (${v(expr)}) {
            |${i}  ${cases map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }

      case CaseRegular(cond, Nil) => s"${cond map v mkString ", "} : {}"
      case CaseDefault(Nil)       => s"default : {}"
      case CaseGen(gen)           => v(indent)(gen)

      case CaseRegular(cond, stmts) => {
        s"""|${cond map v mkString ", "} : {
            |${i}  ${stmts map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }
      case CaseDefault(stmts) => {
        s"""|default : {
            |${i}  ${stmts map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }

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
        s"""|let (${inits map v(indent) mkString s", "}) {
            |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
            |${i}}""".stripMargin
      }

      case StmtFence()              => "fence;"
      case StmtBreak()              => "break;"
      case StmtContinue()           => "continue;"
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
      case StmtGen(gen)             => v(indent)(gen)
      case StmtError()              => "/* Error statement */"
    }
  }

  def toSource(implicit cc: CompilerContext): String = v(0)(this)
}
