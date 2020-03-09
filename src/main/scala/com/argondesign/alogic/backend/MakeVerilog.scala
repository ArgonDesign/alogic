////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Verilog backend
////////////////////////////////////////////////////////////////////////////////
package com.argondesign.alogic.backend

import alogic.backend.CodeWriter
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.enums.ResetStyle
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable.ListBuffer

final class MakeVerilog(
    presentDetails: EntityDetails,
    details: => Map[Symbol, EntityDetails]
)(
    implicit cc: CompilerContext
) {

  import presentDetails._

  // Render Verilog declaration List(signed, packed, id) strings for symbol
  private def vdecl(symbol: Symbol): String = {
    def decl(id: String, kind: Type): String = kind match {
      case k: TypeInt =>
        val signedStr = if (kind.isSigned) "signed " else ""
        if (k.width == 1) {
          s"$signedStr$id"
        } else {
          s"$signedStr[${k.width - 1}:0] $id"
        }
      case TypeArray(elemKind, size) =>
        s"${decl(id, elemKind)} [${size - 1}:0]"
      case _ => cc.ice(s"Don't know how to declare this type in Verilog:", kind.toString)
    }

    decl(symbol.name, symbol.kind.underlying)
  }

  // Render expression to Verilog
  private def vexpr(expr: Expr, wrapCat: Boolean, indent: Int): String = {
    lazy val i0 = "  "
    lazy val i = i0 * indent

    def aexpr(arg: Arg) = arg match {
      case ArgP(e) => vexpr(e)
      case _       => unreachable
    }

    def loop(expr: Expr) = {
      expr match {
        case ExprCall(e, args)         => s"${vexpr(e)}(${args map aexpr mkString ", "})"
        case ExprUnary(op, e)          => s"($op${vexpr(e)})"
        case ExprBinary(l, op, r)      => s"(${vexpr(l)} $op ${vexpr(r)})"
        case ExprTernary(cond, te, ee) => s"(${vexpr(cond)} ? ${vexpr(te)} : ${vexpr(ee)})"
        case ExprRep(count, e)         => s"{${vexpr(count)}{${vexpr(e)}}}"
        case ExprCat(parts) =>
          if (wrapCat) {
            parts map vexpr mkString (s"{\n$i0$i", s",\n$i0$i", s"\n$i}")
          } else {
            parts map vexpr mkString ("{", ", ", "}")
          }
        case ExprIndex(e, index)           => s"${vexpr(e)}[${vexpr(index)}]"
        case ExprSlice(e, lidx, op, ridx)  => s"${vexpr(e)}[${vexpr(lidx)}$op${vexpr(ridx)}]"
        case ExprSelect(e, sel, Nil)       => s"${vexpr(e)}${cc.sep}$sel"
        case ExprSym(symbol)               => symbol.name
        case ExprInt(false, w, v)          => s"$w'd$v"
        case ExprInt(true, w, v) if v >= 0 => s"$w'sd$v"
        case ExprInt(true, w, v)           => s"-$w'sd${-v}"
        case ExprNum(false, value)         => s"'d$value"
        case ExprNum(true, value)          => s"$value"
        case ExprStr(str)                  => s""""$str""""
        case _ =>
          cc.ice(expr, "Don't know how to emit Verilog for expression", expr.toString)
      }
    }

    loop(expr)
  }

  private def vexpr(expr: Expr): String = vexpr(expr, wrapCat = false, indent = 0)

  //////////////////////////////////////////////////////////////////////////////
  // Emit code
  //////////////////////////////////////////////////////////////////////////////

  private def emitDeclarationSection(body: CodeWriter): Unit = {
    if (hasConsts || hasFlops || hasCombSignals || hasArrays || hasInterconnect || canStall) {
      body.emitSection(1, "Declaration section") {
        if (hasConsts) {
          // Emit const declarations
          body.emitBlock(1, "Local parameter declarations") {
            defn.defns collect {
              case DefnConst(symbol, init) => (symbol, init)
            } foreach {
              case (symbol, init) => body.emit(1)(s"localparam ${vdecl(symbol)} = ${vexpr(init)};")
            }
          }
        }

        def emitVarDecl(symbol: Symbol): Unit = {
          if (netSymbols contains symbol) {
            body.emit(1)(s"wire ${vdecl(symbol)};")
          } else {
            body.emit(1)(s"reg ${vdecl(symbol)};")
          }
        }

        if (hasFlops) {
          // Emit flop declarations
          body.emitBlock(1, "Flop declarations") {
            for {
              Decl(qSymbol) <- decl.decls
              dSymbol <- qSymbol.attr.flop.get
            } {
              emitVarDecl(qSymbol)
              emitVarDecl(dSymbol)
            }
          }
        }

        if (hasCombSignals) {
          body.emitBlock(1, "Combinational signal declarations") {
            for {
              Decl(symbol) <- decl.decls
              if symbol.attr.combSignal.get contains true
            } {
              emitVarDecl(symbol)
            }
          }
        }

        if (hasArrays) {
          // Emit memory declarations
          body.emitBlock(1, "Memory declarations") {
            for {
              Decl(qSymbol) <- decl.decls
              if qSymbol.attr.memory.isSet
            } yield {
              emitVarDecl(qSymbol)
            }
          }
        }

        if (hasInterconnect) {
          // Emit interconnect declarations
          body.emitBlock(1, "Interconnect declarations") {
            for {
              (iSymbol, nSymbols) <- groupedInterconnectSymbols
            } {
              body.ensureBlankLine()
              body.emit(1)(s"// ${iSymbol.name}")
              nSymbols foreach emitVarDecl
            }
          }
        }
      }
    }
  }

  private def emitStatement(body: CodeWriter, indent: Int, stmt: Stmt): Unit = {
    def wrapIfCat(expr: Expr) = expr match {
      case _: ExprCat => vexpr(expr, wrapCat = true, indent = indent)
      case _          => vexpr(expr)
    }

    stmt match {
      // Block
      case StmtBlock(stmts) =>
        body.emit(indent)("begin")
        stmts foreach {
          emitStatement(body, indent + 1, _)
        }
        body.emit(indent)("end")

      // If statement
      case StmtIf(cond, thenStmts, elseStmts) =>
        body.emit(indent)(s"if (${vexpr(cond)}) begin")
        thenStmts foreach { emitStatement(body, indent + 1, _) }
        if (elseStmts.nonEmpty) {
          body.emit(indent)(s"end else begin")
          elseStmts foreach { emitStatement(body, indent + 1, _) }
        }
        body.emit(indent)(s"end")

      // Case statement
      case StmtCase(cond, cases) =>
        body.emit(indent)(s"case (${vexpr(cond)})")
        cases foreach {
          case CaseRegular(conds, stmts) =>
            body.emit(indent + 1)(s"${conds map vexpr mkString ", "}: begin")
            stmts foreach { emitStatement(body, indent + 2, _) }
            body.emit(indent + 1)("end")
          case CaseDefault(stmts) =>
            body.emit(indent + 1)("default: begin")
            stmts foreach { emitStatement(body, indent + 2, _) }
            body.emit(indent + 1)("end")
          case _: CaseGen => unreachable
        }
        body.emit(indent)(s"endcase")

      // Blocking assignment statements
      case StmtAssign(lhs, rhs) => body.emit(indent)(s"${wrapIfCat(lhs)} = ${wrapIfCat(rhs)};")

      // Non-blocking assignment statements
      case StmtDelayed(lhs, rhs) => body.emit(indent)(s"${wrapIfCat(lhs)} <= ${wrapIfCat(rhs)};")

      // Expression statements like $display();
      case StmtExpr(expr) => body.emit(indent)(s"${vexpr(expr)};")

      // Comment
      case StmtComment(str) => body.emit(indent)("// " + str)

      case other => cc.ice(other, "Don't know how to emit Verilog for statement", other.toString)
    }
  }

  private def emitClockedProcesses(body: CodeWriter, processes: List[EntClockedProcess]): Unit =
    body.emitSection(1, "Clocked processes") {
      processes foreach {
        case EntClockedProcess(reset, stmts) =>
          assert(stmts.nonEmpty)
          body.ensureBlankLine()
          val header = cc.settings.resetStyle match {
            case ResetStyle.AsyncLow if reset  => s"always @(posedge clk or negedge ${cc.rst})"
            case ResetStyle.AsyncHigh if reset => s"always @(posedge clk or posedge ${cc.rst})"
            case _                             => "always @(posedge clk)"
          }
          body.emit(1)(header + " begin")
          stmts foreach { stmt =>
            emitStatement(body, 2, stmt)
          }
          body.emit(1)("end")
      }
    }

  private def emitCombProcesses(body: CodeWriter, processes: List[EntCombProcess]): Unit =
    // TODO: allow multiple
    body.emitSection(1, "State system") {
      require(processes.lengthIs == 1)
      processes foreach {
        case EntCombProcess(stmts) =>
          assert(stmts.nonEmpty)
          body.emit(1)("always @* begin")
          stmts foreach { stmt =>
            emitStatement(body, 2, stmt)
          }
          body.emit(1)("end")
      }
    }

  private def emitInstances(body: CodeWriter): Unit = {
    if (decl.instances.nonEmpty) {
      body.emitSection(1, "Instances") {
        for (DeclInstance(iSymbol, ExprSym(eSymbol)) <- decl.instances) {
          body.ensureBlankLine()
          body.emit(1)(s"${eSymbol.name} ${iSymbol.name} (")

          body.emitTable(2, " ") {
            val items = new ListBuffer[List[String]]

            if (details(eSymbol).needsClock) {
              items append { List(".clk", "         (clk),") }
            }
            if (details(eSymbol).needsReset) {
              items append { List(s".${cc.rst}", s"         (${cc.rst}),") }
            }

            val TypeEntity(_, pSymbols) = iSymbol.kind.asEntity

            val lastIndex = pSymbols.length - 1

            for ((pSymbol, i) <- pSymbols.zipWithIndex) {
              val dir = pSymbol.kind match {
                case _: TypeIn => "<-"
                case _         => "->"
              }

              val pStr = instancePortExpr.get(iSymbol) flatMap {
                _.get(pSymbol.name)
              } map { expr =>
                vexpr(expr, wrapCat = true, indent = 2)
              } getOrElse {
                "/* not connected */"
              }

              val comma = if (i == lastIndex) "" else ","

              items append { List(s".${pSymbol.name}", s"/* $dir */ ($pStr)$comma") }
            }

            items.toList
          }

          body.emit(1)(");")
        }
      }
    }
  }

  private def emitConnects(body: CodeWriter): Unit = {
    if (nonPortConnects.nonEmpty) {
      body.emitSection(1, "Connections") {
        for (EntConnect(lhs, rhs :: Nil) <- nonPortConnects) {
          val assignLhs = vexpr(rhs, wrapCat = true, indent = 1)
          val assignRhs = vexpr(lhs, wrapCat = true, indent = 1)
          body.emit(1)(s"assign $assignLhs = $assignRhs;")
        }
      }
    }
  }

  private def emitVerbatimSection(body: CodeWriter): Unit = {
    val text = defn.verbatims collect {
      case EntVerbatim("verilog", b) => b
    } mkString "\n"

    if (text.nonEmpty) {
      body.emitSection(1, "Verbatim section") {
        body.emit(1)(text)
      }
    }
  }

  private def emitBody(body: CodeWriter): Unit = {
    emitDeclarationSection(body)

    if (defn.clockedProcesses.nonEmpty) {
      emitClockedProcesses(body, defn.clockedProcesses)
    }

    if (defn.combProcesses.nonEmpty) {
      emitCombProcesses(body, defn.combProcesses)
    }

    emitInstances(body)

    emitConnects(body)

    emitVerbatimSection(body)
  }

  def emitPortDeclarations(body: CodeWriter): Unit = {
    val items = new ListBuffer[String]

    if (needsClock) {
      items append "input  wire clk"
    }
    if (needsReset) {
      items append s"input  wire ${cc.rst}"
    }

    for (Decl(symbol) <- decl.decls) {
      symbol.kind match {
        case _: TypeIn => items append s"input  wire ${vdecl(symbol)}"
        case _: TypeOut =>
          val word = if (isVerbatim || (netSymbols contains symbol)) "wire" else "reg "
          items append s"output $word ${vdecl(symbol)}"
        case _ => ()
      }
    }

    if (items.nonEmpty) {
      items.init foreach { line =>
        body.emit(1)(line + ",")
      }
      body.emit(1)(items.last)
    }
  }

  def moduleSource: String = {
    val body = new CodeWriter

    body.emit(0)("`default_nettype none")
    body.ensureBlankLine()

    body.emit(0)(s"module ${decl.symbol.name}(")
    emitPortDeclarations(body)
    body.emit(0)(");")
    body.ensureBlankLine()

    emitBody(body)

    body.emit(0)("endmodule")
    body.ensureBlankLine()
    body.emit(0)("`default_nettype wire")

    body.text
  }
}
