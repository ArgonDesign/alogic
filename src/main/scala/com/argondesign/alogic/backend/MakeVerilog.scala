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

import scala.annotation.nowarn
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

final class MakeVerilog(
    presentDetails: EntityDetails,
    details: => Map[Symbol, EntityDetails]
  )(
    implicit
    cc: CompilerContext) {

  import presentDetails._

  // Render Verilog declaration List(signed, packed, id) strings for symbol
  private def vdecl(symbol: Symbol, indent: Int = 0): String = {
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
      case TypeXenoFunc(symbol, retType, _) =>
        val is = symbol.decl.asInstanceOf[DeclFunc].args map {
          case Decl(symbol) => s"input bit [${symbol.kind.width - 1}:0] ${symbol.name}"
        }
        val as = retType match {
          case TypeVoid => is
          case kind     => s"output bit [${kind.width - 1}:0] _result" :: is
        }
        if (as.lengthIs <= 1) {
          s"""import "DPI-C" context function void $id(${as.headOption.getOrElse("")});"""
        } else {
          val i0 = "  " * indent
          val i1 = i0 + "  "
          s"""import "DPI-C" context function void $id(${as mkString ("\n" + i1, ",\n" + i1, "\n" + i0)});"""
        }
      case _ => cc.ice(s"Don't know how to declare this type in Verilog:", kind.toString)
    }

    decl(symbol.name, symbol.kind.underlying)
  }

  private val multiBinOps = Set("*", "+", "-", "|", "&", "||", "&&")

  // Render expression to Verilog
  @nowarn("msg=Recursive call used default arguments")
  private def vexpr(expr: Expr, indent: Int = 0): String = expr match {
    case ExprCall(e, as) =>
      val aa = as map {
        case ArgP(e) => vexpr(e)
        case _       => unreachable
      }
      s"${vexpr(e)}(${aa mkString ", "})"
    case ExprUnary(op, e) =>
      e match {
        case _: ExprBinary | _: ExprTernary => s"$op(${vexpr(e)})"
        case ExprInt(_, _, v) if v < 0      => s"$op(${vexpr(e)})"
        case ExprNum(_, v) if v < 0         => s"$op(${vexpr(e)})"
        case _                              => s"$op${vexpr(e)}"
      }
    case ExprBinary(l, op, r) =>
      val ll = l match {
        case ExprBinary(_, `op`, _) if multiBinOps(op) => vexpr(l)
        case _: ExprBinary | _: ExprTernary            => s"(${vexpr(l)})"
        case _                                         => vexpr(l)
      }
      val rr = r match {
        case ExprBinary(_, `op`, _) if multiBinOps(op) => vexpr(r)
        case ExprUnary(`op`, _)                        => s"(${vexpr(r)})"
        case _: ExprBinary | _: ExprTernary            => s"(${vexpr(r)})"
        case _                                         => vexpr(r)
      }
      s"$ll $op $rr"
    case ExprTernary(c, t, e) =>
      val cc = c match {
        case _: ExprTernary => s"(${vexpr(c)})"
        case _              => vexpr(c)
      }
      val tt = t match {
        case _: ExprTernary => s"(${vexpr(t)})"
        case _              => vexpr(t)
      }
      val ee = e match {
        case _: ExprTernary => s"(${vexpr(e)})"
        case _              => vexpr(e)
      }
      s"$cc ? $tt : $ee"
    case ExprRep(c, e) => s"{${vexpr(c)}{${vexpr(e)}}}"
    case ExprCat(ps) if indent > 0 =>
      val i0 = "  "
      val i = i0 * indent
      ps map { vexpr(_, indent + 1) } mkString (s"{\n$i0$i", s",\n$i0$i", s"\n$i}")
    case ExprCat(ps)                   => ps map { vexpr(_) } mkString ("{", ", ", "}")
    case ExprIndex(e, i)               => s"${vexpr(e)}[${vexpr(i)}]"
    case ExprSlice(e, l, op, r)        => s"${vexpr(e)}[${vexpr(l)}$op${vexpr(r)}]"
    case ExprSym(symbol)               => symbol.name
    case ExprInt(false, w, v)          => s"$w'd$v"
    case ExprInt(true, w, v) if v >= 0 => s"$w'sd$v"
    case ExprInt(true, w, v)           => s"-$w'sd${-v}"
    case ExprNum(false, v)             => s"'d$v"
    case ExprNum(true, v)              => s"$v"
    case ExprStr(str)                  => s""""$str""""
    case _ =>
      cc.ice(expr, "Don't know how to emit Verilog for expression", expr.toString)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Emit code
  //////////////////////////////////////////////////////////////////////////////

  private def emitDeclarationSection(body: CodeWriter): Unit = {
    if (hasConsts || hasFlops || hasCombSignals || hasArrays || hasInterconnect || hasXenoFuncs) {
      body.emitSection(1, "Declaration section") {
        if (hasXenoFuncs) {
          body.emitBlock(1, "Foreign functions") {
            decl.decls filter { _.symbol.kind.isXenoFunc } foreach { decl =>
              body.emit(1)(vdecl(decl.symbol, 1))
            }
          }
        }

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
              if (decl.decls exists { _.symbol eq dSymbol }) {
                emitVarDecl(dSymbol)
              }
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
      case _: ExprCat => vexpr(expr, indent)
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
        @tailrec
        def loop(ts: List[Stmt], es: List[Stmt]): Unit = {
          ts foreach { emitStatement(body, indent + 1, _) }
          es match {
            case Nil =>
              body.emit(indent)(s"end")
            case List(StmtIf(cond, thenStmts, elseStmts)) =>
              body.emit(indent)(s"end else if (${vexpr(cond)}) begin")
              loop(thenStmts, elseStmts)
            case _ =>
              body.emit(indent)(s"end else begin")
              es foreach { emitStatement(body, indent + 1, _) }
              body.emit(indent)(s"end")
          }
        }
        loop(thenStmts, elseStmts)

      // Case statement
      case StmtCase(cond, cases) =>
        body.emit(indent)(s"case (${vexpr(cond)})")
        cases foreach {
          case CaseRegular(conds, stmts) =>
            body.emit(indent + 1)(s"${conds map { vexpr(_) } mkString ", "}: begin")
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

      case StmtOutcall(o, f, is) =>
        body.emit(indent)(s"${vexpr(f)}(${(o :: is) map { vexpr(_) } mkString ", "});")

      // Expression statements like $display();
      case StmtExpr(expr) => body.emit(indent)(s"${vexpr(expr)};")

      // Comment
      case StmtComment(str) => body.emit(indent)("// " + str)

      // Procedural assertion
      case StmtAssertion(AssertionAssert(cond, msgOpt)) =>
        val elsePart = msgOpt match {
          case None      => ""
          case Some(msg) => s""" else $$error("$msg")"""
        }
        body.emit(indent)(s"assert (${vexpr(cond)})$elsePart;")

      case other => cc.ice(other, "Don't know how to emit Verilog for statement", other.toString)
    }
  }

  private def emitClockedProcesses(body: CodeWriter, processes: List[EntClockedProcess]): Unit =
    body.emitSection(1, "Clocked processes") {
      processes foreach {
        case EntClockedProcess(clk, rstOpt, stmts) =>
          assert(stmts.nonEmpty)
          body.ensureBlankLine()
          val rstPart = rstOpt match {
            case None => ""
            case Some(rst) =>
              cc.settings.resetStyle match {
                case ResetStyle.AsyncLow  => s" or negedge ${vexpr(rst)}"
                case ResetStyle.AsyncHigh => s" or posedge ${vexpr(rst)}"
                case _                    => ""
              }
          }
          body.emit(1)(s"always @(posedge ${vexpr(clk)}$rstPart)" + " begin")
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
                vexpr(expr, indent = 2)
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
          val assignLhs = vexpr(rhs, indent = 1)
          val assignRhs = vexpr(lhs, indent = 1)
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
