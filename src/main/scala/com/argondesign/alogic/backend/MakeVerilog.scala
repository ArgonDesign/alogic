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
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable.ListBuffer

final class MakeVerilog(entity: Entity)(implicit cc: CompilerContext) {

  //////////////////////////////////////////////////////////////////////////////
  // Compute necessary values
  //////////////////////////////////////////////////////////////////////////////

  private val Entity(
    Sym(eSymbol),
    decls,
    instances,
    connects,
    Nil,
    states,
    fenceStmts,
    Nil,
    verbatim
  ) = entity

  assert {
    decls forall {
      case Decl(symbol, _) => {
        symbol.denot.kind match {
          case _: TypeConst => true
          case _: TypeIn    => true
          case _: TypeOut   => true
          case _: TypeArray => true
          case _: TypeInt   => true
          case _            => false
        }
      }
      case _ => unreachable
    }
  }

  assert(states.nonEmpty || fenceStmts.isEmpty)

  private val isVerbatim = entity.variant == "verbatim"

  private val hasConsts = decls exists {
    case Decl(symbol, _) => symbol.denot.kind.isInstanceOf[TypeConst]
    case _               => false
  }

  private val hasFlops = decls exists {
    case Decl(symbol, _) => symbol.attr.flop.isSet
    case _               => false
  }

  private val hasCombSignals = decls exists {
    case Decl(symbol, _) => symbol.attr.combSignal.isSet
    case _               => false
  }

  private val hasArrays = decls exists {
    case Decl(symbol, _) => symbol.attr.arr.isSet
    case _               => false
  }

  private val hasInterconnect = decls exists {
    case Decl(symbol, _) => symbol.attr.interconnect.isSet
    case _               => false
  }

  private val canStall = entity.preOrderIterator exists {
    case _: StmtStall => true
    case _            => false
  }

  // Any symbol that is driven by a connect must be a net
  private val netSymbols = connects flatMap {
    case Connect(_, rhs :: Nil) => {
      rhs collect {
        case ExprRef(Sym(symbol)) => symbol
      }
    }
    case _ => Nil
  }

  // Group and sort interconnect symbols by instance, then by port declaration order
  lazy val groupedInterconnectSymbols: List[(TermSymbol, List[TermSymbol])] = {
    // Calculate (instance symbol, port name, interconnect symbol) triplets
    val trip = for {
      Decl(nSymbol, _) <- decls
      (iSymbol, sel) <- nSymbol.attr.interconnect.get
    } yield {
      (iSymbol, sel, nSymbol)
    }

    // Group by instance, loose instance symbol from values
    val groups = trip groupBy { _._1 } mapValues { _ map { case (_, s, n) => (s, n) } }

    // Sort by groups by instance order
    val sortedInstances = {
      // Sorting map for instance symbols
      val ordering = {
        val pairs = for {
          (Instance(Sym(symbol: TermSymbol), _, _, _), i) <- instances.zipWithIndex
        } yield {
          symbol -> i
        }
        pairs.toMap
      }
      // Sort by instance
      groups.toList sortBy { case (i, _) => ordering(i) }
    }

    // Sort within group by port definition order
    sortedInstances map {
      case (iSymbol, list) =>
        // Sorting map for port selectors
        val ordering = {
          val pairs = for {
            (symbol, i) <- iSymbol.denot.kind.asInstanceOf[TypeInstance].portSymbols.zipWithIndex
          } yield {
            symbol.name -> i
          }
          pairs.toMap
        }
        // Sort by port selector, then loose them, note that some interconnect
        // symbols can remain as placeholders in concatenations while their
        // corresponding ports have ben removed. We put these at the end sorted
        // lexically.
        val sortedSymbols = list sortWith {
          case ((a, _), (b, _)) => {
            (ordering.get(a), ordering.get(b)) match {
              case (Some(oa), Some(ob)) => oa < ob
              case (Some(_), None)      => true
              case (None, Some(_))      => false
              case (None, None)         => a < b
            }
          }
        } map { _._2 }
        (iSymbol, sortedSymbols)
    }
  }

  // Function from 'instance symbol => port selector => connected expression'
  lazy val instancePortExpr: Map[TermSymbol, Map[String, Expr]] = {
    val trip = connects collect {
      case Connect(ExprSelect(ExprRef(Sym(iSymbol: TermSymbol)), sel), rhs :: Nil) => {
        (iSymbol, sel, rhs)
      }
      case Connect(lhs, ExprSelect(ExprRef(Sym(iSymbol: TermSymbol)), sel) :: Nil) => {
        (iSymbol, sel, lhs)
      }
    }

    val groupped = trip groupBy { _._1 } mapValues { _ map { case (_, s, e) => (s, e) } }

    groupped mapValues { pairs =>
      pairs.toMap ensuring { _.size == pairs.length }
    }
  }

  // Connects that are not of the form 'a.b -> SOMETHING' or 'SOMETHING -> a.b'
  // where a is an instance
  val nonPortConnects = connects filter {
    case Connect(ExprSelect(ExprRef(Sym(symbol)), _), _) => {
      !symbol.denot.kind.isInstanceOf[TypeInstance]
    }
    case Connect(_, ExprSelect(ExprRef(Sym(symbol)), _) :: Nil) => {
      !symbol.denot.kind.isInstanceOf[TypeInstance]
    }
    case _ => true
  }

  // Render Verilog declaration List(signed, packed, id) strings for term symbol
  private def vdecl(symbol: TermSymbol): String = {
    def decl(id: String, kind: Type): String = {
      kind match {
        case intKind: TypeInt => {
          val signedStr = if (kind.isSigned) "signed " else ""
          val width = intKind.width.value.get
          if (width == 1) {
            s"${signedStr}${id}"
          } else {
            s"${signedStr}[${width - 1}:0] ${id}"
          }
        }
        case TypeArray(elemKind, sizeExpr) => {
          s"${decl(id, elemKind)} [${sizeExpr.value.get - 1}:0]"
        }
        case _ => cc.ice(s"Don't know how to declare this type in Verilog:", kind.toString)
      }
    }

    decl(symbol.name, symbol.denot.kind.underlying)
  }

  // Render expression to Verilog
  private def vexpr(expr: Expr, wrapCat: Boolean, indent: Int): String = {
    lazy val i0 = "  "
    lazy val i = i0 * indent
    def loop(expr: Expr) = {
      expr match {
        case ExprCall(e, args)         => s"${vexpr(e)}(${args map vexpr mkString ", "})"
        case ExprUnary(op, e)          => s"(${op}${vexpr(e)})"
        case ExprBinary(l, op, r)      => s"(${vexpr(l)} ${op} ${vexpr(r)})"
        case ExprTernary(cond, te, ee) => s"(${vexpr(cond)} ? ${vexpr(te)} : ${vexpr(ee)})"
        case ExprRep(count, e)         => s"{${vexpr(count)}{${vexpr(e)}}}"
        case ExprCat(parts) => {
          if (wrapCat) {
            parts map vexpr mkString (s"{\n${i0}${i}", s",\n${i0}${i}", s"\n${i}}")
          } else {
            parts map vexpr mkString ("{", ", ", "}")
          }
        }
        case ExprIndex(e, index)           => s"${vexpr(e)}[${vexpr(index)}]"
        case ExprSlice(e, lidx, op, ridx)  => s"${vexpr(e)}[${vexpr(lidx)}${op}${vexpr(ridx)}]"
        case ExprSelect(e, sel)            => s"${vexpr(e)}${cc.sep}${sel}"
        case ExprRef(Sym(symbol))          => symbol.name
        case ExprInt(false, w, v)          => s"${w}'d${v}"
        case ExprInt(true, w, v) if v >= 0 => s"${w}'sd${v}"
        case ExprInt(true, w, v)           => s"-${w}'sd${-v}"
        case ExprNum(false, value)         => s"'d${value}"
        case ExprNum(true, value)          => s"${value}"
        case ExprStr(str)                  => s""""${str}""""
        case _ => {
          cc.ice(expr, "Don't know how to emit Verilog for expression", expr.toString)
        }
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
            decls collect {
              case Decl(symbol, Some(init)) if symbol.denot.kind.isInstanceOf[TypeConst] => {
                (symbol, init)
              }
            } foreach {
              case (symbol, init) =>
                body.emit(1)(s"localparam ${vdecl(symbol)} = ${vexpr(init)};")
            }
          }
        }

        def emitVarDecl(symbol: TermSymbol) = {
          body.emit(1)(s"reg ${vdecl(symbol)};")
        }

        if (hasFlops) {
          // Emit flop declarations
          body.emitBlock(1, "Flop declarations") {
            for {
              Decl(qSymbol, _) <- decls
              dSymbol <- qSymbol.attr.flop.get
            } {
              emitVarDecl(qSymbol)
              emitVarDecl(dSymbol)
            }
          }
        }

        if (hasCombSignals) {
          body.emitBlock(1, "Combinatorial signal declarations") {
            for {
              Decl(symbol, _) <- decls
              if symbol.attr.combSignal.get contains true
            } {
              emitVarDecl(symbol)
            }
          }
        }

        if (hasArrays) {
          // Emit array declarations
          body.emitBlock(1, "Array declarations") {
            for {
              Decl(qSymbol, _) <- decls
              (weSymbol, waSymbol, wdSymbol) <- qSymbol.attr.arr.get
            } yield {
              emitVarDecl(qSymbol)
              emitVarDecl(weSymbol)
              emitVarDecl(waSymbol)
              emitVarDecl(wdSymbol)
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
              for (nSymbol <- nSymbols) {
                if (netSymbols contains nSymbol) {
                  body.emit(1)(s"wire ${vdecl(nSymbol)};")
                } else {
                  body.emit(1)(s"reg  ${vdecl(nSymbol)};")
                }
              }
            }
          }
        }

        // Emit go declarations
        if (canStall) {
          body.emit(1)("// go declaration")
          body.emit(1)("reg go;")
          body.ensureBlankLine()
        }
      }
    }
  }

  private def emitInternalStorageSection(body: CodeWriter): Unit = {
    if (hasFlops || hasArrays) {
      body.emitSection(1, "Storage section") {
        if (hasFlops) {
          body.emitBlock(1, "Flops") {
            body.emit(1)("always @(posedge clk or negedge rst_n) begin")
            body.emit(2)("if (!rst_n) begin")

            body.emit(3) {
              for {
                Decl(qSymbol, initOpt) <- decls
                if qSymbol.attr.flop.isSet
              } yield {
                val id = qSymbol.name
                val init = initOpt match {
                  case Some(expr) => expr
                  case None => {
                    val kind = qSymbol.denot.kind
                    ExprInt(kind.isSigned, kind.width.value.get.toInt, 0)
                  }
                }
                s"${id} <= ${vexpr(init)};"
              }
            }

            if (canStall) {
              body.emit(2)("end else if (go) begin")
            } else {
              body.emit(2)("end else begin")
            }

            body.emit(3) {
              for {
                Decl(qSymbol, initOpt) <- decls
                dSymbol <- qSymbol.attr.flop.get
              } yield {
                s"${qSymbol.name} <= ${dSymbol.name};"
              }
            }

            body.emit(2)("end")
            body.emit(1)("end")
          }
          body.ensureBlankLine()
        }

        if (hasArrays) {
          body.emitBlock(1, "Array storage") {
            for {
              Decl(qSymbol, _) <- decls
              (weSymbol, waSymbol, wdSymbol) <- qSymbol.attr.arr.get
            } {
              body.emit(1) {
                List(
                  "always @(posedge clk) begin",
                  if (canStall) {
                    s"  if (go && ${weSymbol.name}) begin"
                  } else {
                    s"  if (${weSymbol.name}) begin"
                  },
                  s"    ${qSymbol.name}[${waSymbol.name}] <= ${wdSymbol.name};",
                  "  end",
                  "end"
                )
              }
              body.ensureBlankLine()
            }
          }
        }
      }
    }
  }

  private def emitStatement(body: CodeWriter, indent: Int, stmt: Stmt): Unit = {

    def emitWithoutBeginEndIfBlock(indent: Int, stmt: Stmt): Unit = stmt match {
      case StmtBlock(stmts) => stmts foreach { emitStatement(body, indent, _) }
      case stmt             => emitStatement(body, indent, stmt)
    }

    stmt match {
      // Block
      case StmtBlock(stmts) => {
        body.emit(indent)("begin")
        stmts foreach {
          emitStatement(body, indent + 1, _)
        }
        body.emit(indent)("end")
      }

      // If statement
      case StmtIf(cond, thenStmt, optElseStmt) => {
        body.emit(indent)(s"if (${vexpr(cond)}) begin")
        emitWithoutBeginEndIfBlock(indent + 1, thenStmt)
        optElseStmt foreach { elseStmt =>
          body.emit(indent)(s"end else begin")
          emitWithoutBeginEndIfBlock(indent + 1, elseStmt)
        }
        body.emit(indent)(s"end")
      }

      // Case statement
      case StmtCase(cond, caseClauses, defaultStmts) => {
        body.emit(indent)(s"case (${vexpr(cond)})")
        for (CaseClause(conds, stmt) <- caseClauses) {
          body.emit(indent + 1)(s"${conds map vexpr mkString ", "}: begin")
          emitWithoutBeginEndIfBlock(indent + 2, stmt)
          body.emit(indent + 1)("end")
        }
        if (defaultStmts.nonEmpty) {
          body.emit(indent + 1)("default: begin")
          defaultStmts foreach { emitStatement(body, indent + 2, _) }
          body.emit(indent + 1)("end")
        }
        body.emit(indent)(s"endcase")
      }

      // Assignment statements
      case StmtAssign(lhs, rhs) => {
        def wrapIfCat(expr: Expr) = expr match {
          case _: ExprCat => vexpr(expr, wrapCat = true, indent = indent)
          case _          => vexpr(expr)
        }
        body.emit(indent)(s"${wrapIfCat(lhs)} = ${wrapIfCat(rhs)};")
      }

      // Error statement injected by compiler at an earlier error
      case stmts: StmtError => body.emit(indent)(s"/* Error statement from ${stmt.loc.prefix} */")

      // Stall statement
      case _: StmtStall => body.emit(indent)("go = 1'b0;")

      // Fence statement
      case _: StmtFence => ()

      // Expression statements like $display();
      case StmtExpr(expr) => body.emit(indent)(s"${vexpr(expr)};")

      // Alogic comment
      case StmtDollarComment(str) => body.emit(indent)("// " + str)

      case other => cc.ice(other, "Don't know how to emit Verilog for statement", other.toString)
    }
  }

  private def emitState(body: CodeWriter, indent: Int, stmts: List[Stmt]): Unit = {
    body.emit(0)("begin")
    stmts foreach {
      emitStatement(body, indent + 1, _)
    }
    body.emit(indent)("end")
  }

  private def emitStateSystem(body: CodeWriter): Unit = {
    if (states.nonEmpty) {
      body.emitSection(1, "State system") {
        body.emit(1)("always @* begin")

        if (canStall) {
          body.emit(2)("// go default")
          body.emit(2)("go = 1'b1;")
          body.ensureBlankLine()
        }

        // Emit the fence statements, if any
        if (fenceStmts.nonEmpty) {
          body.emitBlock(2, "Fence statements") {
            for (stmt <- fenceStmts) {
              emitStatement(body, 2, stmt)
            }
          }
        }

        // Emit state system case statement
        if (states.length == 1) {
          val State(expr, stmts) = states.head
          val n = expr.value.get
          body.ensureBlankLine()
          body.emit(2)(s"// State ${n}")
          body.append(s"    ")
          emitState(body, 2, stmts)
        } else {
          body.emit(2)(s"case (${eSymbol.attr.stateVar.value.name})")
          for ((state, i) <- states.sortBy(_.expr.value.get).zipWithIndex) {
            val State(expr, stmts) = state
            val n = expr.value.get
            val e = vexpr(expr)
            val selector = if (n == 0) s"default /* $e */" else s"$e"
            if (i > 0) {
              body.ensureBlankLine()
            }
            body.emit(3)(s"// State ${n}")
            body.append(s"      ${selector}: ")
            emitState(body, 3, stmts)
          }
          body.emit(2)("endcase")
        }

        val cSymbols = decls collect {
          case Decl(symbol, _) if symbol.attr.clearOnStall.get contains true => symbol
        }

        if (cSymbols.nonEmpty && canStall) {
          body.ensureBlankLine()
          body.emit(2)("// Clears")
          body.emit(2)("if (!go) begin")
          for (symbol <- cSymbols) {
            val width = symbol.denot.kind.width.value.get
            val s = if (symbol.denot.kind.isSigned) "s" else ""
            body.emit(3)(s"${symbol.name} = ${width}'${s}b0;")
          }
          body.emit(2)("end")
        }

        body.emit(1)("end")
      }
    }
  }

  private def emitInstances(body: CodeWriter): Unit = {
    if (instances.nonEmpty) {
      body.emitSection(1, "Instances") {
        for (Instance(Sym(iSymbol: TermSymbol), _, Nil, Nil) <- instances) {
          val TypeInstance(eSymbol) = iSymbol.denot.kind
          body.ensureBlankLine()
          body.emit(1)(s"${eSymbol.name} ${iSymbol.name} (")

          body.emitTable(1, " ") {
            val items = new ListBuffer[(Boolean, String, String)]

            // TODO: omit if not needed
            items append { (true, ".clk", "         (clk)") }
            items append { (true, ".rst_n", s"         (rst_n)") }

            val TypeEntity(_, pSymbols, _) = eSymbol.denot.kind

            for ((pSymbol, i) <- pSymbols.zipWithIndex) {
              val dir = pSymbol.denot.kind match {
                case _: TypeIn => "<-"
                case _         => "->"
              }

              val (required, pStr) = instancePortExpr.get(iSymbol) flatMap {
                _.get(pSymbol.name)
              } map { expr =>
                (true, vexpr(expr, wrapCat = true, indent = 2))
              } getOrElse {
                (false, "")
              }

              items append { (required, s".${pSymbol.name}", s"/* ${dir} */ (${pStr})") }
            }

            val lastRequied = items lastIndexWhere { _._1 }
            val lastIndex = items.length - 1

            for (((required, l, r), index) <- items.toList.zipWithIndex) yield {
              val nl = if (required) s"  ${l}" else s"//${l}"
              val nr = if (index == lastRequied || index == lastIndex) s"${r}" else s"${r},"
              List(nl, nr)
            }
          }

          body.emit(1)(");")
        }
      }
    }
  }

  private def emitConnects(body: CodeWriter): Unit = {
    if (nonPortConnects.nonEmpty) {
      body.emitSection(1, "Connections") {
        for (Connect(lhs, rhs :: Nil) <- nonPortConnects) {
          val assignLhs = vexpr(rhs, wrapCat = true, indent = 2)
          val assignRhs = vexpr(lhs, wrapCat = true, indent = 2)
          body.emit(1)(s"assign ${assignLhs} = ${assignRhs};")
        }
      }
    }
  }

  private def emitVerbatimSection(body: CodeWriter): Unit = {
    verbatim.get("verilog") foreach { text =>
      body.emitSection(1, "Verbatim section") {
        body.emit(1)(text)
      }
    }
  }

  private def emitBody(body: CodeWriter): Unit = {
    emitDeclarationSection(body)

    emitInternalStorageSection(body)

    emitStateSystem(body)

    emitInstances(body)

    emitConnects(body)

    emitVerbatimSection(body)
  }

  def moduleSource: String = {
    val body = new CodeWriter

    body.emit(0)("`default_nettype none")
    body.ensureBlankLine()

    body.emit(0)(s"module ${eSymbol.name}(")

    // Emit port declarations
    for (Decl(symbol, _) <- decls) {
      symbol.denot.kind match {
        case _: TypeIn => body.emit(1)(s"input  wire ${vdecl(symbol)},")
        case _: TypeOut => {
          if (isVerbatim || (netSymbols contains symbol)) {
            body.emit(1)(s"output wire ${vdecl(symbol)},")
          } else {
            body.emit(1)(s"output reg  ${vdecl(symbol)},")
          }
        }
        case _ => ()
      }
    }
    body.ensureBlankLine()
    // TODO: omit if not needed
    body.emit(1)("input  wire clk,")
    body.emit(1)("input  wire rst_n")
    body.emit(0)(");")
    body.ensureBlankLine()

    emitBody(body)

    body.emit(0)("endmodule")
    body.ensureBlankLine()
    body.emit(0)("`default_nettype wire")

    body.text
  }
}
