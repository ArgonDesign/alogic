////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Pretty printers for Tree nodes
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes._
import com.argondesign.alogic.core.FuncVariant
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.enums.EntityVariant
import com.argondesign.alogic.util.unreachable

import scala.util.chaining._

// $COVERAGE-OFF$ debug code
trait TreePrintOps {
  this: Tree =>

  def toSource: String = v(this)(0)

  final private def block(
      header: String,
      body: List[Tree]
    )(
      implicit
      indent: Int
    ): String = {
    val prefix = if (header.nonEmpty) header + " " else ""
    if (body.isEmpty) {
      s"$prefix{}"
    } else {
      val i = "  " * indent
      s"""$prefix{
         |$i  ${body map {
        v(_)(indent + 1)
      } mkString s"\n$i  "}
         |$i}""".stripMargin
    }
  }

  final private def v(fct: FlowControlType): String = fct match {
    case FlowControlTypeNone  => ""
    case FlowControlTypeValid => "sync "
    case FlowControlTypeReady => "sync ready "
  }

  final private def v(st: StorageType): String = st match {
    case StorageTypeDefault => ""
    case StorageTypeReg     => "reg "
    case StorageTypeWire    => "wire "
    case StorageTypeSlices(slices) =>
      slices
        .map {
          case StorageSliceFwd => "fslice"
          case StorageSliceBwd => "bslice"
          case StorageSliceBub => "bubble"
        }
        .mkString("", " ", " ")
  }

  final private def v(ev: EntityVariant.Type): String = ev match {
    case EntityVariant.Fsm => "fsm"
    case EntityVariant.Net => "network"
    case EntityVariant.Ver => "verbatim entity"
    case _                 => unreachable
  }

  final private def vs(trees: List[Tree])(implicit indent: Int): String =
    trees map v mkString ", "

  final private def vs(
      trees: List[Tree],
      start: String,
      sep: String,
      end: String
    )(
      implicit
      indent: Int
    ): String = trees.map(v).mkString(start, sep, end)

  final private def vo(treeOpt: Option[Tree])(implicit indent: Int): String =
    treeOpt map v getOrElse ""

  final private def v(tree: Tree)(implicit indent: Int): String = tree match {
    case node: Ref       => v(node)
    case node: Desc      => v(node)
    case node: Attr      => v(node)
    case node: GenCase   => v(node)
    case node: Decl      => v(node)
    case node: Defn      => v(node)
    case node: Import    => v(node)
    case node: Using     => v(node)
    case node: From      => v(node)
    case node: Assertion => v(node)
    case node: Pkg       => v(node)
    case node: Ent       => v(node)
    case node: Rec       => v(node)
    case node: Stmt      => v(node)
    case node: Case      => v(node)
    case node: Expr      => v(node)
    case node: Arg       => v(node)
    case node: Thicket   => block("thicket", node.trees)
    case Stump           => "Stump"
  }

  final private def v(ref: Ref)(implicit indent: Int): String = ref match {
    case Ident(name, Nil)     => name
    case Ident(name, indices) => vs(indices, s"$name#[", ",", "]")
    case Sym(symbol)          => s"${symbol.name}@${symbol.id}"
  }

  final private def v(tree: Desc)(implicit indent: Int): String = {
    val attr = tree.ref match {
      case Sym(symbol) =>
        symbol.attr.toSource pipe {
          case ""  => ""
          case str => str + "\n" + "  " * indent
        }
      case _ =>
        tree.attr map {
          v(_)
        } match {
          case Nil => ""
          case as  => as.mkString("(* ", ", ", " *)\n" + "  " * indent)
        }
    }
    val name = v(tree.ref)
    attr + v(tree, name)
  }

  // format: off
  final private def v(tree: Desc, name: String)(implicit indent: Int): String = tree match {
    case DescVar(_, _, spec, None) => s"desc ${v(spec)} $name"
    case DescVar(_, _, spec, Some(init)) => s"desc ${v(spec)} $name = ${v(init)}"
    case DescVal(_, _, spec, init) => s"desc val ${v(spec)} $name = ${v(init)}"
    case DescStatic(_, _, spec, None) => s"desc static ${v(spec)} $name"
    case DescStatic(_, _, spec, Some(init)) => s"desc static ${v(spec)} $name = ${v(init)}"
    case DescIn(_, _, spec, fct) => s"desc in ${v(fct)}${v(spec)} $name"
    case DescOut(_, _, spec, fct, st, None) => s"desc out ${v(fct)}${v(st)}${v(spec)} $name"
    case DescOut(_, _, spec, fct, st, Some(init)) => s"desc out ${v(fct)}${v(st)}${v(spec)} $name = ${v(init)}"
    case DescPipeVar(_, _, spec) => s"desc pipeline ${v(spec)} $name"
    case DescPipeIn(_, _, None, fct) => s"desc in ${v(fct)}pipeline $name"
    case DescPipeIn(_, _, Some(spec), fct) => s"desc in ${v(fct)}pipeline(${v(spec)}) $name"
    case DescPipeOut(_, _, None, fct, st) => s"desc out ${v(fct)}${v(st)}pipeline $name"
    case DescPipeOut(_, _, Some(spec), fct, st) => s"desc out ${v(fct)}${v(st)}pipeline(${v(spec)}) $name"
    case DescParam(_, _, spec, None, _) => s"desc param ${v(spec)} $name"
    case DescParam(_, _, spec, Some(init), _) => s"desc param ${v(spec)} $name = ${v(init)}"
    case DescParamType(_, _, None, _) => s"desc param type $name"
    case DescParamType(_, _, Some(init), _) => s"desc param type $name = ${v(init)}"
    case DescConst(_, _, spec, init) => s"desc const ${v(spec)} $name = ${v(init)}"
    case DescArray(_, _, elem, size) => s"desc ${v(elem)} $name[${v(size)}]"
    case DescSram(_, _, elem, size, st) => s"desc sram ${v(st)}${v(elem)} $name[${v(size)}]"
    case DescType(_, _, spec) => s"desc typedef ${v(spec)} $name"
    case DescEntity(_, _, variant, body) => block(s"desc ${v(variant)} $name", body)
    case DescRecord(_, _, body) => block(s"desc record $name", body)
    case DescInstance(_, _, spec) => s"desc $name = new ${v(spec)};"
    case DescSingleton(_, _, variant, body) => block(s"desc new ${v(variant)} $name", body)
    case DescFunc(_, _, _, ret, args, body) => block(s"desc ${v(ret)} $name(${vs(args)})", body)
    case DescPackage(_, _, body) => block(s"desc package $name", body)
    case DescGenVar(_, _, spec, init) => s"desc gen ${v(spec)} $name = ${v(init)}"
    case DescGenIf(_, _, cases, defaults) => cases.map(v(_)).mkString(s"gen : $name ", " else ", block(" else", defaults))
    case DescGenFor(_, _, inits, cond, steps, body) => block(s"gen for (${vs(inits)}; ${v(cond)} ; ${vs(steps)}) : $name", body)
    case DescGenRange(_, _, init, op, end, body) => block(s"gen for (${v(init)} $op ${v(end)}) : $name", body)
    case DescGenScope(_, _, body, wasLoop) => block(s"gen scope wasLoop=$wasLoop $name", body)
    case DescAlias(_, _, expr, exprt) => s"desc alias ${v(expr)} as $name${if (exprt) " export" else ""}"
    case DescParametrized(_, _, desc, symbolTable) => s"desc parametrized $name : ${v(desc)} with ${symbolTable.toString.replaceAll("\n", "\n" + "  " * indent)}"
  }
  // format: on

  final private def v(tree: Attr)(implicit indent: Int): String = tree match {
    case AttrBool(name)       => name
    case AttrExpr(name, expr) => s"$name = ${v(expr)}"
  }

  // format: off
  final private def v(tree: GenCase)(implicit indent: Int): String = tree match {
    case GenCase(cond, body) => block(s"if (${v(cond)})", body)
  }
  // format: on

  final private def v(tree: Decl)(implicit indent: Int): String = {
    val symbol = tree.symbol
    val attr = symbol.attr.toSource pipe {
      case ""  => ""
      case str => str + "\n" + "  " * indent
    }
    attr + v(tree, s"${symbol.name}@${symbol.id}")
  }

  // format: off
  final private def v(tree: Decl, name: String)(implicit indent: Int): String = tree match {
    case DeclVar(_, spec) => s"decl ${v(spec)} $name;"
    case DeclVal(_, spec) => s"decl val ${v(spec)} $name;"
    case DeclStatic(_, spec) => s"decl static ${v(spec)} $name;"
    case DeclIn(_, spec, fct) => s"decl in ${v(fct)}${v(spec)} $name;"
    case DeclOut(_, spec, fct, st) => s"decl out ${v(fct)}${v(st)}${v(spec)} $name;"
    case DeclPipeVar(_, spec) => s"decl pipeline ${v(spec)} $name;"
    case DeclPipeIn(_, pipeVars, fct) => s"decl in ${v(fct)}pipeline(${vs(pipeVars)}) $name;"
    case DeclPipeOut(_, pipeVars, fct, st) => s"decl out ${v(fct)}${v(st)}pipeline(${vs(pipeVars)}) $name;"
    case DeclConst(_, spec) => s"decl const ${v(spec)} $name;"
    case DeclArray(_, elem, size) => s"decl ${v(elem)} $name[$size];"
    case DeclSram(_, elem, size, st) => s"decl sram ${v(st)}${v(elem)} $name[$size];"
    case DeclStack(_, elem, size) => s"decl stack ${v(elem)} $name[$size];"
    case DeclType(_, spec) => s"decl typedef ${v(spec)} $name;"
    case DeclEntity(_, decls) => block(s"decl entity $name", decls)
    case DeclRecord(_, decls) => block(s"decl record $name", decls)
    case DeclInstance(_, spec) => s"decl $name = new ${v(spec)};"
    case DeclSingleton(_, decls) => block(s"decl new entity $name", decls)
    case DeclFunc(_, FuncVariant.Xeno, ret, args) => s"import ${v(ret)} $name(${vs(args)});"
    case DeclFunc(_, _, ret, args) => s"decl ${v(ret)} $name(${vs(args)});"
    case DeclState(_) => s"decl state $name;"
  }
  // format: on

  final private def v(tree: Defn)(implicit indent: Int): String = {
    val symbol = tree.symbol
    val attr = symbol.attr.toSource pipe {
      case ""  => ""
      case str => str + "\n" + "  " * indent
    }
    attr + v(tree, s"${symbol.name}@${symbol.id}")
  }

  // format: off
  final private def v(tree: Defn, name: String)(implicit indent: Int): String = tree match {
    case DefnVar(_, None) => s"defn $name;"
    case DefnVar(_, Some(init)) => s"defn $name = ${v(init)};"
    case DefnVal(_, init) => s"defn $name = ${v(init)};"
    case DefnStatic(_, None) => s"defn $name;"
    case DefnStatic(_, Some(init)) => s"defn $name = ${v(init)};"
    case DefnIn(_) => s"defn $name;"
    case DefnOut(_, None) => s"defn $name;"
    case DefnOut(_, Some(init)) => s"defn $name = ${v(init)};"
    case DefnPipeVar(_) => s"defn $name;"
    case DefnPipeIn(_) => s"defn $name;"
    case DefnPipeOut(_) => s"defn $name;"
    case DefnConst(_, init) => s"defn $name = ${v(init)};"
    case DefnArray(_) => s"defn $name;"
    case DefnSram(_) => s"defn $name;"
    case DefnStack(_) => s"defn $name;"
    case DefnType(_) => s"defn $name;"
    case DefnEntity(_, variant, body) => block(s"defn ${v(variant)} $name", body)
    case DefnRecord(_, body) => block(s"defn record $name", body)
    case DefnInstance(_) => s"defn $name;"
    case DefnSingleton(_, variant, body) => block(s"defn ${v(variant)} $name", body)
    case DefnFunc(_, args, body) => block(s"defn $name(${vs(args)})", body)
    case DefnState(_, body) => block(s"defn $name", body)
  }
  // format: on

  // format: off
  final private def v(tree: Import)(implicit indent: Int): String = tree match {
    case ImportOne(path, ident) => s"""import "$path" as ${v(ident)};"""
    case ImportPending(_, ident) => s"""import pending as ${v(ident)};"""
  }
  // format: on

  // format: off
  final private def v(tree: Using)(implicit indent: Int): String = tree match {
    case UsingOne(expr, Some(ident)) => s"using ${v(expr)} as ${v(ident)};"
    case UsingOne(expr, None) => s"using ${v(expr)};"
    case UsingAll(expr, exprt) => s"using ${v(expr)}.*${if (exprt) " export" else ""};"
    case UsingGenBody(expr, exclude) => s"using ${v(expr)}.*#[*] except ${exclude mkString ","};"
  }
  // format: on

  // format: off
  final private def v(tree: From)(implicit indent: Int): String = tree match {
    case FromOne(path, name, Some(ident)) => s"""from "$path" import ${v(name)} as ${v(ident)};"""
    case FromOne(path, name, None) => s"""from "$path" import ${v(name)};"""
    case FromAll(path) => s"""from "$path" import *;"""
  }
  // format: on

  final private def v(tree: Assertion)(implicit indent: Int): String = tree match {
    case AssertionAssert(cond, Some(msg))               => s"""assert ${v(cond)}, "$msg";"""
    case AssertionAssert(cond, None)                    => s"assert ${v(cond)};"
    case AssertionAssume(cond, Some(msg))               => s"""assume ${v(cond)}, "$msg";"""
    case AssertionAssume(cond, None)                    => s"assume ${v(cond)};"
    case AssertionStatic(cond, Some(msg))               => s"""static assert ${v(cond)}, "$msg";"""
    case AssertionStatic(cond, None)                    => s"static assert ${v(cond)};"
    case AssertionUnreachable(_, None, Some(msg))       => s"""unreachable "$msg";"""
    case AssertionUnreachable(_, None, None)            => "unreachable;"
    case AssertionUnreachable(_, Some(cond), Some(msg)) => s"""unreachable ${v(cond)} "$msg";"""
    case AssertionUnreachable(_, Some(cond), None)      => s"unreachable ${v(cond)};"
  }

  final private def v(tree: Pkg)(implicit indent: Int): String = tree match {
    case PkgSplice(tree)               => v(tree)
    case PkgCompile(expr, Some(ident)) => s"compile ${v(expr)} as ${v(ident)};"
    case PkgCompile(expr, None)        => s"compile ${v(expr)};"
  }

  // format: off
  final private def v(tree: Ent)(implicit indent: Int): String = tree match {
    case EntSplice(tree) => v(tree)
    case EntConnect(lhs, rhs) => s"${v(lhs)} -> ${vs(rhs)};"
    case EntAssign(lhs, rhs) => s"${v(lhs)} <- ${v(rhs)};"
    case EntCombProcess(stmts) => block("comb-process", stmts)
    case EntClockedProcess(clk, None, stmts) => block(s"clocked-process clk=${v(clk)}", stmts)
    case EntClockedProcess(clk, Some(rst), stmts) => block(s"clocked-process clk=${v(clk)} reset=${v(rst)}", stmts)
    case EntVerbatim(lang, body) => s"verbatim $lang {$body}"
    case EntComment(str) => "//" + str
  }
  // format: on

  final private def v(tree: Rec)(implicit indent: Int): String = tree match {
    case RecSplice(tree) => v(tree)
    case RecComment(str) => "//" + str
  }

  // format: off
  final private def v(tree: Stmt)(implicit indent: Int): String = tree match {
    case StmtSplice(tree) => s"${v(tree)}"
    case StmtBlock(body) => block("", body)
    case StmtIf(cond, ts, Nil) => block(s"if (${v(cond)})", ts)
    case StmtIf(cond, ts, es) => block(s"if (${v(cond)})", ts) + block(s" else", es)
    case StmtCase(expr, cases) => block(s"case (${v(expr)})", cases)
    case StmtLoop(body) => block("loop", body)
    case StmtWhile(cond, body) => block(s"while (${v(cond)})", body)
    case StmtFor(inits, cond, steps, body) => block(s"for (${vs(inits)} ; ${vo(cond)} ; ${vs(steps)})", body)
    case StmtDo(cond, body) => block(s"do", body) + s" while (${v(cond)});"
    case StmtLet(inits, body) => block(s"let (${vs(inits)})", body)
    case StmtFence() => "fence;"
    case StmtBreak() => "break;"
    case StmtContinue() => "continue;"
    case StmtGoto(expr) => s"goto ${v(expr)};"
    case StmtReturn(_, None) => "return;"
    case StmtReturn(_, Some(expr)) => s"return ${v(expr)};"
    case StmtAssign(lhs, rhs) => s"${v(lhs)} = ${v(rhs)};"
    case StmtUpdate(lhs, op, rhs) => s"${v(lhs)} $op= ${v(rhs)};"
    case StmtPost(expr, op) => s"${v(expr)}$op;"
    case StmtDelayed(lhs, rhs) => s"${v(lhs)} <= ${v(rhs)};"
    case StmtOutcall(o, f, is) => s"${v(f)}(${v(o)} <- ${vs(is)});"
    case StmtExpr(expr) => s"${v(expr)};"
    case StmtComment(str) => "// " + str
    case StmtWait(cond) => s"wait ${v(cond)};"
  }
  // format: on

  final private def v(tree: Case)(implicit indent: Int): String = tree match {
    case CaseSplice(tree)         => v(tree)
    case CaseRegular(cond, stmts) => block(s"${vs(cond)} :", stmts)
    case CaseDefault(stmts)       => block("default :", stmts)
  }

  // format: off
  final private def v(tree: Expr)(implicit indent: Int): String = tree match {
    case ExprCall(expr, args) => s"${v(expr)}(${vs(args)})"
    case ExprBuiltin(bf, args) => s"${bf.name}(${vs(args)})"
    case ExprUnary(op, expr) => s"$op${v(expr)}"
    case ExprBinary(lhs, op, rhs) => s"(${v(lhs)} $op ${v(rhs)})"
    case ExprCond(cond, thenExpr, elseExpr) => s"(${v(cond)} ? ${v(thenExpr)} : ${v(elseExpr)})"
    case ExprRep(count, expr) => s"{${v(count)}{${v(expr)}}}"
    case ExprCat(parts) => s"{${vs(parts)}}"
    case ExprIndex(expr, index) => s"${v(expr)}[${v(index)}]"
    case ExprSlice(expr, lIdx, op, rIdx) => s"${v(expr)}[${v(lIdx)}$op${v(rIdx)}]"
    case ExprDot(expr, selector, Nil) => s"${v(expr)}.$selector"
    case ExprDot(expr, selector, idxs) => s"${v(expr)}.$selector#[${vs(idxs)}]"
    case ExprSel(expr, selector) => s"${v(expr)}.$selector"
    case ExprSymSel(expr, selector) => s"${v(expr)}.$selector"
    case ExprIdent(base, Nil) => base
    case ExprIdent(base, idxs) => s"${base}#[${vs(idxs)}]"
    case ExprSym(symbol) => s"${symbol.name}@${symbol.id}"
    case ExprOld(expr) => s"old(${v(expr)})"
    case ExprThis(expr) => s"this(${v(expr)})"
    case ExprType(kind) => s"${kind.toSource}"
    case ExprCast(kind, expr) => s"(${kind.toSource})(${v(expr)})"
    case ExprInt(true, width, value) => s"$width'sd$value"
    case ExprInt(false, width, value) => s"$width'd$value"
    case ExprNum(true, value) => s"'sd$value"
    case ExprNum(false, value) => s"$value"
    case ExprStr(value) => s""""$value""""
  }
  // format: on

  final private def v(tree: Arg)(implicit indent: Int): String = tree match {
    case ArgP(expr)             => v(expr)
    case ArgN(name, expr)       => s"$name = ${v(expr)}"
    case ArgD(name, idxs, expr) => vs(idxs, s"$name#[", ",", s"] = ${v(expr)}")
  }

}
