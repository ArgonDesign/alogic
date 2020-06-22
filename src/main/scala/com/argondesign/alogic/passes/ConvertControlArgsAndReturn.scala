////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Replace control function arguments and return values with static storage.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FuncVariant
import com.argondesign.alogic.core.Symbols
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types.TypeVoid
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class ConvertControlArgsAndReturn(implicit cc: CompilerContext)
    extends StatefulTreeTransformer {

  // Map from control function symbol to their argument symbols
  private val argsMap = mutable.LinkedHashMap[Symbol, List[Symbol]]()
  // Map from control function symbol to their return value symbol
  private val retMap = mutable.LinkedHashMap[Symbol, Symbol]()
  // Temporary symbols
  private val tmpSymbols = mutable.ListBuffer[Symbol]()
  // List of statements to emit before a statement
  private val extraStmts = mutable.Stack[mutable.ListBuffer[Stmt]]()

  override def start(tree: Tree): Unit = tree match {
    case defn: DefnEntity =>
      // Allocate argument and return value symbols up front so we know what to replace
      defn.functions foreach {
        case DefnFunc(symbol, args, _)
            if symbol.kind.isCtrlFunc && (args.nonEmpty || !symbol.kind.asCtrlFunc.retType.isVoid) =>
          // TODO: This is only here because of the confusion it causes. With proper
          // local variables (i.e.: stacked ones), we should have this.
          if (args.nonEmpty && symbol.attr.recLimit.value.value.get != 1) {
            cc.error(args.head, "Recursive control function cannot take arguments")
          }
          // Get hold of arguments, construct return symbol if non-void return
          argsMap(symbol) = args map { _.symbol }
          val retKind = symbol.kind.asCtrlFunc.retType
          if (!retKind.isVoid) {
            val retSymbol = cc.newSymbol(s"_${symbol.name}${cc.sep}ret", tree.loc)
            retSymbol.kind = retKind
            retMap(symbol) = retSymbol
          }
        case _ =>
      }
    case _ =>
  }

  override def skip(tree: Tree): Boolean = argsMap.isEmpty && retMap.isEmpty

  override def replace(symbol: Symbols.Symbol): Boolean =
    (argsMap contains symbol) || (retMap contains symbol)

  override def enter(tree: Tree): Option[Tree] = tree match {
    case _: Stmt =>
      extraStmts.push(new ListBuffer)
      None
    case _ => None
  }

  override def transform(tree: Tree): Tree = tree match {
    case decl: DeclEntity if argsMap.nonEmpty || retMap.nonEmpty =>
      // Update function attributes
      decl.functions.iterator filter { _.symbol.kind.isCtrlFunc } foreach { d =>
        val srp = d.symbol.attr.staticReturnPoint.value match {
          case None                => None
          case some @ Some(symbol) => repl(symbol) orElse some
        }
        d.symbol.attr.staticReturnPoint set srp
      }
      // Add decls
      val extraDecls = argsMap.valuesIterator.flatten map {
        _.decl
      } concat {
        retMap.valuesIterator map { symbol => symbol.mkDecl regularize symbol.loc }
      } concat {
        tmpSymbols.iterator map { symbol => symbol.mkDecl regularize symbol.loc }
      }
      TypeAssigner(decl.copy(decls = decl.decls appendedAll extraDecls) withLoc tree.loc)

    case defn: DefnEntity if argsMap.nonEmpty || retMap.nonEmpty =>
      // Add defns
      val extraBody = argsMap.valuesIterator.flatten map { symbol =>
        TypeAssigner(EntDefn(symbol.defn) withLoc symbol.loc)
      } concat {
        retMap.valuesIterator map { symbol => EntDefn(symbol.mkDefn) regularize symbol.loc }
      } concat {
        tmpSymbols.iterator map { symbol => EntDefn(symbol.mkDefn) regularize symbol.loc }
      }
      TypeAssigner(defn.copy(body = defn.body appendedAll extraBody) withLoc defn.loc)

    case decl @ DeclFunc(symbol, FuncVariant.Ctrl, ret, args)
        if args.nonEmpty || !ret.tpe.asType.kind.isVoid =>
      // Update argMap/retMap (symbol is being replaced)
      argsMap.remove(orig(symbol)) foreach { argsMap(symbol) = _ }
      retMap.remove(orig(symbol)) foreach { retMap(symbol) = _ }
      // Drop arguments from decl, convert return type to void
      ret.tpe.asType.kind match {
        case TypeVoid =>
          TypeAssigner(decl.copy(args = Nil) withLoc tree.loc)
        case _ =>
          val exprVoid = TypeAssigner(ExprType(TypeVoid) withLoc ret.loc)
          TypeAssigner(decl.copy(ret = exprVoid, args = Nil) withLoc tree.loc)
      }

    case defn @ DefnFunc(_, args, _) if args.nonEmpty =>
      // Drop arguments from defn
      TypeAssigner(defn.copy(args = Nil) withLoc tree.loc)

    case StmtReturn(false, Some(expr)) =>
      // Replace return statements containing return values with assignment to
      // return symbol and return without value.
      val fSymbol = enclosingSymbols.head
      assert(fSymbol.kind.isCtrlFunc)
      val stmts = extraStmts.pop()
      stmts append {
        StmtAssign(ExprSym(retMap(fSymbol)), expr) regularize tree.loc
      }
      stmts append {
        TypeAssigner(StmtReturn(comb = false, None) withLoc tree.loc)
      }
      Thicket(stmts.toList)

    case stmt: Stmt =>
      // Emit extra statements, if any
      val stmts = extraStmts.pop()
      if (stmts.nonEmpty) Thicket((stmts append stmt).toList) else tree

    case call: ExprCall if call.expr.tpe.isCtrlFunc =>
      // Convert calls to control functions
      val tgtSymbol = call.expr.tpe.asCtrlFunc.symbol
      // Convert arguments to assignments
      val noArgsCall = call.args pipe {
        case Nil => call
        case args =>
          val argSymbols = argsMap(tgtSymbol)
          assert(argSymbols.lengthIs == args.length)
          (argSymbols lazyZip args) foreach {
            case (argSymbol, ArgP(expr)) =>
              extraStmts.top append {
                StmtAssign(ExprSym(argSymbol), expr) regularize expr.loc
              }
            case _ => unreachable
          }
          TypeAssigner(call.copy(args = Nil) withLoc tree.loc)
      }
      // Replace with return symbol, if non void
      retMap.get(tgtSymbol) match {
        case None            => noArgsCall
        case Some(retSymbol) =>
          // Perform call
          extraStmts.top append {
            TypeAssigner(StmtExpr(noArgsCall) withLoc tree.loc)
          }
          // We use a temporary to stash the result, in case there are multiple
          // calls to the same control function within an expression. This will
          // later be optimized away if redundant.
          val tmpSymbol = cc.newTemp(retSymbol.name + cc.sep + "tmp", retSymbol.loc, retSymbol.kind)
          tmpSymbols.append(tmpSymbol)
          extraStmts.top append {
            StmtAssign(ExprSym(tmpSymbol), ExprSym(retSymbol)) regularize tree.loc
          }
          // Replace with temporary
          TypeAssigner(ExprSym(tmpSymbol) withLoc tree.loc)
      }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(extraStmts.isEmpty)
  }

}

object ConvertControlArgsAndReturn extends EntityTransformerPass(declFirst = false) {
  val name = "convert-control-args-and-return"

  def create(symbol: Symbols.Symbol)(implicit cc: CompilerContext): TreeTransformer =
    new ConvertControlArgsAndReturn
}
