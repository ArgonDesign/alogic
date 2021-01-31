////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Replace control function arguments and return values with entity variables
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.Types.TypeRecord
import com.argondesign.alogic.core.Types.TypeStack
import com.argondesign.alogic.core.Types.TypeType
import com.argondesign.alogic.core.Types.TypeVoid
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class ConvertCtrlFuncArgret(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  // Extra type symbols to add to the global list
  private val extraTypeSymbols = mutable.ListBuffer[Symbol]()
  // Extra symbols to add to the entity
  private val extraEntitySymbols = mutable.ListBuffer[Symbol]()
  // Map from control function symbol to the argument storage symbol
  private val argsMap = mutable.LinkedHashMap[Symbol, Symbol]()
  // Map from control function symbol to their return value symbol
  private val retMap = mutable.LinkedHashMap[Symbol, Symbol]()
  // List of statements to emit before a statement
  private val extraStmts = mutable.Stack[mutable.ListBuffer[Stmt]]()
  // Set of arguments of the current control function
  private var arguments: Set[Symbol] = Set.empty

  override def start(tree: Tree): Unit = tree match {
    case defn: DefnEntity =>
      // Construct argument and return value storage up front so we know what to replace
      defn.functions foreach {
        case DefnFunc(symbol, args, _) if symbol.kind.isCtrlFunc =>
          // Construct argument storage
          if (args.nonEmpty) {
            // Create the args structure type
            val mSymbols = args map { case Defn(symbol) => symbol.dup tap { _.kind = symbol.kind } }
            val sSymbol = cc.newSymbol(s"${symbol.name}${cc.sep}args_t", symbol.loc)
            val sKind = TypeRecord(sSymbol, mSymbols)
            sSymbol.kind = TypeType(sKind)
            extraTypeSymbols append sSymbol
            // Create the args variable/stack
            val aSymbol = cc.newSymbol(s"${symbol.name}${cc.sep}args", symbol.loc)
            aSymbol.kind = {
              val recLimit = symbol.attr.recLimit.value
              if (recLimit > 1) TypeStack(sKind, recLimit) else sKind
            }
            extraEntitySymbols append aSymbol
            // Add to map
            argsMap(symbol) = aSymbol
          }
          // Construct return value storage
          val rKind = symbol.kind.asCtrlFunc.retType
          if (!rKind.isVoid) {
            // Create the return variable
            val rSymbol = cc.newSymbol(s"${symbol.name}${cc.sep}return", symbol.loc)
            rSymbol.kind = rKind
            extraEntitySymbols append rSymbol
            // Add to map
            retMap(symbol) = rSymbol
          }
        case _ =>
      }
    case _ =>
  }

  override def replace(symbol: Symbols.Symbol): Boolean =
    (argsMap contains symbol) || (retMap contains symbol)

  override def enter(tree: Tree): Option[Tree] = tree match {
    case _ if argsMap.isEmpty && retMap.isEmpty => Some(tree)

    case DefnFunc(symbol, args, _) if symbol.kind.isCtrlFunc =>
      arguments = Set from { args.iterator map { _.symbol } }
      None

    case _: Stmt =>
      extraStmts.push(new ListBuffer)
      None

    case _ => None
  }

  override def transform(tree: Tree): Tree = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Rewrite entity Decl/Defn
    ////////////////////////////////////////////////////////////////////////////

    case decl: DeclEntity =>
      // Update control function attributes now that we know all replacements
      decl.functions.iterator filter { _.symbol.kind.isCtrlFunc } foreach { d =>
        d.symbol.attr.staticReturnPoint set {
          d.symbol.attr.staticReturnPoint.value match {
            case None                => None
            case some @ Some(symbol) => repl(symbol) orElse some
          }
        }
      }
      // Add extra decls
      val extraDecls = extraEntitySymbols.iterator map { symbol =>
        symbol.mkDecl regularize symbol.loc
      }
      TypeAssigner(decl.copy(decls = decl.decls appendedAll extraDecls) withLoc tree.loc)

    case defn: DefnEntity =>
      // Add extra defns
      val extraBody = extraEntitySymbols.iterator map { symbol =>
        EntSplice(symbol.mkDefn) regularize symbol.loc
      }
      TypeAssigner(defn.copy(body = defn.body appendedAll extraBody) withLoc defn.loc)

    ////////////////////////////////////////////////////////////////////////////
    // Rewrite control function Decl/Defn
    ////////////////////////////////////////////////////////////////////////////

    case decl @ DeclFunc(symbol, _, ret, args)
        if symbol.kind.isCtrlFunc && (args.nonEmpty || !ret.tpe.asType.kind.isVoid) =>
      // Update argMap/retMap (symbol is being replaced)
      val oSymbol = orig(symbol)
      argsMap.remove(oSymbol) foreach { argsMap(symbol) = _ }
      retMap.remove(oSymbol) foreach { retMap(symbol) = _ }
      // Drop arguments from decl, convert return type to void
      val exprVoid = TypeAssigner(ExprType(TypeVoid) withLoc ret.loc)
      TypeAssigner(decl.copy(ret = exprVoid, args = Nil) withLoc tree.loc)

    case defn @ DefnFunc(symbol, args, _) if symbol.kind.isCtrlFunc && args.nonEmpty =>
      // Drop arguments from defn
      TypeAssigner(defn.copy(args = Nil) withLoc tree.loc)

    ////////////////////////////////////////////////////////////////////////////
    // Rewrite Statements
    ////////////////////////////////////////////////////////////////////////////

    case StmtReturn(false, exprOpt) =>
      val fSymbol = enclosingSymbols.head ensuring { _.kind.isCtrlFunc }
      val stmts = extraStmts.pop()
      // Pop arguments if needed
      argsMap.get(fSymbol) filter { _.kind.isStack } foreach { aSymbol =>
        stmts append { StmtExpr(ExprSym(aSymbol) sel "pop" call Nil) regularize tree.loc }
      }
      // Assign return value if needed
      exprOpt foreach { expr =>
        stmts append { StmtAssign(ExprSym(retMap(fSymbol)), expr) regularize tree.loc }
      }
      // Replace with void return
      stmts append { TypeAssigner(StmtReturn(comb = false, None) withLoc tree.loc) }
      Thicket(stmts.toList)

    case stmt: Stmt =>
      // Emit extra statements, if any
      extraStmts.pop() match {
        case stmts if stmts.nonEmpty => Thicket((stmts append stmt).toList)
        case _                       => tree
      }

    ////////////////////////////////////////////////////////////////////////////
    // Rewrite Expressions
    ////////////////////////////////////////////////////////////////////////////

    case call: ExprCall if call.expr.tpe.isCtrlFunc =>
      // Convert calls to control functions
      val fSymbol = call.expr.tpe.asCtrlFunc.symbol
      val stmts = extraStmts.top
      // Push arguments if needed
      argsMap.get(fSymbol) filter { _.kind.isStack } foreach { aSymbol =>
        stmts append { StmtExpr(ExprSym(aSymbol) sel "push" call Nil) regularize tree.loc }
      }
      // Convert arguments to assignments, construct no arguments call
      val noArgsCall = argsMap.get(fSymbol) map { aSymbol =>
        // Construct base argument storage reference
        val argsExpr = aSymbol.kind pipe {
          case _: TypeStack => ExprSym(aSymbol) sel "top"
          case _            => ExprSym(aSymbol)
        } regularize call.loc
        // The call should have the right number of arguments
        assert(argsExpr.tpe.asRecord.members.length == call.args.length)
        // Construct argument assignments
        (argsExpr.tpe.asRecord.members.iterator zip call.args.iterator) foreach {
          case (mSymbol, ArgP(expr)) =>
            stmts append { StmtAssign(argsExpr sel mSymbol.name, expr) regularize expr.loc }
          case _ => unreachable
        }
        // Construct no arguments call
        TypeAssigner(call.copy(args = Nil) withLoc tree.loc)

      } getOrElse call
      // If non-void, replace with return symbol
      retMap.get(fSymbol) map { rSymbol =>
        // Perform call
        stmts append { TypeAssigner(StmtExpr(noArgsCall) withLoc tree.loc) }
        // We use a temporary to stash the result, in case there are multiple
        // calls to the same control function within an expression. This will
        // later be optimized away if redundant. Note that this needs to be a
        // local variable in case of recursion, so add it as such, initialized
        // to the return value
        val tSymbol = cc.newTemp(s"${rSymbol.name}${cc.sep}copy", tree.loc, rSymbol.kind)
        stmts append { StmtSplice(tSymbol.mkDecl) regularize tree.loc }
        stmts append { StmtSplice(tSymbol.mkDefn(ExprSym(rSymbol))) regularize tree.loc }
        // Replace with the temporary holding the return value
        TypeAssigner(ExprSym(tSymbol) withLoc tree.loc)
      } getOrElse noArgsCall

    case ExprSym(symbol) if arguments(symbol) =>
      // Replace references to arguments
      val fSymbol = enclosingSymbols.find(_.kind.isCtrlFunc).get
      val aSymbol = argsMap(fSymbol)
      aSymbol.kind pipe {
        case _: TypeStack => ExprSym(aSymbol) sel "top" sel symbol.name
        case _            => ExprSym(aSymbol) sel symbol.name
      } regularize tree.loc

    //
    case _ => tree
  }

  override protected def finish(tree: Tree): Tree = tree match {
    case defn: DefnEntity =>
      Thicket(defn :: extraTypeSymbols.iterator.map(s => s.mkDefn regularize s.loc).toList)
    case decl: DeclEntity =>
      Thicket(decl :: extraTypeSymbols.iterator.map(s => s.mkDecl regularize s.loc).toList)
    case _ => unreachable
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(extraStmts.isEmpty)
  }

}

object ConvertCtrlFuncArgret extends EntityTransformerPass(declFirst = false, parallel = true) {
  val name = "convert-ctrl-func-argret"

  override def skip(decl: DeclEntity, defn: DefnEntity): Boolean = defn.functions.isEmpty

  def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
    new ConvertCtrlFuncArgret
}
