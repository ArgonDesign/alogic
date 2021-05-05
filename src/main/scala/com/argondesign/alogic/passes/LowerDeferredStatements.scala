////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Lower deferred statement into separate clocked processes.
// Deferred statements are:
// - assertions (also converts originals to assumptions to aid later folding).
// - some built-in functions like @display and @finish
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.builtins.AtDisplay
import com.argondesign.alogic.builtins.AtFinish
import com.argondesign.alogic.core.Bindings
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.enums.ResetStyle
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

final class LowerDeferredStatements(implicit cc: CompilerContext) extends StatelessTreeTransformer {

  // List of deferred statements
  private val deferredStatements = mutable.ListBuffer[Stmt]()

  // List of additional symbols created
  private val extraSymbols = mutable.ListBuffer[Symbol]()

  // Deferred enable symbol for the whole entity, used to gate deferred
  // statements with go and reset. Separate signal otherwise Verilator
  // complains SYNCASYNCNET about asynchronous reset being used in a block
  // which is not sensitive to it. Also helps readability.
  private var deferredEnSymbolOpt: Option[Symbol] = None

  // Sequence numbers for name disambiguation
  private val sequenceNumbers = mutable.Map[String, Iterator[Int]]()

  // How deep are within a conditional (does not include the state dispatch
  // statement)
  private var condLvl = 0

  // Predicate indicating what kind of symbols need to be copied for deferment
  // as their values might change.
  private def modifiable(symbol: Symbol): Boolean = symbol.kind match {
    // Fundamental type
    case _: TypeInt    => true
    case _: TypeVector => true
    case _: TypeRecord => true
    case _: TypeEntity => false
    // Derived
    case _: TypeIn    => false
    case _: TypeOut   => true // conservative: might be driven by EntAssign...
    case _: TypeConst => false
    case _: TypeArray => false
    case _: TypeSram  => false
    case _: TypeStack => false
    // Pipeline ports
    // Function types
    case _: TypeCombFunc => false
    case _: TypeXenoFunc => false
    // Misc types
    case _: TypeType => false
    //
    case _: TypeNum | TypeVoid | TypeStr | _: TypePipeVar | _: TypeParam | _: TypeGen |
        _: TypeCtrlFunc | _: TypeStaticMethod | _: TypeNormalMethod | _: TypePipeIn |
        _: TypePipeOut | _: TypeNone | _: TypeParametrized | TypeCombStmt | TypeCtrlStmt |
        _: TypeState | TypeMisc | TypeBuiltin | TypeError | _: TypePackage | _: TypeScope |
        _: TypeSnoop =>
      unreachable
  }

  private def nextPrefix(base: String): String =
    s"${base}_${sequenceNumbers.getOrElseUpdate(base, Iterator.from(0)).next()}"

  private def makeEnSymbol(prefix: String, loc: Loc): Symbol = {
    val enSymbol = Symbol(s"${prefix}_en", loc)
    enSymbol.kind = TypeUInt(1)
    enSymbol.attr.combSignal set true
    enSymbol
  }

  private def extractDependencies(
      prefix: String,
      tree: Tree
    ): (List[(Symbol, Symbol)], Bindings) = {
    val deps = List from {
      val seqNum = Iterator.from(0)
      tree.collect {
        case expr @ ExprSym(symbol) if modifiable(symbol) =>
          val newSymbol = Symbol(s"${prefix}_${seqNum.next()}", expr.loc)
          newSymbol.kind = symbol.kind.underlying
          newSymbol.attr.combSignal set true
          symbol -> newSymbol
      }
    }
    val bindings = Bindings from {
      deps.iterator map {
        case (symbol, newSymbol) =>
          symbol -> TypeAssigner(ExprSym(newSymbol) withLoc newSymbol.loc)
      }
    }
    (deps, bindings)
  }

  private def deferredReplacement(
      enSymbol: Symbol,
      dependencies: List[(Symbol, Symbol)]
    ): List[Stmt] =
    // Set enable variable
    (StmtAssign(ExprSym(enSymbol), ExprInt(false, 1, 1)) regularize enSymbol.loc) ::
      // Copy out dependencies
      dependencies.map { case (s, ns) => StmtAssign(ExprSym(ns), ExprSym(s)) regularize ns.loc }

  private def isStateVariable(expr: Expr): Boolean = expr match {
    case ExprSym(symbol) => symbol.attr.stateVariable.isSet
    case _               => false
  }

  override def enter(tree: Tree): Option[Tree] = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Keep track of condition level, but ignore the state dispatch
    ////////////////////////////////////////////////////////////////////////////

    case stmt: StmtIf =>
      if (!isStateVariable(stmt.cond)) {
        condLvl += 1
      }
      None

    case stmt: StmtCase =>
      if (!isStateVariable(stmt.expr)) {
        condLvl += 1
      }
      None

    ////////////////////////////////////////////////////////////////////////////
    // Skip Expr nodes
    ////////////////////////////////////////////////////////////////////////////

    case _: Expr => Some(tree)

    //
    case _ => None
  }

  override def transform(tree: Tree): Tree = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Keep track of condition level, but ignore the state dispatch
    ////////////////////////////////////////////////////////////////////////////

    case stmt: StmtIf =>
      if (!isStateVariable(stmt.cond)) {
        condLvl -= 1
      }
      tree

    case stmt: StmtCase =>
      if (!isStateVariable(stmt.expr)) {
        condLvl -= 1
      }
      tree

    ////////////////////////////////////////////////////////////////////////////
    // Convert 'assert'
    ////////////////////////////////////////////////////////////////////////////

    case StmtSplice(AssertionAssert(cond, msgOpt)) =>
      if (!cc.settings.assertions) {
        // If assertions are not enabled, just convert them to assumptions if the
        // condition is pure, otherwise drop them
        if (cond.isPure) {
          StmtSplice(AssertionAssume(cond, msgOpt)) regularize tree.loc
        } else {
          Stump
        }
      } else {
        // If assertions are enabled, convert them to deferred assertions, but
        // leave in the original as an assume to be used by folding. The assume
        // will be removed by the RemoveAssume pass.
        val prefix = nextPrefix("_assert")
        val enSymbol = makeEnSymbol(prefix, tree.loc)
        val (dependencies, bindings) = extractDependencies(prefix, cond)

        extraSymbols.append(enSymbol)
        extraSymbols.appendAll(dependencies.map(_._2))

        deferredStatements.append(
          StmtComment(s"'assert' on line ${tree.loc.line}") regularize tree.loc
        )
        deferredStatements.append(
          StmtIf(
            ExprSym(enSymbol),
            StmtSplice(AssertionAssert(cond substitute bindings, msgOpt)) :: Nil,
            Nil
          ) regularize tree.loc
        )

        // Replace statement
        Thicket {
          val replacement = deferredReplacement(enSymbol, dependencies)
          // Convert to assume if condition is pure
          if (cond.isPure) {
            (StmtSplice(AssertionAssume(cond, msgOpt)) regularize cond.loc) :: replacement
          } else {
            replacement
          }
        }
      }

    ////////////////////////////////////////////////////////////////////////////
    // Convert 'unreachable'
    ////////////////////////////////////////////////////////////////////////////

    case StmtSplice(AssertionUnreachable(_, condOpt, msgOpt)) =>
      assert(condOpt.isEmpty)
      if (condLvl == 0) {
        // 'unreachable' is not under a conditional, i.e.: it is reachable
        val suffix = msgOpt.map(": " + _).getOrElse("")
        cc.error(tree, s"'unreachable' statement is always reached$suffix")
        Stump
      } else if (!cc.settings.assertions) {
        // If assertions are disabled, just drop them
        Stump
      } else {
        // If assertions are enabled, convert to deferred assertions.
        val prefix = nextPrefix("_unreachable")
        val enSymbol = makeEnSymbol(prefix, tree.loc)

        extraSymbols.append(enSymbol)

        deferredStatements.append(
          StmtComment(s"'unreachable' on line ${tree.loc.line}") regularize tree.loc
        )
        deferredStatements.append(
          StmtSplice(
            AssertionUnreachable(Some(true), Some(~ExprSym(enSymbol)), msgOpt)
          ) regularize tree.loc
        )

        // Replace statement
        Thicket(deferredReplacement(enSymbol, Nil))
      }

    ////////////////////////////////////////////////////////////////////////////
    // Convert deferred built-ins
    ////////////////////////////////////////////////////////////////////////////

    case StmtExpr(expr @ ExprBuiltin(builtin, args)) =>
      if (builtin == AtFinish || builtin == AtDisplay) {
        // Convert to deferred statement
        val prefix = nextPrefix(builtin.name.replace('@', '_'))
        val enSymbol = makeEnSymbol(prefix, tree.loc)
        val (dependencies, bindings) = extractDependencies(prefix, expr)

        extraSymbols.append(enSymbol)
        extraSymbols.appendAll(dependencies.map(_._2))

        deferredStatements.append(
          StmtComment(s"'${builtin.name}' on line ${tree.loc.line}") regularize tree.loc
        )
        deferredStatements.append {
          val newArgs = args.map {
            case ArgP(a) => TypeAssigner(ArgP(a substitute bindings) withLocOf a)
            case _       => unreachable
          }
          StmtIf(
            ExprSym(enSymbol),
            StmtExpr(TypeAssigner(expr.copy(args = newArgs) withLocOf expr)) :: Nil,
            Nil
          ) regularize tree.loc
        }

        Thicket(deferredReplacement(enSymbol, dependencies))
      } else {
        tree
      }

    ////////////////////////////////////////////////////////////////////////////
    // Add implementations of assertions to the entity
    ////////////////////////////////////////////////////////////////////////////

    case defn: DefnEntity if deferredStatements.nonEmpty =>
      val dEnSymbol = Symbol(s"_deferred_en", tree.loc)
      dEnSymbol.kind = TypeUInt(1)
      deferredEnSymbolOpt = Some(dEnSymbol)
      val newBody = List from {
        defn.body.iterator concat {
          // Extra symbol Defns
          extraSymbols.iterator.map(symbol => EntSplice(symbol.mkDefn) regularize symbol.loc)
        } concat {
          // Top enable signal Defn + assignment
          val dEnDefn = EntSplice(dEnSymbol.mkDefn) regularize defn.loc;
          val dEnAssign = {
            val rstInactive = cc.settings.resetStyle match {
              case ResetStyle.AsyncHigh | ResetStyle.SyncHigh => !ExprSym(defn.rst.get)
              case ResetStyle.AsyncLow | ResetStyle.SyncLow   => ExprSym(defn.rst.get)
              case _                                          => unreachable
            }
            val cond = defn.go match {
              case Some(goSymbol) => ExprSym(goSymbol) && rstInactive
              case None           => rstInactive
            }
            EntAssign(ExprSym(dEnSymbol), cond) regularize defn.loc
          }
          Iterator(dEnDefn, dEnAssign)
        } concat {
          // Clocked process for deferred statements
          Iterator single {
            TypeAssigner(
              EntClockedProcess(
                ExprSym(defn.clk.get) regularize tree.loc,
                None,
                List(
                  StmtComment("Deferred statements") regularize tree.loc,
                  StmtIf(ExprSym(dEnSymbol), deferredStatements.toList, Nil) regularize tree.loc
                )
              ) withLoc tree.loc
            )
          }
        }
      }
      TypeAssigner(defn.copy(body = newBody) withLoc tree.loc)

    case decl: DeclEntity if deferredStatements.nonEmpty =>
      val newDecls = List from {
        decl.decls.iterator concat {
          // Extra symbol Decls
          extraSymbols.iterator.map(symbol => symbol.mkDecl regularize symbol.loc)
        } concat {
          // Top enable signal Decl
          deferredEnSymbolOpt.iterator.map(_.mkDecl regularize decl.loc)
        }
      }
      TypeAssigner(decl.copy(decls = newDecls) withLoc tree.loc)

    //
    case _ => tree
  }

  override protected def finalCheck(tree: Tree): Unit = {
    assert(condLvl == 0)
  }

}

object LowerDeferredStatements extends EntityTransformerPass(declFirst = false) {
  val name = "lower-deferred-statements"

  def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
    new LowerDeferredStatements
}
