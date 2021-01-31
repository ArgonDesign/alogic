////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Lower assertions into separate clocked processes, and convert originals
// to assumptions to aid folding.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.core.Bindings
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.enums.ResetStyle
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class LowerAssertions(implicit cc: CompilerContext) extends StatelessTreeTransformer {

  // List of emitted assertions (enable signal, condition, condition symbols, message, comment)
  private val assertions =
    mutable.ListBuffer[(Symbol, Option[Expr], List[Symbol], Option[String], String)]()

  // Assertion enable symbol for the whole entity, used to gate assertions with
  // go and reset. Separate signal otherwise Verilator complains SYNCASYNCNET
  // about asynchronous reset being used in a block which is not sensitive to
  // it. Also helps readability.
  private var aEnSymbolOpt: Option[Symbol] = None

  // Sequence numbers for name disambiguation
  private val assertSeqNum = LazyList.from(0).iterator
  private val unreachableSeqNum = LazyList.from(0).iterator

  // How deep are within a conditional (does not inclde the state dispatch
  // statement)
  private var condLvl = 0

  // Predicate indicating what kind of symbols need to be copied for assertions
  // as their values might change.
  private def modifiable(symbol: Symbol): Boolean = symbol.kind match {
    // Fundamental type
    case _: TypeInt    => true
    case _: TypeVector => true
    case _: TypeRecord => true
    case _: TypeEntity => false
    // Derived
    case _: TypeIn          => false
    case TypeOut(_, fct, _) => fct == FlowControlTypeNone
    case _: TypeConst       => false
    case _: TypeArray       => false
    case _: TypeSram        => false
    case _: TypeStack       => false
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
        _: TypeState | TypeMisc | TypeBuiltin | TypeError | _: TypePackage | _: TypeScope =>
      unreachable
  }

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

    // If assertions are not enabled, just convert them to assumptions if the
    // condition is pure, otherwise drop them
    case StmtSplice(AssertionAssert(cond, msgOpt)) if !cc.settings.assertions =>
      if (cond.isPure) {
        StmtSplice(AssertionAssume(cond, msgOpt)) regularize tree.loc
      } else {
        Stump
      }

    // If assertions are enabled, convert them to deferred assertions, but
    // leave in the original as an assume to be used by folding. The assume
    // will be removed by the RemoveAssume pass.
    case StmtSplice(AssertionAssert(cond, msgOpt)) =>
      val prefix = s"_assert_${assertSeqNum.next()}"
      val comment = s"'assert' on line ${tree.loc.line}"
      val enSymbol = cc.newSymbol(s"${prefix}_en", tree.loc)
      enSymbol.kind = TypeUInt(1)
      enSymbol.attr.combSignal set true
      val deps = List from {
        val seqNum = LazyList.from(0).iterator
        cond collect {
          case expr @ ExprSym(symbol) if modifiable(symbol) =>
            val newSymbol = cc.newSymbol(s"${prefix}_c${seqNum.next()}", expr.loc)
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
      assertions.append(
        (enSymbol, Some(cond substitute bindings), deps map { _._2 }, msgOpt, comment)
      )
      val lb = new ListBuffer[Stmt]
      lb.append(StmtAssign(ExprSym(enSymbol), ExprInt(false, 1, 1)) regularize tree.loc)
      deps foreach {
        case (symbol, newSymbol) =>
          lb.append(StmtAssign(ExprSym(newSymbol), ExprSym(symbol)) regularize cond.loc)
      }
      if (cond.isPure) {
        lb.append(StmtSplice(AssertionAssume(cond, msgOpt)) regularize cond.loc)
      }
      Thicket(lb.toList)

    ////////////////////////////////////////////////////////////////////////////
    // Convert 'unreachable'
    ////////////////////////////////////////////////////////////////////////////

    // 'unreachable' is not under a conditional, i.e.: it is reachable
    case StmtSplice(AssertionUnreachable(msgOpt)) if condLvl == 0 =>
      val suffix = msgOpt.map(": " + _).getOrElse("")
      cc.error(tree, s"'unreachable' statement is always reached$suffix")
      Stump

    // If assertions are disabled, just drop them
    case StmtSplice(_: AssertionUnreachable) if !cc.settings.assertions =>
      Stump

    // If assertions are enabled, convert to deferred assertions.
    case StmtSplice(AssertionUnreachable(msgOpt)) =>
      val prefix = s"_unreachable_${unreachableSeqNum.next()}"
      val comment = s"'unreachable' on line ${tree.loc.line}"
      val enSymbol = cc.newSymbol(s"${prefix}_en", tree.loc)
      enSymbol.kind = TypeUInt(1)
      enSymbol.attr.combSignal set true
      assertions.append((enSymbol, None, Nil, msgOpt, comment))
      StmtAssign(ExprSym(enSymbol), ExprInt(false, 1, 1)) regularize tree.loc

    ////////////////////////////////////////////////////////////////////////////
    // Add implementations of assertions to the entity
    ////////////////////////////////////////////////////////////////////////////

    case defn: DefnEntity if assertions.nonEmpty =>
      val aEnSymbol = cc.newSymbol(s"_assertions_en", tree.loc)
      aEnSymbol.kind = TypeUInt(1)
      aEnSymbolOpt = Some(aEnSymbol)
      val newBody = List from {
        defn.body.iterator concat {
          assertions.iterator flatMap {
            case (enSymbol, _, condSymbols, _, _) =>
              Iterator.single(EntSplice(enSymbol.mkDefn) regularize enSymbol.loc) concat {
                condSymbols.iterator map { condSymbol =>
                  EntSplice(condSymbol.mkDefn) regularize condSymbol.loc
                }
              }
          }
        } concat {
          val aEnDefn = EntSplice(aEnSymbol.mkDefn) regularize defn.loc;
          val aEnAssign = {
            val rstInactive = cc.settings.resetStyle match {
              case ResetStyle.AsyncHigh | ResetStyle.SyncHigh => !ExprSym(defn.rst.get)
              case ResetStyle.AsyncLow | ResetStyle.SyncLow   => ExprSym(defn.rst.get)
              case _                                          => unreachable
            }
            val cond = defn.go match {
              case Some(goSymbol) => ExprSym(goSymbol) && rstInactive
              case None           => rstInactive
            }
            EntAssign(ExprSym(aEnSymbol), cond) regularize defn.loc
          }
          Iterator(aEnDefn, aEnAssign)
        } concat {
          Iterator single {
            val asserts = List from {
              assertions.iterator flatMap {
                case (enSymbol, Some(cond), _, msgOpt, comment) =>
                  Iterator(
                    StmtComment(comment),
                    StmtIf(
                      ExprSym(enSymbol),
                      StmtSplice(AssertionAssert(cond, msgOpt)) :: Nil,
                      Nil
                    )
                  ) tapEach { _ regularize enSymbol.loc }
                case (enSymbol, None, _, msgOpt, comment) =>
                  Iterator(
                    StmtComment(comment),
                    StmtSplice(AssertionAssert(~ExprSym(enSymbol), msgOpt))
                  ) tapEach { _ regularize enSymbol.loc }
              }
            }
            TypeAssigner(
              EntClockedProcess(
                ExprSym(defn.clk.get) regularize tree.loc,
                None,
                List(
                  StmtComment("Assertions") regularize tree.loc,
                  StmtIf(ExprSym(aEnSymbol), asserts, Nil) regularize tree.loc
                )
              ) withLoc tree.loc
            )
          }
        }
      }
      TypeAssigner(defn.copy(body = newBody) withLoc tree.loc)

    case decl: DeclEntity if assertions.nonEmpty =>
      val newDecls = List from {
        decl.decls.iterator concat {
          aEnSymbolOpt.iterator map { _.mkDecl regularize decl.loc }
        } concat {
          assertions.iterator flatMap {
            case (enSymbol, _, condSymbols, _, _) =>
              Iterator.single(enSymbol.mkDecl regularize enSymbol.loc) concat {
                condSymbols.iterator map { condSymbol =>
                  condSymbol.mkDecl regularize condSymbol.loc
                }
              }
          }
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

object LowerAssertions extends EntityTransformerPass(declFirst = false, parallel = true) {
  val name = "lower-assertions"

  override def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
    new LowerAssertions
}
