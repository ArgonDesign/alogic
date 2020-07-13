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

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Bindings
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.enums.ResetStyle
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.SequenceNumbers
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class LowerAssertions(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  // List of emitted assertion (enable signal, condition, condition symbols, message, comment)
  private val assertions =
    mutable.ListBuffer[(Symbol, Expr, List[Symbol], Option[String], String)]()

  // Assertion enable symbol for the whole entity, used to gate assertions with
  // go and reset. Separate signal otherwise Verilator complains SYNCASYNCNET
  // about asynchronous reset being used in a block which is not sensitive to
  // it. Also helps readability.
  private var aEnSymbolOpt: Option[Symbol] = None

  private val assertSeqNum = new SequenceNumbers

  // Predicate indicating what kind of symbols need to be copied for assertions
  // as their values might change.
  private def modifiable(symbol: Symbol): Boolean = symbol.kind match {
    // Fundamental type
    case _: TypeInt    => true
    case _: TypeNum    => unreachable
    case _: TypeVector => true
    case TypeVoid      => unreachable
    case TypeStr       => unreachable
    case _: TypeRecord => true
    case _: TypeEntity => false
    // Derived
    case _: TypeIn          => false
    case TypeOut(_, fct, _) => fct == FlowControlTypeNone
    case _: TypePipeVar     => unreachable
    case _: TypeParam       => unreachable
    case _: TypeConst       => false
    case _: TypeGen         => unreachable
    case _: TypeArray       => false
    case _: TypeSram        => false
    case _: TypeStack       => false
    // Pipeline ports
    case _: TypePipeIn  => unreachable
    case _: TypePipeOut => unreachable
    // Function types
    case _: TypeCombFunc     => false
    case _: TypeCtrlFunc     => unreachable
    case _: TypePolyFunc     => unreachable
    case _: TypeXenoFunc     => false
    case _: TypeStaticMethod => unreachable
    case _: TypeNormalMethod => unreachable
    // Misc types
    case _: TypeType         => false
    case _: TypeNone         => unreachable
    case _: TypeParametrized => unreachable
    case TypeCombStmt        => unreachable
    case TypeCtrlStmt        => unreachable
    //
    case TypeUnknown  => unreachable
    case _: TypeState => unreachable
    case TypeMisc     => unreachable
    case TypeError    => unreachable
  }

  override protected def skip(tree: Tree): Boolean = tree match {
    case _: Expr => true
    case _       => false
  }

  override def transform(tree: Tree): Tree = tree match {

    // If assertions are not enabled, just convert them to assumptions if the
    // condition is pure, otherwise drop them
    case StmtAssertion(AssertionAssert(cond, msgOpt)) if !cc.settings.assertions =>
      if (cond.isPure) {
        StmtAssertion(AssertionAssume(cond, msgOpt)) regularize tree.loc
      } else {
        Stump
      }

    // If assertions are enabled, convert them to deferred assertions, but
    // leave in the original as an assume to be used by folding. The assume
    // will be removed by the RemoveAssume pass.
    case StmtAssertion(AssertionAssert(cond, msgOpt)) =>
      val prefix = s"_assert_${assertSeqNum.next}"
      val comment = s"'assert' on line ${tree.loc.line}"
      val enSymbol = cc.newSymbol(s"${prefix}_en", tree.loc)
      enSymbol.kind = TypeUInt(1)
      enSymbol.attr.combSignal set true
      val deps = List from {
        val seqNum = new SequenceNumbers
        cond collect {
          case expr @ ExprSym(symbol) if modifiable(symbol) =>
            val newSymbol = cc.newSymbol(s"${prefix}_c${seqNum.next}", expr.loc)
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
      assertions.append((enSymbol, cond `given` bindings, deps map { _._2 }, msgOpt, comment))
      val lb = new ListBuffer[Stmt]
      lb.append(StmtAssign(ExprSym(enSymbol), ExprInt(false, 1, 1)) regularize tree.loc)
      deps foreach {
        case (symbol, newSymbol) =>
          lb.append(StmtAssign(ExprSym(newSymbol), ExprSym(symbol)) regularize cond.loc)
      }
      if (cond.isPure) {
        lb.append(StmtAssertion(AssertionAssume(cond, msgOpt)) regularize cond.loc)
      }
      Thicket(lb.toList)

    case defn: DefnEntity if assertions.nonEmpty =>
      val aEnSymbol = cc.newSymbol(s"_assertions_en", tree.loc)
      aEnSymbol.kind = TypeUInt(1)
      aEnSymbolOpt = Some(aEnSymbol)
      val newBody = List from {
        defn.body.iterator concat {
          assertions.iterator flatMap {
            case (enSymbol, _, condSymbols, _, _) =>
              Iterator.single(EntDefn(enSymbol.mkDefn) regularize enSymbol.loc) concat {
                condSymbols.iterator map { condSymbol =>
                  EntDefn(condSymbol.mkDefn) regularize condSymbol.loc
                }
              }
          }
        } concat {
          val aEnDefn = EntDefn(aEnSymbol.mkDefn) regularize defn.loc;
          val aEnAssign = {
            val rstInactive = cc.settings.resetStyle match {
              case ResetStyle.AsyncHigh | ResetStyle.SyncHigh => !ExprSym(defn.rst.get)
              case ResetStyle.AsyncLow | ResetStyle.SyncLow   => ExprSym(defn.rst.get)
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
                case (enSymbol, cond, _, msgOpt, comment) =>
                  Iterator(
                    StmtComment(comment),
                    StmtIf(
                      ExprSym(enSymbol),
                      StmtAssertion(AssertionAssert(cond, msgOpt)) :: Nil,
                      Nil
                    ) regularize enSymbol.loc
                  )
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

}

object LowerAssertions extends EntityTransformerPass(declFirst = false) {
  val name = "lower-assertions"

  override def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
    new LowerAssertions
}
