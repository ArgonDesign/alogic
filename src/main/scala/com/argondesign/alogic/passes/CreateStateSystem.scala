////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018-2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// - Allocate state numbers
// - Add 'go' signal
// - Add state variable if needed
// - Construct the state dispatch statement
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Math
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.SequenceNumbers
import com.argondesign.alogic.util.unreachable

final class CreateStateSystem(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  // The 'go' symbol
  private[this] var goSymbol: Symbol = _
  // whether there is only a single state
  private[this] var singleState: Boolean = _
  // Number of bits in state variable
  private[this] var stateWidth: Int = _
  // The state variable symbol
  private[this] var stateVarSymbol: Symbol = _
  // The entry state
  private[this] var entryState: Symbol = _
  // Map from state symbol to state number
  private[this] var stateNumbers: Map[Symbol, Int] = _

  // Replace the return stack symbol (if it's being kept)
  override protected def replace(symbol: Symbol): Boolean =
    !singleState && symbol.attr.returnStack.isSet

  override def enter(tree: Tree): Option[Tree] = tree match {
    case defn: DefnEntity =>
      goSymbol = cc.newSymbol("go", defn.symbol.loc) tap { s =>
        s.kind = TypeUInt(1)
        s.attr.go set true
        s.attr.combSignal set true
      }

      val nStates = defn.states.length
      singleState = nStates == 1
      stateWidth = Math.clog2(nStates) max 1
      if (nStates > 1) {
        stateVarSymbol = cc.newSymbol("state", defn.symbol.loc) tap { s =>
          s.kind = TypeUInt(stateWidth)
        }
      }

      // Pick up the entry state
      val (entryStates, otherStates) = defn.states partition { _.symbol.attr.entry.isSet }
      assert(entryStates.length == 1)
      entryState = entryStates.head.symbol

      // For now, just allocate state numbers linearly as binary coded
      stateNumbers = {
        val it = new SequenceNumbers
        // Ensure the entry symbol is allocated number 0.
        // This is necessary since the return stack uses the special value 0
        // to mean "stack is empty - return to top of main".
        Map from {
          (entryStates.head :: otherStates) map { _.symbol -> it.next }
        }
      }

      None

    // Update the type of the return stack symbol
    case decl @ DeclStack(symbol, _, _) if symbol.attr.returnStack.isSet =>
      Some {
        val newElem = TypeAssigner(ExprType(TypeUInt(stateWidth)) withLoc tree.loc)
        TypeAssigner(decl.copy(elem = newElem) withLoc tree.loc)
      }

    // Remove goto <current state>
    case StmtGoto(ExprSym(symbol)) if symbol eq enclosingSymbols.head => Some(Stump)

    case _ => None
  }

  override def transform(tree: Tree): Tree = tree match {
    // Replace references to states with the state numbers
    case ExprSym(symbol) =>
      stateNumbers.get(symbol) map { stateNumber =>
        TypeAssigner(ExprInt(false, stateWidth, stateNumber) withLoc tree.loc)
      } getOrElse tree

    // Convert goto <other state> to state assignment
    case StmtGoto(expr) => StmtAssign(ExprSym(stateVarSymbol), expr) regularize tree.loc

    // If only 1 state, drop push to return stack
    case StmtExpr(ExprCall(ExprSelect(ExprSym(symbol), _, _), _))
        if symbol.attr.returnStack.isSet && singleState =>
      Stump

    // Drop State Decl
    case _: DeclState => Stump

    // Add comment to state body (State Defn will be dropped later)
    case desc @ DefnState(_, ExprInt(_, _, value), body) =>
      val cmnt = TypeAssigner(StmtComment(s"State $value - line ${tree.loc.line}") withLoc tree.loc)
      TypeAssigner(desc.copy(body = cmnt :: body) withLoc tree.loc)

    // If only 1 state, drop the return stack Decl/Defn
    case DeclStack(symbol, _, _) if symbol.attr.returnStack.isSet && singleState => Stump
    case DefnStack(symbol) if symbol.attr.returnStack.isSet && singleState       => Stump

    // Add 'go' and state variable Decl
    case decl: DeclEntity =>
      val newDecls = {
        Iterator single {
          goSymbol.mkDecl regularize goSymbol.loc
        }
      } concat {
        // Add state variable Decl
        Option.when(!singleState) {
          stateVarSymbol.mkDecl regularize stateVarSymbol.loc
        }
      } concat decl.decls
      TypeAssigner(decl.copy(decls = List from newDecls) withLoc decl.loc)

    case defn: DefnEntity =>
      assert(defn.combProcesses.lengthIs <= 1)
      TypeAssigner(defn.copy(body = List from {
        {
          // Add 'go' definition
          Iterator single {
            EntDefn(goSymbol.mkDefn) regularize goSymbol.loc
          }
        } concat {
          // Add state variable Defn
          Option.when(!singleState) {
            val init = ExprInt(false, stateWidth, stateNumbers(entryState))
            EntDefn(stateVarSymbol.mkDefn(init)) regularize stateVarSymbol.loc
          }
        } concat {
          // Drop states and the comb process
          defn.body.iterator filter {
            case _: EntCombProcess     => false
            case EntDefn(_: DefnState) => false
            case _                     => true
          }
        } concat {
          // Add the comb process back with the state dispatch
          Iterator single {
            // Ensure entry state is the first
            val (entryStates, otherStates) = defn.states partition {
              case DefnState(symbol, _, _) => symbol == entryState
            }

            assert(entryStates.lengthIs == 1)

            val dispatch = entryStates.head :: otherStates match {
              case Nil          => unreachable
              case entry :: Nil =>
                // Single state, simple.
                entry.body
              case entry :: other :: Nil =>
                // 2 states, use an 'if' statement.
                List(
                  StmtComment("State dispatch"),
                  StmtIf(
                    ExprBinary(
                      ExprSym(stateVarSymbol),
                      "==",
                      ExprInt(false, stateWidth, stateNumbers(entryState))
                    ),
                    entry.body,
                    other.body
                  )
                ) tapEach {
                  _ regularize defn.loc
                }
              case entry :: rest =>
                // Many states, use a 'case' statement.
                List(
                  StmtComment("State dispatch"),
                  StmtCase(
                    ExprSym(stateVarSymbol),
                    CaseDefault(entry.body) :: {
                      rest map {
                        case DefnState(_, expr, body) => CaseRegular(List(expr), body)
                      }
                    }
                  )
                ) tapEach {
                  _ regularize defn.loc
                }
            }

            val goInit = StmtAssign(ExprSym(goSymbol), ExprInt(false, 1, 1)) regularize tree.loc

            TypeAssigner {
              defn.combProcesses.headOption map { tree =>
                EntCombProcess(goInit :: tree.stmts ::: dispatch) withLoc tree.loc
              } getOrElse {
                EntCombProcess(goInit :: dispatch) withLoc dispatch.head.loc
              }
            }
          }
        }
      }) withLoc defn.loc)

    //
    case _ => tree
  }

  override protected def finalCheck(tree: Tree): Unit = tree visit {
    case node: DeclState => cc.ice(node, "DeclState remains")
    case node: DefnState => cc.ice(node, "DefnState remains")
    case node: StmtFence => cc.ice(node, "StmtFence remains")
  }
}

object CreateStateSystem extends EntityTransformerPass(declFirst = false) {
  val name = "create-state-system"

  override def skip(decl: Decl, defn: Defn)(implicit cc: CompilerContext): Boolean =
    super.skip(decl, defn) || decl.asInstanceOf[DeclEntity].states.isEmpty

  def create(symbol: Symbol)(implicit cc: CompilerContext) = new CreateStateSystem
}
