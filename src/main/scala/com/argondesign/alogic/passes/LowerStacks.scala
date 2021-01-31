////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// - Lower stack variables into stack instances
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.StackFactory
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types.TypeStack
import com.argondesign.alogic.core.enums.EntityVariant
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class LowerStacks(implicit cc: CompilerContext) extends StatelessTreeTransformer {

  // Map from original stack variable symbol to the
  // corresponding stack entity and instance symbols
  private val stackMap = mutable.LinkedHashMap[Symbol, ((DeclEntity, DefnEntity), Symbol)]()

  // Stack of extra statements to emit when finished with a statement
  private val extraStmts = mutable.Stack[mutable.ListBuffer[Stmt]]()

  private var entitySymbol: Symbol = _

  override def start(tree: Tree): Unit = tree match {
    case DeclEntity(symbol, _) => entitySymbol = symbol
    case _: DefnEntity         => assert(entitySymbol != null)
    case _                     => unreachable
  }

  override def enter(tree: Tree): Option[Tree] = {
    tree match {
      case Decl(symbol) =>
        symbol.kind match {
          case TypeStack(kind, depth) =>
            // Construct the stack entity
            val loc = tree.loc
            val pName = symbol.name
            // TODO: mark inline
            val eName = entitySymbol.name + cc.sep + "stack" + cc.sep + pName
            val stackEntity = StackFactory(eName, loc, kind, depth.toInt)
            val instanceSymbol = cc.newSymbol(pName, loc) tap {
              _.kind = stackEntity._1.symbol.kind.asType.kind
            }
            stackMap(symbol) = (stackEntity, instanceSymbol)
            // Clear control signals on stall
            entitySymbol.attr.interconnectClearOnStall.append((instanceSymbol, "push"))
            entitySymbol.attr.interconnectClearOnStall.append((instanceSymbol, "pop"))
            entitySymbol.attr.interconnectClearOnStall.append((instanceSymbol, "set"))

          case _ =>
        }

      case _: Stmt =>
        // Whenever we enter a new statement, add a new buffer to
        // store potential extra statements
        extraStmts.push(ListBuffer())

      case _ =>
    }
    None
  }

  private def assignTrue(expr: Expr) = StmtAssign(expr, ExprInt(false, 1, 1))

  private def assignFalse(expr: Expr) = StmtAssign(expr, ExprInt(false, 1, 0))

  override def transform(tree: Tree): Tree = {
    val result: Tree = tree match {

      //////////////////////////////////////////////////////////////////////////
      // Rewrite statements
      //////////////////////////////////////////////////////////////////////////

      case StmtExpr(ExprCall(ExprSel(ExprSym(symbol), "push"), _)) =>
        stackMap.get(symbol) map {
          case (_, iSymbol) => assignTrue(ExprSym(iSymbol) sel "push")
        } getOrElse tree

      case StmtExpr(ExprCall(ExprSel(ExprSym(symbol), "pop"), _)) =>
        stackMap.get(symbol) map {
          case (_, iSymbol) => assignTrue(ExprSym(iSymbol) sel "pop")
        } getOrElse tree

      //////////////////////////////////////////////////////////////////////////
      // Rewrite expressions
      //////////////////////////////////////////////////////////////////////////

      case ExprSel(ExprSym(symbol), "top") =>
        stackMap.get(symbol) map {
          case (_, iSymbol) => ExprSym(iSymbol) sel "d"
        } getOrElse tree

      case ExprSel(ExprSym(symbol), "old") =>
        stackMap.get(symbol) map {
          case (_, iSymbol) => ExprSym(iSymbol) sel "q"
        } getOrElse tree

      //////////////////////////////////////////////////////////////////////////
      // Replace Stack Decl/Defn with the Decl/Defn of the expanded symbols
      //////////////////////////////////////////////////////////////////////////

      case DeclStack(symbol, _, _) => stackMap(symbol)._2.mkDecl

      case DefnStack(symbol) => stackMap(symbol)._2.mkDefn

      //////////////////////////////////////////////////////////////////////////
      // Add stack connections
      //////////////////////////////////////////////////////////////////////////

      case defn: DefnEntity if stackMap.nonEmpty =>
        val newBody = List from {
          // Drop the comb process
          defn.body.iterator filter {
            case _: EntCombProcess => false
            case _                 => true
          } concat {
            Iterator single {
              // Add leading statements to the state system
              assert(defn.combProcesses.lengthIs <= 1)

              val leading = stackMap.values map {
                _._2
              } map { iSymbol =>
                val iRef = ExprSym(iSymbol)
                StmtBlock(
                  List(
                    StmtAssign(iRef sel "d", iRef sel "q"),
                    assignFalse(iRef sel "push"),
                    assignFalse(iRef sel "pop"),
                    assignTrue(iRef sel "set")
                  )
                )
              }

              defn.combProcesses.headOption map {
                case EntCombProcess(stmts) => EntCombProcess(List.concat(leading, stmts))
              } getOrElse {
                EntCombProcess(leading.toList)
              }
            }
          }
        }

        defn.copy(body = newBody)

      //
      case _ => tree
    }

    // Emit any extra statement with this statement
    val result2 = result match {
      case stmt: Stmt =>
        val extra = extraStmts.pop()
        if (extra.isEmpty) stmt else Thicket((extra append stmt).toList)
      case _ => result
    }

    // If we did modify the node, regularize it
    if (result2 ne tree) {
      result2 regularize tree.loc
    }

    // Done
    result2
  }

  override def finish(tree: Tree): Tree = tree match {
    case _: DeclEntity => Thicket(tree :: List.from(stackMap.valuesIterator map { _._1._1 }))
    case _: DefnEntity => Thicket(tree :: List.from(stackMap.valuesIterator map { _._1._2 }))
    case _             => unreachable
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(extraStmts.isEmpty)

    // $COVERAGE-OFF$ Debug code
    tree visit {
      case node @ ExprSel(ref, sel) if ref.tpe.isStack => throw Ice(node, s"Stack .$sel remains")
    }
    // $COVERAGE-ON$
  }

}

object LowerStacks extends EntityTransformerPass(declFirst = true, parallel = true) {
  val name = "lower-stacks"

  override def skip(decl: DeclEntity, defn: DefnEntity): Boolean = defn.variant != EntityVariant.Fsm

  def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer = new LowerStacks
}
