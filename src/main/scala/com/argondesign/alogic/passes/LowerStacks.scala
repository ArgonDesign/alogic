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
// - Lower stack variables into stack instances
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.StackFactory
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Stack
import com.argondesign.alogic.util.FollowedBy

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class LowerStacks(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy {

  // Map from original stack variable symbol to the
  // corresponding stack entity and instance symbols
  private[this] val stackMap = mutable.Map[TermSymbol, (Entity, TermSymbol)]()

  // Stack of extra statements to emit when finished with a statement
  private[this] val extraStmts = Stack[mutable.ListBuffer[Stmt]]()

  private[this] var entityName: String = _

  override def enter(tree: Tree): Unit = tree match {
    case entity: Entity => {
      val Sym(symbol: TypeSymbol) = entity.ref
      entityName = symbol.denot.name.str
    }

    case Decl(Sym(symbol: TermSymbol), TypeStack(kind, depth), _) => {
      // Construct the stack entity
      val loc = tree.loc
      val pName = symbol.denot.name.str
      // TODO: mark inline
      val eName = entityName + "__stack__" + pName
      val stackEntity: Entity = StackFactory(eName, loc, kind, depth)
      val Sym(entitySymbol) = stackEntity.ref
      val instanceSymbol = {
        cc.newTermSymbol(pName, loc, entitySymbol.denot.kind)
      }
      stackMap(symbol) = (stackEntity, instanceSymbol)
    }

    ////////////////////////////////////////////////////////////////////////////
    // FlowControlTypeReady
    ////////////////////////////////////////////////////////////////////////////

    case _: Stmt => {
      // Whenever we enter a new statement, add a new buffer to
      // store potential extra statements
      extraStmts.push(ListBuffer())
    }

    case _ =>
  }

  private[this] def assignTrue(expr: Expr) = StmtAssign(expr, ExprInt(false, 1, 1))
  private[this] def assignFalse(expr: Expr) = StmtAssign(expr, ExprInt(false, 1, 0))

  override def transform(tree: Tree): Tree = {
    val result: Tree = tree match {

      //////////////////////////////////////////////////////////////////////////
      // Rewrite statements
      //////////////////////////////////////////////////////////////////////////

      case StmtExpr(ExprCall(ExprSelect(ExprRef(Sym(symbol: TermSymbol)), "push"), args)) => {
        stackMap.get(symbol) map {
          case (_, iSymbol) => {
            StmtBlock(
              List(
                assignTrue(ExprRef(Sym(iSymbol)) select "push"),
                StmtAssign(ExprRef(Sym(iSymbol)) select "d", args.head)
              ))
          }
        } getOrElse {
          tree
        }
      }

      case StmtExpr(ExprCall(ExprSelect(ExprRef(Sym(symbol: TermSymbol)), "set"), args)) => {
        stackMap.get(symbol) map {
          case (_, iSymbol) => StmtAssign(ExprRef(Sym(iSymbol)) select "d", args.head)
        } getOrElse {
          tree
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // Rewrite expressions
      //////////////////////////////////////////////////////////////////////////

      case ExprCall(ExprSelect(ExprRef(Sym(symbol: TermSymbol)), "pop"), Nil) => {
        stackMap.get(symbol) map {
          case (_, iSymbol) => {
            extraStmts.top append assignTrue(ExprRef(Sym(iSymbol)) select "pop")
            ExprRef(Sym(iSymbol)) select "q"
          }
        } getOrElse {
          tree
        }
      }

      case ExprSelect(ExprRef(Sym(symbol: TermSymbol)), "top") => {
        stackMap.get(symbol) map {
          case (_, iSymbol) => ExprRef(Sym(iSymbol)) select "q"
        } getOrElse {
          tree
        }
      }

      case ExprSelect(ExprRef(Sym(symbol: TermSymbol)), "full") => {
        stackMap.get(symbol) map {
          case (_, iSymbol) => ExprRef(Sym(iSymbol)) select "full"
        } getOrElse {
          tree
        }
      }

      case ExprSelect(ExprRef(Sym(symbol: TermSymbol)), "empty") => {
        stackMap.get(symbol) map {
          case (_, iSymbol) => ExprRef(Sym(iSymbol)) select "empty"
        } getOrElse {
          tree
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // Add stack entities
      //////////////////////////////////////////////////////////////////////////

      case entity: Entity if stackMap.nonEmpty => {
        // Drop stack declarations
        val declarations = entity.declarations filter {
          case Decl(_, _: TypeStack, _) => false
          case _                        => true
        }

        // Add instances
        val instances = for ((entity, instance) <- stackMap.values) yield {
          Instance(Sym(instance), entity.ref, Nil, Nil)
        }

        // Add fence statemetns
        val fenceStmts = stackMap.values map { _._2 } map { iSymbol =>
          val iRef = ExprRef(Sym(iSymbol))
          StmtBlock(
            List(
              StmtAssign(iRef select "d", iRef select "q"),
              assignFalse(iRef select "push"),
              assignFalse(iRef select "pop")
            )
          )
        }

        val newEntity = entity.copy(
          declarations = declarations,
          instances = instances.toList ::: entity.instances,
          fenceStmts = fenceStmts.toList ::: entity.fenceStmts
        ) withVariant entity.variant

        val stackEntities = stackMap.values map { _._1 }

        Thicket(newEntity :: stackEntities.toList)
      }

      case _ => tree
    }

    // Emit any extra statement with this statement
    val result2 = result match {
      case stmt: Stmt => {
        val extra = extraStmts.top
        if (extra.isEmpty) {
          stmt
        } else {
          extra append stmt
          StmtBlock(extra.toList)
        }
      } followedBy {
        extraStmts.pop()
      }
      case _ => result
    }

    // If we did modify the node, regularize it
    if (result2 ne tree) {
      result2 regularize tree.loc
    }

    // Done
    result2
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(extraStmts.isEmpty)

    tree visit {
      case node @ ExprCall(ExprSelect(ref, sel), _) if ref.tpe.isInstanceOf[TypeStack] => {
        cc.ice(node, s"Stack .${sel} remains")
      }
    }
  }

}
