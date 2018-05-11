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
import com.argondesign.alogic.core.Types.TypeStack
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Stack

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class LowerStacks(implicit cc: CompilerContext) extends TreeTransformer {

  // Map from original stack variable symbol to the
  // corresponding stack entity and instance symbols
  private[this] val stackMap = mutable.Map[TermSymbol, (Entity, TermSymbol)]()

  // Stack of extra statements to emit when finished with a statement
  private[this] val extraStmts = Stack[mutable.ListBuffer[Stmt]]()

  private[this] var entityName: String = _

  override def skip(tree: Tree): Boolean = tree match {
    case entity: Entity => entity.variant == "network"
    case _              => false
  }

  override def enter(tree: Tree): Unit = tree match {
    case entity: Entity => {
      val Sym(symbol: TypeSymbol) = entity.ref
      entityName = symbol.name
    }

    case Decl(symbol, _) if symbol.kind.isInstanceOf[TypeStack] => {
      // Construct the stack entity
      val TypeStack(kind, depth) = symbol.kind
      val loc = tree.loc
      val pName = symbol.name
      // TODO: mark inline
      val eName = entityName + cc.sep + "stack" + cc.sep + pName
      val stackEntity: Entity = StackFactory(eName, loc, kind, depth)
      val Sym(stackEntitySymbol: TypeSymbol) = stackEntity.ref
      val instanceSymbol = {
        cc.newTermSymbol(pName, loc, TypeInstance(stackEntitySymbol))
      }
      stackMap(symbol) = (stackEntity, instanceSymbol)
      // Clear enabel when the entity stalls
      entitySymbol.attr.interconnectClearOnStall.append((instanceSymbol, "en"))
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

      case StmtExpr(ExprCall(ExprSelect(ExprRef(symbol: TermSymbol), "push"), args)) => {
        stackMap.get(symbol) map {
          case (_, iSymbol) => {
            StmtBlock(
              List(
                assignTrue(ExprRef(iSymbol) select "en"),
                assignTrue(ExprRef(iSymbol) select "push"),
                StmtAssign(ExprRef(iSymbol) select "d", args.head)
              ))
          }
        } getOrElse {
          tree
        }
      }

      case StmtExpr(ExprCall(ExprSelect(ExprRef(symbol: TermSymbol), "set"), args)) => {
        stackMap.get(symbol) map {
          case (_, iSymbol) => {
            StmtBlock(
              List(
                assignTrue(ExprRef(iSymbol) select "en"),
                StmtAssign(ExprRef(iSymbol) select "d", args.head)
              ))
          }
        } getOrElse {
          tree
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // Rewrite expressions
      //////////////////////////////////////////////////////////////////////////

      case ExprCall(ExprSelect(ExprRef(symbol: TermSymbol), "pop"), Nil) => {
        stackMap.get(symbol) map {
          case (_, iSymbol) => {
            extraStmts.top append assignTrue(ExprRef(iSymbol) select "en")
            extraStmts.top append assignTrue(ExprRef(iSymbol) select "pop")
            ExprRef(iSymbol) select "q"
          }
        } getOrElse {
          tree
        }
      }

      case ExprSelect(ExprRef(symbol: TermSymbol), "top") => {
        stackMap.get(symbol) map {
          case (_, iSymbol) => ExprRef(iSymbol) select "q"
        } getOrElse {
          tree
        }
      }

      case ExprSelect(ExprRef(symbol: TermSymbol), "full") => {
        stackMap.get(symbol) map {
          case (_, iSymbol) => ExprRef(iSymbol) select "full"
        } getOrElse {
          tree
        }
      }

      case ExprSelect(ExprRef(symbol: TermSymbol), "empty") => {
        stackMap.get(symbol) map {
          case (_, iSymbol) => ExprRef(iSymbol) select "empty"
        } getOrElse {
          tree
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // Add stack entities
      //////////////////////////////////////////////////////////////////////////

      case entity: Entity if stackMap.nonEmpty => {
        // Drop stack declarations
        val declarations = entity.declarations filterNot {
          case Decl(symbol, _) => symbol.kind.isInstanceOf[TypeStack]
          case _               => false
        }

        // Add instances
        val instances = for ((entity, instance) <- stackMap.values) yield {
          Instance(Sym(instance), entity.ref, Nil, Nil)
        }

        // Add fence statemetns
        val fenceStmts = stackMap.values map { _._2 } map { iSymbol =>
          val iRef = ExprRef(iSymbol)
          StmtBlock(
            List(
              assignFalse(iRef select "en"),
              StmtAssign(iRef select "d", iRef select "q"), // TODO: redundant
              assignFalse(iRef select "push"), // TODO: redundant
              assignFalse(iRef select "pop") // TODO: redundant
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

object LowerStacks extends TreeTransformerPass {
  val name = "lower-stacks"
  def create(implicit cc: CompilerContext) = new LowerStacks
}
