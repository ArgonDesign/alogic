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
// Lower drivers of instance ports by allocating interconnect variables:
//  - Ensure at least either one of source or destination of Connect
//    is not an 'instance.port' (or concatenations of such), but a
//    proper expression
//  - Allocate intermediate variables for instance port access in states
//  - Ensure an 'instance.port' is only present in a single Connect
// After this stage, the only place where an 'instance.port' reference can
// remain is on either side of a Connect, and only one side of a connect
// can be such an reference. Furthermore, there is only one Connect which
// to any one 'instance.port'.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees.Expr.InstancePortRef
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.TreeInTypeTransformer
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.PartialMatch
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class LowerInterconnect(implicit cc: CompilerContext)
    extends TreeTransformer
    with PartialMatch {

  // Map from (instance symbol, selector) to the new interconnect symbol
  private[this] val newSymbols = mutable.LinkedHashMap[(TermSymbol, String), TermSymbol]()

  // List of new Connect instances to emit
  private[this] val newConnects = new ListBuffer[Connect]()

  // Keep a stack of booleans indicating that we should
  // be allocating interconnect symbols in a connect expression
  private[this] val enableStack = mutable.Stack[Boolean]()

  // Keep track of whether we are in a connect expression
  private[this] var inConnect = false

  // Convert interconnectClearOnStall attribute to a set
  private[this] lazy val interconnectClearOnStall = {
    entitySymbol.attr.interconnectClearOnStall.getOrElse(Nil).toSet
  }

  // We need to fold const references in interconnect symbol types,
  // as these consts are defined in the entity being instantiated,
  // and not the entity being processed (which does the instantiation)
  lazy val typeFoldExpr = new FoldExpr(foldRefs = true)
  object TypeFoldExpr extends TreeInTypeTransformer(typeFoldExpr)

  // Return the interconnect symbol for 'iSymbol.sel', if any. If the
  // interconnect symbol does not exist and alloc is true, allocate
  // it and connect it up
  def handlePortSelect(select: ExprSelect, alloc: Boolean): Option[TermSymbol] = select match {
    case ExprSelect(ExprRef(iSymbol: TermSymbol), sel) =>
      val key = (iSymbol, sel)
      if (!alloc) {
        newSymbols.get(key)
      } else {
        lazy val newSymbol = {
          // Allocate interconnect symbol
          val name = iSymbol.name + cc.sep + sel
          val pKind = iSymbol.kind.asInstanceOf[TypeInstance](sel).get
          val nKind = pKind.underlying rewrite TypeFoldExpr
          val symbol = cc.newTermSymbol(name, iSymbol.loc, nKind)
          symbol.attr.interconnect.set((iSymbol, sel))

          // If this is an interconnect symbol that is in the entity
          // interconnectClearOnStall attribute, then set clearOnStall on it
          if (interconnectClearOnStall contains key) {
            symbol.attr.clearOnStall set true
          }

          // Build connection
          val loc = select.loc
          val ref = ExprRef(symbol) regularize loc
          val conn = TypeAssigner {
            pKind match {
              case _: TypeIn => Connect(ref, List(select)) withLoc loc
              case _         => Connect(select, List(ref)) withLoc loc
            }
          }
          newConnects append conn

          // The new symbol
          symbol
        }
        Some(newSymbols.getOrElseUpdate(key, newSymbol))
      }
    case _ => unreachable
  }

  override def enter(tree: Tree): Unit = tree match {

    // Nested expression in a connect, do the same as before
    case _: Expr if inConnect => {
      enableStack push enableStack.top
    }

    // a.b -> c.d, allocate on left hand side only
    case Connect(InstancePortRef(_, _), List(InstancePortRef(_, _))) => {
      assert(enableStack.isEmpty)
      enableStack push false push true
      inConnect = true
    }

    // a.b -> SOMETHING, allocate on right hand side only
    case Connect(InstancePortRef(_, _), _) => {
      assert(enableStack.isEmpty)
      enableStack push true push false
      inConnect = true
    }

    // SOMETHING -> a.b, allocate on left hand side only
    case Connect(_, List(InstancePortRef(_, _))) => {
      assert(enableStack.isEmpty)
      enableStack push false push true
      inConnect = true
    }

    // SOMETHING -> SOMETHING, allocate on both sides
    case _: Connect => {
      assert(enableStack.isEmpty)
      enableStack push true push true
      inConnect = true
    }

    case _ => ()
  }

  override def transform(tree: Tree): Tree = {
    tree match {
      //////////////////////////////////////////////////////////////////////////
      // Rewrite references, allocating if enabled and necessary
      //////////////////////////////////////////////////////////////////////////

      case select @ InstancePortRef(_, _) => {
        handlePortSelect(select, !inConnect || enableStack.top) map { nSymbol =>
          ExprRef(nSymbol) regularize tree.loc
        } getOrElse {
          tree
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // Emit any new connections created under a Connect
      //////////////////////////////////////////////////////////////////////////

      case conn: Connect if newConnects.nonEmpty => {
        newConnects append conn
        TypeAssigner {
          Thicket(newConnects.toList) withLoc tree.loc
        }
      } tap { _ =>
        newConnects.clear()
      }

      //////////////////////////////////////////////////////////////////////////
      // Add new symbols and connections, ensure single sink for 'instance.port'
      //////////////////////////////////////////////////////////////////////////

      case entity: EntityLowered => {

        // Ensure that any 'instance.port' is only present on the left
        // of a single Connect instance (i.e.: there is only 1 sink variable)
        val newConn = {
          // Collect the sinks of all 'instance.port'
          val sinks: Map[ExprSelect, List[Expr]] = {
            val pairs = entity.connects collect {
              case Connect(select @ InstancePortRef(_, _), List(sink)) => {
                select -> sink
              }
            }
            pairs.groupMap(_._1)(_._2)
          }

          // For ports with multiple sinks, compute the map from
          // 'instance.port' to the interconnect symbol, allocating
          // it if necessary (have not been allocated already)
          val nMap = {
            for {
              (select, exprs) <- sinks
              if exprs.lengthCompare(1) > 0
            } yield {
              select -> handlePortSelect(select, alloc = true).get
            }
          }

          // Update all connect instances that reference on the left hand side
          // a port with multiple sinks, and the right hand side is not the
          // interconnect symbol
          for (conn @ Connect(lhs, List(rhs)) <- entity.connects) yield {
            lhs match {
              case expr: ExprSelect => {
                nMap get expr map { nSymbol =>
                  rhs match {
                    case ExprRef(`nSymbol`) => conn // Already the nSymbol, leave it alone
                    case _ => {
                      Connect(ExprRef(nSymbol), List(rhs)) regularize rhs.loc
                    }
                  }
                } getOrElse {
                  conn
                }
              }
              case _ => conn
            }
          }
        }

        // Add declarations for the newly defined symbols
        val newDecls = for ((_, symbol) <- newSymbols) yield {
          Decl(symbol, None) regularize symbol.loc
        }

        // If we have lowered a signal with a dontCareUnless attribute, inside
        // an entity with a state system, transfer the attribute. At the
        // moment, we don't need this in entities without a state system, if we
        // do, we can do it by building a complete map based on the connections
        // we ended up with.
        if (entity.statements.nonEmpty) {
          for {
            ((iSymbol, sel), symbol) <- newSymbols
            pSymbol = iSymbol.kind.asInstanceOf[TypeInstance].portSymbol(sel).get
            gSymbol <- pSymbol.attr.dontCareUnless.get
          } {
            symbol.attr.dontCareUnless set newSymbols((iSymbol, gSymbol.name))
          }
        }

        // Tran

        TypeAssigner {
          entity.copy(
            declarations = newDecls.toList ::: entity.declarations,
            connects = newConnects.toList ::: newConn
          ) withLoc tree.loc
        }
      } tap { _ =>
        newConnects.clear()
      }

      case _ => tree
    }
  } tap { _ =>
    if (inConnect) {
      tree match {
        // If we just processed an expression in a connect, pop the enableStack.
        // If we are now back to 2 elements, then this was the root expression
        // on either side of the connect, so pop one extra element, and double
        // up the bottom (in case this was the left expression in the connect,
        // the right one still needs to proceed)
        case _: Expr => {
          enableStack.pop()
          assert(enableStack.nonEmpty)
          if (enableStack.size == 2) {
            enableStack.pop()
            enableStack push enableStack.top
          }
        }
        // If we just processed a connect, mark we are
        // out and empty the enableStack
        case _: Connect => {
          assert(enableStack.size == 2)
          enableStack.pop()
          enableStack.pop()
          inConnect = false
        }
        case _ => ()
      }
    }
  }

  override protected def finalCheck(tree: Tree): Unit = {
    assert(newConnects.isEmpty)
    assert(enableStack.isEmpty)
    assert(!inConnect)

    def check(tree: Tree) = {
      tree visit {
        case node @ ExprSelect(ExprRef(symbol: TermSymbol), _)
            if symbol.kind.isInstanceOf[TypeInstance] => {
          cc.ice(node, "Direct port access remains")
        }
      }
    }

    tree visit {
      case node @ Connect(InstancePortRef(_, _), List(rhs)) => check(rhs)
      case node @ Connect(lhs, List(InstancePortRef(_, _))) => check(lhs)
      case node: Expr                                       => check(node)
    }
  }

}

object LowerInterconnect extends TreeTransformerPass {
  val name = "lower-interconnect"
  def create(implicit cc: CompilerContext) = new LowerInterconnect
}
