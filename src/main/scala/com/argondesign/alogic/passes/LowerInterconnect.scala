////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//
// Lower drivers of instance ports by allocating interconnect variables:
//  - Ensure at least either one of source or destination of EntAssign
//    is not an 'instance.port' (or expression containing of such)
//  - Allocate intermediate variables for instance port access in states
//  - Ensure an 'instance.port' is only present in a single Connect
// After this stage, the only place where an 'instance.port' reference can
// remain is on either side of a EntAssign, and only one side of an assign
// can be such an reference. Furthermore, there is only one EntAssign
// to any one 'instance.port'.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.Expr.InstancePortSel
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.Types._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class LowerInterconnect(implicit cc: CompilerContext) extends StatelessTreeTransformer {

  // Map from (instance symbol, selector) to the new interconnect symbol
  private val newSymbols = mutable.LinkedHashMap[(Symbol, String), Symbol]()

  // List of new Connect instances to emit
  private val newAssigns = new ListBuffer[EntAssign]()

  // Convert interconnectClearOnStall attribute to a set
  private var interconnectClearOnStall: Set[(Symbol, String)] = _

  override protected def start(tree: Tree): Unit = tree match {
    case defn: DefnEntity =>
      interconnectClearOnStall = defn.symbol.attr.interconnectClearOnStall.getOrElse(Nil).toSet
    case _ =>
      assert(interconnectClearOnStall != null)
  }

  override def transform(tree: Tree): Tree = tree match {
    //////////////////////////////////////////////////////////////////////////
    // Rewrite references, allocating interconnect symbols
    //////////////////////////////////////////////////////////////////////////

    case select @ InstancePortSel(iSymbol, pSymbol) =>
      val key = (iSymbol, pSymbol.name)
      val cSymbol = newSymbols.getOrElseUpdate(
        key, {
          // Allocate interconnect symbol
          val name = iSymbol.name + cc.sep + pSymbol.name
          val pKind = pSymbol.kind
          val nKind = pKind.underlying
          val symbol = Symbol(name, iSymbol.loc)
          symbol.kind = nKind
          symbol.attr.interconnect.set((iSymbol, pSymbol))

          // If this is an interconnect symbol that is in the entity
          // interconnectClearOnStall attribute, then set clearOnStall on it
          if (interconnectClearOnStall contains key) {
            symbol.attr.clearOnStall set true
          }

          // Build connection
          val loc = select.loc
          val ref = ExprSym(symbol) regularize loc
          val conn = TypeAssigner {
            pKind match {
              case _: TypeIn => EntAssign(select, ref) withLoc loc
              case _         => EntAssign(ref, select) withLoc loc
            }
          }
          newAssigns append conn

          // The new symbol
          symbol
        }
      )
      TypeAssigner(ExprSym(cSymbol) withLocOf tree)

    //////////////////////////////////////////////////////////////////////////
    // Add new symbols and connections
    //////////////////////////////////////////////////////////////////////////

    case defn: DefnEntity =>
      // To reduce the number of interconnect symbols, eliminate all newly
      // allocated symbols that are simple aliases of other symbols.

      val assigns = defn.assigns ++ newAssigns
      newAssigns.clear()

      val interconnectSymbols = newSymbols.valuesIterator.toSet

      val aliases = {
        @tailrec
        def loop(curr: Map[Symbol, Symbol]): Map[Symbol, Symbol] = {
          val next = curr.view.mapValues(symbol => curr.getOrElse(symbol, symbol)).toMap
          if (next == curr) curr else loop(next)
        }

        loop {
          Map from {
            assigns.flatMap {
              case EntAssign(ExprSym(l), ExprSym(r)) =>
                if (interconnectSymbols(l)) {
                  Option.when(interconnectSymbols(r) || r.kind.isIn || r.kind.isOut)(l -> r)
                } else if (interconnectSymbols(r)) {
                  Option.when(l.kind.isIn || l.kind.isOut)(r -> l)
                } else {
                  None
                }
              case _ => None
            }
          }
        }
      }

      newSymbols.filterInPlace {
        case (_, symbol) => !(aliases contains symbol)
      }

      object Transform extends StatelessTreeTransformer {
        override protected def transform(tree: Tree): Tree = tree match {
          case ExprSym(symbol) =>
            aliases.get(symbol) map { symbol =>
              TypeAssigner(ExprSym(symbol) withLocOf tree)
            } getOrElse tree
          case _ => tree
        }
      }

      val modAssigns = assigns.flatMap(
        _ rewrite Transform match {
          case EntAssign(a, b) if a == b => None
          case other                     => Some(other)
        }
      )

      // Compute the new body: drop original connects, add definitions,
      // add modified connects
      val body = List from {
        defn.body.iterator filter {
          case _: EntAssign => false
          case _            => true
        } concat {
          newSymbols.valuesIterator
            .map(symbol => EntSplice(symbol.mkDefn) regularize symbol.loc)
        } concat {
          modAssigns
        }
      }

      // If we have lowered a signal with a dontCareUnless attribute, inside
      // an entity with a state system, transfer the attribute. At the
      // moment, we don't need this in entities without a state system, if we
      // do, we can do it by building a complete map based on the connections
      // we ended up with.
      if (defn.combProcesses.nonEmpty) {
        for {
          ((iSymbol, sel), symbol) <- newSymbols
          pSymbol = iSymbol.kind.asEntity(sel).get
          gSymbol <- pSymbol.attr.dontCareUnless.get
          cSymbol <- newSymbols.get((iSymbol, gSymbol.name))
        } {
          symbol.attr.dontCareUnless set cSymbol
        }
      }

      TypeAssigner(defn.copy(body = body) withLoc defn.loc)

    case decl: DeclEntity =>
      val decls = decl.decls ++ {
        newSymbols.valuesIterator map { symbol =>
          symbol.mkDecl regularize symbol.loc
        }
      }

      TypeAssigner(decl.copy(decls = decls) withLoc decl.loc)

    //
    case _ => tree
  }

  override protected def finalCheck(tree: Tree): Unit = {
    assert(newAssigns.isEmpty)

    def check(tree: Tree): Unit = tree visit {
      // $COVERAGE-OFF$ Debug code
      case node @ ExprSel(ExprSym(symbol), _) if symbol.kind.isEntity =>
        throw Ice(node, "Direct port access remains")
      // $COVERAGE-ON$
    }

    tree visit {
      case EntAssign(lhs, InstancePortSel(_, _)) => check(lhs)
      case EntAssign(InstancePortSel(_, _), rhs) => check(rhs)
      case node: Expr                            => check(node)
    }
  }

}

object LowerInterconnect extends EntityTransformerPass(declFirst = false) {
  val name = "lower-interconnect"

  def create(symbol: Symbol)(implicit cc: CompilerContext) = new LowerInterconnect
}
