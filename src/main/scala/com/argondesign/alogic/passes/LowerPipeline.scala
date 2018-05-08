////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017 - 2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Peter de Rivaz/Geza Lore
//
// DESCRIPTION:
//
// Transform pipeline variables and read/write statements into pipeline ports
// and .read()/.write() on those ports.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeReady
import com.argondesign.alogic.core.StorageTypes.StorageSliceFwd
import com.argondesign.alogic.core.StorageTypes.StorageTypeSlices
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.core.Symbols.TypeSymbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Stack
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.FollowedBy
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

final class LowerPipeline(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy {

  // TODO: special case the empty pipe stage (with body 'read; write; fence;') to avoid packing

  // List of active pipeline symbols, for each stage
  private var actSets: List[Set[TermSymbol]] = Nil

  // Previous stage active list
  private var prevActSet: Set[TermSymbol] = Set()

  // Pipeline input and output port symbols for current stage
  private var iPortSymbolOpt: Option[TermSymbol] = None
  private var oPortSymbolOpt: Option[TermSymbol] = None

  // Pipelined variable symbols declared in this stage
  private var freshSymbols: ListMap[String, TermSymbol] = ListMap()

  // Stack of booleans to indicate whether to rewrite this entity
  private val rewriteEntity: Stack[Boolean] = Stack()

  private var firstEntity = true

  override def skip(tree: Tree): Boolean = tree match {
    // If this is the root entity, skip it if it has no pipeline declarations
    case entity: Entity if firstEntity => {
      firstEntity = false
      entity.declarations forall {
        case Decl(symbol, _) => !symbol.kind.isInstanceOf[TypePipeline]
        case _               => unreachable
      }
    }
    case _ => false
  }

  override def enter(tree: Tree): Unit = tree match {
    case outer: Entity if outer.entities.nonEmpty => {
      rewriteEntity.push(true)
      // Collect pipeline symbols used in nested entities
      val useSets = for {
        inner <- outer.entities
      } yield {
        val it = inner collect {
          case Sym(symbol: TermSymbol) if symbol.kind.isInstanceOf[TypePipeline] => symbol
        }
        it.toSet
      }

      // TODO: this assumes stages are in order. Do a topological sort or err..
      // Propagate uses between first and last use to create activeSets
      actSets = {
        @tailrec
        def loop(useSets: List[Set[TermSymbol]],
                 actSets: List[Set[TermSymbol]]): List[Set[TermSymbol]] = {
          if (useSets.isEmpty) {
            actSets.reverse
          } else {
            // symbols active at any stage later than the current stage
            val actTail = (Set.empty[TermSymbol] /: useSets.tail) { _ | _ }
            // symbols active at the previous stage
            val actPrev = actSets.head
            // symbols active at the current stage
            val actHead = useSets.head | (actTail & actPrev)
            // Do next stage
            loop(useSets.tail, actHead :: actSets)
          }
        }
        loop(useSets.tail, List(useSets.head))
      }
    }

    case inner: Entity if actSets.nonEmpty => {
      rewriteEntity.push(true)

      // Figure out pipeline port types
      val actPrev = prevActSet
      val actCurr = actSets.head
      val actNext = if (actSets.length > 1) actSets.tail.head else Set[TermSymbol]()

      def makePortSymbol(in: Boolean, actSet: Set[TermSymbol]): Option[TermSymbol] = {
        if (actSet.isEmpty) {
          None
        } else {
          val act = actSet.toList sortWith { _.id < _.id }
          val name = if (in) "pipeline_i" else "pipeline_o"
          val ident = Ident(name) withLoc inner.loc
          val struct = TypeStruct(
            s"${name}_t",
            act map { _.name },
            act map { _.kind.underlying }
          )
          val kind = if (in) {
            TypeIn(struct, FlowControlTypeReady)
          } else {
            val st = StorageTypeSlices(List(StorageSliceFwd))
            TypeOut(struct, FlowControlTypeReady, st)
          }
          val symbol = cc.newTermSymbol(ident, kind)
          Some(symbol)
        }
      }

      iPortSymbolOpt = makePortSymbol(in = true, actPrev & actCurr)
      oPortSymbolOpt = makePortSymbol(in = false, actNext & actCurr)

      freshSymbols = {
        val pairs = for (psymbol <- actCurr.toList sortWith { _.id < _.id }) yield {
          val name = psymbol.name
          val kind = psymbol.kind.underlying
          val ident = Ident(name) withLoc inner.loc
          val vsymbol = cc.newTermSymbol(ident, kind)
          (name -> vsymbol)
        }
        ListMap(pairs: _*)
      }

      prevActSet = actSets.head
      actSets = actSets.tail
    }

    case _: Entity => {
      rewriteEntity.push(false)
    }

    case _ =>
  }

  override def transform(tree: Tree): Tree = tree match {
    // Transform the outer entity
    case entity: Entity if rewriteEntity.top && entity.entities.nonEmpty => {
      rewriteEntity.pop()

      // loose pipeline variable declarations
      val newDecls = entity.declarations filter {
        case Decl(symbol, _) => !symbol.kind.isInstanceOf[TypePipeline]
        case _               => true
      }

      // rewrite pipeline connections
      val newConns = entity.connects map {
        case conn @ Connect(lhs, List(rhs)) => {
          (lhs.tpe, rhs.tpe) match {
            // If its an instance -> instance connection, select the new pipeline ports
            case (_: TypeInstance, _: TypeInstance) => {
              val newLhs = ExprSelect(Tree.untype(lhs), "pipeline_o") withLoc lhs.loc
              val newRhs = ExprSelect(Tree.untype(rhs), "pipeline_i") withLoc rhs.loc
              val newConn = Connect(newLhs, List(newRhs))
              newConn regularize conn.loc
            }
            // Otherwise leave it alone
            case _ => conn
          }
        }
        case other => other
      }

      val result = entity.copy(
        declarations = newDecls,
        connects = newConns
      ) withLoc entity.loc withVariant entity.variant
      TypeAssigner(result)
    }

    // Transform the nested entity
    // TODO: mark inlined
    case entity: Entity if rewriteEntity.top => {
      rewriteEntity.pop()

      // Construct pipeline port declarations
      val iPortOpt = iPortSymbolOpt map { symbol =>
        Decl(symbol, None) regularize entity.loc
      }
      val oPortOpt = oPortSymbolOpt map { symbol =>
        Decl(symbol, None) regularize entity.loc
      }

      // Construct declarations for pipeline variables
      val freshDecls = for { symbol <- freshSymbols.values.toList } yield {
        Decl(symbol, None) regularize entity.loc
      }

      // List of new decls
      val newDecls = iPortOpt.toList ::: oPortOpt.toList ::: freshDecls ::: entity.declarations

      // Update type of entity to include new ports
      val Sym(symbol: TypeSymbol) = entity.ref
      val newKind = symbol.kind match {
        case kind: TypeEntity => {
          val newPortSymbols = iPortSymbolOpt.toList ::: oPortSymbolOpt.toList ::: kind.portSymbols
          kind.copy(portSymbols = newPortSymbols)
        }
        case _ => unreachable
      }
      symbol.kind = newKind

      val result = entity.copy(
        declarations = newDecls
      ) withLoc entity.loc withVariant entity.variant
      TypeAssigner(result)
    }

    case node: Entity => node followedBy rewriteEntity.pop()

    case node: StmtRead if iPortSymbolOpt.isEmpty => {
      cc.fatal(node, "'read' statement in first pipeline stage")
    }

    case node: StmtWrite if oPortSymbolOpt.isEmpty => {
      cc.fatal(node, "'write' statement in last pipeline stage")
    }

    // Rewrite 'read;' statement to '{....} = pipeline_i.read();'
    case node: StmtRead => {
      val TypeIn(iPortKind: TypeStruct, _) = iPortSymbolOpt.get.kind
      val lhsRefs = for (name <- iPortKind.fieldNames) yield ExprRef(Sym(freshSymbols(name)))
      val lhs = ExprCat(lhsRefs)
      val rhs = ExprCall(ExprSelect(ExprRef(Sym(iPortSymbolOpt.get)), "read"), Nil)
      StmtAssign(lhs, rhs) regularize node.loc
    }

    // Rewrite 'write;' statement to 'pipeline_o.write({....});'
    case node: StmtWrite => {
      val TypeOut(oPortKind: TypeStruct, _, _) = oPortSymbolOpt.get.kind
      val rhsRefs = for (name <- oPortKind.fieldNames) yield ExprRef(Sym(freshSymbols(name)))
      val rhs = ExprCat(rhsRefs)
      val call = ExprCall(ExprSelect(ExprRef(Sym(oPortSymbolOpt.get)), "write"), List(rhs))
      StmtExpr(call) regularize node.loc
    }

    // Rewrite references to pipeline variables to references to the newly declared symbols
    case node @ ExprRef(Sym(symbol)) if symbol.kind.isInstanceOf[TypePipeline] => {
      ExprRef(Sym(freshSymbols(symbol.name))) regularize node.loc
    }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(actSets.isEmpty)
    assert(rewriteEntity.isEmpty)

    tree visit {
      case node: StmtRead  => cc.ice(node, "read statement remains after LowerPipeline")
      case node: StmtWrite => cc.ice(node, "write statement remains after LowerPipeline")
      case node @ Decl(symbol, _) if symbol.kind.isInstanceOf[TypePipeline] => {
        cc.ice(node, "Pipeline variable declaration remains after LowerPipeline")
      }
      case node @ Sym(symbol) => {
        symbol.kind match {
          case _: TypePipeline => {
            cc.ice(node, "Pipeline variable reference remains after LowerPipeline")
          }
          case _ =>
        }
      }
    }

    tree visitAll {
      case node: Tree if node.hasTpe => {
        node.tpe match {
          case _: TypePipeline => {
            cc.ice(node, "Pipeline variable type remains after LowerPipeline")
          }
          case _ =>
        }
      }
    }
  }

}

object LowerPipeline extends TreeTransformerPass {
  val name = "lower-pipeline"
  def create(implicit cc: CompilerContext) = new LowerPipeline
}
