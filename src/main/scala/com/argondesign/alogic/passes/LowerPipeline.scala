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
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.language.postfixOps

final class LowerPipeline(implicit cc: CompilerContext) extends TreeTransformer {

  // TODO: special case the empty pipe stage (with body 'read; write; fence;') to avoid packing

  // List of active pipeline symbols, for each stage
  private var actSetMap: Map[TypeSymbol, Set[TermSymbol]] = _

  // Map to previous/next stage
  private var prevMap: Map[TypeSymbol, TypeSymbol] = _
  private var nextMap: Map[TypeSymbol, TypeSymbol] = _

  // Pipeline input and output port symbols for current stage
  private var iPortSymbolOpt: Option[TermSymbol] = None
  private var oPortSymbolOpt: Option[TermSymbol] = None

  // Pipelined variable symbols declared in this stage
  private var freshSymbols: ListMap[String, TermSymbol] = ListMap()

  // Stack of booleans to indicate whether to rewrite this entity
  private val rewriteEntity = mutable.Stack[Boolean]()

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

      // Work out the prev an next maps
      nextMap = outer.connects collect {
        case EntConnect(ExprSym(iSymbolA), List(ExprSym(iSymbolB))) =>
          (iSymbolA.kind, iSymbolB.kind) match {
            case (TypeInstance(eSymbolA), TypeInstance(eSymbolB)) => eSymbolA -> eSymbolB
            case _                                                => unreachable
          }
      } toMap

      prevMap = nextMap map { _.swap }

      // Sort entities using pipeline connections
      val entities = {
        @tailrec
        def lt(a: TypeSymbol, b: TypeSymbol): Boolean = nextMap.get(a) match {
          case Some(`b`)   => true
          case Some(other) => lt(other, b)
          case None        => false
        }
        outer.entities sortWith { case (aEntity, bEntity) => lt(aEntity.symbol, bEntity.symbol) }
      }

      // Collect pipeline symbols used in nested entities
      val useSets = for {
        inner <- entities
      } yield {
        inner collect {
          case ExprSym(symbol: TermSymbol) if symbol.kind.isInstanceOf[TypePipeline] => symbol
          case Sym(symbol: TermSymbol, _) if symbol.kind.isInstanceOf[TypePipeline]  => symbol
        } toSet
      }

      // Propagate uses between first and last use to create activeSets
      actSetMap = {
        @tailrec
        def loop(useSets: List[Set[TermSymbol]],
                 actSets: List[Set[TermSymbol]]): List[Set[TermSymbol]] = {
          if (useSets.isEmpty) {
            actSets.reverse
          } else {
            // symbols active at any stage later than the current stage
            val actTail = useSets.tail.foldLeft(Set.empty[TermSymbol]) { _ | _ }
            // symbols active at the previous stage
            val actPrev = actSets.head
            // symbols active at the current stage
            val actHead = useSets.head | (actTail & actPrev)
            // Do next stage
            loop(useSets.tail, actHead :: actSets)
          }
        }
        val propagated = loop(useSets.tail, List(useSets.head))
        (entities zip propagated) map { case (entity, actSet) => entity.symbol -> actSet } toMap
      }
    }

    case inner: Entity if actSetMap.nonEmpty => {
      rewriteEntity.push(true)

      // Figure out pipeline port types
      val actPrev = prevMap.get(inner.symbol) map { actSetMap(_) } getOrElse Set[TermSymbol]()
      val actCurr = actSetMap(inner.symbol)
      val actNext = nextMap.get(inner.symbol) map { actSetMap(_) } getOrElse Set[TermSymbol]()

      def makePortSymbol(in: Boolean, actSet: Set[TermSymbol]): Option[TermSymbol] = {
        if (actSet.isEmpty) {
          None
        } else {
          val act = actSet.toList sortWith { _.id < _.id }
          val name = if (in) "pipeline_i" else "pipeline_o"
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
          Some(cc.newTermSymbol(name, inner.loc, kind))
        }
      }

      iPortSymbolOpt = makePortSymbol(in = true, actPrev & actCurr)
      oPortSymbolOpt = makePortSymbol(in = false, actNext & actCurr)

      freshSymbols = {
        val pairs = for (psymbol <- actCurr.toList sortWith { _.id < _.id }) yield {
          val name = psymbol.name
          val kind = psymbol.kind.underlying
          name -> cc.newTermSymbol(name, inner.loc, kind)
        }
        ListMap(pairs: _*)
      }
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

      val newBody = entity.body filter {
        // loose pipeline variable declarations
        case EntDecl(Decl(symbol, _)) => !symbol.kind.isInstanceOf[TypePipeline]
        case _                        => true
      } map {
        // rewrite pipeline connections
        case conn @ EntConnect(lhs, List(rhs)) => {
          (lhs.tpe, rhs.tpe) match {
            // If its an instance -> instance connection, select the new pipeline ports
            case (_: TypeInstance, _: TypeInstance) => {
              val newLhs = ExprSelect(Tree.untype(lhs), "pipeline_o", Nil) withLoc lhs.loc
              val newRhs = ExprSelect(Tree.untype(rhs), "pipeline_i", Nil) withLoc rhs.loc
              val newConn = EntConnect(newLhs, List(newRhs))
              newConn regularize conn.loc
            }
            // Otherwise leave it alone
            case _ => conn
          }
        }
        case other => other
      }

      TypeAssigner(entity.copy(body = newBody) withLoc entity.loc)
    }

    // Transform the nested entity
    // TODO: mark inlined
    case entity: Entity if rewriteEntity.top => {
      rewriteEntity.pop()

      // Construct pipeline port declarations
      val iPortOpt = iPortSymbolOpt map { symbol =>
        EntDecl(Decl(symbol, None)) regularize entity.loc
      }
      val oPortOpt = oPortSymbolOpt map { symbol =>
        EntDecl(Decl(symbol, None)) regularize entity.loc
      }

      // Construct declarations for pipeline variables
      val freshDecls = freshSymbols.values map { symbol =>
        EntDecl(Decl(symbol, None)) regularize entity.loc
      }

      // Update type of entity to include new ports
      val newKind = entity.symbol.kind match {
        case kind: TypeEntity => {
          val newPortSymbols = List.concat(iPortSymbolOpt, oPortSymbolOpt, kind.portSymbols)
          kind.copy(portSymbols = newPortSymbols)
        }
        case _ => unreachable
      }
      entity.symbol.kind = newKind

      val result = entity.copy(
        body = List.concat(iPortOpt, oPortOpt, freshDecls, entity.body)
      ) withLoc entity.loc
      TypeAssigner(result)
    }

    case node: Entity =>
      node tap { _ =>
        rewriteEntity.pop()
      }

    case node: StmtRead if iPortSymbolOpt.isEmpty => {
      cc.fatal(node, "'read' statement in first pipeline stage")
    }

    case node: StmtWrite if oPortSymbolOpt.isEmpty => {
      cc.fatal(node, "'write' statement in last pipeline stage")
    }

    // Rewrite 'read;' statement to '{....} = pipeline_i.read();'
    case node: StmtRead =>
      iPortSymbolOpt.get.kind match {
        case TypeIn(iPortKind: TypeStruct, _) =>
          val lhsRefs = for (name <- iPortKind.fieldNames) yield ExprSym(freshSymbols(name))
          val lhs = ExprCat(lhsRefs)
          val rhs = ExprCall(ExprSelect(ExprSym(iPortSymbolOpt.get), "read", Nil), Nil)
          StmtAssign(lhs, rhs) regularize node.loc
        case _ => unreachable
      }

    // Rewrite 'write;' statement to 'pipeline_o.write({....});'
    case node: StmtWrite =>
      oPortSymbolOpt.get.kind match {
        case TypeOut(oPortKind: TypeStruct, _, _) =>
          val rhsRefs = for (name <- oPortKind.fieldNames) yield ExprSym(freshSymbols(name))
          val rhs = ExprCat(rhsRefs)
          val call = ExprCall(ExprSelect(ExprSym(oPortSymbolOpt.get), "write", Nil), List(rhs))
          StmtExpr(call) regularize node.loc
        case _ => unreachable
      }

    // Rewrite references to pipeline variables to references to the newly declared symbols
    case node @ ExprSym(symbol) =>
      symbol.kind match {
        case _: TypePipeline => ExprSym(freshSymbols(symbol.name)) regularize node.loc
        case _               => tree
      }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(rewriteEntity.isEmpty)

    tree visit {
      case node: StmtRead  => cc.ice(node, "read statement remains after LowerPipeline")
      case node: StmtWrite => cc.ice(node, "write statement remains after LowerPipeline")
      case node @ Decl(symbol, _) if symbol.kind.isInstanceOf[TypePipeline] => {
        cc.ice(node, "Pipeline variable declaration remains after LowerPipeline")
      }
      case node @ ExprSym(symbol) => {
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
