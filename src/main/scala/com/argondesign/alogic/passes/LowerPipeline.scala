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
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.StorageTypes.StorageSliceFwd
import com.argondesign.alogic.core.StorageTypes.StorageTypeSlices
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.language.postfixOps

final class LowerPipelineStage(
    iPortSymbolOpt: Option[(Symbol, TypeRecord)],
    oPortSymbolOpt: Option[(Symbol, TypeRecord)],
    pipelinedSymbols: Map[Symbol, Symbol]
)(implicit cc: CompilerContext)
    extends TreeTransformer {

  private val pipelinedNames = pipelinedSymbols map { case (k, v) => (k.name, v) }

  private def mkEntDefn(symbol: Symbol) = EntDefn(symbol.mkDefn) regularize symbol.loc
  private def mkDecl(symbol: Symbol) = symbol.mkDecl regularize symbol.loc

  override def transform(tree: Tree): Tree = tree match {
    // Add definitions
    case defn: DefnEntity =>
      val iPortDefn = iPortSymbolOpt.iterator flatMap {
        case (port, struct) => Iterator(mkEntDefn(struct.symbol), mkEntDefn(port))
      }
      val oPortDefn = oPortSymbolOpt.iterator flatMap {
        case (port, struct) => Iterator(mkEntDefn(struct.symbol), mkEntDefn(port))
      }
      val pipelinedDefns = pipelinedSymbols.valuesIterator map mkEntDefn
      TypeAssigner {
        defn.copy(
          body = List.from(iPortDefn ++ oPortDefn ++ pipelinedDefns ++ defn.body.iterator)
        ) withLoc defn.loc
      }

    // Add declarations
    case decl: DeclEntity =>
      val iPortDecl = iPortSymbolOpt.iterator flatMap {
        case (port, struct) => Iterator(mkDecl(struct.symbol), mkDecl(port))
      }
      val oPortDecl = oPortSymbolOpt.iterator flatMap {
        case (port, struct) => Iterator(mkDecl(struct.symbol), mkDecl(port))
      }
      val pipelinedDecls = pipelinedSymbols.valuesIterator map mkDecl
      TypeAssigner {
        decl.copy(
          decls = List.from(iPortDecl ++ oPortDecl ++ pipelinedDecls ++ decl.decls.iterator)
        ) withLoc decl.loc
      }

    // Rewrite 'read;' statement to '{....} = pipeline_i.read();'
    case node: StmtRead =>
      iPortSymbolOpt match {
        case None =>
          cc.error(node, "'read' statement in first pipeline stage")
          Stump
        case Some((iPortSymbol, iPortKind)) =>
          val lhsRefs = iPortKind.publicSymbols map { symbol =>
            ExprSym(pipelinedNames(symbol.name))
          }
          val lhs = ExprCat(lhsRefs)
          val rhs = ExprCall(ExprSelect(ExprSym(iPortSymbol), "read", Nil), Nil)
          StmtAssign(lhs, rhs) regularize node.loc
        case _ => unreachable
      }

    // Rewrite 'write;' statement to 'pipeline_o.write({....});'
    case node: StmtWrite =>
      oPortSymbolOpt match {
        case None =>
          cc.error(node, "'write' statement in last pipeline stage")
          Stump
        case Some((oPortSymbol, oPortKind)) =>
          val rhsRefs = oPortKind.publicSymbols map { symbol =>
            ExprSym(pipelinedNames(symbol.name))
          }
          val arg = ArgP(ExprCat(rhsRefs))
          val call = ExprCall(ExprSelect(ExprSym(oPortSymbol), "write", Nil), List(arg))
          StmtExpr(call) regularize node.loc
        case _ => unreachable
      }

    // Rewrite references to pipeline symbols with the local pipelined symbol
    case ExprSym(symbol) =>
      pipelinedSymbols get symbol match {
        case Some(newSymbol) => TypeAssigner(ExprSym(newSymbol) withLoc tree.loc)
        case None            => tree
      }

    //
    case _ => tree
  }
}

final class LowerPipelineHost(implicit cc: CompilerContext) extends TreeTransformer {

  // Map from old symbols to replaced symbol/decl/defn
  private val symbolMap: mutable.Map[Symbol, (Symbol, Decl, Defn)] = mutable.Map()

  override def enter(tree: Tree): Option[Tree] = {
    tree match {
      // Process entities which contain pipeline variables
      case outer: DefnEntity if outer.symbol.decl.decls exists { _.symbol.kind.isPipeline } =>
        // Work out the prev an next maps
        val nextMap = Map from {
          outer.connects collect {
            case EntConnect(ExprSym(iSymbolA), List(ExprSym(iSymbolB))) =>
              (iSymbolA.kind, iSymbolB.kind) match {
                case (TypeEntity(eSymbolA, _), TypeEntity(eSymbolB, _)) => eSymbolA -> eSymbolB
                case _                                                  => unreachable
              }
          }
        }

        val prevMap = nextMap map { _.swap }

        // Sort stages using pipeline connections
        val entities = {
          @tailrec
          def lt(a: Symbol, b: Symbol): Boolean = nextMap.get(a) match {
            case Some(`b`)   => true
            case Some(other) => lt(other, b)
            case None        => false
          }
          outer.entities sortWith {
            case (aEntity, bEntity) => lt(aEntity.symbol, bEntity.symbol)
          }
        }

        // Collect pipeline symbols referenced in nested entities to create the use sets
        val useSets = for { inner <- entities } yield {
          Set from {
            inner collect {
              case ExprSym(symbol) if symbol.kind.isPipeline => symbol
            }
          }
        }

        // Propagate uses between first and last use to create the active sets
        val actSetsMap = {
          @tailrec
          def loop(useSets: List[Set[Symbol]], actSets: List[Set[Symbol]]): List[Set[Symbol]] = {
            if (useSets.isEmpty) {
              actSets.reverse
            } else {
              // symbols active at any stage later than the current stage
              val actTail = useSets.tail.foldLeft(Set.empty[Symbol]) { _ | _ }
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

        // Transform stage entities
        for (inner <- outer.entities) {
          // Figure out pipeline port types
          val actPrev = prevMap.get(inner.symbol) map actSetsMap getOrElse Set[Symbol]()
          val actCurr = actSetsMap(inner.symbol)
          val actNext = nextMap.get(inner.symbol) map actSetsMap getOrElse Set[Symbol]()

          def makePortSymbol(in: Boolean, actSet: Set[Symbol]): Option[(Symbol, TypeRecord)] = {
            if (actSet.isEmpty) {
              None
            } else {
              val aSymbols = actSet.toList sortWith { _.id < _.id }
              val mSymbols = aSymbols map { symbol =>
                cc.newSymbol(symbol.name, symbol.loc) tap { _.kind = symbol.kind.underlying }
              }
              val name = if (in) "pipeline_i" else "pipeline_o"
              val sSymbol = cc.newSymbol(s"${name}_t", Loc.synthetic)
              val pType = TypeRecord(sSymbol, mSymbols)
              sSymbol.kind = TypeType(pType)
              val kind = if (in) {
                TypeIn(pType, FlowControlTypeReady)
              } else {
                val slices = inner.symbol.attr.pipelineStorage.getOrElse(List(StorageSliceFwd))
                TypeOut(pType, FlowControlTypeReady, StorageTypeSlices(slices))
              }
              Some((cc.newSymbol(name, inner.loc) tap { _.kind = kind }, pType))
            }
          }

          val iPortSymbolOpt = makePortSymbol(in = true, actPrev & actCurr)
          val oPortSymbolOpt = makePortSymbol(in = false, actNext & actCurr)
          val pipelinedSymbols = ListMap from {
            for (pSymbol <- actCurr.toList sortWith { _.id < _.id }) yield {
              pSymbol -> (pSymbol.dup tap { _.kind = pSymbol.kind.underlying })
            }
          }

          val transform = new LowerPipelineStage(
            iPortSymbolOpt,
            oPortSymbolOpt,
            pipelinedSymbols
          )

          val newSymbol = inner.symbol.dup
          val newDecl = TypeAssigner(inner.symbol.decl.cpy(symbol = newSymbol) withLoc inner.loc) rewrite transform
          val newDefn = TypeAssigner(inner.copy(symbol = newSymbol) withLoc inner.loc) rewrite transform
          symbolMap(inner.symbol) = (newSymbol, newDecl, newDefn)
        }

        // Transform stage instances
        for (inner <- outer.instances) {
          val eSymbol = inner.symbol.kind.asEntity.symbol
          symbolMap.get(eSymbol) match {
            case None =>
            case Some((newESymbol, _, _)) =>
              val newSymbol = inner.symbol.dup tap { _.kind = newESymbol.kind.asType.kind }
              val newDecl = newSymbol.mkDecl regularize inner.loc
              val newDefn = newSymbol.mkDefn regularize inner.loc
              symbolMap(inner.symbol) = (newSymbol, newDecl, newDefn)
          }
        }

      case _ =>
    }
    None
  }

  override def transform(tree: Tree): Tree = tree match {
    // Drop Pipeline decl/defn
    case _: DeclPipeline => Stump
    case _: DefnPipeline => Stump

    // Update declarations of replaced symbols
    case decl: Decl =>
      symbolMap get decl.symbol match {
        case None                  => tree
        case Some((_, newDecl, _)) => newDecl
      }

    // Update definitions of replaced symbols
    case defn: Defn =>
      symbolMap get defn.symbol match {
        case None                  => tree
        case Some((_, _, newDefn)) => newDefn
      }

    // Rewrite pipeline connections
    case EntConnect(lhs, List(rhs)) if lhs.tpe.isEntity && rhs.tpe.isEntity =>
      val newLhs = ExprSelect(lhs, "pipeline_o", Nil) withLoc lhs.loc
      val newRhs = ExprSelect(rhs, "pipeline_i", Nil) withLoc rhs.loc
      val newConn = EntConnect(newLhs, List(newRhs))
      newConn regularize tree.loc

    // Rewrite references to replaced symbols
    case ExprSym(symbol) =>
      symbolMap get symbol match {
        case Some((newSymbol, _, _)) => TypeAssigner(ExprSym(newSymbol) withLoc tree.loc)
        case None                    => tree
      }

    //
    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    tree visit {
      case node: StmtRead  => cc.ice(node, "read statement remains after LowerPipeline")
      case node: StmtWrite => cc.ice(node, "write statement remains after LowerPipeline")
      case node: DeclPipeline =>
        cc.ice(node, "Pipeline variable declaration remains after LowerPipeline")
      case node: DefnPipeline =>
        cc.ice(node, "Pipeline variable definition remains after LowerPipeline")
      case node @ ExprSym(symbol) if symbol.kind.isPipeline =>
        cc.ice(node, "Pipeline variable reference remains after LowerPipeline")
      case node @ ExprSym(symbol) if symbolMap contains symbol =>
        cc.ice(node, "Reference to replaced symbol remains")
    }

    tree visitAll {
      case node: Tree if node.tpe.isPipeline =>
        cc.ice(node, "Pipeline variable type remains after LowerPipeline")
    }
  }
}

object LowerPipeline extends PairTransformerPass {
  val name = "lower-pipeline"

  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) = {
    val transformer = new LowerPipelineHost
    // First transform the defn
    val newDefn = transformer(defn)
    // Then transform the decl
    val newDecl = transformer(decl)
    (newDecl, newDefn)
  }
}
