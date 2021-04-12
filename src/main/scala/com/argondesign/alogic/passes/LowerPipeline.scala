////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Transform pipeline variables and ports into local variables and common ports
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.Expr.InstancePortSel
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.TypeCompound
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer

final class LowerPipelineStage(
    portSymbolMap: Map[Symbol, Set[Symbol]],
    localSymbols: Set[Symbol])
    extends StatefulTreeTransformer {

  private var stageSymbol: Symbol = _

  private val pipeAllSymbols = localSymbols.toList.sortBy(symbol => (symbol.loc, symbol.name))

  private def pipePortSymbols(portSymbol: Symbol): List[Symbol] =
    pipeAllSymbols.filter(portSymbolMap(orig.getOrElse(portSymbol, portSymbol)))

  // Map from active pipeline symbol to local symbol
  private val pipeSymbolMap = ListMap from {
    pipeAllSymbols map { symbol => symbol -> dupDropPipeVar(symbol) }
  }

  private val newSymbols = new ListBuffer[Symbol]
  newSymbols.appendAll(pipeSymbolMap.valuesIterator)

  override protected def start(tree: Tree): Unit = tree match {
    case decl: DeclEntity => stageSymbol = decl.symbol
    case _: DefnEntity    => assert(stageSymbol != null)
    case _                => unreachable
  }

  private def dupDropPipeVar(symbol: Symbol): Symbol = {
    require(symbol.kind.isPipeVar)
    symbol.dup tap { _.kind = symbol.kind.underlying }
  }

  private def makePortType(name: String, symbols: List[Symbol]): TypeRecord = {
    require(symbols.nonEmpty)
    val recSymbol = Symbol(name, Loc.synthetic)
    recSymbol.scopeName = stageSymbol.hierName
    val kind = TypeRecord(recSymbol, symbols map dupDropPipeVar)
    recSymbol.kind = TypeType(kind)
    kind
  }

  override protected def replace(symbol: Symbol): Boolean = symbol.kind match {
    case TypePipeIn(_, FlowControlTypeNone) =>
      portSymbolMap(symbol).nonEmpty // Otherwise will be removed
    case TypePipeOut(_, FlowControlTypeNone, _) =>
      portSymbolMap(symbol).nonEmpty // Otherwise will be removed
    case _: TypePipeIn | _: TypePipeOut => true
    case _                              => false
  }

  override protected def enter(tree: Tree): Option[Tree] = tree match {
    // Skip nested entities (already transformed)
    case decl: DeclEntity if decl.symbol ne stageSymbol => Some(tree)
    case defn: DefnEntity if defn.symbol ne stageSymbol => Some(tree)

    // Rewrite 'TypePipeIn.read();' statements to '{....} = TypeIn.read();'
    case StmtExpr(call @ ExprCall(ExprSel(ExprSym(symbol), "read"), Nil)) if symbol.kind.isPipeIn =>
      Some {
        val ppss = pipePortSymbols(symbol)
        if (symbol.kind.asPipeIn.fc == FlowControlTypeNone && ppss.isEmpty) {
          // This port is being removed
          Stump
        } else {
          // The rhs is the call, but with the type update, 'walk' does just
          // that because we are replacing the pipeline port symbols.
          val rhs = walkSame(call)
          if (ppss.isEmpty) {
            // The result type is void, so no assignment is required
            StmtExpr(rhs) regularize tree.loc
          } else {
            // The lhs is all local copies of the incoming pipeline variable symbols
            val lhs = ExprCat(ppss.map(pps => ExprSym(pipeSymbolMap(pps))))
            // Form the assignment
            StmtAssign(lhs, rhs) regularize tree.loc
          }
        }
      }

    // Rewrite 'TypePipeOut.write();' statements to 'TypeOut.write({....});'.
    case StmtExpr(ExprCall(sel @ ExprSel(ExprSym(symbol), "write"), Nil))
        if symbol.kind.isPipeOut =>
      Some {
        val ppss = pipePortSymbols(symbol)
        if (symbol.kind.asPipeOut.fc == FlowControlTypeNone && ppss.isEmpty) {
          // This port is being removed
          Stump
        } else {
          // The call target is the same port, but with the type updated, so 'walk' ...
          val tgt = walkSame(sel)
          val args = if (ppss.isEmpty) {
            // No outgoing pipeline variables
            Nil
          } else {
            // The argument is all local copies of the outgoing pipeline variable symbols
            ArgP(ExprCat(ppss.map(pps => ExprSym(pipeSymbolMap(pps))))) :: Nil
          }
          // Form the call
          StmtExpr(ExprCall(tgt, args)) regularize tree.loc
        }
      }

    case _ => None
  }

  override def transform(tree: Tree): Tree = tree match {
    // Update pipeline port Decl/Defn
    case DeclPipeIn(symbol, _, fc) =>
      pipePortSymbols(symbol) match {
        case Nil =>
          if (fc == FlowControlTypeNone) {
            Stump
          } else {
            DeclIn(symbol, ExprType(TypeVoid), fc) regularize tree.loc
          }
        case symbols =>
          val kind = makePortType("pipeline_in", symbols)
          newSymbols.append(kind.symbol)
          DeclIn(symbol, ExprSym(kind.symbol), fc) regularize tree.loc
      }
    case DefnPipeIn(symbol) =>
      if (
        pipePortSymbols(symbol).isEmpty && orig
          .getOrElse(symbol, symbol)
          .kind
          .asPipeIn
          .fc == FlowControlTypeNone
      ) {
        Stump
      } else {
        DefnIn(symbol) regularize tree.loc
      }
    case DeclPipeOut(symbol, _, fc, st) =>
      pipePortSymbols(symbol) match {
        case Nil =>
          if (fc == FlowControlTypeNone) {
            Stump
          } else {
            DeclOut(symbol, ExprType(TypeVoid), fc, st) regularize tree.loc
          }
        case symbols =>
          val kind = makePortType("pipeline_out", symbols)
          newSymbols.append(kind.symbol)
          DeclOut(symbol, ExprSym(kind.symbol), fc, st) regularize tree.loc
      }
    case DefnPipeOut(symbol) =>
      if (
        pipePortSymbols(symbol).isEmpty && orig
          .getOrElse(symbol, symbol)
          .kind
          .asPipeOut
          .fc == FlowControlTypeNone
      ) {
        Stump
      } else {
        DefnOut(symbol, None) regularize tree.loc
      }

    // Add Decls/Defns of pipelined variables and the port type
    case decl: DeclEntity =>
      val newDecls = newSymbols map { symbol => symbol.mkDecl regularize symbol.loc }
      TypeAssigner {
        decl.copy(
          decls = List.from(decl.decls.iterator ++ newDecls)
        ) withLoc decl.loc
      }
    case defn: DefnEntity =>
      val newDefns = newSymbols map { symbol => EntSplice(symbol.mkDefn) regularize symbol.loc }
      TypeAssigner {
        defn.copy(
          body = List.from(defn.body.iterator ++ newDefns)
        ) withLoc defn.loc
      }

    // Rewrite references to pipelined variables with the local symbol
    case ExprSym(symbol) =>
      pipeSymbolMap get symbol map { localSymbol =>
        TypeAssigner(ExprSym(localSymbol) withLoc tree.loc)
      } getOrElse tree

    //
    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    // $COVERAGE-OFF$ Debug code
    tree visit {
      case node: DeclPipeIn  => throw Ice(node, "DeclPipeIn remains after LowerPipeline")
      case node: DefnPipeIn  => throw Ice(node, "DefnPipeIn remains after LowerPipeline")
      case node: DeclPipeOut => throw Ice(node, "DeclPipeOut remains after LowerPipeline")
      case node: DefnPipeOut => throw Ice(node, "DefnPipeOut remains after LowerPipeline")
    }

    tree visitAll {
      case node if node.tpe.isPipeVar => throw Ice(node, "TypePipeVar tree remains")
      case node if node.tpe.isPipeIn  => throw Ice(node, "TypePipeIn tree remains")
      case node if node.tpe.isPipeOut => throw Ice(node, "TypePipeOut tree remains")
    }
    // $COVERAGE-ON$
  }

}

final class LowerPipelineHost extends StatefulTreeTransformer {

  // Map from stage entity symbol the transform to apply to its Decl/Defn
  private var stageTransform: Map[Symbol, LowerPipelineStage] = Map.empty

  override protected def start(tree: Tree): Unit = tree match {
    case defn: DefnEntity =>
      // Map of 'entity' -> 'set of pipeline variables available within'
      val pipeVars = {
        def gather(defn: DefnEntity, outer: Set[Symbol]): Map[Symbol, Set[Symbol]] = {
          val pipeVars = Set from {
            defn.defns.iterator.collect { case DefnPipeVar(symbol) => symbol }
          }
          val current =
            outer.filterNot(symbol => pipeVars.exists(_.name == symbol.name)) union pipeVars
          val recursive = defn.entities.foldLeft(Map.empty[Symbol, Set[Symbol]]) {
            _ ++ gather(_, current)
          }

          recursive + (defn.symbol -> outer)
        }
        gather(defn, Set.empty)
      }

      // Map of 'pipeline port' -> 'containing entity'
      val enclosing = {
        def gather(defn: DefnEntity): Map[Symbol, Symbol] =
          defn.body.foldLeft(Map.empty[Symbol, Symbol]) {
            case (acc, EntSplice(DefnPipeIn(symbol)))  => acc + (symbol -> defn.symbol)
            case (acc, EntSplice(DefnPipeOut(symbol))) => acc + (symbol -> defn.symbol)
            case (acc, EntSplice(d: DefnEntity))       => acc ++ gather(d)
            case (acc, _)                              => acc
          }
        gather(defn)
      }

      // The 'port' -> 'port' connections (based on entities, not instances!)
      val assignments = Set from {
        defn.collect {
          case EntAssign(InstancePortSel(_, l), InstancePortSel(_, r)) if r.kind.isPipeOut =>
            r -> l
          case EntAssign(InstancePortSel(_, l), ExprSym(r)) if r.kind.isPipeIn =>
            r -> l
          case EntAssign(ExprSym(l), InstancePortSel(_, r)) if r.kind.isPipeOut =>
            r -> l
          case EntAssign(ExprSym(l), ExprSym(r)) if r.kind.isPipeIn =>
            r -> l
        }
      }

      // Map of 'port' -> 'pipe vars present'
      val portVars: Map[Symbol, Set[Symbol]] = {
        @tailrec
        def loop(curr: Map[Symbol, Set[Symbol]]): Map[Symbol, Set[Symbol]] = {
          // Run one round of contraction based on the connections
          val next = assignments.foldLeft(curr) {
            case (acc, (a, b)) =>
              val intersection = curr(a) intersect curr(b)
              acc.updated(a, intersection).updated(b, intersection)
          }
          if (next == curr) curr else loop(next)
        }
        loop {
          // Start from all active pipe vars within the enclosing entity
          enclosing.map { case (pSymbol, eSymbol) => pSymbol -> pipeVars(eSymbol) }
        }
      }

      val entitiesWithPipelinePorts = enclosing.valuesIterator.toSet

      // Create the stage transforms
      stageTransform = pipeVars.collect {
        case (symbol, localVars) if localVars.nonEmpty || entitiesWithPipelinePorts(symbol) =>
          symbol -> new LowerPipelineStage(portVars, localVars)
      }

    case _ =>
  }

  // Replace stage entities and instances
  override protected def replace(symbol: Symbol): Boolean = symbol.kind match {
    case TypeType(_: TypeEntity) => stageTransform contains symbol // Stage entity
    case kind: TypeEntity        => stageTransform contains kind.symbol // Stage instance
    case _                       => false
  }

  override def enter(tree: Tree): Option[Tree] = tree match {
    // If a port got removed, drop the connect
    case EntAssign(ExprSel(lTgt, lSel), ExprSel(rTgt, rSel)) =>
      val newLTgt = walk(lTgt).asInstanceOf[Expr]
      val newRTgt = walk(rTgt).asInstanceOf[Expr]
      val lOk = newLTgt.tpe match {
        case kind: TypeCompound => kind(lSel).isDefined
        case _                  => unreachable
      }
      val rOk = newRTgt.tpe match {
        case kind: TypeCompound => kind(rSel).isDefined
        case _                  => unreachable
      }
      Option.when(!lOk || !rOk) {
        Stump
      }

    case _ => None
  }

  override def transform(tree: Tree): Tree = tree match {
    // Drop PipeVar decl/defn
    case _: DeclPipeVar => Stump
    case _: DefnPipeVar => Stump

    // Apply the stage transforms if exists
    case Decl(symbol) =>
      orig.get(symbol) flatMap stageTransform.get map { _(tree) } getOrElse tree
    case Defn(symbol) =>
      orig.get(symbol) flatMap stageTransform.get map { _(tree) } getOrElse tree

    //
    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    // $COVERAGE-OFF$ Debug code
    tree visit {
      case node: DeclPipeVar => throw Ice(node, "DeclPipeVar remains after LowerPipeline")
      case node: DefnPipeVar => throw Ice(node, "DefnPipeVar remains after LowerPipeline")
    }

    tree visitAll {
      case node if node.tpe.isPipeVar => throw Ice(node, "TypePipeVar tree remains")
      case node if node.tpe.isPipeIn  => throw Ice(node, "TypePipeIn tree remains")
      case node if node.tpe.isPipeOut => throw Ice(node, "TypePipeOut tree remains")
    }
    // $COVERAGE-ON$
  }

}

object LowerPipeline extends EntityTransformerPass(declFirst = false) {
  val name = "lower-pipeline"

  def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer = new LowerPipelineHost
}
