////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017 - 2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Transform pipeline variables and ports into local variables and common ports
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees.Expr.InstancePortSel
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.typer.TypeAssigner

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

final class LowerPipelineStage(
    pipeInSymbols: List[Symbol], // List of incoming PipeVar symbols
    pipeOutSymbols: List[Symbol], // List of outgoing PipeVar symbols
    pipeAllSymbols: List[Symbol] // List of all active PipeVar symbols
  )(
    implicit
    cc: CompilerContext)
    extends StatefulTreeTransformer {
  require(pipeInSymbols forall { pipeAllSymbols contains _ })
  require(pipeOutSymbols forall { pipeAllSymbols contains _ })

  private def dupDropPipeVar(symbol: Symbol): Symbol = {
    require(symbol.kind.isPipeVar)
    symbol.dup tap { _.kind = symbol.kind.underlying }
  }

  // Map from active pipeline symbol to local symbol
  private val pipeSymbolMap = ListMap from {
    pipeAllSymbols map { symbol => symbol -> dupDropPipeVar(symbol) }
  }

  private def makeRecordTypeSymbol(name: String, symbols: List[Symbol]): Symbol = {
    require(symbols.nonEmpty)
    val recSymbol = cc.newSymbol(name, Loc.synthetic)
    val kind = TypeRecord(recSymbol, symbols map dupDropPipeVar)
    recSymbol.kind = TypeType(kind)
    recSymbol
  }

  private lazy val iPortTypeSymbol = makeRecordTypeSymbol("pipeline_in_t", pipeInSymbols)
  private lazy val oPortTypeSymbol = makeRecordTypeSymbol("pipeline_out_t", pipeOutSymbols)

  // New symbols added to the transformed entity
  private def newSymbols: Iterator[Symbol] = pipeSymbolMap.valuesIterator concat {
    if (pipeInSymbols.nonEmpty) Iterator.single(iPortTypeSymbol) else Iterator.empty
  } concat {
    if (pipeOutSymbols.nonEmpty) Iterator.single(oPortTypeSymbol) else Iterator.empty
  }

  override protected def replace(symbol: Symbol): Boolean = symbol.kind match {
    case _: TypePipeIn | _: TypePipeOut => true
    case _                              => false
  }

  override protected def enter(tree: Tree): Option[Tree] = tree match {
    // Rewrite 'TypePipeIn.read();' statements to '{....} = TypeIn.read();'
    // Note TypeIPipeIn.read() is Type Void, but TypeIn.read() is not, so we
    // need to do this at the statement level.
    case StmtExpr(call @ ExprCall(ExprSel(ExprSym(symbol), "read", Nil), Nil))
        if symbol.kind.isPipeIn =>
      Some {
        // The lhs is all local copies of the incoming pipeline variable symbols
        val lhs = ExprCat(pipeInSymbols map { symbol => ExprSym(pipeSymbolMap(symbol)) })
        // The rhs is the call, but with the type update, 'walk' does just that
        // because we are replacing the pipeline port symbols.
        val rhs = walk(call).asInstanceOf[Expr]
        // Form the assignment
        StmtAssign(lhs, rhs) regularize tree.loc
      }

    // Rewrite 'TypePipeOut.write()' to 'TypeOut.write({....})'. Note both of
    // these expressions are TypeVoid, so we cando it at the expression level.
    case ExprCall(sel @ ExprSel(ExprSym(symbol), "write", Nil), Nil) if symbol.kind.isPipeOut =>
      Some {
        // The argument is all local copies of the outgoing pipeline variable symbols
        val arg = ArgP(ExprCat(pipeOutSymbols map { symbol => ExprSym(pipeSymbolMap(symbol)) }))
        // The call target is the same port, but with the type updated, so 'walk' ...
        val tgt = walk(sel).asInstanceOf[Expr]
        // Form the call
        ExprCall(tgt, arg :: Nil) regularize tree.loc
      }

    case _ => None
  }

  override def transform(tree: Tree): Tree = tree match {
    // Update pipeline port Decl/Defn
    case DeclPipeIn(symbol, fc) =>
      DeclIn(symbol, ExprSym(iPortTypeSymbol), fc) regularize tree.loc
    case DefnPipeIn(symbol) =>
      DefnIn(symbol) regularize tree.loc
    case DeclPipeOut(symbol, fc, st) =>
      DeclOut(symbol, ExprSym(oPortTypeSymbol), fc, st) regularize tree.loc
    case DefnPipeOut(symbol) =>
      DefnOut(symbol, None) regularize tree.loc

    // Add Decls/Defns of pipelined variables and the port type
    case decl: DeclEntity =>
      val newDecls = newSymbols map { symbol => symbol.mkDecl regularize symbol.loc }
      TypeAssigner {
        decl.copy(
          decls = List.from(decl.decls.iterator ++ newDecls)
        ) withLoc decl.loc
      }
    case defn: DefnEntity =>
      val newDefns = newSymbols map { symbol => EntDefn(symbol.mkDefn) regularize symbol.loc }
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
    tree visit {
      case node: DeclPipeIn  => cc.ice(node, "DeclPipeIn remains after LowerPipeline")
      case node: DefnPipeIn  => cc.ice(node, "DefnPipeIn remains after LowerPipeline")
      case node: DeclPipeOut => cc.ice(node, "DeclPipeOut remains after LowerPipeline")
      case node: DefnPipeOut => cc.ice(node, "DefnPipeOut remains after LowerPipeline")
    }

    tree visitAll {
      case node if node.tpe.isPipeVar => cc.ice(node, "TypePipeVar tree remains")
      case node if node.tpe.isPipeIn  => cc.ice(node, "TypePipeIn tree remains")
      case node if node.tpe.isPipeOut => cc.ice(node, "TypePipeOut tree remains")
    }
  }

}

final class LowerPipelineHost(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  // Map from stage entity symbol the transform to apply to its Decl/Defn
  private var stageTransform: Map[Symbol, LowerPipelineStage] = _

  // Replace stage entities and instances
  override protected def replace(symbol: Symbol): Boolean = symbol.kind match {
    case _ if stageTransform == null => false
    case TypeType(_: TypeEntity)     => stageTransform contains symbol // Stage entity
    case kind: TypeEntity            => stageTransform contains kind.symbol // Stage instance
    case _                           => false
  }

  override def enter(tree: Tree): Option[Tree] = tree match {
    // Process entities which contain pipeline variables
    case host: DefnEntity if host.symbol.decl.decls exists { _.symbol.kind.isPipeVar } =>
      // Map from stage entity to next stage entity
      val nextMap: Map[Symbol, Symbol] = Map from {
        host.connects collect {
          case EntConnect(
                InstancePortSel(iSymbolL, pSymbolL),
                List(InstancePortSel(iSymbolR, pSymbolR))
              ) if pSymbolL.kind.isPipeOut =>
            assert(pSymbolR.kind.isPipeIn)
            iSymbolL.kind.asEntity.symbol -> iSymbolR.kind.asEntity.symbol
        }
      }

      // Map from stage entity to previous stage entity
      val prevMap: Map[Symbol, Symbol] = nextMap map { _.swap }

      // Sort stages in pipeline order, based on connections
      val stages = {
        @tailrec
        def lt(a: Symbol, b: Symbol): Boolean = nextMap.get(a) match {
          case Some(`b`)   => true
          case Some(other) => lt(other, b)
          case None        => false
        }
        (nextMap.keySet union prevMap.keySet).toList sortWith lt
      }

      // Map from stage entity to pipeline variables that are active in that stage.
      // A pipeline variable is considered active in a stage if either:
      // - It is used in the given stage
      // - It is used in a later stage, and is generated in an earlier stage
      //   (i.e: needs to be passed through)
      val actSetsMap: Map[Symbol, Set[Symbol]] = {
        // Collect pipeline variable symbols referenced in each stage
        val useSets = stages map { stage =>
          Set from {
            stage.defn collect { case ExprSym(symbol) if symbol.kind.isPipeVar => symbol }
          }
        }

        // Propagate uses between first and last use to create the active sets
        @tailrec
        def loop(useSets: List[Set[Symbol]], actSets: List[Set[Symbol]]): Seq[Set[Symbol]] =
          useSets match {
            case Nil             => actSets.reverse // We have built it in reverse
            case useHead :: tail =>
              // symbols active at any stage after the current stage
              val useTail = tail.foldLeft(Set.empty[Symbol])(_ union _)
              // symbols active at the previous stage
              val actPrev = actSets.headOption getOrElse Set.empty
              // symbols active at the current stage
              val actCurr = useHead union (useTail intersect actPrev)
              // Do next stage
              loop(tail, actCurr :: actSets)
          }
        Map from { stages zip loop(useSets, Nil) }
      }

      // Create the stage transforms
      stageTransform = Map from {
        stages.iterator map { stage =>
          // Figure out pipeline symbols in, active, out
          val actPrev = prevMap.get(stage) map actSetsMap getOrElse Set.empty
          val actCurr = actSetsMap(stage)
          val actNext = nextMap.get(stage) map actSetsMap getOrElse Set.empty

          val pipeInSymbols = (actPrev & actCurr).toList.sorted
          val pipeOutSymbols = (actNext & actCurr).toList.sorted
          val pipeAllSymbols = actCurr.toList.sorted

          stage -> new LowerPipelineStage(pipeInSymbols, pipeOutSymbols, pipeAllSymbols)
        }
      }

      None
    case _ => None
  }

  override def transform(tree: Tree): Tree = tree match {
    // Drop PipeVar decl/defn
    case _: DeclPipeVar => Stump
    case _: DefnPipeVar => Stump

    // Apply the stage transforms if exists
    case Decl(symbol) => orig.get(symbol) flatMap stageTransform.get map { _(tree) } getOrElse tree
    case Defn(symbol) => orig.get(symbol) flatMap stageTransform.get map { _(tree) } getOrElse tree

    //
    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    tree visit {
      case node: DeclPipeVar => cc.ice(node, "DeclPipeVar remains after LowerPipeline")
      case node: DefnPipeVar => cc.ice(node, "DefnPipeVar remains after LowerPipeline")
    }

    tree visitAll {
      case node if node.tpe.isPipeVar => cc.ice(node, "TypePipeVar tree remains")
      case node if node.tpe.isPipeIn  => cc.ice(node, "TypePipeIn tree remains")
      case node if node.tpe.isPipeOut => cc.ice(node, "TypePipeOut tree remains")
    }
  }

}

object LowerPipeline extends EntityTransformerPass(declFirst = false) {
  val name = "lower-pipeline"

  override def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer =
    new LowerPipelineHost
}
