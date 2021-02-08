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
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.Expr.InstancePortSel
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.TypeCompound
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.IteratorOps._
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

final class LowerPipelineStage(
    stageSymbol: Symbol,
    pipeInSymbols: List[Symbol], // List of incoming PipeVar symbols
    pipeOutSymbols: List[Symbol], // List of outgoing PipeVar symbols
    pipeAllSymbols: List[Symbol] // List of all active PipeVar symbols
  ) extends StatefulTreeTransformer {
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

  private def makePortType(name: String, symbols: List[Symbol]): Type = symbols match {
    case Nil => TypeVoid
    case _ =>
      val recSymbol = Symbol(name, Loc.synthetic)
      recSymbol.scopeName = stageSymbol.hierName
      val kind = TypeRecord(recSymbol, symbols map dupDropPipeVar)
      recSymbol.kind = TypeType(kind)
      kind
  }

  private lazy val iPortType = makePortType("pipeline_in", pipeInSymbols)
  private lazy val oPortType = makePortType("pipeline_out", pipeOutSymbols)

  // New symbols added to the transformed entity
  private def newSymbols: Iterator[Symbol] = pipeSymbolMap.valuesIterator concat {
    iPortType match {
      case TypeRecord(symbol, _) => Iterator.single(symbol)
      case _                     => Iterator.empty
    }
  } concat {
    oPortType match {
      case TypeRecord(symbol, _) => Iterator.single(symbol)
      case _                     => Iterator.empty
    }
  }

  override protected def replace(symbol: Symbol): Boolean = symbol.kind match {
    case TypePipeIn(_, FlowControlTypeNone) =>
      pipeInSymbols.nonEmpty // Otherwise will be removed
    case TypePipeOut(_, FlowControlTypeNone, _) =>
      pipeOutSymbols.nonEmpty // Otherwise will be removed
    case _: TypePipeIn | _: TypePipeOut => true
    case _                              => false
  }

  override protected def enter(tree: Tree): Option[Tree] = tree match {
    // Rewrite 'TypePipeIn.read();' statements to '{....} = TypeIn.read();'
    case StmtExpr(call @ ExprCall(ExprSel(ExprSym(symbol), "read"), Nil)) if symbol.kind.isPipeIn =>
      Some {
        if (symbol.kind.asPipeIn.fc == FlowControlTypeNone && iPortType == TypeVoid) {
          // This port is being removed
          Stump
        } else {
          // The rhs is the call, but with the type update, 'walk' does just
          // that because we are replacing the pipeline port symbols.
          val rhs = walk(call).asInstanceOf[Expr]
          if (iPortType == TypeVoid) {
            // The result type is void, so no assignment is required
            StmtExpr(rhs) regularize tree.loc
          } else {
            // The lhs is all local copies of the incoming pipeline variable symbols
            val lhs = ExprCat(pipeInSymbols map { symbol => ExprSym(pipeSymbolMap(symbol)) })
            // Form the assignment
            StmtAssign(lhs, rhs) regularize tree.loc
          }
        }
      }

    // Rewrite 'TypePipeOut.write();' statements to 'TypeOut.write({....});'.
    case StmtExpr(ExprCall(sel @ ExprSel(ExprSym(symbol), "write"), Nil))
        if symbol.kind.isPipeOut =>
      Some {
        if (symbol.kind.asPipeOut.fc == FlowControlTypeNone && oPortType == TypeVoid) {
          // This port is being removed
          Stump
        } else {
          // The call target is the same port, but with the type updated, so 'walk' ...
          val tgt = walk(sel).asInstanceOf[Expr]
          val args = if (oPortType.isVoid) {
            // No outgoing pipeline variables
            Nil
          } else {
            // The argument is all local copies of the outgoing pipeline variable symbols
            ArgP(ExprCat(pipeOutSymbols map { symbol => ExprSym(pipeSymbolMap(symbol)) })) :: Nil
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
      iPortType match {
        case TypeVoid =>
          if (fc == FlowControlTypeNone) {
            Stump
          } else {
            DeclIn(symbol, ExprType(TypeVoid), fc) regularize tree.loc
          }
        case TypeRecord(rSymbol, _) => DeclIn(symbol, ExprSym(rSymbol), fc) regularize tree.loc
        case _                      => unreachable
      }
    case DefnPipeIn(symbol) =>
      if (
        iPortType.isVoid && orig.getOrElse(symbol, symbol).kind.asPipeIn.fc == FlowControlTypeNone
      ) {
        Stump
      } else {
        DefnIn(symbol) regularize tree.loc
      }
    case DeclPipeOut(symbol, _, fc, st) =>
      oPortType match {
        case TypeVoid =>
          if (fc == FlowControlTypeNone) {
            Stump
          } else {
            DeclOut(symbol, ExprType(TypeVoid), fc, st) regularize tree.loc
          }
        case TypeRecord(rSymbol, _) => DeclOut(symbol, ExprSym(rSymbol), fc, st) regularize tree.loc
        case _                      => unreachable
      }
    case DefnPipeOut(symbol) =>
      if (
        oPortType.isVoid && orig.getOrElse(symbol, symbol).kind.asPipeOut.fc == FlowControlTypeNone
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
  private var stageTransform: Map[Symbol, LowerPipelineStage] = _

  // Replace stage entities and instances
  override protected def replace(symbol: Symbol): Boolean = symbol.kind match {
    case _ if stageTransform == null => false
    case TypeType(_: TypeEntity)     => stageTransform contains symbol // Stage entity
    case kind: TypeEntity            => stageTransform contains kind.symbol // Stage instance
    case _                           => false
  }

  private def isPipelineHost(defn: DefnEntity): Boolean = defn.defns exists {
    case _: DefnPipeVar => true // Has a pipeline variable
    case defn: DefnEntity => // Has an entity with a pipeline port
      defn.defns exists {
        case _: DefnPipeIn  => true
        case _: DefnPipeOut => true
        case _              => false
      }
    case _ => false
  }

  override def enter(tree: Tree): Option[Tree] = tree match {
    // Process entities which contain pipeline variables or pipeline stages
    case host: DefnEntity if isPipelineHost(host) =>
      // Map from stage entity to next stage entity
      val nextMap: Map[Symbol, Symbol] = Map from {
        host.assigns collect {
          case EntAssign(
                InstancePortSel(iSymbolR, pSymbolR),
                InstancePortSel(iSymbolL, pSymbolL)
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
            stage.defn flatCollect {
              case ExprSym(symbol) =>
                Iterator.when(symbol.kind.isPipeVar) thenSingle { symbol }
              case ExprSel(ExprSym(symbol), selector) =>
                if (symbol.kind.isPipeVar) {
                  Iterator.single(symbol)
                } else {
                  val selected = symbol.kind pipe {
                    case TypePipeIn(pipelineVariables, _)     => pipelineVariables
                    case TypePipeOut(pipelineVariables, _, _) => pipelineVariables
                    case _                                    => Nil
                  } pipe {
                    _.find(_.name == selector)
                  }
                  Iterator.when(selected.exists(_.kind.isPipeVar)) thenIterator {
                    selected.iterator
                  }
                }
            }
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

          stage -> new LowerPipelineStage(stage, pipeInSymbols, pipeOutSymbols, pipeAllSymbols)
        }
      }

      None

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
    case Decl(symbol) => orig.get(symbol) flatMap stageTransform.get map { _(tree) } getOrElse tree
    case Defn(symbol) => orig.get(symbol) flatMap stageTransform.get map { _(tree) } getOrElse tree

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

object LowerPipeline extends EntityTransformerPass(declFirst = false, parallel = true) {
  val name = "lower-pipeline"

  def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer = new LowerPipelineHost
}
