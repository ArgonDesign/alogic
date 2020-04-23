////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Lift srams to outer entities and wire them through ports.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees.Expr.InstancePortRef
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.StorageTypes.StorageTypeWire
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types.TypeEntity
import com.argondesign.alogic.core.Types.TypeIn
import com.argondesign.alogic.core.Types.TypeOut
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.mutable

final class LiftSramsFrom(
    replacements: mutable.Map[Symbol, Symbol],
    liftFromMap: Map[Symbol, List[Symbol]]
  )(
    implicit
    cc: CompilerContext)
    extends StatefulTreeTransformer {

  private val fcn = FlowControlTypeNone
  private val stw = StorageTypeWire

  // Map from 'instance.port' -> 'lifted port'
  private val portMap = mutable.LinkedHashMap[(Symbol, Symbol), Symbol]()

  override def replace(symbol: Symbol): Boolean = {
    enclosingSymbols.isEmpty && (liftFromMap contains symbol)
  }

  override def skip(tree: Tree): Boolean = tree match {
    case DeclEntity(symbol, _)    => !(liftFromMap contains symbol)
    case DefnEntity(symbol, _, _) => !(liftFromMap contains symbol)
    case _                        => false
  }

  override protected def enter(tree: Tree): Option[Tree] = {
    tree match {
      case DeclEntity(symbol, _) => orig.get(symbol) foreach { replacements(_) = symbol }
      case _                     =>
    }
    None
  }

  override protected def transform(tree: Tree): Tree = tree match {

    ////////////////////////////////////////////////////////////////////////////
    // Replace lifted instances with wired-through ports
    ////////////////////////////////////////////////////////////////////////////

    case DeclInstance(iSymbol, _) if liftFromMap(orig(entitySymbol)) contains iSymbol =>
      // For each port of the instances being lifted, create a new port
      val newSymbols = for {
        pSymbol <- iSymbol.kind.asEntity.publicSymbols
      } yield {
        val name = iSymbol.name + cc.sep + pSymbol.name
        val loc = iSymbol.loc
        val nSymbol = pSymbol.kind match {
          case TypeIn(kind, `fcn`) =>
            cc.newSymbol(name, loc) tap { _.kind = TypeOut(kind, fcn, stw) }
          case TypeOut(kind, `fcn`, `stw`) =>
            cc.newSymbol(name, loc) tap { _.kind = TypeIn(kind, fcn) }
          case _ => unreachable
        }
        // Propagate interconnectClearOnStall to lifted node
        for {
          icos <- entitySymbol.attr.interconnectClearOnStall.get
          if icos contains ((iSymbol, pSymbol.name))
        } {
          nSymbol.attr.clearOnStall set true
        }
        portMap((iSymbol, pSymbol)) = nSymbol
        nSymbol
      }
      Thicket(newSymbols map { _.mkDecl regularize tree.loc })

    case DefnInstance(iSymbol) if liftFromMap(orig(entitySymbol)) contains iSymbol =>
      val newSymbols = portMap.iterator collect {
        case ((`iSymbol`, _), nSymbol) => nSymbol
      }
      Thicket(List.from(newSymbols map { _.mkDefn regularize tree.loc }))

    ////////////////////////////////////////////////////////////////////////////
    // Rewrite references to instance.port as references to the new port
    ////////////////////////////////////////////////////////////////////////////

    case InstancePortRef(iSymbol, pSymbol) =>
      portMap.get((iSymbol, pSymbol)) map { nSymbol =>
        ExprSym(nSymbol) regularize tree.loc
      } getOrElse {
        tree
      }

    //
    case _ => tree
  }

}

final class LiftSramsTo(
    replacements: collection.Map[Symbol, Symbol],
    liftFromMap: Map[Symbol, List[Symbol]]
  )(
    implicit
    cc: CompilerContext)
    extends StatefulTreeTransformer {

  private def portRef(iSymbol: Symbol, sel: String) = ExprSelect(ExprSym(iSymbol), sel, Nil)

  override def replace(symbol: Symbol): Boolean = symbol.kind match {
    case TypeEntity(symbol, _) => liftFromMap contains symbol
    case _                     => false
  }

  var newInstances: List[(Symbol, Iterator[EntConnect])] = _

  override protected def transform(tree: Tree): Tree = tree match {
    // Update instances
    case decl @ DeclInstance(_, ExprSym(eSymbol)) =>
      replacements.get(eSymbol) map { nSymbol =>
        decl.copy(spec = ExprSym(nSymbol)) regularize tree.loc
      } getOrElse tree

    case decl: DeclEntity =>
      // For each instance that we lifted something out from,
      // create the lifted instances and connect them up
      newInstances = for {
        DeclInstance(iSymbol, _) <- decl.instances
        oSymbol <- orig.get(iSymbol).iterator
        eSymbol = oSymbol.kind.asEntity.symbol
        lSymbols <- liftFromMap.get(eSymbol).iterator
        lSymbol <- lSymbols
      } yield {
        val prefix = "sram" + cc.sep
        assert(lSymbol.name startsWith prefix)
        val name = prefix + iSymbol.name + cc.sep + lSymbol.name.drop(prefix.length)
        val lKind = lSymbol.kind.asEntity
        // Create the local instance
        val nSymbol = cc.newSymbol(name, iSymbol.loc) tap { _.kind = lKind }
        // Create the connections
        val connects = for (pSymbol <- lKind.publicSymbols.iterator) yield {
          val iPortName = lSymbol.name + cc.sep + pSymbol.name
          val connect = pSymbol.kind match {
            case _: TypeIn =>
              EntConnect(portRef(iSymbol, iPortName), List(portRef(nSymbol, pSymbol.name)))
            case _: TypeOut =>
              EntConnect(portRef(nSymbol, pSymbol.name), List(portRef(iSymbol, iPortName)))
            case _ => unreachable
          }
          (connect regularize lSymbol.loc).asInstanceOf[EntConnect]
        }
        nSymbol -> connects
      }

      // Add declarations of lifted instances
      val newDecls = newInstances.iterator map {
        case (nSymbol, _) => nSymbol.mkDecl regularize nSymbol.loc
      }
      TypeAssigner(decl.copy(decls = decl.decls ++ newDecls) withLoc decl.loc)

    case defn: DefnEntity if newInstances.nonEmpty =>
      // Add definitions of lifted instances and connects
      val extraBody = newInstances.iterator flatMap {
        case (nSymbol, connects) =>
          Iterator.single(EntDefn(nSymbol.mkDefn) regularize nSymbol.loc) ++ connects
      }
      TypeAssigner(defn.copy(body = defn.body ++ extraBody) withLoc defn.loc)

    //
    case _ => tree
  }

}

object LiftSrams extends PairsTransformerPass {
  val name = "lift-srams"

  def process(
      pairs: List[(Decl, Defn)]
    )(
      implicit
      cc: CompilerContext
    ): List[(Decl, Defn)] = {

    // Lift entities by 1 level in each iteration
    @tailrec
    def loop(pairs: List[(Decl, Defn)], liftFromSet: Set[Symbol]): List[(Decl, Defn)] = {
      // Gather the 'parent entity' -> 'List(instances to be lifted)' map
      val liftFromMap: Map[Symbol, List[Symbol]] = Map from {
        for {
          (decl, _) <- pairs.iterator
          eSymbol = decl.symbol
          if liftFromSet contains eSymbol
          desc = decl.asInstanceOf[DeclEntity]
          iSymbols: List[Symbol] = for {
            instance <- desc.instances
            if instance.symbol.kind.asEntity.symbol.attr.sram contains true
          } yield {
            instance.symbol
          }
          if iSymbols.nonEmpty
        } yield {
          eSymbol -> iSymbols
        }
      }

      if (liftFromMap.isEmpty) {
        // If no more instances to be lifted, we are done
        pairs
      } else {
        val replacements = TrieMap[Symbol, Symbol]()

        // Apply the liftFrom transform
        val liftedFrom = pairs map {
          case (decl, defn) =>
            val transform = new LiftSramsFrom(replacements, liftFromMap)
            val newDecl = decl rewrite transform
            val newDefn = defn rewrite transform
            (newDecl, newDefn)
        }

        // Apply the liftTo transform
        val liftedTo = liftedFrom map {
          case (decl, defn) =>
            val transform = new LiftSramsTo(replacements, liftFromMap)
            val newDecl = decl rewrite transform
            val newDefn = defn rewrite transform
            (newDecl, newDefn)
        }

        // Go again, with a remapped the liftFromSet
        loop(
          liftedTo,
          liftFromSet map { symbol =>
            replacements.getOrElse(symbol, symbol)
          }
        )
      }
    }

    // TODO: We just accidentally lost all non-entitities here
    val eSymbols = pairs collect { case (DeclEntity(symbol, _), _) => symbol }

    // Gather entities with the liftSrams attribute,
    // and all other entities instantiated by them
    val liftFromSet: Set[Symbol] = {
      val seedSet = Set from { eSymbols filter { _.attr.liftSrams contains true } }

      // Expand entity set by adding all instantiated entities
      @tailrec
      def expand(add: Set[Symbol], acc: Set[Symbol]): Set[Symbol] = {
        if (add.isEmpty) {
          acc
        } else {
          val extra = add flatMap {
            _.decl.asInstanceOf[DeclEntity].instances map { _.symbol.kind.asEntity.symbol }
          } diff add diff acc
          expand(extra, acc union add)
        }
      }

      expand(seedSet, Set())
    }

    loop(
      eSymbols map { s =>
        (s.decl, s.defn)
      },
      liftFromSet
    )
  }

}
