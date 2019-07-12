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
// Lift srams to outer entities and wire them through ports.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees.Expr.InstancePortRef
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.StorageTypes.StorageTypeWire
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.core.Symbols.TypeSymbol
import com.argondesign.alogic.core.Types.TypeEntity
import com.argondesign.alogic.core.Types.TypeIn
import com.argondesign.alogic.core.Types.TypeInstance
import com.argondesign.alogic.core.Types.TypeOut
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._
import scala.language.postfixOps

final class LiftSramsFrom(
    liftFromMap: Map[TypeSymbol, List[TermSymbol]]
)(implicit cc: CompilerContext)
    extends TreeTransformer {

  private val fcn = FlowControlTypeNone
  private val stw = StorageTypeWire

  // Map from 'instance.port' -> 'lifted port'
  private val portMap = mutable.LinkedHashMap[(TermSymbol, TermSymbol), TermSymbol]()

  override def skip(tree: Tree): Boolean = tree match {
    case _: Entity => !(liftFromMap contains entitySymbol)
    case _         => false
  }

  override protected def enter(tree: Tree): Unit = tree match {
    case entity: Entity => {
      val ourList = liftFromMap(entitySymbol)

      // For each port of the instances being lifted,
      // create a new port on the current entity
      for {
        EntInstance(Sym(iSymbol: TermSymbol), _, _, _) <- entity.instances
        if ourList contains iSymbol
        pSymbol <- iSymbol.kind.asInstanceOf[TypeInstance].portSymbols
      } yield {
        val name = iSymbol.name + cc.sep + pSymbol.name
        val loc = iSymbol.loc
        val nSymbol = pSymbol.kind match {
          case TypeIn(kind, `fcn`)         => cc.newTermSymbol(name, loc, TypeOut(kind, fcn, stw))
          case TypeOut(kind, `fcn`, `stw`) => cc.newTermSymbol(name, loc, TypeIn(kind, fcn))
          case _                           => unreachable
        }
        // Propagate interconnectClearOnStall to lifted node
        for {
          icos <- entity.symbol.attr.interconnectClearOnStall.get
          if icos contains ((iSymbol, pSymbol.name))
        } {
          nSymbol.attr.clearOnStall set true
        }
        portMap((iSymbol, pSymbol)) = nSymbol
      }
    }

    case _ => ()
  }

  override protected def transform(tree: Tree): Tree = tree match {

    ////////////////////////////////////////////////////////////////////////////
    // Rewrite references to instance.port as references to the new port
    ////////////////////////////////////////////////////////////////////////////

    case InstancePortRef(iSymbol, pSymbol) => {
      portMap.get((iSymbol, pSymbol)) map { nSymbol =>
        ExprRef(nSymbol) regularize tree.loc
      } getOrElse {
        tree
      }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Drop the instances, add the new ports
    ////////////////////////////////////////////////////////////////////////////

    case entity: Entity => {
      val ourList = liftFromMap(entitySymbol)

      val newBody = List from {
        {
          // Add new declarations
          portMap.valuesIterator map { symbol =>
            EntDecl(Decl(symbol, None)) regularize symbol.loc
          }
        } concat {
          // Loose lifted instances
          entity.body.iterator filterNot {
            case EntInstance(Sym(iSymbol: TermSymbol), _, _, _) => ourList contains iSymbol
            case _                                              => false
          }
        }
      }

      // Update type of entity for new ports
      val portSymbols = newBody collect {
        case EntDecl(Decl(symbol, _)) if symbol.kind.isInstanceOf[TypeIn]  => symbol
        case EntDecl(Decl(symbol, _)) if symbol.kind.isInstanceOf[TypeOut] => symbol
      }
      entitySymbol.kind = entitySymbol.kind.asInstanceOf[TypeEntity].copy(portSymbols = portSymbols)

      TypeAssigner(entity.copy(body = newBody) withLoc tree.loc)
    }

    case _ => tree
  }

}

final class LiftSramsTo(
    liftFromMap: Map[TypeSymbol, List[TermSymbol]]
)(implicit cc: CompilerContext)
    extends TreeTransformer {

  override protected def skip(tree: Tree): Boolean = tree match {
    case _: Entity => false
    case _         => true
  }

  private def portRef(iSymbol: TermSymbol, sel: String) = ExprSelect(ExprRef(iSymbol), sel)

  override protected def transform(tree: Tree): Tree = tree match {
    case entity: Entity => {
      // For each instance that we lifted something out from,
      // create the lifted instances and connect them up
      val (newInstances, newConnects) = {
        val items = for {
          EntInstance(Sym(iSymbol: TermSymbol), Sym(eSymbol: TypeSymbol), _, _) <- entity.instances
          lSymbols <- (liftFromMap get eSymbol).toList
          lSymbol <- lSymbols
        } yield {
          val prefix = "sram" + cc.sep
          assert(lSymbol.name startsWith prefix)
          val name = prefix + iSymbol.name + cc.sep + lSymbol.name.drop(prefix.length)
          val lKind = lSymbol.kind.asInstanceOf[TypeInstance]
          // Create the local instance
          val nSymbol = cc.newTermSymbol(name, iSymbol.loc, lKind)
          val instance = {
            EntInstance(Sym(nSymbol), Sym(lKind.entitySymbol), Nil, Nil) regularize lSymbol.loc
          }
          // Create the connections
          val connects = for (pSymbol <- lKind.portSymbols.iterator) yield {
            val iPortName = lSymbol.name + cc.sep + pSymbol.name
            val connect = pSymbol.kind match {
              case _: TypeIn => {
                EntConnect(portRef(iSymbol, iPortName), List(portRef(nSymbol, pSymbol.name)))
              }
              case _: TypeOut => {
                EntConnect(portRef(nSymbol, pSymbol.name), List(portRef(iSymbol, iPortName)))
              }
              case _ => unreachable
            }
            connect regularize lSymbol.loc
          }
          (instance, connects)
        }
        items.unzip
      }

      if (newInstances.isEmpty) {
        tree
      } else {
        TypeAssigner {
          entity.copy(body = entity.body ::: newInstances ::: newConnects.flatten) withLoc tree.loc
        }
      }
    }

    case _ => unreachable
  }
}

object LiftSrams extends Pass {
  val name = "lift-srams"

  def apply(trees: List[Tree])(implicit cc: CompilerContext): List[Tree] = {

    val entities = trees map {
      case entity: Entity => entity
      case _              => unreachable
    }

    // Build an eSymbol -> entity map
    val eMap = Map from {
      entities map { entity =>
        entity.symbol -> entity
      }
    }

    // Gather entities with the liftSrams attribute,
    // and all other entities instantiated by them
    val liftFromSet = {
      val seed = Set from {
        entities filter { entity =>
          entity.symbol.attr.liftSrams contains true
        }
      }

      // Expand entity set by adding all instantiated entities
      @tailrec
      def expand(curr: Set[Entity], prev: Set[Entity]): Set[Entity] = {
        if (curr == prev) {
          curr
        } else {
          val extra = for {
            entity <- curr
            EntInstance(_, Sym(eSymbol: TypeSymbol), _, _) <- entity.instances
          } yield {
            eMap(eSymbol)
          }
          expand(curr union extra, curr)
        }
      }

      expand(seed, Set()) map { entity =>
        entity.symbol
      }
    }

    // Lift entities by 1 level in each iteration
    @tailrec
    def loop(entities: List[Entity]): List[Entity] = {
      // Gather the 'parent entity' -> 'List(lifted instance)' map
      val liftFromMap = {
        val pairs = for {
          entity <- entities
          if liftFromSet contains entity.symbol
        } yield {
          val iSymbols = for {
            EntInstance(Sym(iSymbol: TermSymbol), Sym(sSymbol: TypeSymbol), _, _) <- entity.instances
            if sSymbol.attr.sram contains true
          } yield {
            iSymbol
          }
          entity.symbol -> iSymbols
        }
        pairs filter { _._2.nonEmpty } toMap
      }

      if (liftFromMap.isEmpty) {
        entities
      } else {
        // Apply the liftFrom transform
        val liftedFrom = entities.par map { entity =>
          (entity rewrite new LiftSramsFrom(liftFromMap)).asInstanceOf[Entity]
        }

        // Apply the liftTo transform
        val liftedTo = liftedFrom map { entity =>
          (entity rewrite new LiftSramsTo(liftFromMap)).asInstanceOf[Entity]
        } seq

        // Repeat until settles
        loop(liftedTo.toList)
      }
    }

    loop(entities)
  }
}
