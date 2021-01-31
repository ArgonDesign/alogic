////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Unpicked entity details used in the backed
////////////////////////////////////////////////////////////////////////////////
package com.argondesign.alogic.backend

import com.argondesign.alogic.analysis.WrittenSymbols
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.Expr.InstancePortSel
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.enums.EntityVariant

import scala.util.chaining.scalaUtilChainingOps

final class EntityDetails(val decl: DeclEntity, val defn: DefnEntity) {

  assert {
    decl.decls forall {
      _.symbol.kind match {
        case _: TypeInt      => true
        case _: TypeEntity   => true
        case _: TypeIn       => true
        case _: TypeOut      => true
        case _: TypeConst    => true
        case _: TypeArray    => true
        case _: TypeXenoFunc => true
        case _               => false
      }
    }
  }

  val clockedProcesses: List[EntClockedProcess] = defn.clockedProcesses

  lazy val isVerbatim: Boolean = defn.variant == EntityVariant.Ver

  lazy val hasConsts: Boolean = constants.nonEmpty

  lazy val hasFlops: Boolean = decl.decls exists { _.symbol.attr.flop.isSet }

  lazy val hasCombSignals: Boolean = decl.decls exists { _.symbol.attr.combSignal.isSet }

  lazy val hasArrays: Boolean = decl.decls exists { _.symbol.attr.memory.isSet }

  lazy val hasInterconnect: Boolean = decl.decls exists { _.symbol.attr.interconnect.isSet }

  lazy val hasXenoFuncs: Boolean = xenoFuncs.nonEmpty

  lazy val hasInstances: Boolean = decl.instances.nonEmpty

  // Any symbol that is driven by a connect must be a net
  lazy val netSymbols: List[Symbol] = defn.assigns flatMap {
    case EntAssign(lhs, _) => lhs collect { case ExprSym(symbol) => symbol }
  }

  // Group and sort interconnect symbols by instance, then by port declaration order
  lazy val groupedInterconnectSymbols: List[(Symbol, List[Symbol])] = {
    // Calculate (instance symbol, port name, interconnect symbol) triplets
    val trip = decl.decls map {
      _.symbol
    } filter {
      _.attr.interconnect.isSet
    } flatMap { nSymbol =>
      defn.assigns collectFirst {
        case EntAssign(lhs, ExprSel(ExprSym(iSymbol), sel))
            if WrittenSymbols(lhs) contains nSymbol =>
          (iSymbol, sel, nSymbol)
        case EntAssign(ExprSel(ExprSym(iSymbol), sel), rhs)
            if rhs.isLValueExpr && (WrittenSymbols(rhs) contains nSymbol) =>
          (iSymbol, sel, nSymbol)
      }
    }

    // Group by instance, loose instance symbol from values
    val groups = trip.groupMap(_._1) { case (_, s, n) => (s, n) }

    // Sort by groups by instance order
    val sortedInstances = {
      // Sorting map for instance symbols
      val ordering = Map from {
        for {
          (decl, i) <- decl.instances.iterator.zipWithIndex
        } yield {
          decl.symbol -> i
        }
      }
      // Sort by instance
      groups.toList sortBy { case (i, _) => ordering(i) }
    }

    // Sort within group by port definition order
    sortedInstances map {
      case (iSymbol, list) =>
        // Sorting map for port selectors
        val ordering = Map from {
          for {
            (symbol, i) <- iSymbol.kind.asEntity.publicSymbols.iterator.zipWithIndex
          } yield {
            symbol.name -> i
          }
        }
        // Sort by port selector, then loose them, note that some interconnect
        // symbols can remain as placeholders in concatenations while their
        // corresponding ports have ben removed. We put these at the end sorted
        // lexically.
        val sortedSymbols = list sortWith {
          case ((a, _), (b, _)) =>
            (ordering.get(a), ordering.get(b)) match {
              case (Some(oa), Some(ob)) => oa < ob
              case (Some(_), None)      => true
              case (None, Some(_))      => false
              case (None, None)         => a < b
            }
        } map { _._2 }
        (iSymbol, sortedSymbols)
    }
  }

  // Function from 'instance symbol => port selector => connected expression'
  lazy val instancePortExpr: Map[Symbol, Map[String, Expr]] = {
    val trip = defn.assigns collect {
      case EntAssign(lhs, InstancePortSel(iSymbol, pSymbol)) =>
        (iSymbol, pSymbol.name, lhs)
      case EntAssign(InstancePortSel(iSymbol, pSymbol), rhs) =>
        (iSymbol, pSymbol.name, rhs)
    }

    val grouped = trip.groupMap(_._1)({ case (_, s, e) => (s, e) })

    Map from {
      grouped.view mapValues { pairs =>
        pairs.toMap ensuring { _.size == pairs.length }
      }
    }
  }

  // Connects that are not of the form 'a.b -> SOMETHING' or 'SOMETHING -> a.b'
  // where a is an instance
  lazy val nonPortAssigns: List[EntAssign] = defn.assigns filter {
    case EntAssign(_, InstancePortSel(_, _)) => false
    case EntAssign(InstancePortSel(_, _), _) => false
    case _                                   => true
  }

  // Constants referenced by this entity
  lazy val constants: List[Symbol] = List from {
    decl.decls collect {
      case DeclConst(symbol, _) => symbol
    } concat {
      defn collect {
        case ExprSym(symbol) if symbol.kind.isConst => symbol
      }
    }
  } pipe { _.distinct.sortBy(_.loc) }

  // Foreign functions referenced by this entity
  lazy val xenoFuncs: List[Symbol] = {
    val list = List from {
      defn collect {
        case ExprSym(symbol) if symbol.kind.isXenoFunc => symbol
      }
    }
    list.distinct
  }

}
