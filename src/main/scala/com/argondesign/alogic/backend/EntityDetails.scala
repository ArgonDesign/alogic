////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Unpicked entity details used in the backed
////////////////////////////////////////////////////////////////////////////////
package com.argondesign.alogic.backend

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.Expr.InstancePortSel
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.enums.EntityVariant
import com.argondesign.alogic.core.FuncVariant

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

  // Any symbol that is driven by a connect must be a net
  lazy val netSymbols: List[Symbol] = defn.assigns flatMap {
    case EntAssign(lhs, _) => lhs collect { case ExprSym(symbol) => symbol }
  }

  // Group and sort interconnect symbols by instance, then by port declaration order
  lazy val groupedInterconnectSymbols: List[(Symbol, List[Symbol])] =
    decl.decls
      .flatMap { decl => // Calculate (instance symbol, port symbol, interconnect symbol) triplets
        decl.symbol.attr.interconnect.get.map {
          case (iSymbol, pSymbol) => (iSymbol, pSymbol, decl.symbol)
        }
      }
      .groupBy(_._1) // Group by instance
      .view
      .mapValues(_.sortBy(_._2).map(_._3)) // Sort within groups by port, keep interconnect
      .toList
      .sortBy(_._1) // Sort groups by instance

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

  // Constants defined in this entity
  lazy val constants: List[Symbol] =
    List
      .from(decl.decls.collect { case DeclConst(symbol, _) => symbol })
      .distinct
      .sortBy(_.loc)

  // Foreign functions defined in this entity
  lazy val xenoFuncs: List[Symbol] =
    List
      .from(decl.decls.collect { case DeclFunc(symbol, FuncVariant.Xeno, _, _) => symbol })
      .distinct
      .sortBy(_.loc)

}
