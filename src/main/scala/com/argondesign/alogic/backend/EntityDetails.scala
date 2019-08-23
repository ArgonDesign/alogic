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
// Unpicked entity details used in the backed
////////////////////////////////////////////////////////////////////////////////
package com.argondesign.alogic.backend

import com.argondesign.alogic.ast.Trees.Expr.InstancePortRef
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable

final class EntityDetails(val entity: Entity, details: => Map[TypeSymbol, EntityDetails])(
    implicit cc: CompilerContext) {

  assert {
    entity.declarations forall {
      case Decl(symbol, _) => {
        symbol.kind match {
          case _: TypeConst => true
          case _: TypeIn    => true
          case _: TypeOut   => true
          case _: TypeArray => true
          case _: TypeInt   => true
          case _            => false
        }
      }
      case _ => unreachable
    }
  }

  val resetFlops: List[Declaration] = entity.declarations filter {
    case Decl(symbol, Some(_)) => symbol.attr.flop.isSet
    case Decl(symbol, None)    => symbol.attr.flop.isSet && cc.settings.resetAll
    case _                     => unreachable
  }

  val unresetFlops: List[Declaration] = entity.declarations filter {
    case Decl(symbol, None) => symbol.attr.flop.isSet && !cc.settings.resetAll
    case _                  => false
  }

  lazy val isVerbatim: Boolean = entity.symbol.attr.variant.value == "verbatim"

  lazy val hasConsts: Boolean = entity.declarations exists {
    case Decl(symbol, _) => symbol.kind.isInstanceOf[TypeConst]
    case _               => false
  }

  lazy val hasFlops: Boolean = resetFlops.nonEmpty || unresetFlops.nonEmpty

  lazy val hasCombSignals: Boolean = entity.declarations exists {
    case Decl(symbol, _) => symbol.attr.combSignal.isSet
    case _               => false
  }

  lazy val hasArrays: Boolean = entity.declarations exists {
    case Decl(symbol, _) => symbol.attr.memory.isSet
    case _               => false
  }

  lazy val hasInterconnect: Boolean = entity.declarations exists {
    case Decl(symbol, _) => symbol.attr.interconnect.isSet
    case _               => false
  }

  lazy val hasInstances: Boolean = entity.instances.nonEmpty

  lazy val canStall: Boolean = entity.preOrderIterator exists {
    case _: StmtStall => true
    case _            => false
  }

  lazy val needsClock: Boolean = isVerbatim || hasFlops || hasArrays || {
    entity.instances exists {
      case EntInstance(_, Sym(symbol: TypeSymbol, _), _, _) => details(symbol).needsClock
      case _                                                => unreachable
    }
  }

  lazy val needsReset: Boolean = isVerbatim || resetFlops.nonEmpty || {
    entity.instances exists {
      case EntInstance(_, Sym(symbol: TypeSymbol, _), _, _) => details(symbol).needsReset
      case _                                                => unreachable
    }
  }

  // Any symbol that is driven by a connect must be a net
  lazy val netSymbols: List[TermSymbol] = entity.connects flatMap {
    case EntConnect(_, rhs :: Nil) => {
      rhs collect {
        case ExprSym(symbol: TermSymbol) => symbol
      }
    }
    case _ => Nil
  }

  // Group and sort interconnect symbols by instance, then by port declaration order
  lazy val groupedInterconnectSymbols: List[(TermSymbol, List[TermSymbol])] = {
    // Calculate (instance symbol, port name, interconnect symbol) triplets
    val trip = for {
      Decl(nSymbol, _) <- entity.declarations
      (iSymbol, sel) <- nSymbol.attr.interconnect.get
    } yield {
      (iSymbol, sel, nSymbol)
    }

    // Group by instance, loose instance symbol from values
    val groups = trip.groupMap { _._1 } { case (_, s, n) => (s, n) }

    // Sort by groups by instance order
    val sortedInstances = {
      // Sorting map for instance symbols
      val ordering = {
        val pairs = for {
          (EntInstance(Sym(symbol: TermSymbol, _), _, _, _), i) <- entity.instances.zipWithIndex
        } yield {
          symbol -> i
        }
        pairs.toMap
      }
      // Sort by instance
      groups.toList sortBy { case (i, _) => ordering(i) }
    }

    // Sort within group by port definition order
    sortedInstances map {
      case (iSymbol, list) =>
        // Sorting map for port selectors
        val ordering = {
          val pairs = for {
            (symbol, i) <- iSymbol.kind.asInstanceOf[TypeInstance].portSymbols.zipWithIndex
          } yield {
            symbol.name -> i
          }
          pairs.toMap
        }
        // Sort by port selector, then loose them, note that some interconnect
        // symbols can remain as placeholders in concatenations while their
        // corresponding ports have ben removed. We put these at the end sorted
        // lexically.
        val sortedSymbols = list sortWith {
          case ((a, _), (b, _)) => {
            (ordering.get(a), ordering.get(b)) match {
              case (Some(oa), Some(ob)) => oa < ob
              case (Some(_), None)      => true
              case (None, Some(_))      => false
              case (None, None)         => a < b
            }
          }
        } map { _._2 }
        (iSymbol, sortedSymbols)
    }
  }

  // Function from 'instance symbol => port selector => connected expression'
  lazy val instancePortExpr: Map[TermSymbol, Map[String, Expr]] = {
    val trip = entity.connects collect {
      case EntConnect(InstancePortRef(iSymbol, pSymbol), rhs :: Nil) => {
        (iSymbol, pSymbol.name, rhs)
      }
      case EntConnect(lhs, List(InstancePortRef(iSymbol, pSymbol))) => {
        (iSymbol, pSymbol.name, lhs)
      }
    }

    val grouped = trip.groupMap({ _._1 })({ case (_, s, e) => (s, e) })

    (grouped.view mapValues { pairs =>
      pairs.toMap ensuring { _.size == pairs.length }
    }).toMap
  }

  // Connects that are not of the form 'a.b -> SOMETHING' or 'SOMETHING -> a.b'
  // where a is an instance
  lazy val nonPortConnects: List[EntConnect] = entity.connects filter {
    case EntConnect(InstancePortRef(_, _), _)       => false
    case EntConnect(_, List(InstancePortRef(_, _))) => false
    case _                                          => true
  }

}
