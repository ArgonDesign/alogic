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

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.Expr.InstancePortRef
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable

final class EntityDetails(val entity: Entity, details: => Map[TypeSymbol, EntityDetails]) {

  val Entity(
    Sym(eSymbol: TypeSymbol),
    decls,
    instances,
    connects,
    Nil,
    states,
    fenceStmts,
    Nil,
    verbatim
  ) = entity

  assert {
    decls forall {
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

  assert(states.nonEmpty || fenceStmts.isEmpty)

  lazy val isVerbatim: Boolean = entity.variant == "verbatim"

  lazy val hasConsts: Boolean = decls exists {
    case Decl(symbol, _) => symbol.kind.isInstanceOf[TypeConst]
    case _               => false
  }

  lazy val hasFlops: Boolean = decls exists {
    case Decl(symbol, _) => symbol.attr.flop.isSet
    case _               => false
  }

  lazy val hasCombSignals: Boolean = decls exists {
    case Decl(symbol, _) => symbol.attr.combSignal.isSet
    case _               => false
  }

  lazy val hasArrays: Boolean = decls exists {
    case Decl(symbol, _) => symbol.attr.memory.isSet
    case _               => false
  }

  lazy val hasInterconnect: Boolean = decls exists {
    case Decl(symbol, _) => symbol.attr.interconnect.isSet
    case _               => false
  }

  lazy val hasInstances: Boolean = instances.nonEmpty

  lazy val canStall: Boolean = entity.preOrderIterator exists {
    case _: StmtStall => true
    case _            => false
  }

  lazy val needsClock: Boolean = isVerbatim || hasFlops || hasArrays || {
    instances exists {
      case Instance(_, Sym(symbol: TypeSymbol), _, _) => details(symbol).needsClock
      case _                                          => unreachable
    }
  }

  // Any symbol that is driven by a connect must be a net
  lazy val netSymbols: List[TermSymbol] = connects flatMap {
    case Connect(_, rhs :: Nil) => {
      rhs collect {
        case ExprRef(symbol: TermSymbol) => symbol
      }
    }
    case _ => Nil
  }

  // Group and sort interconnect symbols by instance, then by port declaration order
  lazy val groupedInterconnectSymbols: List[(TermSymbol, List[TermSymbol])] = {
    // Calculate (instance symbol, port name, interconnect symbol) triplets
    val trip = for {
      Decl(nSymbol, _) <- decls
      (iSymbol, sel) <- nSymbol.attr.interconnect.get
    } yield {
      (iSymbol, sel, nSymbol)
    }

    // Group by instance, loose instance symbol from values
    val groups = trip groupBy { _._1 } mapValues { _ map { case (_, s, n) => (s, n) } }

    // Sort by groups by instance order
    val sortedInstances = {
      // Sorting map for instance symbols
      val ordering = {
        val pairs = for {
          (Instance(Sym(symbol: TermSymbol), _, _, _), i) <- instances.zipWithIndex
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
    val trip = connects collect {
      case Connect(InstancePortRef(iSymbol, pSymbol), rhs :: Nil) => {
        (iSymbol, pSymbol.name, rhs)
      }
      case Connect(lhs, List(InstancePortRef(iSymbol, pSymbol))) => {
        (iSymbol, pSymbol.name, lhs)
      }
    }

    val groupped = trip groupBy { _._1 } mapValues { _ map { case (_, s, e) => (s, e) } }

    groupped mapValues { pairs =>
      pairs.toMap ensuring { _.size == pairs.length }
    }
  }

  // Connects that are not of the form 'a.b -> SOMETHING' or 'SOMETHING -> a.b'
  // where a is an instance
  lazy val nonPortConnects: List[Connect] = connects filter {
    case Connect(InstancePortRef(_, _), _)       => false
    case Connect(_, List(InstancePortRef(_, _))) => false
    case _                                       => true
  }

}
