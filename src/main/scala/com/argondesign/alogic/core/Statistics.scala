////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Facility for recording design statistics
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.lib.Json

import java.io.PrintWriter
import scala.collection.concurrent.TrieMap
import scala.collection.immutable.ListMap

final class Statistics {

  // (symbol,  statName) -> statValue
  private val stats: TrieMap[(Symbol, String), Any] = TrieMap.empty

  def set(symbol: Symbol, statName: String, statValue: Any): Unit =
    stats.putIfAbsent((symbol, statName), statValue) match {
      case None    => // OK
      case Some(_) => throw Ice(s"recording statistic ($symbol, $statName) multiple times")
    }

  def write(writer: PrintWriter): Unit = {
    val json = ListMap.from {
      stats
        .groupBy(_._1._1.loc.file)
        .toSeq
        .sortBy(_._1)
        .map {
          case (fileName, dict) =>
            fileName -> ListMap.from {
              dict
                .groupMap(_._1._1) { case ((_, name), value) => (name, value) }
                .toSeq
                .sortBy(_._1.loc)
                .map {
                  case (symbol, dict) =>
                    symbol.hierName -> ListMap.from {
                      dict.toSeq.sortBy(_._1)
                    }
                }
            }
        }
    }
    Json.write(writer, json)
  }

}
