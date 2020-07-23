////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Emit auxiliary output
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.BuildInfo
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes._
import com.argondesign.alogic.core.Types.TypeIn
import com.argondesign.alogic.core.Types.TypeOut
import com.argondesign.alogic.core.enums.ResetStyle
import com.argondesign.alogic.lib.Json
import com.argondesign.alogic.util.unreachable

import scala.collection.immutable.ListMap
import scala.collection.parallel.CollectionConverters._

object WriteAux extends PairsTransformerPass {
  val name = "write-aux"

  def topLevelManifest(
      pairs: Iterable[(Decl, Defn)]
    )(
      implicit
      cc: CompilerContext
    ): Map[String, Any] = {

    ////////////////////////////////////////////////////////////////////////////
    // Build nested manifest as a Map (dictionary)
    ////////////////////////////////////////////////////////////////////////////

    val dict = for {
      (Decl(eSymbol), defn: DefnEntity) <- pairs.par
      if eSymbol.attr.topLevel.isSet
    } yield {
      // High level port symbols
      val pSymbols = eSymbol.attr.highLevelKind.get map { _.ports } getOrElse Nil sortBy { symbol =>
        (symbol.loc.start, symbol.name)
      }
      // Low level signal symbols
      val sSymbols = eSymbol.kind.asType.kind.asEntity.ports sortBy { symbol =>
        (symbol.loc.start, symbol.name)
      }

      //////////////////////////////////////////////////////////////////////////
      // Compute signals
      //////////////////////////////////////////////////////////////////////////

      // Compute payload -> port, valid -> port, ready -> port maps

      val d2p = Map from {
        for {
          dSymbol <- sSymbols.iterator
          pSymbol <- dSymbol.attr.payloadOfPort.get
        } yield {
          dSymbol -> pSymbol
        }
      }

      val v2p = Map from {
        for {
          vSymbol <- sSymbols.iterator
          pSymbol <- vSymbol.attr.validOfPort.get
        } yield {
          vSymbol -> pSymbol
        }
      }

      val r2p = Map from {
        for {
          rSymbol <- sSymbols.iterator
          pSymbol <- rSymbol.attr.readyOfPort.get
        } yield {
          rSymbol -> pSymbol
        }
      }

      val signals = ListMap from {
        sSymbols.iterator map { sSymbol =>
          // Figure out whether this is payload, valid or ready
          val value = v2p.get(sSymbol).map { pSymbol =>
            ListMap("port" -> pSymbol.name, "component" -> "valid")
          } orElse r2p.get(sSymbol).map { pSymbol =>
            ListMap("port" -> pSymbol.name, "component" -> "ready")
          } getOrElse {
            ListMap[String, Any](
              "port" -> d2p.getOrElse(sSymbol, sSymbol).name,
              "component" -> "payload",
              "width" -> sSymbol.kind.width,
              "signed" -> sSymbol.kind.isSigned,
              "offset" -> (sSymbol.attr.fieldOffset getOrElse 0)
            )
          }
          sSymbol.name -> value
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // Compute ports
      //////////////////////////////////////////////////////////////////////////

      val ports = {
        // High level ports
        val hlPorts = for {
          pSymbol <- pSymbols
        } yield {
          val (dir, fc) = pSymbol.kind match {
            case TypeIn(_, FlowControlTypeNone)      => ("in", "none")
            case TypeIn(_, FlowControlTypeValid)     => ("in", "sync")
            case TypeIn(_, FlowControlTypeReady)     => ("in", "sync ready")
            case TypeOut(_, FlowControlTypeNone, _)  => ("out", "none")
            case TypeOut(_, FlowControlTypeValid, _) => ("out", "sync")
            case TypeOut(_, FlowControlTypeReady, _) => ("out", "sync ready")
            case _                                   => unreachable
          }
          pSymbol.name -> ListMap("dir" -> dir, "flow-control" -> fc)
        }

        // Low level ports with no matching high level port
        val llPorts = {
          val coveredPorts = Set from (d2p.keysIterator ++ v2p.keysIterator ++ r2p.keysIterator)
          for {
            sSymbol <- sSymbols
            if !coveredPorts(sSymbol)
          } yield {
            val dir = if (sSymbol.kind.isIn) "in" else "out"
            sSymbol.name -> ListMap("dir" -> dir, "flow-control" -> "none")
          }
        }

        assert((hlPorts.map(_._1).toSet intersect llPorts.map(_._1).toSet).isEmpty)

        ListMap from {
          (hlPorts ::: llPorts) sortBy { _._1 }
        }
      }

      //////////////////////////////////////////////////////////////////////////
      // The entry for this entity
      //////////////////////////////////////////////////////////////////////////

      eSymbol.name -> ListMap(
        "alogic-name" -> eSymbol.sourceName,
        "ports" -> ports,
        "signals" -> signals,
        "clock" -> (defn.clk map { _.name }).orNull,
        "reset" -> (defn.rst map { _.name }).orNull,
        "reset-style" -> {
          cc.settings.resetStyle match {
            case ResetStyle.AsyncLow  => "async-low"
            case ResetStyle.AsyncHigh => "async-high"
            case ResetStyle.SyncLow   => "sync-low"
            case ResetStyle.SyncHigh  => "sync-high"
          }
        }
      )
    }

    ListMap.from(dict)
  }

  override def process(
      input: Iterable[(Decl, Defn)]
    )(
      implicit
      cc: CompilerContext
    ): Iterable[(Decl, Defn)] = {

    // Add the module manifest
    cc.manifest("top-levels") = topLevelManifest(input)

    // Add compiler version
    cc.manifest("alogic-version") = BuildInfo.version

    // Write out the manifest
    val w = cc.getOutputWriter("manifest.json")
    Json.write(w, cc.manifest)
    w.close()

    // Write out the stats
    if (cc.settings.stats) {
      val w = cc.getOutputWriter("stats.json")
      val stats: ListMap[String, ListMap[String, Any]] = ListMap from {
        cc.stats.groupBy(_._1._1).toSeq.sortBy(_._1) map {
          case (sourceName, dict) =>
            sourceName -> {
              ListMap from {
                dict.toSeq.sortBy(_._1) map { case ((_, key), value) => key -> value }
              }
            }
        }
      }
      Json.write(w, stats)
      w.close()
    }

    // Done
    input
  }

}
