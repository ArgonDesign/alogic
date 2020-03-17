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
// Gather all parameter bindings from instances, and the default parameter
// bindings from entities. Specialize top level entities.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import java.io.Writer

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes._
import com.argondesign.alogic.core.Types.TypeIn
import com.argondesign.alogic.core.Types.TypeOut
import com.argondesign.alogic.core.enums.ResetStyle
import com.argondesign.alogic.util.unreachable

import scala.collection.parallel.CollectionConverters._

object WriteModuleManifest extends PairsTransformerPass {
  val name = "write-module-manifest"

  def emit(ow: Writer, pairs: List[(Decl, Defn)])(implicit cc: CompilerContext): Unit = {

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

      val signals = for {
        sSymbol <- sSymbols
      } yield {
        // Figure out whether this is payload, valid or ready
        val value = v2p.get(sSymbol).map { pSymbol =>
          List("port" -> pSymbol.name, "component" -> "valid")
        } orElse r2p.get(sSymbol).map { pSymbol =>
          List("port" -> pSymbol.name, "component" -> "ready")
        } getOrElse {
          List(
            "port" -> d2p.getOrElse(sSymbol, sSymbol).name,
            "component" -> "payload",
            "width" -> sSymbol.kind.width,
            "signed" -> sSymbol.kind.isSigned,
            "offset" -> (sSymbol.attr.fieldOffset getOrElse 0)
          )
        }

        sSymbol.name -> value
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
            case TypeIn(_, FlowControlTypeNone)       => ("in", "none")
            case TypeIn(_, FlowControlTypeValid)      => ("in", "sync")
            case TypeIn(_, FlowControlTypeReady)      => ("in", "sync ready")
            case TypeIn(_, FlowControlTypeAccept)     => ("in", "sync accept")
            case TypeOut(_, FlowControlTypeNone, _)   => ("out", "none")
            case TypeOut(_, FlowControlTypeValid, _)  => ("out", "sync")
            case TypeOut(_, FlowControlTypeReady, _)  => ("out", "sync ready")
            case TypeOut(_, FlowControlTypeAccept, _) => ("out", "sync accept")
            case _                                    => unreachable
          }
          pSymbol.name -> List("dir" -> dir, "flow-control" -> fc)
        }

        // Low level ports with no matching high level port
        val llPorts = {
          val coveredPorts = Set from (d2p.keysIterator ++ v2p.keysIterator ++ r2p.keysIterator)
          for {
            sSymbol <- sSymbols
            if !coveredPorts(sSymbol)
          } yield {
            val dir = if (sSymbol.kind.isIn) "in" else "out"
            sSymbol.name -> List("dir" -> dir, "flow-control" -> "none")
          }
        }

        assert((hlPorts.map(_._1).toSet intersect llPorts.map(_._1).toSet).isEmpty)

        (hlPorts ::: llPorts) sortBy { _._1 }
      }

      //////////////////////////////////////////////////////////////////////////
      // The entry for this entity
      //////////////////////////////////////////////////////////////////////////

      eSymbol.name -> List(
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

    ////////////////////////////////////////////////////////////////////////////
    // Write out the nested dict
    ////////////////////////////////////////////////////////////////////////////

    val i0 = "  "

    def writeDict(dict: Seq[Any], level: Int): Unit = {
      if (dict.isEmpty) {
        ow.write("{}")
      } else {
        val indent = i0 * level

        // Open dict
        ow.write("{\n")

        def writePair(key: Any, value: Any): Unit = {
          ow.write(indent)
          ow.write(s"""$i0"$key" : """)
          value match {
            case null          => ow.write("null")
            case child: Seq[_] => writeDict(child, level + 1)
            case str: String   => ow.write(s""""$str"""")
            case other         => ow.write(other.toString)
          }
        }

        // Write all but last pair with trailing commas
        for ((key, value) <- dict.init) {
          writePair(key, value)
          ow.write(",\n")
        }

        // Write last pair without trailing comma
        val (key, value) = dict.last
        writePair(key, value)
        ow.write("\n")

        // Close dict
        ow.write(indent)
        ow.write("}")
      }
    }

    writeDict(dict.seq.sortBy(_._1), level = 0)
    ow.write("\n")

    ow.close()
  }

  override def process(input: List[(Decl, Defn)])(
      implicit cc: CompilerContext): List[(Decl, Defn)] = {
    cc.settings.manifestWriterFactory match {
      case Some(f) => emit(f(), input)
      case None    =>
    }
    input
  }

}
