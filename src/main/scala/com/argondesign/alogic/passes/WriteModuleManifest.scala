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

import java.io.PrintWriter
import java.nio.file.Path

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeAccept
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeReady
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeValid
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.core.Types.TypeEntity
import com.argondesign.alogic.core.Types.TypeIn
import com.argondesign.alogic.core.Types.TypeOut
import com.argondesign.alogic.util.unreachable

import scala.collection.parallel.CollectionConverters._
import scala.language.postfixOps

object WriteModuleManifest extends Pass {
  val name = "write-module-manifest"

  def emit(path: Path, trees: List[Tree])(implicit cc: CompilerContext): Unit = {

    ////////////////////////////////////////////////////////////////////////////
    // Build nested manifest as a Map (dictionary)
    ////////////////////////////////////////////////////////////////////////////

    val dict = for {
      EntityLowered(eSymbol, _, insts, _, _, _) <- trees.par
    } yield {
      // High level port symbols
      val TypeEntity(_, pSymbols, _) = eSymbol.attr.highLevelKind.value.asEntity
      // Low level signal symbols
      val TypeEntity(_, sSymbols, _) = eSymbol.kind.asEntity

      //////////////////////////////////////////////////////////////////////////
      // Compute signals
      //////////////////////////////////////////////////////////////////////////

      // Compute payload -> port, valid -> port, ready -> port maps

      def payloadSymbol(pSymbol: TermSymbol): Option[TermSymbol] = {
        val attr = pSymbol.attr
        if (!attr.fcv.isSet && !attr.fcr.isSet) {
          Some(pSymbol)
        } else {
          attr.fcv.get.map(_._1) orElse attr.fcr.get.map(_._1) match {
            case opt @ Some(_: TermSymbol) => opt.asInstanceOf[Option[TermSymbol]]
            case _                         => None
          }
        }
      }

      val d2p = {
        for {
          pSymbol <- pSymbols
          dSymbol <- payloadSymbol(pSymbol)
        } yield {
          dSymbol -> pSymbol
        }
      } toMap

      val v2p = {
        for {
          pSymbol <- pSymbols
          vSymbol <- (pSymbol.attr.fcv.get map { _._2 }) orElse (pSymbol.attr.fcr.get map { _._2 })
        } yield {
          vSymbol -> pSymbol
        }
      } toMap

      val r2p = {
        for {
          pSymbol <- pSymbols
          rSymbol <- pSymbol.attr.fcr.get map { _._3 }
        } yield {
          rSymbol -> pSymbol
        }
      } toMap

      // Compute the field -> struct map
      val f2t = {
        for {
          sSymbol <- sSymbols
          tSymbol <- sSymbol.attr.structSymbol.get
        } yield {
          sSymbol -> tSymbol
        }
      } toMap

      val signals = for {
        sSymbol <- sSymbols
      } yield {
        // Map field back to struct (if any)
        val tSymbol = f2t.getOrElse(sSymbol, sSymbol)

        // Figure out whether this is payload, valid or ready
        val value = v2p.get(tSymbol).map { pSymbol =>
          List("port" -> pSymbol.name, "component" -> "valid")
        } orElse r2p.get(tSymbol).map { pSymbol =>
          List("port" -> pSymbol.name, "component" -> "ready")
        } getOrElse {
          val pSymbol = d2p.getOrElse(tSymbol, tSymbol)
          List(
            "port" -> pSymbol.name,
            "component" -> "payload",
            "width" -> sSymbol.kind.width,
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
          val coveredPorts = d2p.keySet union v2p.keySet union r2p.keySet
          for {
            sSymbol <- sSymbols
            tSymbol = f2t.getOrElse(sSymbol, sSymbol)
            if !(coveredPorts contains tSymbol)
          } yield {
            val dir = if (sSymbol.kind.isIn) "in" else "out"
            sSymbol.name -> List("dir" -> dir, "flow-control" -> "none")
          }
        }

        assert((hlPorts.map(_._1).toSet intersect llPorts.map(_._1).toSet).isEmpty)

        hlPorts ::: llPorts
      }

      //////////////////////////////////////////////////////////////////////////
      // Compute instances
      //////////////////////////////////////////////////////////////////////////

      val instances = for {
        Instance(Sym(iSymbol), Sym(eSymbol), _, _) <- insts
      } yield {
        iSymbol.name -> eSymbol.name
      }

      eSymbol.name -> List(
        "top-level" -> eSymbol.attr.topLevel.isSet,
        "ports" -> ports,
        "signals" -> signals,
        "instances" -> instances
      )
    }

    ////////////////////////////////////////////////////////////////////////////
    // Write out the nested dict
    ////////////////////////////////////////////////////////////////////////////

    val pw = new PrintWriter(path.toFile)

    val i0 = "  "

    def writeDict(dict: Seq[Any], level: Int): Unit = {
      if (dict.isEmpty) {
        pw.write("{}")
      } else {
        val indent = i0 * level

        // Open dict
        pw.write("{\n")

        def writePair(key: Any, value: Any): Unit = {
          pw.write(indent)
          pw.write(s"""${i0}"${key}" : """)
          value match {
            case child: Seq[_] => writeDict(child, level + 1)
            case str: String   => pw.write(s""""${str}"""")
            case other         => pw.write(other.toString)
          }
        }

        // Write all but last pair with trailing commas
        for ((key, value) <- dict.init) {
          writePair(key, value)
          pw.write(",\n")
        }

        // Write last pair without trailing comma
        val (key, value) = dict.last
        writePair(key, value)
        pw.write("\n")

        // Close dict
        pw.write(indent)
        pw.write("}")
      }
    }

    writeDict(dict.seq, level = 0)
    pw.write("\n")

    pw.close()
  }

  def apply(trees: List[Tree])(implicit cc: CompilerContext): List[Tree] = {
    cc.settings.moduleManifestPath match {
      case Some(path) => emit(path, trees)
      case None       =>
    }
    trees
  }

}
