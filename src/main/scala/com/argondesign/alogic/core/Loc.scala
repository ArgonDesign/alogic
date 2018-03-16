////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Source location handling
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap

case class Loc(source: Source, line: Int) {
  def context = source.text.split("\n")(line - 1)
  override def toString = s"${source.name}:${line}"
}

object Loc {
  final val synthetic = Loc(Source("<synthetic>", ""), 0)
}

trait LocationRemapping { self: CompilerContext =>

  private[this] type LineMap = TrieMap[Range, Source]

  // Global object and hence accessed from multiple threads
  private[this] val locMap = TrieMap[Source, LineMap]()

  // Construct Loc instance, applying remapping
  def loc(source: Source, line: Int): Loc = {
    // Map modified location to canonical source location
    @tailrec def loop(source: Source, line: Int): (Source, Int) = {
      if (!(locMap contains source)) {
        (source, line)
      } else {
        val lineMap = locMap(source)

        // Find the mapping that contains this line
        lineMap.find(_._1 contains line) match {
          // If found a mapping, remap source and location and apply recursively
          case Some((range, source)) => {
            loop(source, line - range.start + 1)
          }
          // If not found a mapping, then adjust our own line number based on
          // how many lines were added by the mappings preceding this line
          case None => {
            val addedLines = (0 /: lineMap.keys.filter(_.end < line))(_ + _.size)
            (source, line - addedLines)
          }
        }
      }
    }

    val (s, l) = loop(source, line)

    Loc(s, l)
  }

//  def loc(file: String, line: Int): Loc = loc(new File(file), line)

  // Adjust map so that lines form 'source' that are in 'range'
  // will be printed as belonging to 'origSource'
  def remap(source: Source, range: Range, origSource: Source): Unit = {
    val lineMap = locMap.getOrElseUpdate(source, new LineMap)
    lineMap(range) = origSource
  }
}
