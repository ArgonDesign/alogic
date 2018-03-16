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
import scala.io.AnsiColor

case class Loc(source: Source, start: Int, end: Int, point: Int) {

  def context = {
    val startLine = source.lineFor(start)
    val endLine = source.lineFor(start)
    val lines = source.lines.slice(startLine - 1, endLine)
    val startLineOffset = source.offsetFor(startLine)
    val s = start - startLineOffset
    val e = end - startLineOffset
    val p = point - startLineOffset
    val squiggle = for (line <- lines) yield {
      val chars = 0 to line.length map {
        case c if c == p => '^'
        case c if c < s  => ' '
        case c if c < e  => '~'
        case _           => ' '
      }
      chars mkString ""
    }
    val text = lines mkString "\n"
    val colorText = text.slice(0, s) + AnsiColor.RED + AnsiColor.BOLD +
      text.slice(s, e) + AnsiColor.RESET +
      text.slice(e, text.length)

    val combed = for ((line, squiggle) <- colorText.split("\n") zip squiggle) yield {
      line + "\n" + squiggle
    }

    combed mkString ""
  }
  def line(implicit cc: CompilerContext): Int = cc.remapLine(source, source.lineFor(start))._2
  def prefix(implicit cc: CompilerContext): String = s"${source.name}:${line}"
}

object Loc {
  final val synthetic = Loc(Source("<synthetic>", ""), 0, 0, 0)
}

trait LocationRemapping { self: CompilerContext =>

  private[this] type LineMap = TrieMap[Range, Source]

  // Global object and hence accessed from multiple threads
  private[this] val locMap = TrieMap[Source, LineMap]()

  // Map modified location to canonical source location
  @tailrec
  final def remapLine(source: Source, line: Int): (Source, Int) = {
    if (!(locMap contains source)) {
      (source, line)
    } else {
      val lineMap = locMap(source)

      // Find the mapping that contains this line
      lineMap.find(_._1 contains line) match {
        // If found a mapping, remap source and location and apply recursively
        case Some((range, source)) => {
          remapLine(source, line - range.start + 1)
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

  // Adjust map so that lines form 'source' that are in 'range'
  // will be printed as belonging to 'origSource'
  def remap(source: Source, range: Range, origSource: Source): Unit = {
    val lineMap = locMap.getOrElseUpdate(source, new LineMap)
    lineMap(range) = origSource
  }
}
