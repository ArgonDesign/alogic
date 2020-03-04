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

  def context(ansiColor: String)(implicit cc: CompilerContext) = {
    val startLine = source.lineFor(start)
    val endLine = source.lineFor(end)
    val lines = source.lines.slice(startLine - 1, endLine)
    val startLineOffset = source.offsetFor(startLine)
    val s = start - startLineOffset
    val e = end - startLineOffset
    val p = point - startLineOffset
    val text = lines mkString "" match {
      case s if s.nonEmpty && s.last != '\n' => s + '\n'
      case s                                 => s
    }
    val squiggle = text.zipWithIndex map {
      case ('\n', _)        => '\n'
      case (_, i) if i == p => '^'
      case (_, i) if i < s  => ' '
      case (_, i) if i < e  => '~'
      case _                => ' '
    } mkString ""

    val useColours = cc.settings.colourize && s != e
    val colourOn = if (useColours) ansiColor else ""
    val colourOff = if (useColours) AnsiColor.RESET else ""
    val colourBold = if (useColours) AnsiColor.BOLD else ""

    val colorText = text.take(s) + colourOn + text.slice(s, e) + colourOff + text.drop(e)

    // Zip the lines with the squiggles line by line
    // (but not the color escapes), and join them
    val sb = new StringBuilder()
    for ((line, squiggle) <- colorText.linesWithSeparators zip squiggle.linesWithSeparators) {
      if (useColours && (line contains colourOn) || (sb endsWith colourOn)) {
        sb append line
        sb append colourOff
        sb append squiggle
        if (!(line contains colourOff)) {
          sb append colourOn
        }
      } else {
        sb append line
        sb append squiggle
      }
    }

    val txt = sb.toString
    val lns = txt split '\n'
    if (lns.lengthCompare(8) <= 0) {
      txt
    } else {
      (lns.take(4) ++
        Vector(colourOff, colourBold, "  ... omitted ...", colourOff, colourOn) ++
        lns.takeRight(4)) mkString "\n"
    }
  }

  def file(implicit cc: CompilerContext): String =
    cc.remapLine(source, source.lineFor(start))._1.name
  def line(implicit cc: CompilerContext): Int =
    cc.remapLine(source, source.lineFor(start))._2
  def prefix(implicit cc: CompilerContext): String = s"${file}:${line}"
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
          val addedLines = lineMap.keys.filter(_.end < line).foldLeft(0)(_ + _.size)
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
