////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import scala.io.AnsiColor

case class Loc(
    // Source location - Used to print location info in messages
    file: String, // Source file name
    line: Int, // Source line number
    // Source context - Used to retrieve source context
    source: Source, // The Source holding source file contents
    start: Int, // Start index into text of 'source'
    end: Int, // End index (exclusive) into text of 'source'
    point: Int // Index of '^' caret in text of 'source'
  ) {
  require(start <= point && point <= end)

  // Prefix used for printing messages
  def prefix: String = s"$file:$line"

  // Construct context lines
  def context(ansiColor: Option[String]): String = {
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

    val useColors = ansiColor.isDefined
    val colorOn = ansiColor.getOrElse("")
    val colorOff = if (useColors) AnsiColor.RESET else ""
    val colorBold = if (useColors) AnsiColor.BOLD else ""

    val colorText = text.take(s) + colorOn + text.slice(s, e) + colorOff + text.drop(e)

    // Zip the lines with the squiggles line by line
    // (but not the color escapes), and join them
    val sb = new StringBuilder()
    for ((line, squiggle) <- colorText.linesWithSeparators zip squiggle.linesWithSeparators) {
      if (useColors && (line contains colorOn) || (sb endsWith colorOn)) {
        sb append line
        sb append colorOff
        sb append squiggle
        if (!(line contains colorOff)) {
          sb append colorOn
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
        Vector(colorOff, colorBold, "  ... omitted ...", colorOff, colorOn) ++
        lns.takeRight(4)) mkString "\n"
    }
  }

}

object Loc {
  final val synthetic = Loc("<synthetic>", 0, Source("", ""), 0, 0, 0)
}
