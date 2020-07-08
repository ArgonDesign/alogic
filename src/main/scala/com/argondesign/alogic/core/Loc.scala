////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

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
  def context(highlightStart: String, highlightReset: String): String = {
    require(highlightStart.nonEmpty == highlightReset.nonEmpty)
    val startLine = source.lineFor(start)
    val endLine = source.lineFor(end)
    val nLines = endLine - startLine + 1
    val sourceLines = source.lines.slice(startLine - 1, endLine)
    val startLineOffset = source.offsetFor(startLine)
    val s = start - startLineOffset
    val e = end - startLineOffset
    val p = point - startLineOffset

    val text = sourceLines mkString "" match {
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

    val colorText = text.take(s) + highlightStart + text.slice(s, e) + highlightReset + text.drop(e)

    // Zip the lines with the squiggles line by line, but not the color escapes.
    // We know that the highlight starts on the first line and ends on the last.
    val result = List from {
      for {
        ((cLine, sLine), idx) <- (colorText.linesIterator zip squiggle.linesIterator).zipWithIndex
      } yield {
        val cRTrim = cLine.replaceFirst("\\s+$", "")
        val sRTrim = sLine.replaceFirst("\\s+$", "")
        val init = cRTrim + highlightReset + System.lineSeparator + sRTrim
        if (idx + 1 < nLines) {
          init + highlightStart // All but last
        } else {
          init // Last line
        }
      }
    }

    if (result.lengthIs <= 4) {
      result.mkString(System.lineSeparator)
    } else {
      val init = result.take(2).iterator
      val mid = Iterator(highlightReset, "  ... omitted ...", highlightStart)
      val tail = result.takeRight(2).iterator
      (init ++ mid ++ tail).mkString(System.lineSeparator)
    }
  }

}

object Loc {
  final val synthetic = Loc("<synthetic>", 0, Source("", ""), 0, 0, 0)
  final val unknown = Loc("<unknown>", 0, Source("", ""), 0, 0, 0)

  //////////////////////////////////////////////////////////////////////////////
  // Ordering for Loc
  //////////////////////////////////////////////////////////////////////////////

  implicit val locOrdering: Ordering[Loc] = (x: Loc, y: Loc) => {
    val compare1 = x.file compare y.file
    if (compare1 != 0) {
      compare1
    } else {
      val compare2 = x.start compare y.start
      if (compare2 != 0) {
        compare2
      } else {
        x.end compare y.end
      }
    }
  }

}
