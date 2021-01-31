////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// An abstraction for a text source files.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import java.io.File

class Source(val file: File, val contents: String) {
  val path: String = file.toString

  val name: String = file.getName

  lazy val lines: Seq[String] = contents.linesWithSeparators.toSeq

  def linesIterator: Iterator[String] = contents.linesIterator

  // Return line number (1 based) offset belongs to
  def lineFor(offset: Int): Int = contents.slice(0, offset + 1).linesWithSeparators.length

  // Return offset of line number (1 based)
  def offsetFor(line: Int): Int = lines.slice(0, line - 1).foldLeft(0)(_ + _.length)
}

object Source {
  def apply(file: File, contents: String): Source = new Source(file, contents)
  def apply(file: String, contents: String): Source = Source(new File(file), contents)
}
