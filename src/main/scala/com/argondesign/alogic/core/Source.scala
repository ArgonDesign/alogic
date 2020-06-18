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
// An abstraction for a source files.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path

class Source(val file: File, val text: String) {
  val name = file.toString

  lazy val lines: List[String] = text.linesWithSeparators.toList

  def linesIterator: Iterator[String] = text.linesIterator

  // Return line number (1 based) offset belongs to
  def lineFor(offset: Int): Int = text.slice(0, offset + 1).linesWithSeparators.length

  // Return offset of line number (1 based)
  def offsetFor(line: Int): Int = lines.slice(0, line - 1).foldLeft(0)(_ + _.length)
}

object Source {
  def apply(file: File, text: String): Source = new Source(file, text)

  def apply(file: File): Source = {
    val text = new String(Files.readAllBytes(file.toPath), StandardCharsets.UTF_8)
    new Source(file, text)
  }

  def apply(path: Path): Source = Source(path.toFile)

  def apply(file: String, text: String): Source = Source(new File(file), text)
  def apply(file: String): Source = Source(new File(file))
}
