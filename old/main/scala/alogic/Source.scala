////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017 Argon Design Ltd. All rights reserved.
//
// Module : Scala Alogic Compiler
// Author : Geza Lore
//
// DESCRIPTION:
//
// An abstraction for a source file that facilitates in-memory testing.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic

import scalax.file.Path

class Source(val path: Path, _text: => String) {
  val name = path.name

  lazy val text = _text
}

object Source {
  def apply(path: Path, text: String): Source = new Source(path, text)
  def apply(path: Path): Source = new Source(path, path.lines(includeTerminator = true).mkString)
  def apply(path: String, text: String): Source = Source(Path.fromString(path), text)
  def apply(path: String): Source = Source(Path.fromString(path))
}
