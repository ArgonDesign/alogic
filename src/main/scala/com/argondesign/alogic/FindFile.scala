////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Utility to do a file system search for a given file
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.BasicFileAttributes
import java.util.function.BiPredicate

import scala.compat.java8.StreamConverters.RichStream

object FindFile {
  def apply(tail: String, searchPaths: List[File], maxDepth: Int = Int.MaxValue): Option[File] = {
    val paths = searchPaths filter { _.exists } map { _.toPath }

    val predicate = new BiPredicate[Path, BasicFileAttributes] {
      override def test(path: Path, attr: BasicFileAttributes) = path endsWith tail
    }

    val results = paths.toStream flatMap { Files.find(_, maxDepth, predicate).toScala[Stream] }

    results.headOption map { _.toFile }
  }
}
