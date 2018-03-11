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

import java.io.File

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap

case class Loc(file: String, line: Int) {
  override def toString = s"${file}:${line}"
}

object Loc {
  final val synthetic = Loc("<synthetic>", 0)
}

trait LocationRemapping { self: CompilerContext =>

  // Canonicalise file names so equivalent paths map to the same string
  private[this] def canon(file: File): String = file.getCanonicalFile.toString

  private[this] type LineMap = TrieMap[Range, String]

  // Global object and hence accessed from multiple threads
  private[this] val locMap = TrieMap[String, LineMap]()

  // Construct Loc instance, applying remapping
  def loc(file: File, line: Int): Loc = {
    // Map modified location to canonical source location
    @tailrec def loop(file: String, line: Int): (String, Int) = {
      if (!(locMap contains file)) {
        (file, line)
      } else {
        val lineMap = locMap(file)

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
            (file, line - addedLines)
          }
        }
      }
    }

    val (f, l) = loop(canon(file), line)

    Loc(f, l)
  }

  def loc(file: String, line: Int): Loc = loc(new File(file), line)

  // Adjust map so that lines form 'file' that are in 'range'
  // will be printed as belonging to file 'source'
  def remap(file: File, range: Range, source: File): Unit = {
    val lineMap = locMap.getOrElseUpdate(canon(file), new LineMap)
    lineMap(range) = canon(source)
  }
}
