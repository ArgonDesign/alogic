////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module : Alogic Compiler
// Author : Geza Lore
//
// DESCRIPTION:
//
// Utility class to write formatted source text to output
////////////////////////////////////////////////////////////////////////////////
package com.argondesign.alogic.backend

final class CodeWriter {

  private[this] val i0 = "  "

  private[this] val sb = new StringBuilder

  def ensureBlankLine(): Unit = {
    val lastIndex = sb.length - 1
    if (sb(lastIndex) != '\n') {
      sb append "\n\n"
    } else if (sb(lastIndex - 1) != '\n') {
      sb append "\n"
    }
  }

  def append(text: String): Unit = {
    sb append text
  }

  // Recursively emit arbitrarily nested lists of anything as lines into the output
  def emit(indent: String)(item: Any): Unit = item match {
    case items: List[_] =>
      for (item <- items) {
        emit(indent)(item)
      }
    case item => {
      if (sb.isEmpty || sb.last == '\n') {
        sb append indent
      }
      sb append item
      sb append "\n"
    }
  }

  def emit(indent: Int)(item: Any): Unit = emit(i0 * indent)(item)

  def emitTable(indent: String, sep: String)(rows: List[List[String]]): Unit = {
    if (rows.nonEmpty) {
      val colCount = rows.head.length
      assert(rows forall { _.lengthCompare(colCount) == 0 })
      // Compute width of each output columns, set the width of
      // the last colum to 0 to avoid trailing whitepace
      val fieldWidths = {
        val initWidths = rows.transpose.init map { _ map { _.length } } map { _.max }
        initWidths ::: List(0)
      }

      // Emit each row
      for (row <- rows) {
        // Pad all columns to the output column width
        val parts = for ((col, fieldWidth) <- row lazyZip fieldWidths) yield {
          val pad = fieldWidth - col.length
          if (pad <= 0) {
            col
          } else {
            val buf = new StringBuilder(col)
            for (i <- 0 until pad) buf append " "
            buf.toString
          }
        }
        emit(indent)(parts mkString sep)
      }
    }
  }

  def emitTable(indent: Int, sep: String)(rows: List[List[String]]): Unit = {
    emitTable(i0 * indent, sep)(rows)
  }

  def emitBlock(indent: String, header: String)(rest: => Unit): Unit = {
    sb append indent
    sb append "// "
    sb append header
    sb append "\n"
    rest
    ensureBlankLine()
  }

  def emitBlock(indent: Int, header: String)(rest: => Unit): Unit = {
    emitBlock(i0 * indent, header)(rest)
  }

  def emitSection(indent: String, title: String)(rest: => Unit) = {
    ensureBlankLine()
    sb append indent + ("/" * (80 - indent.length)) + "\n"
    sb append indent + "// " + title + "\n"
    sb append indent + ("/" * (80 - indent.length)) + "\n"
    sb append "\n"
    rest
    ensureBlankLine()
  }

  def emitSection(indent: Int, title: String)(rest: => Unit): Unit = {
    emitSection(i0 * indent, title)(rest)
  }

  def text = sb.toString

}
