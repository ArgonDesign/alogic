////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Json writer
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.lib

import scala.collection.Map
import scala.collection.Seq
import java.io.Writer

object Json {

  private val i0 = "  "

  private def writeAny(w: Writer, thing: Any, level: Int): Unit = thing match {
    case child: Map[_, _] =>
      require(child.keysIterator.nextOption forall { _.isInstanceOf[String] })
      writeObj(w, child.asInstanceOf[Map[String, _]], level)
    case child: Seq[_] => writeArr(w, child, level)
    case null          => w.write("null")
    case str: String   => w.write(s""""$str"""")
    case other         => w.write(other.toString)
  }

  private def writeObj(w: Writer, obj: Map[String, Any], level: Int): Unit =
    if (obj.isEmpty) {
      w.write("{}")
    } else {
      val indent = i0 * level

      // Open dict
      w.write("{")

      def writePair(key: String, value: Any): Unit = {
        w.write("\n")
        w.write(indent)
        w.write(i0)
        w.write('"')
        w.write(key)
        w.write('"')
        w.write(" : ")
        writeAny(w, value, level + 1)
      }

      val it = obj.iterator

      // Write the first pair
      if (it.hasNext) {
        val (k, v) = it.next
        writePair(k, v)
      }

      // Write the remaining pairs, with separators
      while (it.hasNext) {
        w.write(",")
        val (k, v) = it.next
        writePair(k, v)
      }

      // Close dict
      w.write("\n")
      w.write(indent)
      w.write("}")
    }

  private def writeArr(w: Writer, arr: Seq[Any], level: Int): Unit =
    if (arr.isEmpty) {
      w.write("[]")
    } else {
      val indent = i0 * level

      // Open array
      w.write("[")

      def writeValue(value: Any): Unit = {
        w.write("\n")
        w.write(indent)
        w.write(i0)
        writeAny(w, value, level + 1)
      }

      val it = arr.iterator

      // Write the first element
      if (it.hasNext) {
        writeValue(it.next)
      }

      // Write the remaining elements, with separators
      while (it.hasNext) {
        w.write(",")
        writeValue(it.next)
      }

      // Close array
      w.write("\n")
      w.write(indent)
      w.write("]")
    }

  def write(w: Writer, obj: Map[String, Any]): Unit = {
    writeObj(w, obj, 0)
    w.write("\n")
  }

}
