////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Compiler profiling
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import java.io.PrintWriter

import scala.annotation.tailrec
import scala.collection.mutable

trait Profiling { this: CompilerContext =>

  private val measurements = mutable.Map[List[String], Double]()
  private var trace: List[String] = Nil

  def timeit[T](key: String)(block: => T): T = {
    if (!settings.profile) {
      block
    } else {
      trace = key :: trace
      val t0 = System.nanoTime()
      val result = block
      val elapsed = (System.nanoTime() - t0).toDouble / 1e9
      measurements.updateWith(trace.reverse) {
        case None        => Some(elapsed)
        case Some(value) => Some(value + elapsed)
      }
      trace = trace.tail
      result
    }
  }

  def writeProfile(writer: => PrintWriter): Unit = {
    @tailrec
    def lt(a: List[String], b: List[String]): Boolean = (a, b) match {
      case (Nil, _)             => true
      case (_, Nil)             => false
      case (a :: Nil, b :: Nil) => a < b
      case (a :: as, b :: bs)   => if (a < b) true else if (a > b) false else lt(as, bs)
    }

    val width = 60
    val total = (measurements filter { _._1.lengthIs == 1 }).valuesIterator.sum
    measurements.toSeq sortWith {
      case ((as, _), (bs, _)) => lt(as, bs)
    } foreach {
      case (key, value) =>
        val level = key.init.length
        writer println {
          f"${"  " * level}${key.last.padTo(width, ' ')} $value%8.2f ${value / total * 100}%6.2f%%"
        }
    }
    writer.println(f"${"TOTAL".padTo(width, ' ')} $total%8.2f 100.00%%")
    writer.flush()
  }
}
