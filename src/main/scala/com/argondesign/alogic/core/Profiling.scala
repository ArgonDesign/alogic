////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Internal profiling utilities, for developer assistance only
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import java.io.PrintWriter
import scala.annotation.tailrec
import scala.collection.mutable

trait Profiling { this: CompilerContext =>

  // $COVERAGE-OFF$ Debug code

  private val measurements = mutable.Map[List[String], Double]()
  private val trace: ThreadLocal[List[String]] = ThreadLocal.withInitial[List[String]](() => Nil)

  def timeit[T](key: String)(block: => T): T = {
    if (!settings.profile) {
      block
    } else {
      trace.set(key :: trace.get())
      val t0 = System.nanoTime()
      val result = block
      val elapsed = (System.nanoTime() - t0).toDouble / 1E9
      measurements.updateWith(trace.get()) {
        case None        => Some(elapsed)
        case Some(value) => Some(value + elapsed)
      }
      trace.set(trace.get().tail)
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

    val width = 100
    val extra = 17

    writer.println("=== tree profile:")

    val total = measurements.filter(_._1.lengthIs == 1).valuesIterator.sum
    measurements.toSeq sortWith {
      case ((as, _), (bs, _)) => lt(as.reverse, bs.reverse)
    } foreach {
      case (key, value) =>
        val level = key.tail.length
        writer println {
          f"${"  " * level}${key.head.padTo(width, ' ')} $value%8.2f ${value / total * 100}%6.2f%%"
        }
    }
    writer.println("-" * (width + extra))
    writer.println(f"${"TOTAL".padTo(width, ' ')} $total%8.2f 100.00%%")

    val selfTimes = measurements.clone()
    measurements foreach {
      case (trace, t) =>
        trace.tail match {
          case Nil   =>
          case trace => selfTimes(trace) -= t
        }
    }

    val total2 = selfTimes.valuesIterator.sum
    assert(total - total2 < 1E-6)
    writer.println()
    writer.println("=== flat profile:")
    selfTimes.groupMapReduce(_._1.head)(_._2)(_ + _).toSeq.sortBy(pair => -pair._2) foreach {
      case (key, value) =>
        writer println {
          f"  ${key.padTo(width, ' ')} $value%8.2f ${value / total2 * 100}%6.2f%%"
        }
    }
    writer.println("-" * (width + extra))
    writer.println(f"${"TOTAL".padTo(width, ' ')} $total2%8.2f 100.00%%")

    writer.flush()
  }

  // $COVERAGE-ON$

}
