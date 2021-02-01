////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import org.scalactic.TypeCheckedTripleEquals._
import org.scalatest.Assertions.fail

import scala.annotation.tailrec

object CheckJson {

  def apply(json: io.circe.Json, expected: Map[String, String]): Unit =
    expected foreach {
      case (key, value) =>
        @tailrec
        def select(key: String, cursor: io.circe.ACursor): io.circe.ACursor = if (key.isEmpty) {
          cursor
        } else {
          val (first, rest) = {
            def splitEscaped(string: String): (String, String) = {
              val (first, rest) = string.span(_ != '|')
              if (first.endsWith("\\")) {
                val (subFirst, subRest) = splitEscaped(rest.tail)
                (first.init + "|" + subFirst, subRest)
              } else {
                (first, rest.tail)
              }
            }
            splitEscaped(key)
          }
          val (name, idx) = first.span(_ != '[')
          if (name.endsWith("\\")) {
            select(rest, cursor.downField(name.init + idx))
          } else {
            val fCursor = cursor.downField(name)
            select(rest, if (idx.isEmpty) fCursor else fCursor.downN(idx.tail.init.toInt))
          }
        }

        select(key, json.hcursor).focus match {
          case None => fail(s"No json entry with expected key '$key'")
          case Some(json) =>
            if (value == "*") {
              // Wildcard: OK
            } else {
              val expectedValue = io.circe.parser.parse(value) match {
                case Left(failure) =>
                  fail(s"Failed to parse expected json value for key '$key'. " + failure.message)
                case Right(json) => json
              }
              assert(json === expectedValue, s"json entry with '$key' does not match")
            }
        }
    }

}
