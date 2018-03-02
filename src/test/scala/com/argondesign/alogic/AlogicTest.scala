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
// Base traits for compiler tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import scala.reflect.ClassTag

import com.argondesign.alogic.core.FatalErrorException
import com.argondesign.alogic.core.InternalCompilerErrorException
import com.argondesign.alogic.core.Message
import com.argondesign.alogic.util.FollowedBy.any2FollowedByWord

import org.scalatest.Failed
import org.scalatest.Inside
import org.scalatest.Inspectors
import org.scalatest.LoneElement
import org.scalatest.Matchers
import org.scalatest.OneInstancePerTest
import org.scalatest.OptionValues
import org.scalatest.TestSuite
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

trait AlogicTest
  extends TestSuite
  with Matchers
  with LoneElement
  with OptionValues
  with Inspectors
  with Inside
  with OneInstancePerTest {

  override def withFixture(test: NoArgTest) = {
    super.withFixture(test) match {
      case failed: Failed => failed followedBy {
        failed.exception match {
          case FatalErrorException(cc, _)            => cc.messages foreach Console.err.println
          case InternalCompilerErrorException(cc, _) => cc.messages foreach Console.err.println
          case _                                     =>
        }
      }
      case other => other
    }
  }

  final def beThe[T <: Message: ClassTag](lines: String*): Matcher[Message] = Matcher { message: Message =>
    lazy val linesMatch = lines.length == message.msg.length && {
      val regexes = lines map { _.r }
      (regexes zip message.msg) forall { pair =>
        pair._1.pattern.matcher(pair._2).matches()
      }
    }

    val tag = implicitly[ClassTag[T]]
    lazy val msgName = tag.runtimeClass.getName.split("[.]").last

    val (pass, hint) = if (!tag.runtimeClass.isInstance(message)) {
      (false, s"was not an instance of ${msgName}")
    } else if (!linesMatch) {
      (false, s"did not match the expected ${msgName} message")
    } else {
      (true, s"matches the expected ${msgName} message ")
    }

    val reply = s"""|--- Message
                    |${message}
                    |--- ${hint} ---
                    |${lines mkString ("  ", "\n  ", "")}
                    |---""".stripMargin

    MatchResult(pass, reply, reply)
  }
}
