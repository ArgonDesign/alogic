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

import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FatalErrorException
import com.argondesign.alogic.core.InternalCompilerErrorException
import com.argondesign.alogic.core.Message
import org.scalatest._
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

import scala.reflect.ClassTag
import scala.util.ChainingSyntax

trait AlogicTest
    extends TestSuite
    with Matchers
    with LoneElement
    with OptionValues
    with Inspectors
    with Inside
    with OneInstancePerTest
    with ChainingSyntax {

  override def withFixture(test: NoArgTest) = {
    super.withFixture(test) match {
      case failed: Failed =>
        failed tap { _ =>
          failed.exception match {
            case FatalErrorException(cc, _) => {
              cc.messages map { _.string(cc) } foreach Console.err.println
            }
            case InternalCompilerErrorException(cc, _) => {
              cc.messages map { _.string(cc) } foreach Console.err.println
            }
            case _ =>
          }
        }
      case other => other
    }
  }

  final def beThe[T <: Message: ClassTag](lines: String*)(
      implicit cc: CompilerContext): Matcher[Message] = Matcher { message: Message =>
    lazy val linesMatch = lines.length == message.msg.length && {
      val regexes = lines map { _.r }
      (regexes zip message.msg) forall { pair =>
//        println(pair._1)
//        println(pair._2)
//        println(pair._1.pattern.matcher(pair._2).matches())
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
                    |${message.string}
                    |--- ${hint} ---
                    |${lines mkString ("  ", "\n  ", "")}
                    |---""".stripMargin

    MatchResult(pass, reply, reply)
  }
}
