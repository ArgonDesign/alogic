////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Base traits for compiler tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import com.argondesign.alogic.ast.Trees.Arg
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.Messages.Message
import com.argondesign.alogic.core.Messages.Warning
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.passes.Pass
import org.scalatest._
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.should.Matchers

import java.util.regex.Pattern
import scala.reflect.ClassTag
import scala.util.ChainingSyntax

trait AlogicTest
    extends TestSuite
    with Matchers
    with LoneElement
    with OptionValues
    with Inspectors
    with Inside
    with OneInstancePerTest // ????
    with ChainingSyntax {

  final protected def beThe[T <: Message: ClassTag](
      lines: String*
    ): Matcher[Message] = Matcher { (message: Message) =>
    lazy val linesMatch = lines.length == message.msg.length && {
      val regexes = lines map { _.r }
      (regexes zip message.msg) forall { pair =>
        pair._1.pattern.matcher(pair._2).matches()
      }
    }

    val tag = implicitly[ClassTag[T]]
    lazy val msgName = tag.runtimeClass.getName.split("[.]").last

    val (pass, hint) = if (!tag.runtimeClass.isInstance(message)) {
      (false, s"was not an instance of $msgName")
    } else if (!linesMatch) {
      (false, s"did not match the expected $msgName message")
    } else {
      (true, s"matches the expected $msgName message ")
    }

    val reply = s"""--- Message
                   |${message.render}
                   |--- $hint ---
                   |${lines mkString ("  ", "\n  ", "")}
                   |---""".stripMargin

    MatchResult(pass, reply, reply)
  }

  def beSyntaxError: Matcher[Message] = beSyntaxError("")

  def beSyntaxError(text: String): Matcher[Message] = Matcher { (msg: Message) =>
    val matchesMessage = if (text.isEmpty) {
      msg.msg.head startsWith "Syntax error: "
    } else {
      msg.msg.head == s"Syntax error: $text"
    }
    MatchResult(
      msg.isInstanceOf[Error] && matchesMessage,
      s"'$msg' was not a Syntax error message matching 'Syntax error: $text'",
      s"'$msg' was the correct Syntex error message"
    )
  }

  final protected def transformWithPass[R](
      pass: Pass[(Source, Loc, List[Arg]), R],
      text: String
    )(
      implicit
      cc: CompilerContext
    ): Option[R] = pass((Source("<test>", text), Loc.synthetic, Nil))

  final protected def checkSingleError(err: List[String])(implicit cc: CompilerContext): Unit = {
    val errors = cc.messages.filter(_.isInstanceOf[Error])
    if (err.isEmpty) {
      errors shouldBe empty
    } else {
      errors.loneElement should beThe[Error](err map Pattern.quote: _*)
    }
  }

  final protected def checkSingleWarning(err: List[String])(implicit cc: CompilerContext): Unit = {
    val errors = cc.messages.filter(_.isInstanceOf[Warning])
    if (err.isEmpty) {
      errors shouldBe empty
    } else {
      errors.loneElement should beThe[Warning](err map Pattern.quote: _*)
    }
  }

}
