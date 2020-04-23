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

import java.util.regex.Pattern

import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees.Ident
import com.argondesign.alogic.ast.Trees.RizDesc
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.core.FatalErrorException
import com.argondesign.alogic.core.InternalCompilerErrorException
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Message
import com.argondesign.alogic.core.SourceAttribute
import com.argondesign.alogic.core.Warning
import com.argondesign.alogic.passes.Pass
import org.scalatest._
import org.scalatest.matchers.should.Matchers
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
    with OneInstancePerTest // ????
    with ChainingSyntax {

  override def withFixture(test: NoArgTest): Outcome = {
    super.withFixture(test) match {
      case failed: Failed =>
        failed tap { _ =>
          failed.exception match {
            case FatalErrorException(cc, _) =>
              cc.messages map { _.string(cc) } foreach Console.err.println
            case InternalCompilerErrorException(cc, _) =>
              cc.messages map { _.string(cc) } foreach Console.err.println
            case _ =>
          }
        }
      case other => other
    }
  }

  final protected def beThe[T <: Message: ClassTag](
      lines: String*
    )(
      implicit
      cc: CompilerContext
    ): Matcher[Message] = Matcher { message: Message =>
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

    val reply = s"""|--- Message
                    |${message.string}
                    |--- $hint ---
                    |${lines mkString ("  ", "\n  ", "")}
                    |---""".stripMargin

    MatchResult(pass, reply, reply)
  }

  final protected def transformWithPass[R](
      pass: Pass[(List[Root], List[Expr]), R],
      text: String
    )(
      implicit
      cc: CompilerContext
    ): Option[R] = {
    // Parse it
    val root = text.stripMargin.asTree[Root]

    // If there is only one desc in the root, mark it as the toplevel for convenience
    root.body match {
      case List(RizDesc(desc)) =>
        desc.ref.asInstanceOf[Ident].attr("toplevel") = SourceAttribute.Flag()
      case _ =>
    }

    // Find the top level
    val topLevelDesc = root.body collectFirst {
      case RizDesc(desc) if desc.ref.asInstanceOf[Ident].attr contains "toplevel" => desc
    } getOrElse {
      fail("Don't know which Desc is the top level")
    }

    // Add it to the global scope
    cc.addGlobalDescs(List(topLevelDesc))

    // For convenience, set the top-level spec to use default parameters if the
    // top-level is parametrized -- this is only for unit testing
    val topLevelSpec = {
      val ref = ExprRef(topLevelDesc.ref) withLoc Loc.synthetic
      if (topLevelDesc.isParametrized) {
        ExprCall(ref, Nil) withLoc Loc.synthetic
      } else {
        ref
      }
    }

    // Apply transform
    pass((List(root), List(topLevelSpec)))
  }

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
