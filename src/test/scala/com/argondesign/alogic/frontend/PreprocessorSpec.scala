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
// Preprocessor tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import java.io.File

import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.core.Fatal
import com.argondesign.alogic.core.FatalErrorException
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.core.Warning
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class PreprocessorSpec extends AnyFlatSpec with Matchers {

  val emptyDefines = Map.empty[String, String]

  trait Fixture {
    implicit val cc = new CompilerContext
    val preproc = new Preprocessor
  }

  /////////////////////////////////////////////////////////////////////////////
  // Unchanged text
  /////////////////////////////////////////////////////////////////////////////

  "Preproc" should "not change plain text" in new Fixture {

    val source = Source(
      "test.alogic",
      """plain text should
        |be unchanged""".stripMargin
    )

    preproc(source, emptyDefines, Nil).text should be {
      """plain text should
        |be unchanged""".stripMargin
    }

    cc.messages shouldBe empty
  }

  it should "not affect comments by default" in new Fixture {

    val source = Source(
      "test.alogic",
      """// comments should
        |/* be unchanged*/""".stripMargin
    )

    preproc(source, emptyDefines, Nil).text should be {
      """// comments should
        |/* be unchanged*/""".stripMargin
    }

    cc.messages shouldBe empty
  }

  /////////////////////////////////////////////////////////////////////////////
  // #define/#def
  /////////////////////////////////////////////////////////////////////////////

  it should "replace macro identifiers with definitions" in new Fixture {

    val source = Source(
      "test.alogic",
      """#define plain macro
        |#define unchanged changed
        |plain text should
        |be unchanged""".stripMargin
    )

    preproc(source, emptyDefines, Nil).text should be {
      """
        |
        |macro text should
        |be changed""".stripMargin
    }

    cc.messages shouldBe empty
  }

  it should "honour initial defines" in new Fixture {

    val source = Source(
      "test.alogic",
      """plain text should
        |be unchanged""".stripMargin
    )

    val initialDefines = Map("plain" -> "not so plain", "unchanged" -> "changed")

    preproc(source, initialDefines, Nil).text should be {
      """not so plain text should
        |be changed""".stripMargin
    }

    cc.messages shouldBe empty
  }

  it should "warn for redefinition of previos #define" in new Fixture {

    val source = Source(
      "test.alogic",
      """#define foo first
        |
        |#define foo again""".stripMargin
    )

    preproc(source, emptyDefines, Nil)

    cc.messages should have length 1

    val message = cc.messages(0)

    message shouldBe a[Warning]
    message.loc.source should be(source)
    message.loc.line should be(3)
    message.msg(0) should be("Redefined preprocessor identifier 'foo'")
  }

  it should "warn for redefinition of initial definition" in new Fixture {

    val source = Source(
      "test.alogic",
      """#define foo maybe first""".stripMargin
    )

    preproc(source, Map("foo" -> "real first"), Nil)

    cc.messages should have length 1

    val message = cc.messages(0)

    message shouldBe a[Warning]
    message.loc.source should be(source)
    message.loc.line should be(1)
    message.msg(0) should be("Redefined preprocessor identifier 'foo'")
  }

  /////////////////////////////////////////////////////////////////////////////
  // #if
  /////////////////////////////////////////////////////////////////////////////

  {
    val source = Source(
      "test.alogic",
      """#if COND
        |  Yay!!
        |#endif""".stripMargin
    )

    it should "handle true #if without #else" in new Fixture {
      preproc(source, Map("COND" -> "1"), Nil).text should be {
        """
          |  Yay!!
          |""".stripMargin
      }

      cc.messages shouldBe empty
    }

    it should "handle false #if without #else" in new Fixture {
      preproc(source, Map("COND" -> "0"), Nil).text should be {
        """
          |
          |""".stripMargin
      }

      cc.messages shouldBe empty
    }

    it should "error for undefined #if condition" in new Fixture {
      preproc(source, emptyDefines, Nil)

      cc.messages should have length 1

      val message = cc.messages(0)

      message shouldBe an[Error]
      message.loc.source should be(source)
      message.loc.line should be(1)
      message.msg(0) should be("#if condition macro 'COND' is not defined")
    }

    it should "error for non-integer #if condition" in new Fixture {
      preproc(source, Map("COND" -> "?"), Nil)

      val message = cc.messages(0)

      message shouldBe an[Error]
      message.loc.source should be(source)
      message.loc.line should be(1)
      message.msg(0) should be("#if condition macro 'COND' must be defined as a single integer,")
      message.msg(1) should be("not '?'")
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // #ifdef
  /////////////////////////////////////////////////////////////////////////////

  {
    val source = Source(
      "test.alogic",
      """#ifdef COND
        |  Yay!!
        |#endif""".stripMargin
    )

    it should "handle true #ifdef without #else" in new Fixture {
      preproc(source, Map("COND" -> "abc"), Nil).text should be {
        """
          |  Yay!!
          |""".stripMargin
      }

      cc.messages shouldBe empty
    }

    it should "handle false #ifdef without #else" in new Fixture {
      preproc(source, Map("NotCOND" -> "abc"), Nil).text should be {
        """
          |
          |""".stripMargin
      }

      cc.messages shouldBe empty
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // #if/#else
  /////////////////////////////////////////////////////////////////////////////

  {
    val source = Source(
      "test.alogic",
      """#if COND
        |  Yay!!
        |#else
        |  Boo!
        |#endif""".stripMargin
    )

    it should "handle true #if with #else" in new Fixture {
      preproc(source, Map("COND" -> "1"), Nil).text should be {
        """
          |  Yay!!
          |
          |
          |""".stripMargin
      }

      cc.messages shouldBe empty
    }

    it should "handle false #if with #else" in new Fixture {
      preproc(source, Map("COND" -> "0"), Nil).text should be {
        """
          |
          |
          |  Boo!
          |""".stripMargin
      }

      cc.messages shouldBe empty
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // #ifdef/#else
  /////////////////////////////////////////////////////////////////////////////

  {
    val source = Source(
      "text.alogic",
      """#ifdef COND
        |  Yay!!
        |#else
        |  Boo!
        |#endif""".stripMargin
    )

    it should "handle true #ifdef with #else" in new Fixture {
      preproc(source, Map("COND" -> "abc"), Nil).text should be {
        """
          |  Yay!!
          |
          |
          |""".stripMargin
      }

      cc.messages shouldBe empty
    }

    it should "handle false #ifdef with #else" in new Fixture {
      preproc(source, Map("NotCOND" -> "abc"), Nil).text should be {
        """
          |
          |
          |  Boo!
          |""".stripMargin
      }

      cc.messages shouldBe empty
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // #include
  /////////////////////////////////////////////////////////////////////////////

  {
    def includeResolver(source: Source, spec: String) = {
      val result = spec match {
        case "foo.h" =>
          Source(
            "foo.h",
            """This is
              |from foo.h
              |#include "bar.h"
              |foo.h again ...
              |#include "bar.h"
              |... and again
              |""".stripMargin
          )
        case "bar.h" =>
          Source("bar.h", """--This is
                           |--from bar.h""".stripMargin)
        case "def1.h" =>
          Source(
            "def1.h",
            """
              |#define a def1
              |#include "def2.h"
              |#define b def1
              |#include "def2.h"
              |#define a def1
              |""".stripMargin
          )
        case "def2.h" =>
          Source("def2.h", """#define a def2
                            |#define b def2""".stripMargin)
      }
      Right(result)
    }

    it should "handle simple #include" in new Fixture {
      val source = Source(
        "test.alogic",
        """Pre
          |
          |#include "bar.h"
          |
          |Post""".stripMargin
      )

      preproc(source, emptyDefines, includeResolver(_, _)).text should be {
        """Pre
          |
          |--This is
          |--from bar.h
          |
          |Post""".stripMargin
      }

      cc.messages shouldBe empty
    }

    it should "handle recursive #include" in new Fixture {
      val source = Source(
        "test.alogic",
        """Pre
          |
          |#include "foo.h"
          |
          |Post""".stripMargin
      )

      preproc(source, emptyDefines, includeResolver(_, _)).text should be {
        """Pre
          |
          |This is
          |from foo.h
          |--This is
          |--from bar.h
          |foo.h again ...
          |--This is
          |--from bar.h
          |... and again
          |
          |
          |Post""".stripMargin
      }

      cc.messages shouldBe empty
    }

    it should "remap line numbers after resolving #include" in new Fixture {
      val source = Source(
        "test.alogic",
        """#define a root
          |#define b root
          |#include "def1.h"
          |#define a root
          |#define b root
          |""".stripMargin
      )

      preproc(source, emptyDefines, includeResolver(_, _)).text should be("\n" * 13)

      cc.messages should have length 9

      cc.messages(0).loc.source.name should endWith("def1.h")
      cc.messages(0).loc.line should be(2)
      cc.messages(1).loc.source.name should endWith("def2.h")
      cc.messages(1).loc.line should be(1)
      cc.messages(2).loc.source.name should endWith("def2.h")
      cc.messages(2).loc.line should be(2)
      cc.messages(3).loc.source.name should endWith("def1.h")
      cc.messages(3).loc.line should be(4)
      cc.messages(4).loc.source.name should endWith("def2.h")
      cc.messages(4).loc.line should be(1)
      cc.messages(5).loc.source.name should endWith("def2.h")
      cc.messages(5).loc.line should be(2)
      cc.messages(6).loc.source.name should endWith("def1.h")
      cc.messages(6).loc.line should be(6)
      cc.messages(7).loc.source.name should endWith("test.alogic")
      cc.messages(7).loc.line should be(4)
      cc.messages(8).loc.source.name should endWith("test.alogic")
      cc.messages(8).loc.line should be(5)
    }

    it should "fatal for absolute include paths" in new Fixture {
      val source = Source(
        "test.alogic",
        """#include "/abs.h"
          |""".stripMargin
      )

      a[FatalErrorException] should be thrownBy {
        preproc(source, emptyDefines, Nil)
      }

      cc.messages should have length 1

      val message = cc.messages(0)

      message shouldBe a[Fatal]
      message.loc.source should be(source)
      message.loc.line should be(1)
      message.msg(0) should be("No absolute include paths allowed: \"/abs.h\"")
    }

    it should "fatal for unsuccessful path search" in new Fixture {
      val source = Source(
        "test.alogic",
        """#include "missing.h"
          |""".stripMargin
      )

      val includeSearchPaths = List(
        new File("/hopefully/nonexistent/path/where/alogic/will/look/for/header"),
        new File("/another/hopefully/nonexistent/path/where/alogic/will/look/for/header")
      )

      a[FatalErrorException] should be thrownBy {
        preproc(source, emptyDefines, includeSearchPaths)
      }

      cc.messages should have length 1

      val message = cc.messages(0)

      message shouldBe a[Fatal]
      message.loc.source should be(source)
      message.loc.line should be(1)
      message.msg(0) should be("Cannot find include file \"missing.h\". Looked in:")
      message.msg(1) should be(includeSearchPaths(0).getCanonicalPath)
      message.msg(2) should be(includeSearchPaths(1).getCanonicalPath)
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Indented #   if and similar
  /////////////////////////////////////////////////////////////////////////////

  {
    it should "handle true indented #if without #else" in new Fixture {
      val source = Source(
        "test.alogic",
        """#   if COND
          |  Yay!!
          |#    endif""".stripMargin
      )

      preproc(source, Map("COND" -> "1"), Nil).text should be {
        """
          |  Yay!!
          |""".stripMargin
      }

      cc.messages shouldBe empty
    }

    it should "handle false indented #if without #else" in new Fixture {
      val source = Source(
        "test.alogic",
        """#   if COND
          |  Yay!!
          |#    endif""".stripMargin
      )

      preproc(source, Map("COND" -> "0"), Nil).text should be {
        """
          |
          |""".stripMargin
      }

      cc.messages shouldBe empty
    }
  }

}
