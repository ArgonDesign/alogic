////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017 Argon Design Ltd. All rights reserved.
//
// Module : Scala Alogic Compiler
// Author : Geza Lore
//
// DESCRIPTION:
//
// Preprocessor tests
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class PreprocSpec extends FlatSpec with Matchers {

  val emptyDefines = Map.empty[String, String]

  trait Fixture {
    implicit val cc = new CompilerContext
    val preproc = cc.preproc
  }

  /////////////////////////////////////////////////////////////////////////////
  // Unchanged text
  /////////////////////////////////////////////////////////////////////////////

  "Preproc" should "not change plain text" in new Fixture {
    val text = """|plain text should
                  |be unchanged""".stripMargin

    val result = preproc(Source("test.alogic", text), emptyDefines, Nil)

    result should be("""|plain text should
                        |be unchanged""".stripMargin)
  }

  it should "not affect comments by default" in new Fixture {
    val text = """|// comments should
                  |/* be unchanged*/""".stripMargin

    val result = preproc(Source("test.alogic", text), emptyDefines, Nil)

    result should be("""|// comments should
                        |/* be unchanged*/""".stripMargin)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Macro replacement
  /////////////////////////////////////////////////////////////////////////////

  it should "replace macro identifiers with definitions" in new Fixture {
    val text = """|#define plain macro
                  |#def unchanged changed
                  |plain text should
                  |be unchanged""".stripMargin

    val result = preproc(Source("test.alogic", text), emptyDefines, Nil)

    result should be("""|
                        |
                        |macro text should
                        |be changed""".stripMargin)
  }

  it should "honour initial defines" in new Fixture {
    val text = """|plain text should
                  |be unchanged""".stripMargin

    val initialDefines = Map("plain" -> "not so plain", "unchanged" -> "changed")

    val result = preproc(Source("test.alogic", text), initialDefines, Nil)

    result should be("""|not so plain text should
                        |be changed""".stripMargin)
  }

  /////////////////////////////////////////////////////////////////////////////
  // #if
  /////////////////////////////////////////////////////////////////////////////

  {
    val source = Source(
      "test.alogic",
      """|#if COND
         |  Yay!!
         |#endif""".stripMargin)

    it should "handle true #if without #else" in new Fixture {
      preproc(source, Map("COND" -> "1"), Nil) should be {
        """|
           |  Yay!!
           |""".stripMargin
      }
    }

    it should "handle false #if without #else" in new Fixture {
      preproc(source, Map("COND" -> "0"), Nil) should be {
        """|
           |
           |""".stripMargin
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // #ifdef
  /////////////////////////////////////////////////////////////////////////////

  {
    val source = Source(
      "test.alogic",
      """|#ifdef COND
         |  Yay!!
         |#endif""".stripMargin)

    it should "handle true #ifdef without #else" in new Fixture {
      preproc(source, Map("COND" -> "abc"), Nil) should be {
        """|
           |  Yay!!
           |""".stripMargin
      }
    }

    it should "handle false #ifdef without #else" in new Fixture {
      preproc(source, Map("NotCOND" -> "abc"), Nil) should be {
        """|
           |
           |""".stripMargin
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // #if/#else
  /////////////////////////////////////////////////////////////////////////////

  {
    val source = Source(
      "test.alogic",
      """|#if COND
         |  Yay!!
         |#else
         |  Boo!
         |#endif""".stripMargin)

    it should "handle true #if with #else" in new Fixture {
      preproc(source, Map("COND" -> "1"), Nil) should be {
        """|
           |  Yay!!
           |
           |
           |""".stripMargin
      }
    }

    it should "handle false #if with #else" in new Fixture {
      preproc(source, Map("COND" -> "0"), Nil) should be {
        """|
           |
           |
           |  Boo!
           |""".stripMargin
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // #ifdef/#else
  /////////////////////////////////////////////////////////////////////////////

  {
    val source = Source(
      "text.alogic",
      """|#ifdef COND
         |  Yay!!
         |#else
         |  Boo!
         |#endif""".stripMargin)

    it should "handle true #ifdef with #else" in new Fixture {
      preproc(source, Map("COND" -> "abc"), Nil) should be {
        """|
           |  Yay!!
           |
           |
           |""".stripMargin
      }
    }

    it should "handle false #ifdef with #else" in new Fixture {
      preproc(source, Map("NotCOND" -> "abc"), Nil) should be {
        """|
           |
           |
           |  Boo!
           |""".stripMargin
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // #include
  /////////////////////////////////////////////////////////////////////////////

  {
    def includeResolver(source: Source, spec: String) = {
      val result = spec match {
        case "foo.h" => Source("foo.h", """|This is
                                           |from foo.h
                                           |#include "bar.h"
                                           |foo.h again ...
                                           |#include "bar.h"
                                           |... and again
                                           |""".stripMargin)
        case "bar.h" => Source("foo.h", """|--This is
                                           |--from bar.h""".stripMargin)
      }
      Right(result)
    }

    it should "handle simple #include" in new Fixture {
      val source = Source(
        "test.alogic",
        """|Pre
           |
           |#include "bar.h"
           |
           |Post""".stripMargin)

      preproc(source, emptyDefines, includeResolver(_, _)) should be {
        """|Pre
           |
           |--This is
           |--from bar.h
           |
           |Post""".stripMargin
      }
    }

    it should "handle recursive #include" in new Fixture {
      val source = Source(
        "test.alogic",
        """|Pre
           |
           |#include "foo.h"
           |
           |Post""".stripMargin)

      preproc(source, emptyDefines, includeResolver(_, _)) should be {
        """|Pre
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
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Indented #   if and similar
  /////////////////////////////////////////////////////////////////////////////

  {
    it should "handle true indented #if without #else" in new Fixture {
      val source = Source(
        "test.alogic",
        """|#   if COND
           |  Yay!!
           |#    endif""".stripMargin)
      preproc(source, Map("COND" -> "1"), Nil) should be {
        """|
           |  Yay!!
           |""".stripMargin
      }
    }

    it should "handle false indented #if without #else" in new Fixture {
      val source = Source(
        "test.alogic",
        """|#   if COND
           |  Yay!!
           |#    endif""".stripMargin)
      preproc(source, Map("COND" -> "0"), Nil) should be {
        """|
           |
           |""".stripMargin
      }
    }
  }

}
