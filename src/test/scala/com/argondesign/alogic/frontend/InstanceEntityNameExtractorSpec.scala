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
// InstanceEntityNameExtractor tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Source

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class InstanceEntityNameExtractorSpec extends FlatSpec with Matchers {

  trait Fixture {
    implicit val cc = new CompilerContext
  }

  /////////////////////////////////////////////////////////////////////////////
  // Unchanged text
  /////////////////////////////////////////////////////////////////////////////

  "InstanceEntityNameExtractor" should "find all instance entity names" in new Fixture {
    val source = Source(
      "test.alogic",
      """|network foo {
         | b = new bar();
         | c = new baz();
         |}""".stripMargin
    )

    val parseTreeOpt = Parser(source)

    parseTreeOpt shouldBe defined

    val names = InstanceEntityNameExtractor(parseTreeOpt.get)

    names.length should be { 2 }
    names(0) should be { "bar" }
    names(1) should be { "baz" }
  }

  it should "return duplicates only once" in new Fixture {
    val source = Source(
      "test.alogic",
      """|network foo {
         | b = new meh();
         | c = new meh();
         |}""".stripMargin
    )

    val parseTreeOpt = Parser(source)

    parseTreeOpt shouldBe defined

    val names = InstanceEntityNameExtractor(parseTreeOpt.get)

    names.length should be { 1 }
    names(0) should be { "meh" }
  }

  it should "return the empty list for leaves" in new Fixture {
    val source = Source(
      "test.alogic",
      """|fsm foo {
         |}""".stripMargin
    )

    val parseTreeOpt = Parser(source)

    parseTreeOpt shouldBe defined

    val names = InstanceEntityNameExtractor(parseTreeOpt.get)

    names shouldBe empty
  }

}
