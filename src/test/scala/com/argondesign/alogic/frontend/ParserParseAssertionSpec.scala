////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Tests for parsing Assertion
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import org.scalatest.freespec.AnyFreeSpec

final class ParserParseAssertionSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  "The parser should build correct ASTs for Assertion" - {
    "assert statement with no message" in {
      "assert true;".asTree[Assertion] shouldBe {
        AssertionAssert(ExprInt(false, 1, 1), None)
      }
    }

    "assert statement with message" in {
      """assert false, "msg";""".asTree[Assertion] shouldBe {
        AssertionAssert(ExprInt(false, 1, 0), Some("msg"))
      }
    }

    "static assert with no message" in {
      "static assert false;".asTree[Assertion] shouldBe {
        AssertionStatic(ExprInt(false, 1, 0), None)
      }
    }

    "static assert with message" in {
      """static assert 2'd2, "msg";""".asTree[Assertion] shouldBe {
        AssertionStatic(ExprInt(false, 2, 2), Some("msg"))
      }
    }
  }
}
