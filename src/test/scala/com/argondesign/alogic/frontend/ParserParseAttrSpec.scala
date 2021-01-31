////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Tests for parsing Attr (attributes)
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import org.scalatest.freespec.AnyFreeSpec

final class ParserParseAttrSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  "The parser should build correct ASTs for Attr (attributes)" - {
    "boolean attribute" in {
      "foo".asTree[Attr]() shouldBe {
        AttrBool("foo")
      }
    }

    "expression attribute" in {
      "foo = bar".asTree[Attr]() shouldBe {
        AttrExpr("foo", ExprIdent("bar", Nil))
      }
    }
  }
}
