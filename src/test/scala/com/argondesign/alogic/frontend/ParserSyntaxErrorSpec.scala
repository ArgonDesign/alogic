////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Tests for syntax errors
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import org.scalatest.freespec.AnyFreeSpec

final class ParserSyntaxErrorSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  "The parser should yield a syntax error on some invalid input" - {
    "incomplete" in {
      an[AsTreeSyntaxErrorException] shouldBe thrownBy {
        "fsm".asTree[DescPackage]()
      }
      cc.messages.loneElement should beSyntaxError
    }

    "empty let statement" in {
      an[AsTreeSyntaxErrorException] shouldBe thrownBy {
        "let () for(a=1;a;a--) { a+=b; }".asTree[Stmt]()
      }
      cc.messages should not be empty
      cc.messages(0) should beSyntaxError("empty 'let ()' statement")
    }

    "empty do loop" in {
      an[AsTreeSyntaxErrorException] shouldBe thrownBy {
        "do { a=1; } while();".asTree[Stmt]()
      }
      cc.messages.loneElement should beSyntaxError("empty 'while ()' condition")
    }

    "empty while loop" in {
      an[AsTreeSyntaxErrorException] shouldBe thrownBy {
        "while ()".asTree[Stmt]()
      }
      cc.messages should not be empty
      cc.messages(0) should beSyntaxError("empty 'while ()' condition")
    }

    "empty concatenation" in {
      an[AsTreeSyntaxErrorException] shouldBe thrownBy {
        "{}".asTree[Expr]()
      }
      cc.messages should not be empty
    }

    "missing closing brace" in {
      an[AsTreeSyntaxErrorException] shouldBe thrownBy {
        "a = {".asTree[Stmt]()
      }
      cc.messages should not be empty
    }

    "missing opening brace" in {
      an[AsTreeSyntaxErrorException] shouldBe thrownBy {
        "a = }".asTree[Stmt]()
      }
      cc.messages should not be empty
    }

    // TODO: Mandatory blocks

    "gen for with" - {
      "empty decl" in {
        an[AsTreeSyntaxErrorException] shouldBe thrownBy {
          "gen for(;1;1++){}".asTree[Desc]()
        }
      }

      "empty cond" in {
        an[AsTreeSyntaxErrorException] shouldBe thrownBy {
          "gen for(uint a = 0;;1++){}".asTree[Desc]()
        }
      }

      "empty step" in {
        an[AsTreeSyntaxErrorException] shouldBe thrownBy {
          "gen for(uint a = 0;1;){}".asTree[Desc]()
        }
      }
    }
  }
}
