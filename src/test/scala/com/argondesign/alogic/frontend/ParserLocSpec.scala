////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Tests for locations set by parser
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FuncVariant
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.enums.EntityVariant
import org.scalatest.freespec.AnyFreeSpec

final class ParserLocSpec extends AnyFreeSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  "The parser should assign correct locations to tree nodes" - {

    "simple" in {
      val tree = """fsm foo {
                   |  void main() {
                   |    bar i;
                   |    loop { }
                   |  }
                   |}""".stripMargin.asTree[Desc]()

      inside(tree) {
        case entity @ DescEntity(_, _, EntityVariant.Fsm, eBody) =>
          entity.loc.line shouldBe 1
          entity.loc.file shouldBe "<asTree>"
          inside(eBody.loneElement) {
            case function @ EntSplice(
                  DescFunc(_, _, FuncVariant.Ctrl, ExprType(TypeVoid), Nil, fBody)
                ) =>
              function.loc.line shouldBe 2
              function.loc.file shouldBe "<asTree>"
              inside(fBody(0)) {
                case stmtDesc: StmtSplice =>
                  stmtDesc.loc.line shouldBe 3
                  stmtDesc.loc.file shouldBe "<asTree>"
                  inside(stmtDesc.tree) {
                    case desc @ DescVar(ident: Ident, _, spec, None) =>
                      desc.loc.line shouldBe 3
                      desc.loc.file shouldBe "<asTree>"
                      ident.loc.line shouldBe 3
                      ident.loc.file shouldBe "<asTree>"
                      inside(spec) {
                        case ident @ ExprIdent("bar", Nil) =>
                          ident.loc.line shouldBe 3
                          ident.loc.file shouldBe "<asTree>"
                      }
                  }
              }
              inside(fBody(1)) {
                case stmtLoop: StmtLoop =>
                  stmtLoop.loc.line shouldBe 4
                  stmtLoop.loc.file shouldBe "<asTree>"
                  stmtLoop.body shouldBe empty
              }
          }
      }

      cc.messages shouldBe empty
    }

    "with line directives" in {
      val tree = """fsm foo {
                   |#line 20 "foo.bar"
                   |  void main() {
                   |#line 100 // Comment
                   |    bar
                   |    i;
                   |#line 2 /* Comment */ "another"
                   |    loop { }
                   |  }
                   |}""".stripMargin.asTree[Desc]()

      inside(tree) {
        case entity @ DescEntity(_, _, EntityVariant.Fsm, eBody) =>
          entity.loc.line shouldBe 1
          entity.loc.file shouldBe "<asTree>"
          inside(eBody.loneElement) {
            case function @ EntSplice(
                  DescFunc(_, _, FuncVariant.Ctrl, ExprType(TypeVoid), Nil, fBody)
                ) =>
              function.loc.line shouldBe 20
              function.loc.file shouldBe "foo.bar"
              inside(fBody(0)) {
                case stmtDesc: StmtSplice =>
                  stmtDesc.loc.line shouldBe 100
                  stmtDesc.loc.file shouldBe "foo.bar"
                  inside(stmtDesc.tree) {
                    case desc @ DescVar(ident: Ident, _, spec, None) =>
                      desc.loc.line shouldBe 100
                      desc.loc.file shouldBe "foo.bar"
                      ident.loc.line shouldBe 101
                      ident.loc.file shouldBe "foo.bar"
                      inside(spec) {
                        case ident @ ExprIdent("bar", Nil) =>
                          ident.loc.line shouldBe 100
                          ident.loc.file shouldBe "foo.bar"
                      }
                  }
              }
              inside(fBody(1)) {
                case stmtLoop: StmtLoop =>
                  stmtLoop.loc.line shouldBe 2
                  stmtLoop.loc.file shouldBe "another"
                  stmtLoop.body shouldBe empty
              }
          }
      }

      cc.messages shouldBe empty
    }
  }
}
