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
// TreeTransformer tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._

import org.scalatest.FlatSpec
import com.argondesign.alogic.util.FollowedBy
import com.argondesign.alogic.core.Warning

final class TreeTransformerSpec extends FlatSpec with AlogicTest {

  implicit val cc = new CompilerContext

  "A TreeTransformer" should "rewrite all nodes where it applies" in {
    val tree = """|{
                  |  u1 foo;
                  |  u2 foo;
                  |}""".asTree[Stmt] rewrite new TreeTransformer {
      override def transform(tree: Tree) = tree match {
        case _: Ident => Sym(new TermSymbol(0)) withLoc tree.loc
        case other    => other
      }
    }

    tree should matchPattern {
      case StmtBlock(List(StmtDecl(Decl(Sym(_), _, _)), StmtDecl(Decl(Sym(_), _, _)))) =>
    }
  }

  it should "return the same tree instance if it is not rewritten" in {
    val oldTree = "{ a = b; bool c = a; }".asTree[Stmt]
    val newTree = oldTree rewrite new TreeTransformer with FollowedBy {
      override def transform(tree: Tree) = tree followedBy { cc.warning(tree, "Saw it") }
    }

    newTree should be theSameInstanceAs oldTree

    cc.messages should have length 11
    forAll(cc.messages) { msg => msg should beThe[Warning]("Saw it") }
  }

}
