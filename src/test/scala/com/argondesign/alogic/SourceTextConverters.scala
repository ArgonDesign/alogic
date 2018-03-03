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
// Utility to be used in tests to build trees out of some text
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import com.argondesign.alogic.core.Source
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr.TypeBuilder
import com.argondesign.alogic.core.Types.Type
import com.argondesign.alogic.frontend.Parser
import com.argondesign.alogic.frontend.Builder

object SourceTextConverters {

  case class AsTreeSyntaxErrorException() extends Exception

  abstract class String2TreeAsTreeDispatcher[T <: Tree] {
    def dispatch(source: Source)(implicit cc: CompilerContext): T
  }

  implicit val string2TreeAsTreeDispatcherRoot = new String2TreeAsTreeDispatcher[Root] {
    def dispatch(source: Source)(implicit cc: CompilerContext): Root = {
      Builder(Parser[StartContext](source) getOrElse { throw AsTreeSyntaxErrorException() })
    }
  }

  implicit val string2TreeAsTreeDispatcherEntity = new String2TreeAsTreeDispatcher[Entity] {
    def dispatch(source: Source)(implicit cc: CompilerContext): Entity = {
      Builder(Parser[EntityContext](source) getOrElse { throw AsTreeSyntaxErrorException() })
    }
  }

  implicit val string2TreeAsTreeDispatcherTypeDefinition = new String2TreeAsTreeDispatcher[TypeDefinition] {
    def dispatch(source: Source)(implicit cc: CompilerContext): TypeDefinition = {
      Builder(Parser[Type_definitionContext](source) getOrElse { throw AsTreeSyntaxErrorException() })
    }
  }

  implicit val string2TreeAsTreeDispatcherDecl = new String2TreeAsTreeDispatcher[Decl] {
    def dispatch(source: Source)(implicit cc: CompilerContext): Decl = {
      Builder(Parser[DeclContext](source) getOrElse { throw AsTreeSyntaxErrorException() })
    }
  }

  implicit val string2TreeAsTreeDispatcherConnect = new String2TreeAsTreeDispatcher[Connect] {
    def dispatch(source: Source)(implicit cc: CompilerContext): Connect = {
      Builder(Parser[ConnectContext](source) getOrElse { throw AsTreeSyntaxErrorException() })
    }
  }

  implicit val string2TreeAsTreeDispatcherBlock = new String2TreeAsTreeDispatcher[StmtBlock] {
    def dispatch(source: Source)(implicit cc: CompilerContext): StmtBlock = {
      Builder(Parser[BlockContext](source) getOrElse { throw AsTreeSyntaxErrorException() })
    }
  }

  implicit val string2TreeAsTreeDispatcherStmt = new String2TreeAsTreeDispatcher[Stmt] {
    def dispatch(source: Source)(implicit cc: CompilerContext): Stmt = {
      Builder(Parser[StatementContext](source) getOrElse { throw AsTreeSyntaxErrorException() })
    }
  }

  implicit val string2TreeAsTreeDispatcherExpr = new String2TreeAsTreeDispatcher[Expr] {
    def dispatch(source: Source)(implicit cc: CompilerContext): Expr = {
      Builder(Parser[ExprContext](source) getOrElse { throw AsTreeSyntaxErrorException() })
    }
  }

  implicit class String2Repr(val string: String) {
    val source = {
      val text = if (string startsWith "|") string.stripMargin else string
      Source("nofile", text)
    }

    def asTree[T <: Tree](implicit cc: CompilerContext, dispatcher: String2TreeAsTreeDispatcher[T]): T = {
      dispatcher.dispatch(source)
    }

    def asType(implicit cc: CompilerContext): Type = {
      val ctx = Parser[KindContext](source) getOrElse { throw AsTreeSyntaxErrorException() }
      TypeBuilder(ctx)
    }
  }
}
