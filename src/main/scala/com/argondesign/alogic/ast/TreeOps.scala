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
// Common members of ast.Trees.Tree
// These are factored out into a separate file to keep ast.Trees readable
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.antlr._
import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.SourceContext
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.frontend.Parser.Parseable
import com.argondesign.alogic.transform.Regularize
import com.argondesign.alogic.util.unreachable

import scala.language.implicitConversions

trait TreeOps extends TreePrintOps { this: Tree =>

  //////////////////////////////////////////////////////////////////////////////
  // Trees nodes have a type 'tpe' which can be set once
  //////////////////////////////////////////////////////////////////////////////

  final private var _tpe: Type = null

  final def hasTpe: Boolean = _tpe != null

  final def tpe: Type = if (hasTpe) _tpe else unreachable

  final def tpeOpt: Option[Type] = Option(_tpe)

  final def withTpe(kind: Type): this.type = {
    if (hasTpe) {
      unreachable
    } else {
      _tpe = kind
    }
    this
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Enumerations of the tree
  ////////////////////////////////////////////////////////////////////////////////

  final def preOrderIterator: Iterator[Tree] = {
    Iterator.single(this) ++ (children flatMap { _.preOrderIterator })
  }

  final def postOrderIterator: Iterator[Tree] = {
    (children flatMap { _.postOrderIterator }) ++ Iterator.single(this)
  }

  ////////////////////////////////////////////////////////////////////////////////
  // visit methods
  ////////////////////////////////////////////////////////////////////////////////

  // Walk the tree in pre-order with partial function
  // Wherever the partial function is not defined, we recurse.
  // Wherever the partial function is defined, we apply it and stop recursion.
  // To continue recursing after application, the client can invoke visit
  // explicitly on the children
  final def visit(pf: PartialFunction[Tree, Unit]): Unit = {
    def iterate(it: Iterator[Tree]): Unit = {
      if (it.hasNext) {
        val next = it.next()
        if (pf.isDefinedAt(next)) {
          pf(next)
        } else {
          iterate(next.children)
        }
        iterate(it)
      }
    }
    iterate(Iterator.single(this))
  }

  // Same as visit but always recurse through the whole tree
  // Note that this could be written as just:
  //   preOrder foreach { node => if (pf.isDefinedAt(node)) pf(node) }
  // Bit doing it directly as below avoids creating a lot of iterator
  // instances on the heap making iteration faster
  final def visitAll(pf: PartialFunction[Tree, Unit]): Unit = {
    def iterate(it: Iterator[Tree]): Unit = {
      if (it.hasNext) {
        val next = it.next()
        if (pf.isDefinedAt(next)) {
          pf(next)
        }
        iterate(next.children)
        iterate(it)
      }
    }
    iterate(Iterator.single(this))
  }

  ////////////////////////////////////////////////////////////////////////////////
  // collect methods
  ////////////////////////////////////////////////////////////////////////////////

  // Collect results of walking the tree in pre-order with partial function
  // Wherever the partial function is not defined, we recurse.
  // Wherever the partial function is defined, we apply it, collect the result and stop recursion.
  final def collect[E](pf: PartialFunction[Tree, E]): Iterator[E] = {
    def iterate(it: Iterator[Tree]): Iterator[E] = {
      if (it.hasNext) {
        val next = it.next()
        if (pf.isDefinedAt(next)) {
          Iterator.single(pf(next)) ++ iterate(it)
        } else {
          iterate(next.children ++ it)
        }
      } else {
        Iterator.empty
      }
    }
    iterate(Iterator.single(this))
  }

  // Same as collect but always recurse through the whole tree
  final def collectAll[E](pf: PartialFunction[Tree, E]): Iterator[E] = {
    def iterate(it: Iterator[Tree]): Iterator[E] = {
      if (it.hasNext) {
        val next = it.next()
        if (pf.isDefinedAt(next)) {
          Iterator.single(pf(next)) ++ iterate(next.children ++ it)
        } else {
          iterate(next.children ++ it)
        }
      } else {
        Iterator.empty
      }
    }
    iterate(Iterator.single(this))
  }

  ////////////////////////////////////////////////////////////////////////////////
  // flatCollect methods
  ////////////////////////////////////////////////////////////////////////////////

  final def flatCollect[E](pf: PartialFunction[Tree, IterableOnce[E]]): Iterator[E] = {
    def iterate(it: Iterator[Tree]): Iterator[E] = {
      if (it.hasNext) {
        val next = it.next()
        if (pf.isDefinedAt(next)) {
          pf(next).iterator ++ iterate(it)
        } else {
          iterate(next.children ++ it)
        }
      } else {
        Iterator.empty
      }
    }
    iterate(Iterator.single(this))
  }

  final def flatCollectAll[E](pf: PartialFunction[Tree, IterableOnce[E]]): Iterator[E] = {
    def iterate(it: Iterator[Tree]): Iterator[E] = {
      if (it.hasNext) {
        val next = it.next()
        if (pf.isDefinedAt(next)) {
          pf(next).iterator ++ iterate(next.children ++ it)
        } else {
          iterate(next.children ++ it)
        }
      } else {
        Iterator.empty
      }
    }
    iterate(Iterator.single(this))
  }

  ////////////////////////////////////////////////////////////////////////////////
  // collectFirst
  ////////////////////////////////////////////////////////////////////////////////

  final def collectFirst[E](pf: PartialFunction[Tree, E]): Option[E] = {
    def iterate(it: Iterator[Tree]): Option[E] = {
      if (it.hasNext) {
        val next = it.next()
        if (pf.isDefinedAt(next)) {
          Some(pf(next))
        } else {
          iterate(next.children) match {
            case None   => iterate(it)
            case result => result
          }
        }
      } else {
        None
      }
    }
    iterate(Iterator.single(this))
  }

  ////////////////////////////////////////////////////////////////////////////////
  // getFirst
  ////////////////////////////////////////////////////////////////////////////////

  final def getFirst[E](pf: PartialFunction[Tree, E]): E = this.collectFirst(pf).get

  ////////////////////////////////////////////////////////////////////////////////
  // forall predicate tests
  ////////////////////////////////////////////////////////////////////////////////

  // Note that these also use partial functions and do not check nodes where they do not apply

  final def forall(pf: PartialFunction[Tree, Boolean]): Boolean = {
    def iterate(it: Iterator[Tree]): Boolean = {
      if (it.hasNext) {
        val next = it.next()
        if (pf.isDefinedAt(next)) {
          pf(next) && iterate(it)
        } else {
          iterate(next.children) && iterate(it)
        }
      } else {
        true
      }
    }
    iterate(Iterator.single(this))
  }

  final def forallAll(pf: PartialFunction[Tree, Boolean]): Boolean = {
    def iterate(it: Iterator[Tree]): Boolean = {
      if (it.hasNext) {
        val next = it.next()
        if (pf.isDefinedAt(next)) {
          pf(next) && iterate(next.children) && iterate(it)
        } else {
          iterate(next.children) && iterate(it)
        }
      } else {
        true
      }
    }
    iterate(Iterator.single(this))
  }

  ////////////////////////////////////////////////////////////////////////////////
  // exists predicate tests
  ////////////////////////////////////////////////////////////////////////////////

  // Note that these also use partial functions and do not check nodes where they do not apply

  // Check in *some* order that predicate holds for at least one node
  final def exists(pf: PartialFunction[Tree, Boolean]): Boolean = {
    def iterate(it: Iterator[Tree]): Boolean = {
      if (it.hasNext) {
        val next = it.next()
        if (pf.isDefinedAt(next)) {
          pf(next) || iterate(it)
        } else {
          iterate(next.children) || iterate(it)
        }
      } else {
        false
      }
    }
    iterate(Iterator.single(this))
  }

  final def existsAll(pf: PartialFunction[Tree, Boolean]): Boolean = {
    def iterate(it: Iterator[Tree]): Boolean = {
      if (it.hasNext) {
        val next = it.next()
        if (pf.isDefinedAt(next)) {
          pf(next) || iterate(next.children) || iterate(it)
        } else {
          iterate(next.children) || iterate(it)
        }
      } else {
        false
      }
    }
    iterate(Iterator.single(this))
  }

  ////////////////////////////////////////////////////////////////////////////
  // Regularize the tree
  ////////////////////////////////////////////////////////////////////////////

  def regularize(loc: Loc): this.type = {
    if (this ne Stump) {
      this rewrite new Regularize(loc)
    }
    this
  }

}

//////////////////////////////////////////////////////////////////////////////
// Polymorphic extension methods on Trees
//////////////////////////////////////////////////////////////////////////////

final class TreeExt[T <: Tree](val tree: T) extends AnyVal {

  //////////////////////////////////////////////////////////////////////////////
  // Rewrite with TreeTransformer
  //////////////////////////////////////////////////////////////////////////////

  def rewrite(tt: TreeTransformer): T = tt(tree).asInstanceOf[T]
}

trait ObjectTreeOps {

  //////////////////////////////////////////////////////////////////////////////
  // Typeclass instances for Parseable[_ <: Tree]
  //////////////////////////////////////////////////////////////////////////////

  implicit object parseableFile extends Parseable[DescPackage] {
    type C = FileContext
    def parse(parser: AlogicParser): FileContext = parser.file()
    def build(ctx: FileContext)(implicit mb: MessageBuffer, sc: SourceContext): DescPackage =
      PackageBuilder(ctx)
  }

  implicit object parseableDesc extends Parseable[Desc] {
    type C = DescContext
    def parse(parser: AlogicParser): DescContext = parser.desc()
    def build(ctx: DescContext)(implicit mb: MessageBuffer, sc: SourceContext): Desc =
      DescBuilder(ctx)
  }

  implicit object parseableAttr extends Parseable[Attr] {
    type C = AttrContext
    def parse(parser: AlogicParser): AttrContext = parser.attr()
    def build(ctx: AttrContext)(implicit mb: MessageBuffer, sc: SourceContext): Attr =
      AttrBuilder(ctx)
  }

  implicit object parseableImport extends Parseable[Import] {
    type C = ImprtContext
    def parse(parser: AlogicParser): ImprtContext = parser.imprt()
    def build(ctx: ImprtContext)(implicit mb: MessageBuffer, sc: SourceContext): Import =
      ImportBuilder(ctx)
  }

  implicit object parseableUsing extends Parseable[Using] {
    type C = UsngContext
    def parse(parser: AlogicParser): UsngContext = parser.usng()
    def build(ctx: UsngContext)(implicit mb: MessageBuffer, sc: SourceContext): Using =
      UsingBuilder(ctx)
  }

  implicit object parseableFrom extends Parseable[From] {
    type C = FromContext
    def parse(parser: AlogicParser): FromContext = parser.from()
    def build(ctx: FromContext)(implicit mb: MessageBuffer, sc: SourceContext): From =
      FromBuilder(ctx)
  }

  implicit object parseableAssertion extends Parseable[Assertion] {
    type C = AssertionContext
    def parse(parser: AlogicParser): AssertionContext = parser.assertion()
    def build(ctx: AssertionContext)(implicit mb: MessageBuffer, sc: SourceContext): Assertion =
      AssertionBuilder(ctx)
  }

  implicit object parseablePkg extends Parseable[Pkg] {
    type C = PkgContext
    def parse(parser: AlogicParser): PkgContext = parser.pkg()
    def build(ctx: PkgContext)(implicit mb: MessageBuffer, sc: SourceContext): Pkg =
      PkgBuilder(ctx)
  }

  implicit object parseableEnt extends Parseable[Ent] {
    type C = EntContext
    def parse(parser: AlogicParser): EntContext = parser.ent()
    def build(ctx: EntContext)(implicit mb: MessageBuffer, sc: SourceContext): Ent =
      EntBuilder(ctx)
  }

  implicit object parseableRec extends Parseable[Rec] {
    type C = RecContext
    def parse(parser: AlogicParser): RecContext = parser.rec()
    def build(ctx: RecContext)(implicit mb: MessageBuffer, sc: SourceContext): Rec =
      RecBuilder(ctx)
  }

  implicit object parseableStmt extends Parseable[Stmt] {
    type C = StmtContext
    def parse(parser: AlogicParser): StmtContext = parser.stmt()
    def build(ctx: StmtContext)(implicit mb: MessageBuffer, sc: SourceContext): Stmt =
      StmtBuilder(ctx)
  }

  implicit object parseableExpr extends Parseable[Expr] {
    type C = ExprContext
    def parse(parser: AlogicParser): ExprContext = parser.expr()
    def build(ctx: ExprContext)(implicit mb: MessageBuffer, sc: SourceContext): Expr =
      ExprBuilder(ctx)
  }

  implicit object parseableArg extends Parseable[Arg] {
    type C = ArgContext
    def parse(parser: AlogicParser): ArgContext = parser.arg()
    def build(ctx: ArgContext)(implicit mb: MessageBuffer, sc: SourceContext): Arg =
      ArgBuilder(ctx)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Tree to TreeExt implicit conversion
  //////////////////////////////////////////////////////////////////////////////

  implicit def tree2TreeExt[T <: Tree](tree: T): TreeExt[T] = new TreeExt[T](tree)
}
