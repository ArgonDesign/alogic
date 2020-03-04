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

import com.argondesign.alogic.antlr.AlogicParser._
import com.argondesign.alogic.antlr._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.frontend.Parser.Parseable
import com.argondesign.alogic.passes.AddCasts
import com.argondesign.alogic.passes.ReplaceUnaryTicks
import com.argondesign.alogic.passes.ResolvePolyFunc
import com.argondesign.alogic.transform.Regularize
import com.argondesign.alogic.util.unreachable

import scala.language.implicitConversions

trait TreeOps extends TreePrintOps { this: Tree =>

  //////////////////////////////////////////////////////////////////////////////
  // Trees nodes have a type 'tpe' which can be set once
  //////////////////////////////////////////////////////////////////////////////

  final private[this] var _tpe: Type = null

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
        val next = it.next
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
        val next = it.next
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
        val next = it.next
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
        val next = it.next
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
        val next = it.next
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
        val next = it.next
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
        val next = it.next
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
        val next = it.next
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
        val next = it.next
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
        val next = it.next
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
        val next = it.next
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

  def regularize(loc: Loc)(implicit cc: CompilerContext): this.type = {
    this rewrite new Regularize(loc)
    this
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Pretty print
  ////////////////////////////////////////////////////////////////////////////////

  def toPrettyString: String = {
    val sb = new StringBuilder
    var lvl = 0
    for (c <- this.toString) {
      sb append c
      c match {
        case '(' =>
          lvl += 1
          sb append ("\n" + "  " * lvl)
        case ')' =>
          lvl -= 1
        case ',' =>
          sb append ("\n" + "  " * lvl)
        case _ =>
      }
    }
    sb.toString
  }
}

//////////////////////////////////////////////////////////////////////////////
// Polymorphic extension methods on Trees
//////////////////////////////////////////////////////////////////////////////

final class TreeExt[T <: Tree](val tree: T) extends AnyVal {

  //////////////////////////////////////////////////////////////////////////////
  // Bring tree into a normal form that can be directly evaluated
  //////////////////////////////////////////////////////////////////////////////

  def normalize(implicit cc: CompilerContext): T =
    rewrite {
      new ReplaceUnaryTicks
    } rewrite {
      new ResolvePolyFunc
    } rewrite {
      new AddCasts
    }

  //////////////////////////////////////////////////////////////////////////////
  // Rewrite with TreeTransformer
  //////////////////////////////////////////////////////////////////////////////

  def rewrite(tt: TreeTransformer): T = tt(tree).asInstanceOf[T]
}

trait ObjectTreeOps {

  //////////////////////////////////////////////////////////////////////////////
  // Implicit dispatchers for any Tree that can be directly parsed by the parser
  //////////////////////////////////////////////////////////////////////////////

  implicit final val parseableRoot: Parseable[Root] = new Parseable[Root] {
    type C = RootContext
    def parse(parser: AlogicParser): RootContext = parser.root()
    def build(ctx: RootContext)(implicit cc: CompilerContext): Root = RootBuilder(ctx)
  }

  implicit final val parseableDesc: Parseable[Desc] = new Parseable[Desc] {
    type C = DescContext
    def parse(parser: AlogicParser): DescContext = parser.desc()
    def build(ctx: DescContext)(implicit cc: CompilerContext): Desc = DescBuilder(ctx)
  }

  implicit final val parseableRiz: Parseable[Riz] = new Parseable[Riz] {
    type C = RizContext
    def parse(parser: AlogicParser): RizContext = parser.riz()
    def build(ctx: RizContext)(implicit cc: CompilerContext): Riz = RizBuilder(ctx)
  }

  implicit final val parseableEnt: Parseable[Ent] = new Parseable[Ent] {
    type C = EntContext
    def parse(parser: AlogicParser): EntContext = parser.ent()
    def build(ctx: EntContext)(implicit cc: CompilerContext): Ent = EntBuilder(ctx)
  }

  implicit final val parseableRec: Parseable[Rec] = new Parseable[Rec] {
    type C = RecContext
    def parse(parser: AlogicParser): RecContext = parser.rec()
    def build(ctx: RecContext)(implicit cc: CompilerContext): Rec = RecBuilder(ctx)
  }

  implicit final val parseableGen: Parseable[Gen] = new Parseable[Gen] {
    type C = GenContext
    def parse(parser: AlogicParser): GenContext = parser.gen()
    def build(ctx: GenContext)(implicit cc: CompilerContext): Gen = GenBuilder(ctx)
  }

  implicit final val parseableStmt: Parseable[Stmt] = new Parseable[Stmt] {
    type C = StmtContext
    def parse(parser: AlogicParser): StmtContext = parser.stmt()
    def build(ctx: StmtContext)(implicit cc: CompilerContext): Stmt = StmtBuilder(ctx)
  }

  implicit final val parseableExpr: Parseable[Expr] = new Parseable[Expr] {
    type C = ExprContext
    def parse(parser: AlogicParser): ExprContext = parser.expr()
    def build(ctx: ExprContext)(implicit cc: CompilerContext): Expr = ExprBuilder(ctx)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Tree to TreeExt implicit conversion
  //////////////////////////////////////////////////////////////////////////////

  implicit def tree2TreeExt[T <: Tree](tree: T): TreeExt[T] = new TreeExt[T](tree)
}
