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

import scala.collection.GenTraversableOnce

import Trees.Tree

trait TreeOps { this: Tree =>

  // Iterate all children of this node, including ones that are held through a list or option member
  def children: Iterator[Tree] = {
    val childLists = productIterator collect {
      case tree: Tree       => tree :: Nil
      case xs: List[_]      => xs collect { case tree: Tree => tree }
      case Some(tree: Tree) => tree :: Nil
    }
    childLists.flatten
  }

  ////////////////////////////////////////////////////////////////////////////////
  // visit methods
  ////////////////////////////////////////////////////////////////////////////////

  // Walk the tree in pre-order with partial function
  // Wherever the partial function is not defined, we recurse.
  // Wherever the partial function is defined, we apply it and stop recursion.
  // To continue recursing after application, the client can invoke visit
  // selectively, or visitChildren to visit all children.
  def visit(visitor: PartialFunction[Tree, Unit]): Unit = {
    def v(tree: Tree): Unit = {
      if (visitor.isDefinedAt(tree)) {
        visitor(tree)
      } else {
        tree.children foreach v
      }
    }
    v(this)
  }

  // Visit all children of this tree
  def visitChildren(visitor: PartialFunction[Tree, Unit]): Unit = {
    children foreach { _ visit visitor }
  }

  // Same as visit but always recurse through the whole tree
  def visitAll(visitor: PartialFunction[Tree, Unit]): Unit = {
    def v(tree: Tree): Unit = {
      if (visitor.isDefinedAt(tree)) {
        visitor(tree)
      }
      tree.children foreach v
    }
    v(this)
  }

  ////////////////////////////////////////////////////////////////////////////////
  // collect methods
  ////////////////////////////////////////////////////////////////////////////////

  // Collect results of walking the tree in pre-order with partial function
  // Wherever the partial function is not defined, we recurse.
  // Wherever the partial function is defined, we apply it, collect the result and stop recursion.
  def collect[E](pf: PartialFunction[Tree, E]): List[E] = {
    def c(tree: Tree): Iterator[E] = {
      if (pf.isDefinedAt(tree)) {
        Iterator.single(pf(tree))
      } else {
        tree.children flatMap c
      }
    }
    c(this).toList
  }

  // Collect all children of this tree
  def collectChildren[E](pf: PartialFunction[Tree, E]): List[E] = {
    (children flatMap { _ collect pf }).toList
  }

  // Same as collect but always recurse through the whole tree
  def collectAll[E](pf: PartialFunction[Tree, E]): List[E] = {
    def c(tree: Tree): Iterator[E] = {
      if (pf.isDefinedAt(tree)) {
        Iterator.single(pf(tree)) ++ (tree.children flatMap c)
      } else {
        tree.children flatMap c
      }
    }
    c(this).toList
  }

  ////////////////////////////////////////////////////////////////////////////////
  // flatCollect methods
  ////////////////////////////////////////////////////////////////////////////////

  def flatCollect[E](pf: PartialFunction[Tree, GenTraversableOnce[E]]): List[E] = {
    def c(tree: Tree): Iterator[E] = {
      if (pf.isDefinedAt(tree)) {
        pf(tree).toIterator
      } else {
        tree.children flatMap c
      }
    }
    c(this).toList
  }

  def flatCollectChildren[E](pf: PartialFunction[Tree, GenTraversableOnce[E]]): List[E] = {
    (children flatMap { _ flatCollect pf }).toList
  }

  def flatCollectAll[E](pf: PartialFunction[Tree, GenTraversableOnce[E]]): List[E] = {
    def c(tree: Tree): Iterator[E] = {
      if (pf.isDefinedAt(tree)) {
        pf(tree).toIterator ++ (tree.children flatMap c)
      } else {
        tree.children flatMap c
      }
    }
    c(this).toList
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Rewrie with TreeTransformer
  ////////////////////////////////////////////////////////////////////////////////

  def rewrite(tt: TreeTransformer): Tree = {
    tt.walk(this)
  }

}
