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
// Base trait for any tree-like data structure.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import scala.collection.GenTraversableOnce

trait TreeLike extends Product {

  // Iterate all children of this node, including ones that are held through a list or option member
  def children: Iterator[TreeLike] = {
    val childLists = productIterator collect {
      case tree: TreeLike       => tree :: Nil
      case xs: List[_]          => xs collect { case tree: TreeLike => tree }
      case Some(tree: TreeLike) => tree :: Nil
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
  def visit(visitor: PartialFunction[TreeLike, Unit]): Unit = {
    def v(tree: TreeLike): Unit = {
      if (visitor.isDefinedAt(tree)) {
        visitor(tree)
      } else {
        tree.children foreach v
      }
    }
    v(this)
  }

  // Visit all children of this tree
  def visitChildren(visitor: PartialFunction[TreeLike, Unit]): Unit = {
    children foreach { _ visit visitor }
  }

  // Same as visit but always recurse through the whole tree
  def visitAll(visitor: PartialFunction[TreeLike, Unit]): Unit = {
    def v(tree: TreeLike): Unit = {
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
  def collect[E](pf: PartialFunction[TreeLike, E]): List[E] = {
    def c(tree: TreeLike): Iterator[E] = {
      if (pf.isDefinedAt(tree)) {
        Iterator.single(pf(tree))
      } else {
        tree.children flatMap c
      }
    }
    c(this).toList
  }

  // Collect all children of this tree
  def collectChildren[E](pf: PartialFunction[TreeLike, E]): List[E] = {
    (children flatMap { _ collect pf }).toList
  }

  // Same as collect but always recurse through the whole tree
  def collectAll[E](pf: PartialFunction[TreeLike, E]): List[E] = {
    def c(tree: TreeLike): Iterator[E] = {
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

  def flatCollect[E](pf: PartialFunction[TreeLike, GenTraversableOnce[E]]): List[E] = {
    def c(tree: TreeLike): Iterator[E] = {
      if (pf.isDefinedAt(tree)) {
        pf(tree).toIterator
      } else {
        tree.children flatMap c
      }
    }
    c(this).toList
  }

  def flatCollectChildren[E](pf: PartialFunction[TreeLike, GenTraversableOnce[E]]): List[E] = {
    (children flatMap { _ flatCollect pf }).toList
  }

  def flatCollectAll[E](pf: PartialFunction[TreeLike, GenTraversableOnce[E]]): List[E] = {
    def c(tree: TreeLike): Iterator[E] = {
      if (pf.isDefinedAt(tree)) {
        pf(tree).toIterator ++ (tree.children flatMap c)
      } else {
        tree.children flatMap c
      }
    }
    c(this).toList
  }
}
