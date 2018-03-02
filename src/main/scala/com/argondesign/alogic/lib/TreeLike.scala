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

package com.argondesign.alogic.lib

import scala.collection.GenTraversableOnce

// A TreeLike is characterised by the possession of a collection of other
// TreeLike objects called children. Note that it is *not* required that
// the object graph is actually a tree. In particular, isomorphic subtrees
// can be represented by multiple parents referring to the same child instance.
// The object graph however *must* be a DAG.
trait TreeLike extends Product {

  // The children of the node
  def children: Iterator[TreeLike]

  ////////////////////////////////////////////////////////////////////////////////
  // visit methods
  ////////////////////////////////////////////////////////////////////////////////

  // Walk the tree in pre-order with partial function
  // Wherever the partial function is not defined, we recurse.
  // Wherever the partial function is defined, we apply it and stop recursion.
  // To continue recursing after application, the client can invoke visit
  // explicitly on the children
  final def visit(visitor: PartialFunction[TreeLike, Unit]): Unit = {
    def v(tree: TreeLike): Unit = {
      if (visitor.isDefinedAt(tree)) {
        visitor(tree)
      } else {
        tree.children foreach v
      }
    }
    v(this)
  }

  // Same as visit but always recurse through the whole tree
  final def visitAll(visitor: PartialFunction[TreeLike, Unit]): Unit = {
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
  final def collect[E](pf: PartialFunction[TreeLike, E]): Iterator[E] = {
    def c(tree: TreeLike): Iterator[E] = {
      if (pf.isDefinedAt(tree)) {
        Iterator.single(pf(tree))
      } else {
        tree.children flatMap c
      }
    }
    c(this)
  }

  // Same as collect but always recurse through the whole tree
  final def collectAll[E](pf: PartialFunction[TreeLike, E]): Iterator[E] = {
    def c(tree: TreeLike): Iterator[E] = {
      if (pf.isDefinedAt(tree)) {
        Iterator.single(pf(tree)) ++ (tree.children flatMap c)
      } else {
        tree.children flatMap c
      }
    }
    c(this)
  }

  ////////////////////////////////////////////////////////////////////////////////
  // flatCollect methods
  ////////////////////////////////////////////////////////////////////////////////

  final def flatCollect[E](pf: PartialFunction[TreeLike, GenTraversableOnce[E]]): Iterator[E] = {
    def c(tree: TreeLike): Iterator[E] = {
      if (pf.isDefinedAt(tree)) {
        pf(tree).toIterator
      } else {
        tree.children flatMap c
      }
    }
    c(this)
  }

  def flatCollectAll[E](pf: PartialFunction[TreeLike, GenTraversableOnce[E]]): Iterator[E] = {
    def c(tree: TreeLike): Iterator[E] = {
      if (pf.isDefinedAt(tree)) {
        pf(tree).toIterator ++ (tree.children flatMap c)
      } else {
        tree.children flatMap c
      }
    }
    c(this)
  }

  ////////////////////////////////////////////////////////////////////////////////
  // collectFirst methods
  ////////////////////////////////////////////////////////////////////////////////

  final def collectFirst[E](pf: PartialFunction[TreeLike, E]): Option[E] = {
    def c(tree: TreeLike): Option[E] = {
      if (pf.isDefinedAt(tree)) {
        Some(pf(tree))
      } else {
        val a = tree.children flatMap c
        if (a.hasNext) Some(a.next()) else None
      }
    }
    c(this)
  }
}
