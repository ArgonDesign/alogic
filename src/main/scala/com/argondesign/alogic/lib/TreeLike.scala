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
  // Enumerations of the tree
  ////////////////////////////////////////////////////////////////////////////////

  final def preOrder: Iterator[TreeLike] = {
    Iterator.single(this) ++ (children flatMap { _.preOrder })
  }

  final def postOrder: Iterator[TreeLike] = {
    (children flatMap { _.postOrder }) ++ Iterator.single(this)
  }

  ////////////////////////////////////////////////////////////////////////////////
  // visit methods
  ////////////////////////////////////////////////////////////////////////////////

  // Walk the tree in pre-order with partial function
  // Wherever the partial function is not defined, we recurse.
  // Wherever the partial function is defined, we apply it and stop recursion.
  // To continue recursing after application, the client can invoke visit
  // explicitly on the children
  final def visit(pf: PartialFunction[TreeLike, Unit]): Unit = {
    def iterate(it: Iterator[TreeLike]): Unit = {
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
  final def visitAll(pf: PartialFunction[TreeLike, Unit]): Unit = {
    def iterate(it: Iterator[TreeLike]): Unit = {
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
  final def collect[E](pf: PartialFunction[TreeLike, E]): Iterator[E] = {
    def iterate(it: Iterator[TreeLike]): Iterator[E] = {
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
  final def collectAll[E](pf: PartialFunction[TreeLike, E]): Iterator[E] = {
    def iterate(it: Iterator[TreeLike]): Iterator[E] = {
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

  final def flatCollect[E](pf: PartialFunction[TreeLike, GenTraversableOnce[E]]): Iterator[E] = {
    def iterate(it: Iterator[TreeLike]): Iterator[E] = {
      if (it.hasNext) {
        val next = it.next
        if (pf.isDefinedAt(next)) {
          pf(next).toIterator ++ iterate(it)
        } else {
          iterate(next.children ++ it)
        }
      } else {
        Iterator.empty
      }
    }
    iterate(Iterator.single(this))
  }

  def flatCollectAll[E](pf: PartialFunction[TreeLike, GenTraversableOnce[E]]): Iterator[E] = {
    def iterate(it: Iterator[TreeLike]): Iterator[E] = {
      if (it.hasNext) {
        val next = it.next
        if (pf.isDefinedAt(next)) {
          pf(next).toIterator ++ iterate(next.children ++ it)
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
  // collectFirst methods
  ////////////////////////////////////////////////////////////////////////////////

  final def collectFirst[E](pf: PartialFunction[TreeLike, E]): Option[E] = {
    def iterate(it: Iterator[TreeLike]): Option[E] = {
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
}
