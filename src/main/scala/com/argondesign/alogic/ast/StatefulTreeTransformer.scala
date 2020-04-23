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
// A Tree transformer used to modify Trees
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types.TypeEntity
import com.argondesign.alogic.core.Types.TypeType
import com.argondesign.alogic.typer.TypeAssigner

import scala.collection.mutable

abstract class StatefulTreeTransformer(implicit cc: CompilerContext) extends TreeTransformer {

  //////////////////////////////////////////////////////////////////////////////
  // Transform specific interface to be filled in by sub-classes
  //////////////////////////////////////////////////////////////////////////////

  // 'skip' is a predicate that can be used to mark subtrees that should not be
  // visited. If 'skip' returns true for a node, that node will not be visited,
  // i.e.: enter and transform will not be called on that node, or any of their
  // children, leaving the subtree unmodified
  protected def skip(tree: Tree): Boolean = false

  // 'replace' is a predicate indicating which symbols need to be replaced with
  // a new symbol. This is useful for changing the types of symbols in a
  // consistent manner. Symbols which are replaced will have any containing
  // Decl/Defn/Sym/ExprSym nodes copied to use a fresh symbol and this
  // replacement node is walked instead of the original node. For typed
  // transformers, the first time a symbol is encountered, it's replaced Decl
  // is walked first in order to ensure the type of the symbol is known. Note
  // That this can cause the Decls of replaced symbols to be walked out of
  // order.
  protected def replace(symbol: Symbol): Boolean = false

  // 'enter' is called on each non-skipped node, in pre-order (before visiting
  // any of their children) when the node is first encountered. 'enter' is used
  // to modify the state of the TreeTransformer or the context before
  // transforming children. When 'enter' returns Some(node), the node 'enter'
  // is called with is immediately replaced with the returned node, without
  // visiting the children or calling transform on the node, effectively
  // preempting traversal of the subtree below the given tree. If 'enter'
  // returns None, traversal continues by transforming all children and the
  // entered node.
  protected def enter(tree: Tree): Option[Tree] = None

  // 'transform' is called on each non-skipped node, in post-order (after all
  // children have been transformed). 'transform' us used to modify tree nodes.
  // When 'transform' is called, all child nodes have already been transformed
  // using the same function.
  protected def transform(tree: Tree): Tree = tree

  //////////////////////////////////////////////////////////////////////////////
  // Enclosing context tracker
  //////////////////////////////////////////////////////////////////////////////

  final private[this] val _enclosingSymbols = mutable.Stack[Symbol]()

  final protected[this] def enclosingSymbols: collection.Seq[Symbol] = _enclosingSymbols

  final protected[this] def withEnclosingSymbol[R](symbol: Symbol)(f: => R): R = {
    _enclosingSymbols push symbol
    f
  } tap { _ =>
    _enclosingSymbols.pop
  }

  // The Symbol of the closest enclosing entity
  final protected[this] def entitySymbol: Symbol =
    (_enclosingSymbols find {
      _.kind match {
        case TypeType(_: TypeEntity) => true
        case _                       => false
      }
    }).get

  //////////////////////////////////////////////////////////////////////////////
  // Symbol replacement implementation
  //////////////////////////////////////////////////////////////////////////////

  // Map from replacement symbol to original symbol
  final protected val orig: mutable.Map[Symbol, Symbol] = mutable.Map()

  // Replacement symbol for original symbol, if exists
  final protected def repl(symbol: Symbol): Option[Symbol] = _replacementDecl.get(symbol) map {
    _.symbol
  }

  // Map from replacement symbol to its declaration
  final private val _replacementDecl: mutable.Map[Symbol, Decl] = mutable.Map()

  final private[this] def replacementDecl(symbol: Symbol): Decl = {
    // First time we encounter a symbol that is replaced, we create a new
    // symbol and process it's declaration so it's type is known.
    _replacementDecl.getOrElseUpdate(
      symbol, {
        // Create the fresh symbol
        val newSymbol = symbol.dup
        // Add it to the reverse map
        orig(newSymbol) = symbol
        // Copy the decl replacing the symbol with the fresh symbol
        val cpyDecl = TypeAssigner(symbol.decl.cpy(symbol = newSymbol) withLoc symbol.decl.loc)
        // Transform the replaced decl immediately to ensure types are known
        descend(cpyDecl).asInstanceOf[Decl]
      }
    )
  }

  final private[this] def replacementSymbol(symbol: Symbol): Symbol = replacementDecl(symbol).symbol

  final private[this] def mustBeReplaced(symbol: Symbol): Boolean = {
    !(orig contains symbol) && ((_replacementDecl contains symbol) || replace(symbol))
  }

  //////////////////////////////////////////////////////////////////////////////
  // Implementation of walkTree
  //////////////////////////////////////////////////////////////////////////////

  // Walk single node
  def walkTree(tree: Tree): Tree =
    if (skip(tree)) {
      tree
    } else {
      tree match {
        case sym @ Sym(symbol, _) if mustBeReplaced(symbol) =>
          assert(typed)
          descend {
            TypeAssigner(sym.copy(symbol = replacementSymbol(symbol)) withLoc tree.loc)
          }
        case Decl(symbol) if mustBeReplaced(symbol) =>
          assert(typed)
          replacementDecl(symbol)
        case defn @ Defn(symbol) if mustBeReplaced(symbol) =>
          assert(typed)
          descend {
            TypeAssigner(defn.cpy(symbol = replacementSymbol(symbol)) withLoc tree.loc)
          }
        case ExprSym(symbol) if mustBeReplaced(symbol) =>
          assert(typed)
          descend {
            TypeAssigner(ExprSym(replacementSymbol(symbol)) withLoc tree.loc)
          }
        case other => descend(other)
      }
    }

  final def descend(tree: Tree): Tree =
    // Call enter in pre order
    enter(tree) match {
      // 'enter' provided replacement, use it
      case Some(replacement) => replacement
      // 'enter' did not provide replacement, traverse the tree.
      case None =>
        // Work out introduced symbol
        val introducedSymbol = tree match {
          case desc: Desc =>
            desc.ref match {
              case Sym(symbol, _) => Some(symbol)
              case _              => None
            }
          case Decl(symbol) => Some(symbol)
          case Defn(symbol) => Some(symbol)
          case _            => None
        }
        // Push the introduced symbol to the enclosing symbol stack
        introducedSymbol foreach { symbol =>
          _enclosingSymbols.push(symbol)
        }
        // Walk children
        val walked = walkChildren(tree)

        // Pop the introduced symbol from the enclosing symbol stack
        introducedSymbol foreach { _ =>
          _enclosingSymbols.pop()
        }

        // Apply transform
        walked match {
          case Thicket(ts) => Thicket(transform(ts))
          case Stump       => Stump
          case other       => transform(other)
        }
    }

  //////////////////////////////////////////////////////////////////////////////
  // Internals
  //////////////////////////////////////////////////////////////////////////////

  final private def transform(trees: List[Tree]): List[Tree] = trees flatMap { t =>
    transform(t) match {
      case Thicket(results) => results
      case result           => List(result)
    }
  }

  final override def defaultCheck(orig: Tree, tree: Tree): Unit = {
    assert(enclosingSymbols.isEmpty, this.getClass.getName + " " + enclosingSymbols)
    super.defaultCheck(orig, tree)
  }

}
