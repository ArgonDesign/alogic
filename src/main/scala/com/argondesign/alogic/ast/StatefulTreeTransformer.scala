////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  A stateful Tree transformer that walks and transforms according to 'skip',
//  'enter' and 'transform', but also provides:
//  - A standard symbol replacement mechanism via 'replace'
//  - Tracking of enclosing definition symbols
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Types.TypeEntity
import com.argondesign.alogic.core.Types.TypeType

import scala.collection.mutable

abstract class StatefulTreeTransformer(implicit cc: CompilerContext) extends TreeTransformer {

  //////////////////////////////////////////////////////////////////////////////
  // Transform specific interface overridable by sub-classes
  //////////////////////////////////////////////////////////////////////////////

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

  //////////////////////////////////////////////////////////////////////////////
  // Enclosing context tracker
  //////////////////////////////////////////////////////////////////////////////

  final private[this] val _enclosingSymbols = mutable.Stack[Symbol]()

  final protected[this] def enclosingSymbols: collection.Seq[Symbol] = _enclosingSymbols

  final protected[this] def withEnclosingSymbol[R](symbol: Symbol)(f: => R): R = {
    _enclosingSymbols.push(symbol)
    f
  } tap { _ =>
    _enclosingSymbols.pop()
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
        descend(cpyDecl) match {
          case decl: Decl => decl
          case _          => throw Ice("Replaced symbol Decl transformed into something unexpected")
        }
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
  final def walkTree(tree: Tree): Tree =
    if (skip(tree)) {
      tree
    } else {
      tree match {
        case sym @ Sym(symbol) if mustBeReplaced(symbol) =>
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

  final private def descend(tree: Tree): Tree =
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
              case Sym(symbol) => Some(symbol)
              case _           => None
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

  final override def defaultCheck(orig: Tree, tree: Tree): Unit = {
    assert(enclosingSymbols.isEmpty, this.getClass.getName + " " + enclosingSymbols)
    super.defaultCheck(orig, tree)
  }

}
