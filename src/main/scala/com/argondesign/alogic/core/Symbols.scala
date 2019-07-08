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
// Symbol representation and creation
////////////////////////////////////////////////////////////////////////////////

// A Symbol is a unique handle to the definition of a Name

package com.argondesign.alogic.core

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Names.Name
import com.argondesign.alogic.core.Names.TermName
import com.argondesign.alogic.core.Names.TypeName
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.core.Symbols.TypeSymbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.SequenceNumbers
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable
import scala.util.ChainingSyntax

trait Symbols extends ChainingSyntax { self: CompilerContext =>

  // The global scope only holds file level entity symbols
  final private[this] var _globalScope: Option[mutable.HashMap[Name, Symbol]] = Some(
    mutable.HashMap())

  // Can only hand out the final immutable copy
  final lazy val globalScope: Map[Name, Symbol] = {
    _globalScope.get.toMap
  } tap { _ =>
    _globalScope = None
  }

  // Add a symbol to the global scope, assuming it is still open
  final def addGlobalSymbol(symbol: Symbol): Unit = synchronized {
    _globalScope match {
      case None => ice("Global scope is already sealed")
      case Some(scope) => {
        val name = symbol.uniqueName
        if (scope contains name) {
          ice(s"Global scope already contains '${name}'")
        }
        scope(name) = symbol
      }
    }
  }

  final def addGlobalEntities(entities: Iterable[EntityIdent]): Unit = synchronized {
    for (entity <- entities) {
      val symbol = newTypeSymbol(entity.ident, TypeEntity("", Nil, Nil))
      addGlobalSymbol(symbol)
    }

    // Force value to seal global scope
    globalScope
  }

  final def addGlobalEntity(entity: EntityIdent): Unit = addGlobalEntities(List(entity))

  final def lookupGlobalTerm(name: String): TermSymbol = synchronized {
    globalScope.get(TermName(name)) match {
      case Some(symbol: TermSymbol) => symbol
      case Some(_)                  => unreachable
      case None                     => ice(s"Cannot find global term '${name}'")
    }
  }

  final def makeBuiltinCall(name: String, loc: Loc, args: List[Expr]): ExprCall = {
    val polySymbol = lookupGlobalTerm(name)
    assert(polySymbol.isBuiltin(this))
    val typed = args exists { _.hasTpe }
    val symbol = if (typed) {
      polySymbol.kind.asInstanceOf[TypePolyFunc].resolve(args).get
    } else {
      polySymbol
    }
    val call = ExprRef(symbol).call(args)(this)
    if (typed) {
      call.regularize(loc)(this)
    } else {
      call.assignLocs(loc)(this)
    }
  }

  final private[this] val symbolSequenceNumbers = new SequenceNumbers

  //////////////////////////////////////////////////////////////////////////////
  // Creating TermSymbol instances
  //////////////////////////////////////////////////////////////////////////////

  final def newTermSymbol(
      name: String,
      loc: Loc,
      kind: Type
  ): TermSymbol = synchronized {
    new TermSymbol(symbolSequenceNumbers.next, loc, kind, name)
  }

  final def newTermSymbol(ident: Ident, kind: Type): TermSymbol = {
    val symbol = newTermSymbol(ident.name, ident.loc, kind)
    symbol.attr.update(symbol, ident)(this)
    symbol
  }

  final def newSymbolLike(symbol: TermSymbol): TermSymbol = {
    val newSymbol = newTermSymbol(symbol.name, symbol.loc, symbol.kind)
    newSymbol.attr.update(symbol.attr)
    newSymbol
  }

  //////////////////////////////////////////////////////////////////////////////
  // Creating TypeSymbol instances
  //////////////////////////////////////////////////////////////////////////////

  final def newTypeSymbol(
      name: String,
      loc: Loc,
      kind: Type
  ): TypeSymbol = synchronized {
    new TypeSymbol(symbolSequenceNumbers.next, loc, kind, name)
  }

  final def newTypeSymbol(ident: Ident, kind: Type): TypeSymbol = {
    val symbol = newTypeSymbol(ident.name, ident.loc, kind)
    symbol.attr.update(symbol, ident)(this)
    symbol
  }

  final def newSymbolLike(symbol: TypeSymbol): TypeSymbol = {
    val newSymbol = newTypeSymbol(symbol.name, symbol.loc, symbol.kind)
    newSymbol.attr.update(symbol.attr)
    newSymbol
  }
}

object Symbols {

  abstract class Symbol(
      final val id: Int,
      final val loc: Loc,
      initialType: Type,
      initialName: Name
  ) {
    def isTermSymbol: Boolean

    def isTypeSymbol: Boolean

    ////////////////////////////////////////////////////////////////////////////
    // Common implementation
    ////////////////////////////////////////////////////////////////////////////

    final override def hashCode = id

    final override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]

    final var kind: Type = initialType

    protected final var _uniqueName: Name = initialName

    def uniqueName: Name = _uniqueName

    def name: String = _uniqueName.str

    def rename(name: String): this.type

    ////////////////////////////////////////////////////////////////////////////
    // Attributes
    ////////////////////////////////////////////////////////////////////////////

    final val attr: SymbolAttributes = new SymbolAttributes()

    ////////////////////////////////////////////////////////////////////////////

    // Is this a builtin symbol
    def isBuiltin(implicit cc: CompilerContext): Boolean = {
      cc.builtins exists { _ contains this }
    }
  }

  final class TermSymbol(id: Int, loc: Loc, kind: Type, name: String)
      extends Symbol(id, loc, kind, TermName(name)) {
    override def isTermSymbol = true
    override def isTypeSymbol = false

    override def rename(name: String): this.type = {
      _uniqueName = TermName(name)
      this
    }

    override def toString = s"TermSymbol(id=$id, name=${name})"
  }

  final class TypeSymbol(id: Int, loc: Loc, kind: Type, name: String)
      extends Symbol(id, loc, kind, TypeName(name)) {
    override def isTermSymbol = false
    override def isTypeSymbol = true

    override def rename(name: String): this.type = {
      _uniqueName = TypeName(name)
      this
    }

    override def toString = s"TypeSymbol(id=$id, name=${name})"
  }

  final object ErrorSymbol
      extends Symbol(-1, Loc.synthetic, TypeError, TermName("@error-symbol@")) {
    override def isTermSymbol = false
    override def isTypeSymbol = false

    override def rename(name: String): this.type = unreachable

    override def toString = s"ErrorSymbol"
  }
}
