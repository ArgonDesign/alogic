////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Symbol table used for name resolution. Immutable.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.core.Symbols.Symbol

class SymbolTable private (
    private val local: Map[String, Symbol], // The map of names in the local scope
    val outer: Option[SymbolTable] // The symbol table of the enclosing scope, if any
  ) {

  import com.argondesign.alogic.frontend.SymbolTable.Definition
  import com.argondesign.alogic.frontend.SymbolTable.Local
  import com.argondesign.alogic.frontend.SymbolTable.Outer
  import com.argondesign.alogic.frontend.SymbolTable.Undefined

  // Look up symbol name in ascending scopes
  def get(name: String): Definition = local.get(name) map Local.apply getOrElse {
    outer match {
      case None => Undefined
      case Some(symtab) =>
        symtab.get(name) match {
          case Local(symbol) => Outer(symbol)
          case other         => other
        }
    }
  }

  // Create a new symbol table with this symbol table used as the outer scope.
  def push: SymbolTable = new SymbolTable(Map.empty, Some(this))

  // Create a new symbol table with the given 'name' -> 'symbol' mapping added
  // to the local scope.
  def updated(name: String, symbol: Symbol): SymbolTable =
    new SymbolTable(local.updated(name, symbol), outer)

  // Add a symbol the the local scope
  def +(symbol: Symbol): SymbolTable = updated(symbol.name, symbol)

  override def toString: String =
    local.mkString("{\n  ", "\n  ", "\n}") + (outer map { " -> " + _.toString } getOrElse "")

}

object SymbolTable {
  // Possible result of name lookup
  sealed trait Definition
  case class Local(symbol: Symbol) extends Definition
  case class Outer(symbol: Symbol) extends Definition
  case object Undefined extends Definition

  // Extractor for Local(_) or Outer(_)
  object Defined {

    def unapply(definition: Definition): Option[Symbol] = definition match {
      case Local(symbol) => Some(symbol)
      case Outer(symbol) => Some(symbol)
      case Undefined     => None
    }

  }

  // The empty symbol table
  val empty = new SymbolTable(Map.empty, None)
}
