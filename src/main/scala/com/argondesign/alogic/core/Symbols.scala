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

import scala.collection.mutable
import scala.language.implicitConversions

import Denotations.Denotation
import Denotations.TermDenotation
import Denotations.TypeDenotation
import Names.TermName
import Names.TypeName
import Symbols.Symbol
import Symbols.TermSymbol
import Symbols.TypeSymbol

trait Symbols { self: CompilerContext =>

  private[this] val symbolSequenceNumbers = Stream.from(0).iterator

  protected val symbolLocations = mutable.Map[Symbol, Loc]()

  protected var symbolDenotationStack = List[mutable.Map[Symbol, Denotation]]() // scalastyle:ignore var.field

  protected def symbolDenotations = symbolDenotationStack.head

  def newSymbol(loc: Loc, name: TermName): TermSymbol = synchronized {
    val symbol = new TermSymbol(symbolSequenceNumbers.next)
    val denot = TermDenotation(name)
    symbolLocations(symbol) = loc
    symbolDenotations(symbol) = denot
    symbol
  }

  def newSymbol(loc: Loc, name: TypeName): TypeSymbol = synchronized {
    val symbol = new TypeSymbol(symbolSequenceNumbers.next)
    val denot = TypeDenotation(name)
    symbolLocations(symbol) = loc
    symbolDenotations(symbol) = denot
    symbol
  }

}

object Symbols {

  abstract trait Symbol extends Any {
    type ThisDenotation
    def id: Int
    def denot(implicit cc: CompilerContext): ThisDenotation = {
      cc.symbolDenotations(this).asInstanceOf[ThisDenotation] // scalastyle:ignore
    }
  }

  class TermSymbol(val id: Int) extends AnyVal with Symbol {
    type ThisDenotation = TermDenotation
  }
  class TypeSymbol(val id: Int) extends AnyVal with Symbol {
    type ThisDenotation = TypeDenotation
  }

  implicit def toLoc(symbol: Symbol)(implicit cc: CompilerContext): Loc = cc.symbolLocations(symbol)
}
