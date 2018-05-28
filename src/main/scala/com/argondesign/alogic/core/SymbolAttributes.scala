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
// Collection of all symbol attributes
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Symbols._

class SymbolAttributes {
  // Symbol is meant to be unused, do not warn
  val unused = new Attribute[Boolean]()

  // Is this a toplevel entity
  val topLevel = new Attribute[Boolean]()
  // Is this an entry point function
  val entry = new Attribute[Boolean]()

//  // All possible parameter bindings of an entity symbol
//  val paramBindings = new Attribute[List[Map[TermSymbol, Expr]]]()
//  // The actual parameter bindings of an instance symbol
//  val paramBinding = new Attribute[Map[TermSymbol, Expr]]()
//  // The default parameter bindings, in definition order
//  val defaultParamBindings = new Attribute[ListMap[TermSymbol, Expr]]()
  // The entity that owns the definition of this symbol
  val owner = new Attribute[Entity]()
//  // If this is a parametrized entity symbol,
//  // a map from parameter bindings to the specialized entity
//  val specMap = new Attribute[Map[ListMap[TermSymbol, Expr], Entity]]()

  // The initializer expression, from the declaration, if there was one
  val init = new Attribute[Expr]()

  // Entity call stack limit
  val stackLimit = new Attribute[Expr]()
  // Function recursion limit
  val recLimit = new Attribute[Expr]()
  // The return stack symbol, if this is an entity
  val returnStack = new Attribute[TermSymbol]()
  // The state variable symbol, if this is an entity
  val stateVar = new Attribute[TermSymbol]()

  // Is this a FlowControlTypeNone port?
  val fcn = new Attribute[Boolean]()
  // If this is a FlowControlTypeValid port,
  // the corresponding payload and valid symbols
  val fcv = new Attribute[(Symbol, TermSymbol)]()
  // If this is a FlowControlTypeReady port,
  // the corresponding payload, valid and ready symbols
  val fcr = new Attribute[(Symbol, TermSymbol, TermSymbol)]()
  // If this is a FlowControlTypeAccept port,
  // the corresponding payload, valid and accept symbols
  val fca = new Attribute[(Symbol, TermSymbol, TermSymbol)]()
  // Is this a port that has been expanded to multiple signals?
  val expandedPort = new Attribute[Boolean]()

  // If the state system stalls, set this signal to all zeros
  val clearOnStall = new Attribute[Boolean]()
  // If this is an entity symbol, then these are further (instance, portname)
  // pairs (driven from the state system) that need to be cleared on a stall
  val interconnectClearOnStall = new Attribute[List[(TermSymbol, String)]]()

  // If the value of ExprSym(symbol) in this attribute is 0,
  // then the value of this signal is known to be don't care
  val dontCareUnless = new Attribute[TermSymbol]()

  // The output register symbol if this is a registered output port
  val oReg = new Attribute[TermSymbol]()

  // If this is flop _q symbol, the corresponding _d symbol
  val flop = new Attribute[TermSymbol]()

  // If this is a memory _q symbol, the corresponding we, waddr and wdata symbols
  val memory = new Attribute[(TermSymbol, TermSymbol, TermSymbol)]()

  // If this is an interconnect signal, the corresponding instance symbol and port name
  val interconnect = new Attribute[(TermSymbol, String)]()

  // If this signal is a combinatorically driven local signal
  val combSignal = new Attribute[Boolean]()

  // The expanded field symbols of a struct symbol
  val fieldSymbols = new Attribute[List[TermSymbol]]()

  // The default value of this symbol, if required
  val default = new Attribute[Expr]()

  // Is this an SRAM entity?
  val sram = new Attribute[Boolean]()

  // Denotes that SRAM instances should be lifted from the hierarchy below this entity
  val liftSrams = new Attribute[Boolean]()

  // Iterator that enumerates all fields above
  private def attrIterator = Iterator(
    unused,
    topLevel,
    entry,
    owner,
    init,
    stackLimit,
    recLimit,
    returnStack,
    stateVar,
    fcn,
    fcv,
    fcr,
    fca,
    expandedPort,
    clearOnStall,
    interconnectClearOnStall,
    dontCareUnless,
    oReg,
    flop,
    memory,
    interconnect,
    combSignal,
    fieldSymbols,
    default,
    sram,
    liftSrams
  )

  // Iterator that enumerates names of fields above
  private def nameIterator = Iterator(
    "unused",
    "topLevel",
    "entry",
    "owner",
    "init",
    "stackLimit",
    "recLimit",
    "returnStack",
    "stateVar",
    "fcn",
    "fcv",
    "fcr",
    "fca",
    "expandedPort",
    "clearOnStall",
    "interconnectClearOnStall",
    "dontCareUnless",
    "oReg",
    "flop",
    "memory",
    "interconnect",
    "combSignal",
    "fieldSymbols",
    "default",
    "sram",
    "liftSrams"
  )

  // Copy values of attributes from another instance
  def update(that: SymbolAttributes): Unit = {
    for ((a, b) <- attrIterator zip that.attrIterator) {
      a.asInstanceOf[Attribute[Any]] update b.asInstanceOf[Attribute[Any]]
    }
  }

  // Copy values from source attributes
  def update(symbol: Symbol, that: SourceAttributes)(implicit cc: CompilerContext): Unit =
    if (that.hasAttr) {
      for ((name, expr) <- that.attr) {
        name match {
          case "unused"     => unused set true
          case "stacklimit" => stackLimit set expr
          case "reclimit"   => recLimit set expr
          case "toplevel"   => topLevel set true
          case "liftsrams"  => liftSrams set true
          case _            => cc.error(symbol, s"Unknown attribute '${name}'")
        }
      }
    }

  // Render in some human readable form
  def toSource(implicit cc: CompilerContext): String = {
    val parts = for ((name, attr) <- nameIterator zip attrIterator if attr.isSet) yield {
      attr.value match {
        case true       => s"${name}"
        case false      => s"!${name}"
        case expr: Expr => s"${name} = ${expr.toSource}"
        case other      => s"${name} = ${other.toString}"
      }
    }
    if (parts.nonEmpty) parts.mkString("(* ", ", ", " *)") else ""
  }
}
