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
// The Namer:
// - Constructs types and symbols for definitions
// - Resolves identifiers to symbols
// - Converts EntityIdent to EntityNamed
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Names.Name
import com.argondesign.alogic.core.Names.TermName
import com.argondesign.alogic.core.Names.TypeName
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.TreeInTypeTransformer
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.mutable

final class Namer(implicit cc: CompilerContext) extends TreeTransformer { namer =>

  override val typed: Boolean = false
  override val checkDefs: Boolean = false

  final private[this] object Scopes {
    private class SymTab(val weak: Boolean) {
      val tab = mutable.LinkedHashMap[Name, Symbol]()
    }

    private val scopes = mutable.Stack[SymTab]()

    private def current = scopes.head.tab

    def push(weak: Boolean): Unit = scopes.push(new SymTab(weak))

    def pop(): Unit = {
      val finished = scopes.pop()

      if (finished.weak) {
        // Insert symbols into the outer scope via a TypeChoice symbol. If the
        // symbolis already a choice symbol, merge alternatives.
        assert(scopes.nonEmpty)
        for {
          (name, symbol) <- finished.tab
          if !symbol.kind.isGen
        } {
          current.get(name) match {
            case None =>
              val choiceSymbol = symbol.kind match {
                case _: TypeChoice => symbol
                case _ =>
                  if (symbol.isTermSymbol) {
                    cc.newTermSymbol(name.str, symbol.loc, TypeChoice(List(symbol)))
                  } else {
                    cc.newTypeSymbol(name.str, symbol.loc, TypeChoice(List(symbol)))
                  }
              }
              insertSymbol(choiceSymbol)
            case Some(oldSymbol) =>
              oldSymbol.kind match {
                case TypeChoice(symbols) =>
                  assert(oldSymbol.isTermSymbol == symbol.isTermSymbol)
                  symbol.kind match {
                    case TypeChoice(choices) => oldSymbol.kind = TypeChoice(choices ::: symbols)
                    case _                   => oldSymbol.kind = TypeChoice(symbol :: symbols)
                  }
                case _ =>
                  cc.error(
                    symbol.loc,
                    s"Redefinition of '${name}' with previous definition at",
                    oldSymbol.loc.prefix
                  )
              }
          }
        }
      }
    }

    // Lookup name in symbol table
    def lookup(name: Name): Option[Symbol] = {
      @tailrec
      def loop(name: Name, scopes: mutable.Stack[SymTab]): Option[Symbol] = scopes match {
        case scope +: scopes =>
          scope.tab.get(name) match {
            case opt @ Some(_) => opt
            case None          => loop(name, scopes)
          }
        case _ => cc.globalScope.get(name)
      }
      loop(name, scopes)
    }

    // Insert Symbol into current scope
    private def insertSymbol(symbol: Symbol): Unit = {
      val name = symbol.uniqueName

      lazy val flavour = if (symbol.isTermSymbol) "name" else "type"

      current.get(name) match {
        case Some(oldSymbol) =>
          if (!oldSymbol.kind.isChoice) {
            cc.error(
              symbol.loc,
              s"Redefinition of ${flavour} '${name}' with previous definition at",
              oldSymbol.loc.prefix
            )
          }
        case None =>
          lookup(name) filterNot { _.kind.isChoice } foreach { oldSymbol =>
            cc.warning(
              symbol.loc,
              s"Definition of ${flavour} '${name}' hides previous definition at",
              oldSymbol.loc.prefix
            )
          }
      }

      current(name) = symbol
    }

    def insert[T <: Symbol](symbol: T): symbol.type = {
      insertSymbol(symbol)
      symbol
    }

    def finalCheck() = {
      assert(scopes.isEmpty)
    }
  }

  private[this] def lookupTerm(loc: Loc, name: String): Symbol = {
    Scopes.lookup(TermName(name)) match {
      case Some(symbol: TermSymbol) => symbol
      case Some(_)                  => unreachable
      case None => {
        cc.error(loc, s"Name '${name}' is not defined")
        ErrorSymbol
      }
    }
  }

  private[this] def lookupTerm(ident: Ident): Symbol = lookupTerm(ident.loc, ident.name)

  private[this] def lookupType(loc: Loc, name: String): Symbol = {
    Scopes.lookup(TypeName(name)) match {
      case Some(symbol: TypeSymbol) => symbol
      case Some(_)                  => unreachable
      case None => {
        cc.error(loc, s"Type '${name}' is not defined")
        ErrorSymbol
      }
    }
  }

  private[this] def lookupType(ident: Ident): Symbol = lookupType(ident.loc, ident.name)

  private[this] def lookupTermOrType(loc: Loc, name: String): Symbol = {
    val termOpt = Scopes.lookup(TermName(name))
    val typeOpt = Scopes.lookup(TypeName(name))

    (termOpt, typeOpt) match {
      case (Some(termSymbol), None) => {
        termSymbol
      }
      case (None, Some(typeSymbol)) => {
        typeSymbol
      }
      case (Some(termSymbol), Some(typeSymbol)) => {
        cc.error(
          loc,
          s"Name '${name}' in this context can resolve to either of",
          s"term '${name}' defined at ${termSymbol.loc.prefix}",
          s"type '${name}' defined at ${typeSymbol.loc.prefix}"
        )
        ErrorSymbol
      }
      case (None, None) => {
        cc.error(loc, s"Name '${name}' is undefined")
        ErrorSymbol
      }
    }
  }

  private[this] object TypeNamer extends TreeInTypeTransformer(this) {
    override def transform(kind: Type) = kind match {
      case TypeIdent(ident: Ident) => {
        lookupType(ident) match {
          case symbol: TypeSymbol => symbol.kind
          case ErrorSymbol        => TypeError
        }
      }
      case _ => super.transform(kind)
    }
  }

  private[this] var sawLet = false

  private[this] var atBitsEitherTypeOrTerm = false

  private[this] lazy val atBitsSymbol = cc.lookupGlobalTerm("@bits")

  private[this] val swapIfElseScope = mutable.Stack[(List[Tree], Boolean)]()

  private[this] def insertEarlyName(ident: Ident, isTerm: Boolean, asChoice: Boolean): Unit = {
    // Name the source attributes
    if (ident.hasAttr) {
      ident withAttr { (ident.attr.view mapValues { walk(_).asInstanceOf[Expr] }).toMap }
    }

    val kind = if (asChoice) TypeChoice(Nil) else TypeError // TODO: should be TypeMissing

    Scopes insert {
      if (isTerm) {
        cc.newTermSymbol(ident, kind)
      } else {
        cc.newTypeSymbol(ident, kind)
      }
    }
  }

  // Insert function names, instance names and nested entity names early
  // so they can be referred to before the definition site in source
  private[this] def insertEarly(trees: List[Tree], asChoice: Boolean = false): Unit =
    trees foreach {
      case EntFunction(ident: Ident, _)       => insertEarlyName(ident, isTerm = true, asChoice)
      case EntInstance(ident: Ident, _, _, _) => insertEarlyName(ident, isTerm = true, asChoice)
      case EntEntity(Entity(ident: Ident, _)) => insertEarlyName(ident, isTerm = false, asChoice)
      case EntGen(GenIf(_, thenItems, elseItems)) =>
        insertEarly(thenItems, asChoice = true)
        insertEarly(elseItems, asChoice = true)
      case EntGen(GenFor(_, _, _, body))   => insertEarly(body, asChoice = true)
      case EntGen(GenRange(_, _, _, body)) => insertEarly(body, asChoice = true)
      case GenIf(_, thenItems, elseItems) =>
        insertEarly(thenItems, asChoice = true)
        insertEarly(elseItems, asChoice = true)
      case GenFor(_, _, _, body)   => insertEarly(body, asChoice = true)
      case GenRange(_, _, _, body) => insertEarly(body, asChoice = true)
      case _                       => Nil
    }

  override def enter(tree: Tree): Unit = {
    if (swapIfElseScope.headOption map { _._1.head.id } contains tree.id) {
      Scopes.pop()
      Scopes.push(swapIfElseScope.head._2)
      insertEarly(swapIfElseScope.pop()._1)
    }

    tree match {
      case Root(_, Entity(ident: Ident, _)) => {
        Scopes.push(weak = false)

        // Name the source attributes of the root entity, these have already been
        // created so need to name the symbol attributes
        val symbol = lookupType(ident)
        // TODO: do them all in a systematic way..
        symbol.attr.stackLimit.get foreach { symbol.attr.stackLimit set walk(_).asInstanceOf[Expr] }
      }

      case entity: Entity =>
        Scopes.push(weak = false)
        insertEarly(entity.body)

      case _: EntFunction => Scopes.push(weak = false)

      case gen: GenFor =>
        Scopes.push(weak = true)
        insertEarly(gen.body)

      case gen: GenRange =>
        Scopes.push(weak = true)
        insertEarly(gen.body)

      case GenIf(_, thenItems, elseItems) =>
        Scopes.push(weak = true)
        insertEarly(thenItems)
        if (elseItems.nonEmpty) {
          swapIfElseScope.push((elseItems, true))
        }

      case _: StmtBlock => Scopes.push(weak = false)
      case _: StmtLet => {
        assert(!sawLet)
        sawLet = true
        Scopes.push(weak = false)
      }
      case _: StmtLoop => {
        if (!sawLet) Scopes.push(weak = false)
        sawLet = false
      }
      case _: StmtDo => {
        if (!sawLet) Scopes.push(weak = false)
        sawLet = false
      }
      case _: StmtWhile => {
        if (!sawLet) Scopes.push(weak = false)
        sawLet = false
      }
      case _: StmtFor => {
        if (!sawLet) Scopes.push(weak = false)
        sawLet = false
      }
      case StmtIf(_, _, elseStmts) => {
        Scopes.push(weak = false)
        if (elseStmts.nonEmpty) {
          swapIfElseScope.push((elseStmts, false))
        }
      }

      case _: CaseRegular => Scopes.push(weak = false)
      case _: CaseDefault => Scopes.push(weak = false)

      case ExprCall(ExprIdent("@bits"), arg :: _) if arg.isTypeExpr => {
        assert(!atBitsEitherTypeOrTerm)
        atBitsEitherTypeOrTerm = true
      }

      case _ => ()
    }
  }

  override def transform(tree: Tree): Tree = tree match {
    case node: Root =>
      node tap { _ =>
        Scopes.pop()
      }

    case TypeDefinitionTypedef(ident: Ident, kind) => {
      // Lookup target type
      val newKind = kind rewrite TypeNamer
      // Insert new type
      val symbol = Scopes.insert(cc.newTypeSymbol(ident, newKind))
      // Rewrite node
      val sym = Sym(symbol) withLoc ident.loc
      TypeDefinitionTypedef(sym, newKind) withLoc tree.loc
    }

    case TypeDefinitionStruct(ident: Ident, fieldNames, fieldKinds) => {
      // Lookup field types
      val newFieldKinds = fieldKinds map {
        _ rewrite TypeNamer
      }
      for (field <- newFieldKinds) {
        field match {
          case TypeVector(_: TypeStruct, _) => {
            cc.error(tree, "Vector element cannot have a struct type")
          }
          case _ => ()
        }
      }
      val kind = TypeStruct(ident.name, fieldNames, newFieldKinds)
      // Insert new type
      val symbol = Scopes.insert(cc.newTypeSymbol(ident, kind))
      // Rewrite node
      val sym = Sym(symbol) withLoc ident.loc
      TypeDefinitionStruct(sym, fieldNames, newFieldKinds) withLoc tree.loc
    }

    case entity @ Entity(ident @ Ident(name), _) => {
      // Lookup type symbol
      val symbol = lookupType(ident) match {
        case symbol: TypeSymbol => symbol
        case _                  => unreachable
      }

      // Attach proper type
      symbol.kind = {
        val portSymbols = entity.declarations collect {
          case Decl(symbol, _) if symbol.kind.isInstanceOf[TypeIn]  => symbol
          case Decl(symbol, _) if symbol.kind.isInstanceOf[TypeOut] => symbol
        }
        val paramSymbols = entity.declarations collect {
          case Decl(symbol, _) if symbol.kind.isInstanceOf[TypeParam] => symbol
        }
        TypeEntity(name, portSymbols, paramSymbols)
      }

      // Some special behavior for verbatim entities only
      if (symbol.attr.variant.value == "verbatim") {
        // Always lift srams
        symbol.attr.liftSrams set true
      }

      // Rewrite node
      entity.copy(ref = Sym(symbol) withLoc ident.loc) withLoc entity.loc
    } tap { _ =>
      Scopes.pop()
    }

    case EntFunction(ident: Ident, body) => {
      // Lookup term (inserted in enter(Entity))
      val symbol = lookupTerm(ident)

      // Attach proper type
      symbol.kind = TypeCtrlFunc(Nil, TypeVoid)

      if (ident.name == "main") {
        // Mark main as an entry point
        symbol.attr.entry set true
      }

      // Rewrite node
      val sym = Sym(symbol) withLoc ident.loc
      EntFunction(sym, body) withLoc tree.loc
    } tap { _ =>
      Scopes.pop()
    }

    case EntInstance(iIdent: Ident, eIdent: Ident, paramNames, paramExprs) => {
      // Lookup type symbol
      val eSymbol = lookupType(eIdent)
      val eSym = Sym(eSymbol) withLoc eIdent.loc

      // Lookup term symbol (inserted in enter(Entity))
      val iSymbol = lookupTerm(iIdent)

      // Attach proper type
      iSymbol.kind = eSymbol match {
        case ErrorSymbol        => TypeError
        case symbol: TypeSymbol => TypeInstance(symbol)
        case _                  => unreachable
      }

      // Rewrite node
      val iSym = Sym(iSymbol) withLoc iIdent.loc
      EntInstance(iSym, eSym, paramNames, paramExprs) withLoc tree.loc
    }

    case node: GenFor =>
      node tap { _ =>
        Scopes.pop()
      }
    case node @ GenRange(Decl(symbol, _), _, _, _) => {
      node
    } tap { _ =>
      Scopes.pop()
    }
    case node: GenIf =>
      node tap { _ =>
        Scopes.pop()
      }

    case node: StmtBlock =>
      node tap { _ =>
        Scopes.pop()
      }
    case node: StmtLoop =>
      node tap { _ =>
        Scopes.pop()
      }
    case node: StmtDo =>
      node tap { _ =>
        Scopes.pop()
      }
    case node: StmtWhile =>
      node tap { _ =>
        Scopes.pop()
      }
    case node: StmtFor =>
      node tap { _ =>
        Scopes.pop()
      }
    case node: StmtIf =>
      node tap { _ =>
        Scopes.pop()
      }

    case node: CaseRegular =>
      node tap { _ =>
        Scopes.pop()
      }
    case node: CaseDefault =>
      node tap { _ =>
        Scopes.pop()
      }

    case DeclIdent(ident: Ident, kind, init) => {
      // Lookup type
      val newKind = kind rewrite TypeNamer
      // TODO: implement memory of struct, vector of struct, and multi dimenstional memory/sram
      newKind match {
        case TypeVector(_: TypeStruct, _) => {
          cc.error(tree, "Vector element cannot have a struct type")
        }
        case TypeArray(_: TypeStruct, _) => {
          cc.error(tree, "Memory element cannot have a struct type")
        }
        case _ => ()
      }
      // Insert term
      val symbol = Scopes.insert(cc.newTermSymbol(ident, newKind))
      // Rewrite node
      Decl(symbol, init) withLoc tree.loc
    }

    case ExprIdent(name) => {
      // Lookup term (or type if inside @bits)
      val symbol = if (!atBitsEitherTypeOrTerm) {
        lookupTerm(tree.loc, name)
      } else {
        lookupTermOrType(tree.loc, name)
      }
      // Rewrite node
      ExprRef(symbol) withLoc tree.loc
    }

    case ExprCall(ExprRef(symbol), _) if symbol == atBitsSymbol => {
      tree tap { _ =>
        atBitsEitherTypeOrTerm = false
      }
    }

    case ExprType(kind) => {
      ExprType(kind rewrite TypeNamer) withLoc tree.loc
    }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    Scopes.finalCheck()

    assert(!sawLet)
    assert(!atBitsEitherTypeOrTerm)
    assert(swapIfElseScope.isEmpty)

    // Check tree does not contain any Ident related nodes anymore
    def check(tree: Tree): Unit = {
      tree visitAll {
        case node: DeclIdent => cc.ice(node, "DeclIdent remains")
        case node: ExprIdent => cc.ice(node, "ExprIdent remains")
        case node: Ident     => cc.ice(node, "Ident remains")
        case Decl(symbol, _) => symbol.kind visit { case tree: Tree => check(tree) }
        case Sym(symbol)     => symbol.kind visit { case tree: Tree => check(tree) }
        case ExprRef(symbol) => symbol.kind visit { case tree: Tree => check(tree) }
        case ExprType(kind)  => kind visit { case tree: Tree => check(tree) }
        case TypeDefinitionStruct(_, _, fieldTypes) => {
          fieldTypes foreach { _ visit { case tree: Tree => check(tree) } }
        }
        case TypeDefinitionTypedef(_, kind) => kind visit { case tree: Tree => check(tree) }
        case node: TypeIdent                => cc.ice(node.ident, "TypeIdent remains")
      }
    }

    check(tree)
  }

}

object Namer extends TreeTransformerPass {
  val name = "namer"
  def create(implicit cc: CompilerContext) = new Namer
}
