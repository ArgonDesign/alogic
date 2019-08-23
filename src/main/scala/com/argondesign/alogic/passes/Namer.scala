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
import com.argondesign.alogic.lib.TreeLike
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.mutable

final class Namer(implicit cc: CompilerContext) extends TreeTransformer { namer =>

  override val typed: Boolean = false
  override val checkDefs: Boolean = false

  final private[this] object Scopes {

    private val dictSymbols = mutable.Set[Symbol]()

    private class SymTab(val genIf: Boolean, val genLoop: Boolean) {
      val tab: mutable.LinkedHashMap[Name, Symbol] = mutable.LinkedHashMap()
    }

    private val scopes = mutable.Stack[SymTab]()

    private def current = scopes.head.tab

    def push(genIf: Boolean = false, genLoop: Boolean = false): Unit =
      scopes.push(new SymTab(genIf, genLoop))

    def pop(): Unit = {
      val finished = scopes.pop()

      if (finished.genIf || finished.genLoop) {
        // Insert symbols into the outer scope via a TypeChoice symbol. If the
        // symbol is already a choice symbol (nested weak scopes), merge
        // alternatives
        assert(scopes.nonEmpty)
        for {
          (name, symbol) <- finished.tab
          if !symbol.kind.isGen && (finished.genIf || (dictSymbols contains symbol))
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
                    s"Redefinition of '$name' with previous definition at",
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
        case s +: ss =>
          s.tab.get(name) match {
            case opt @ Some(_) => opt
            case None          => loop(name, ss)
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
              s"Redefinition of $flavour '$name' with previous definition at",
              oldSymbol.loc.prefix
            )
          }
        case None =>
          lookup(name) filterNot { _.kind.isChoice } foreach { oldSymbol =>
            cc.warning(
              symbol.loc,
              s"Definition of $flavour '$name' hides previous definition at",
              oldSymbol.loc.prefix
            )
          }
      }

      current(name) = symbol
    }

    def insert[T <: Symbol](symbol: T, isDict: Boolean): symbol.type = {
      insertSymbol(symbol)
      if (isDict) {
        dictSymbols add symbol
      }
      symbol
    }

    def dictCreateAllowed: Boolean = scopes.head.genLoop

    def finalCheck(): Unit = {
      assert(scopes.isEmpty)
    }
  }

  private[this] def lookupTerm(loc: Loc, name: String): Symbol = {
    Scopes.lookup(TermName(name)) match {
      case Some(symbol: TermSymbol) => symbol
      case None =>
        cc.error(loc, s"Name '$name' is not defined")
        ErrorSymbol
      case _ => unreachable
    }
  }

  private[this] def lookupTerm(ident: Ident): Symbol = lookupTerm(ident.loc, ident.name)

  private[this] def lookupType(loc: Loc, name: String): Symbol = {
    Scopes.lookup(TypeName(name)) match {
      case Some(symbol: TypeSymbol) => symbol
      case None =>
        cc.error(loc, s"Type '$name' is not defined")
        ErrorSymbol
      case _ => unreachable
    }
  }

  private[this] def lookupType(ident: Ident): Symbol = lookupType(ident.loc, ident.name)

  private[this] def lookupTermOrType(loc: Loc, name: String): Symbol = {
    val termOpt = Scopes.lookup(TermName(name))
    val typeOpt = Scopes.lookup(TypeName(name))

    (termOpt, typeOpt) match {
      case (Some(termSymbol), None) => termSymbol
      case (None, Some(typeSymbol)) => typeSymbol
      case (Some(termSymbol), Some(typeSymbol)) =>
        cc.error(
          loc,
          s"Name '$name' in this context can resolve to either of",
          s"term '$name' defined at ${termSymbol.loc.prefix}",
          s"type '$name' defined at ${typeSymbol.loc.prefix}"
        )
        ErrorSymbol
      case (None, None) =>
        cc.error(loc, s"Name '$name' is undefined")
        ErrorSymbol
    }
  }

  private[this] object TypeNamer extends TreeInTypeTransformer(this) {
    override def transform(kind: Type): Type = kind match {
      case TypeRef(ident @ Ident(_, idxs)) =>
        lookupType(ident) match {
          case symbol: TypeSymbol => walk(TypeRef(Sym(symbol, idxs) withLoc ident.loc))
          case ErrorSymbol        => TypeError
        }
      case _ => super.transform(kind)
    }
  }

  private[this] def checkDictCreate(tree: Tree, idxs: List[Expr], hint: String): Unit = {
    if (idxs.nonEmpty && !Scopes.dictCreateAllowed) {
      cc.error(tree, s"$hint with dictionary identifier must appear directly in 'gen' loop scope.")
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

    val symbol = if (isTerm) {
      cc.newTermSymbol(ident, kind)
    } else {
      cc.newTypeSymbol(ident, kind)
    }

    Scopes.insert(symbol, ident.idxs.nonEmpty)
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
      Scopes.push(genIf = swapIfElseScope.head._2)
      insertEarly(swapIfElseScope.pop()._1)
    }

    tree match {
      case Root(_, Entity(ident: Ident, _)) =>
        Scopes.push()

        // Name the source attributes of the root entity, these have already been
        // created so need to name the symbol attributes
        val symbol = lookupType(ident)
        // TODO: do them all in a systematic way..
        symbol.attr.stackLimit.get foreach { symbol.attr.stackLimit set walk(_).asInstanceOf[Expr] }

      case entity: Entity =>
        Scopes.push()
        insertEarly(entity.body)

      case _: EntFunction => Scopes.push()

      case gen: GenFor =>
        Scopes.push(genLoop = true)
        insertEarly(gen.body)

      case gen: GenRange =>
        Scopes.push(genLoop = true)
        insertEarly(gen.body)

      case GenIf(_, thenItems, elseItems) =>
        Scopes.push(genIf = true)
        insertEarly(thenItems)
        if (elseItems.nonEmpty) {
          swapIfElseScope.push((elseItems, true))
        }

      case _: StmtBlock => Scopes.push()
      case _: StmtLet =>
        assert(!sawLet)
        sawLet = true
        Scopes.push()
      case _: StmtLoop =>
        if (!sawLet) Scopes.push()
        sawLet = false
      case _: StmtDo =>
        if (!sawLet) Scopes.push()
        sawLet = false
      case _: StmtWhile =>
        if (!sawLet) Scopes.push()
        sawLet = false
      case _: StmtFor =>
        if (!sawLet) Scopes.push()
        sawLet = false
      case StmtIf(_, _, elseStmts) =>
        Scopes.push()
        if (elseStmts.nonEmpty) {
          swapIfElseScope.push((elseStmts, false))
        }

      case _: CaseRegular => Scopes.push()
      case _: CaseDefault => Scopes.push()

      case ExprCall(ExprRef(Ident("@bits", Nil)), arg :: _) if arg.isTypeExpr =>
        assert(!atBitsEitherTypeOrTerm)
        atBitsEitherTypeOrTerm = true

      case _ => ()
    }
  }

  override def transform(tree: Tree): Tree = tree match {
    case node: Root =>
      node tap { _ =>
        Scopes.pop()
      }

    case DefnRef(ident @ Ident(_, idxs), kind) =>
      checkDictCreate(ident, idxs, "Definition")
      // Lookup target type
      val newKind = kind rewrite TypeNamer
      // Insert new type
      val symbol = Scopes.insert(cc.newTypeSymbol(ident, newKind), idxs.nonEmpty)
      // Rewrite node
      if (idxs.isEmpty) {
        Defn(symbol) withLoc tree.loc
      } else {
        val sym = Sym(symbol, idxs) withLoc ident.loc
        DefnRef(sym, newKind) withLoc tree.loc
      }

    case entity @ Entity(ident @ Ident(_, idxs), _) => {
      // Lookup type symbol
      val symbol = lookupType(ident) match {
        case symbol: TypeSymbol => symbol
        case _                  => unreachable
      }

      // Attach proper type
      symbol.kind = entity.typeBasedOnContents

      // Some special behavior for verbatim entities only
      if (symbol.attr.variant.value == "verbatim") {
        // Always lift srams
        symbol.attr.liftSrams set true
      }

      // Rewrite node
      entity.copy(ref = Sym(symbol, idxs) withLoc ident.loc) withLoc entity.loc
    } tap { _ =>
      Scopes.pop()
      checkDictCreate(ident, idxs, "Definition")
    }

    case EntFunction(ident @ Ident(_, idxs), body) => {
      // Lookup term (inserted in enter(Entity))
      val symbol = lookupTerm(ident)

      // Attach proper type
      symbol.kind = TypeCtrlFunc(Nil, TypeVoid)

      if (ident.name == "main") {
        // Mark main as an entry point
        symbol.attr.entry set true
      }

      // Rewrite node
      val sym = Sym(symbol, idxs) withLoc ident.loc
      EntFunction(sym, body) withLoc tree.loc
    } tap { _ =>
      Scopes.pop()
      checkDictCreate(ident, idxs, "Definition")
    }

    case EntInstance(iIdent @ Ident(_, iIdxs), eIdent @ Ident(_, eIdxs), paramNames, paramExprs) =>
      checkDictCreate(iIdent, iIdxs, "Declaration")

      // Lookup type symbol
      val eSymbol = lookupType(eIdent)
      val eSym = Sym(eSymbol, eIdxs) withLoc eIdent.loc

      // Lookup term symbol (inserted in enter(Entity))
      val iSymbol = lookupTerm(iIdent)

      // Attach proper type
      iSymbol.kind = eSymbol match {
        case ErrorSymbol        => TypeError
        case symbol: TypeSymbol => TypeInstance(symbol)
        case _                  => unreachable
      }

      // Rewrite node
      val iSym = Sym(iSymbol, iIdxs) withLoc iIdent.loc
      EntInstance(iSym, eSym, paramNames, paramExprs) withLoc tree.loc

    case node: GenFor =>
      node tap { _ =>
        Scopes.pop()
      }
    case node @ GenRange(_, _, _, _) =>
      node tap { _ =>
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

    case DeclRef(ident @ Ident(_, idxs), kind, init) =>
      checkDictCreate(ident, idxs, "Declaration")
      // Lookup type
      val newKind = kind rewrite TypeNamer
      // Insert term
      val symbol = Scopes.insert(cc.newTermSymbol(ident, newKind), idxs.nonEmpty)
      // Rewrite node
      if (idxs.isEmpty) {
        Decl(symbol, init) withLoc tree.loc
      } else {
        val sym = Sym(symbol, idxs) withLoc ident.loc
        DeclRef(sym, newKind, init) withLoc tree.loc
      }

    case ExprRef(ident @ Ident(name, idxs)) =>
      // Lookup term (or type if inside @bits)
      val symbol = if (!atBitsEitherTypeOrTerm) {
        lookupTerm(tree.loc, name)
      } else {
        lookupTermOrType(tree.loc, name)
      }
      // Rewrite node
      if (idxs.isEmpty) {
        ExprSym(symbol) withLoc tree.loc
      } else {
        val sym = Sym(symbol, idxs) withLoc ident.loc
        ExprRef(sym) withLoc tree.loc
      }

    case ExprCall(ExprSym(`atBitsSymbol`), _) =>
      tree tap { _ =>
        atBitsEitherTypeOrTerm = false
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
    def check(tree: TreeLike): Unit = {
      tree visitAll {
        case node: Ident                       => cc.ice(node, "Ident remains")
        case node @ ExprRef(Sym(_, Nil))       => cc.ice(node, "ExprRef(Sym(_, Nil))")
        case node @ DeclRef(Sym(_, Nil), _, _) => cc.ice(node, "DeclRef(Sym(_, Nil), _, _)")
        case node @ DefnRef(Sym(_, Nil), _)    => cc.ice(node, "DefnRef(Sym(_, Nil), _)")
        case Decl(symbol, _)                   => check(symbol.kind)
        case Defn(symbol)                      => check(symbol.kind)
        case Sym(symbol, _)                    => check(symbol.kind)
        case ExprSym(symbol)                   => check(symbol.kind)
        case TypeRef(Sym(symbol, _))           => check(symbol.kind)
        case TypeChoice(symbols) =>
          symbols foreach { s =>
            check(s.kind)
          }
      }
    }

    check(tree)
  }

}

object Namer extends TreeTransformerPass {
  val name = "namer"
  def create(implicit cc: CompilerContext) = new Namer
}
