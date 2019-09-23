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
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.TreeInTypeTransformer
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.TreeLike

import scala.annotation.tailrec
import scala.collection.mutable

final class Namer(implicit cc: CompilerContext) extends TreeTransformer { namer =>

  override val typed: Boolean = false
  override val checkDefs: Boolean = false

  final private[this] object Scopes {

    private val dictSymbols = mutable.Set[Symbol]()

    private class SymTab(val genIf: Boolean, val genLoop: Boolean) {
      val tab: mutable.LinkedHashMap[String, Symbol] = mutable.LinkedHashMap()
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
                    cc.newTermSymbol(name, symbol.loc, TypeChoice(List(symbol)))
                  } else {
                    cc.newTypeSymbol(name, symbol.loc, TypeChoice(List(symbol)))
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
    def lookup(name: String): Option[Symbol] = {
      @tailrec
      def loop(name: String, scopes: mutable.Stack[SymTab]): Option[Symbol] = scopes match {
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
      val name = symbol.name

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

  private[this] def lookup(loc: Loc, name: String): Option[Symbol] = {
    Scopes.lookup(name) tap {
      case None    => cc.error(loc, s"'$name' is not defined")
      case Some(_) =>
    }
  }

  private[this] def lookup(ident: Ident): Option[Symbol] = lookup(ident.loc, ident.name)

  private[this] object TypeNamer extends TreeInTypeTransformer(this) {
    override def transform(kind: Type): Type = kind match {
      case TypeRef(ident @ Ident(_, idxs)) =>
        lookup(ident) match {
          case Some(symbol) => walk(TypeRef(Sym(symbol, idxs) withLoc ident.loc))
          case None         => TypeError
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

  private[this] val swapIfElseScope = mutable.Stack[(List[Tree], Boolean)]()

  private[this] def insertEarlyName(ident: Ident, isTerm: Boolean, kind: Type): Unit = {
    // Name the source attributes
    if (ident.hasAttr) {
      ident withAttr { (ident.attr.view mapValues { walk(_).asInstanceOf[Expr] }).toMap }
    }

    val symbol = if (isTerm) {
      cc.newTermSymbol(ident, kind)
    } else {
      cc.newTypeSymbol(ident, kind)
    }

    Scopes.insert(symbol, ident.idxs.nonEmpty) tap { symbol =>
      // Drop '$' from name (used only for singleton entities)
      symbol rename (symbol.name filterNot { _ == '$' })
    }
  }

  // Insert function names, instance names and nested entity names early
  // so they can be referred to before the definition site in source
  private[this] def insertEarly(trees: List[Tree], asChoice: Boolean = false): Unit =
    trees foreach {
      case EntFunction(ident: Ident, _) =>
        val kind = if (asChoice) TypeChoice(Nil) else TypeError
        insertEarlyName(ident, isTerm = true, kind)
      case EntInstance(ident: Ident, _, _, _) =>
        val kind = if (asChoice) TypeChoice(Nil) else TypeError
        insertEarlyName(ident, isTerm = true, kind)
      case EntEntity(Entity(ident: Ident, _)) =>
        val kind = if (asChoice) TypeChoice(Nil) else TypeEntity("", Nil, Nil)
        insertEarlyName(ident, isTerm = false, kind)
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
        val symbol = lookup(ident).get
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

      case _ => ()
    }
  }

  override def transform(tree: Tree): Tree = tree match {
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

    case entity @ Entity(ident @ Ident(_, idxs), _) =>
      Scopes.pop()
      checkDictCreate(ident, idxs, "Definition")
      // Lookup symbol (inserted in enter)
      val symbol = lookup(ident).get
      // Attach proper type
      symbol.kind = entity.typeBasedOnContents
      // Some special behavior for verbatim entities only
      if (symbol.attr.variant contains "verbatim") {
        symbol.attr.liftSrams set true // Always lift SRAMs
      }
      // Rewrite node
      entity.copy(ref = Sym(symbol, idxs) withLoc ident.loc) withLoc entity.loc

    case EntFunction(ident @ Ident(_, idxs), body) =>
      Scopes.pop()
      checkDictCreate(ident, idxs, "Definition")
      // Lookup symbol (inserted in enter)
      val symbol = lookup(ident).get
      // Attach proper type
      symbol.kind = TypeCtrlFunc(Nil, TypeVoid)
      // Mark main as an entry point
      if (ident.name == "main") {
        symbol.attr.entry set true
      }
      // Rewrite node
      val sym = Sym(symbol, idxs) withLoc ident.loc
      EntFunction(sym, body) withLoc tree.loc

    case EntInstance(iIdent @ Ident(_, iIdxs), eIdent @ Ident(_, eIdxs), paramNames, paramExprs) =>
      checkDictCreate(iIdent, iIdxs, "Declaration")
      // Lookup instance symbol (inserted in enter(Entity))
      val iSymbol = lookup(iIdent).get
      // Lookup entity symbol
      lookup(eIdent) match {
        case Some(eSymbol: TypeSymbol) if eSymbol.kind.isEntity || eSymbol.kind.isChoice =>
          // Attach proper type
          iSymbol.kind = TypeInstance(eSymbol)
          // Rewrite node
          val eSym = Sym(eSymbol, eIdxs) withLoc eIdent.loc
          val iSym = Sym(iSymbol, iIdxs) withLoc iIdent.loc
          EntInstance(iSym, eSym, paramNames, paramExprs) withLoc tree.loc
        case Some(_) =>
          cc.error(eIdent, s"'${eIdent.name}' does not name an entity")
          Thicket(Nil) withLoc tree.loc
        case _ => Thicket(Nil) withLoc tree.loc
      }

    case ExprRef(ident @ Ident(name, idxs)) =>
      // Lookup symbol
      lookup(tree.loc, name) match {
        case Some(symbol) =>
          // Rewrite node
          if (idxs.isEmpty) {
            ExprSym(symbol) withLoc tree.loc
          } else {
            val sym = Sym(symbol, idxs) withLoc ident.loc
            ExprRef(sym) withLoc tree.loc
          }
        case None => tree
      }

    case ExprType(kind) =>
      ExprType(kind rewrite TypeNamer) withLoc tree.loc

    case _: Root => Scopes.pop(); tree

    case _: GenFor   => Scopes.pop(); tree
    case _: GenRange => Scopes.pop(); tree
    case _: GenIf    => Scopes.pop(); tree

    case _: StmtBlock => Scopes.pop(); tree
    case _: StmtLoop  => Scopes.pop(); tree
    case _: StmtDo    => Scopes.pop(); tree
    case _: StmtWhile => Scopes.pop(); tree
    case _: StmtFor   => Scopes.pop(); tree
    case _: StmtIf    => Scopes.pop(); tree

    case _: CaseRegular => Scopes.pop(); tree
    case _: CaseDefault => Scopes.pop(); tree

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    Scopes.finalCheck()

    assert(!sawLet)
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
