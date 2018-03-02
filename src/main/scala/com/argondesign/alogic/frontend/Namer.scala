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
// The Namer resolves identifiers to symbols
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import scala.annotation.tailrec
import scala.collection.mutable

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Names.Name
import com.argondesign.alogic.core.Names.TermName
import com.argondesign.alogic.core.Names.TypeName
import com.argondesign.alogic.core.Symbols.ErrorSymbol
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.FollowedBy
import com.argondesign.alogic.core.TypeTransformer
import com.argondesign.alogic.util.unreachable

final class Namer(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy { namer =>

  private[this] object Scope {
    private type SymTab = mutable.HashMap[Name, Symbol]

    private var scopes = List[SymTab]()

    private def current = scopes.head

    def push() = {
      scopes = (new SymTab) :: scopes
    }

    def pop() = {
      scopes = scopes.tail
    }

    // Lookup name in symbol table
    def lookup(name: Name): Option[Symbol] = {
      @tailrec
      def loop(name: Name, scopes: List[SymTab]): Option[Symbol] = scopes match {
        case Nil => cc.globalScope.get(name)
        case scope :: scopes => scope.get(name) match {
          case Some(symbol) => Some(symbol)
          case None         => loop(name, scopes)
        }
      }
      loop(name, scopes)
    }

    // Insert Symbol into current scope
    def insert(symbol: Symbol): Symbol = {
      val name = symbol.denot.name

      lazy val flavour = if (symbol.isTermSymbol) "name" else "type"

      current.get(name) match {
        case Some(oldSymbol) => {
          cc.error(
            symbol.loc,
            s"Redefinition of ${flavour} '${name}' with previous definition at",
            oldSymbol.loc.toString
          )
        }
        case None => {
          lookup(name) map { oldSymbol =>
            cc.warning(
              symbol.loc,
              s"Definition of ${flavour} '${name}' hides previous definition at",
              oldSymbol.loc.toString
            )
          }
          current(name) = symbol
        }
      }

      symbol
    }

    def finalCheck() = {
      assert(scopes.isEmpty)
    }
  }

  private[this] def lookupTerm(ident: Ident): Symbol = {
    Scope.lookup(TermName(ident.name)) match {
      case Some(symbol) => symbol
      case None => {
        cc.error(ident, s"Name '${ident.name}' is not defined")
        ErrorSymbol
      }
    }
  }

  private[this] def lookupType(ident: Ident): Symbol = {
    Scope.lookup(TypeName(ident.name)) match {
      case Some(symbol) => symbol
      case None => {
        cc.error(ident, s"Type '${ident.name}' is not defined")
        ErrorSymbol
      }
    }
  }

  private[this] def lookupTermOrType(ident: Ident): Symbol = {
    val termOpt = Scope.lookup(TermName(ident.name))
    val typeOpt = Scope.lookup(TypeName(ident.name))

    (termOpt, typeOpt) match {
      case (Some(termSymbol), None) => termSymbol
      case (None, Some(typeSymbol)) => typeSymbol
      case (Some(termSymbol), Some(typeSymbol)) => {
        cc.error(
          ident,
          s"Name '${ident.name}' in this context can resolve to either of",
          s"term '${ident.name}' defined at ${termSymbol.loc}",
          s"type '${ident.name}' defined at ${typeSymbol.loc}"
        )
        ErrorSymbol
      }
      case (None, None) => {
        cc.error(ident, s"Name '${ident.name}' is undefined")
        ErrorSymbol
      }
    }
  }

  private[this] object TypeNamer extends TypeTransformer {
    override def transform(kind: Type) = kind match {
      case TypeRef(ident: Ident) => TypeRef(Sym(lookupType(ident)) withLoc ident.loc)
      case node @ TypeVector(_, size) => {
        val newSize = namer walk size match {
          case expr: Expr => expr
          case _          => unreachable
        }
        if (newSize eq size) node else node.copy(size = newSize)
      }
      case node @ TypeInt(_, size) => {
        val newSize = namer walk size match {
          case expr: Expr => expr
          case _          => unreachable
        }
        if (newSize eq size) node else node.copy(size = newSize)
      }
      case node @ TypeArray(_, size) => {
        val newSize = namer walk size match {
          case expr: Expr => expr
          case _          => unreachable
        }
        if (newSize eq size) node else node.copy(size = newSize)
      }
      case _ => kind
    }
  }

  private[this] var sawLet = false

  private[this] var inAtBits = false

  override def enter(tree: Tree): Unit = tree match {
    case node: Root => {
      Scope.push()
    }

    case node: Entity => {
      Scope.push()

      // Insert function names before descending an entity so they can be in arbitrary order
      for (Function(ident: Ident, _) <- node.functions) {
        val symbol = cc.newTermSymbol(ident, TypeFunc)
        Scope.insert(symbol)
      }

      // Insert nested entity names so instantiations can resolve them
      for (Entity(ident: Ident, _, _, _, _, _, _, _, _) <- node.entities) {
        val symbol = cc.newTypeSymbol(ident, TypeEntity)
        Scope.insert(symbol)
      }
    }

    case node: Function => {
      Scope.push()
    }
    case node: StmtBlock => {
      Scope.push()
    }
    case node: StmtLet => {
      assert(!sawLet)
      sawLet = true
      Scope.push()
    }
    case node: StmtLoop => {
      if (!sawLet) Scope.push()
      sawLet = false
    }
    case node: StmtDo => {
      if (!sawLet) Scope.push()
      sawLet = false
    }
    case node: StmtWhile => {
      if (!sawLet) Scope.push()
      sawLet = false
    }
    case node: StmtFor => {
      if (!sawLet) Scope.push()
      sawLet = false
    }

    // TODO: fix using type(...) expressions
    case ExprAtCall("bits", _) => {
      assert(!inAtBits)
      inAtBits = true
    }

    case _ => ()
  }

  override def transform(tree: Tree): Tree = tree match {
    case node: Root => node followedBy {
      Scope.pop()
    }

    case TypeDefinitionTypedef(ident: Ident, kind) => {
      // Lookup target type
      val newKind = kind rewrite TypeNamer
      // Insert new type
      val symbol = Scope.insert(cc.newTypeSymbol(ident, newKind))
      // Rewrite node
      val sym = Sym(symbol) withLoc ident.loc
      TypeDefinitionTypedef(sym, newKind) withLoc tree.loc
    }

    case TypeDefinitionStruct(ident: Ident, fieldNames, fieldKinds) => {
      // Lookup field types
      val newFieldKinds = fieldKinds map { _ rewrite TypeNamer }
      val kind = TypeStruct(fieldNames, newFieldKinds)
      // Insert new type
      val symbol = Scope.insert(cc.newTypeSymbol(ident, kind))
      // Rewrite node
      val sym = Sym(symbol) withLoc ident.loc
      TypeDefinitionStruct(sym, fieldNames, newFieldKinds) withLoc tree.loc
    }

    case entity: Entity => {
      // Lookup type
      val Some(ident) = entity.children collectFirst {
        case ident: Ident => ident
      }
      val symbol = lookupType(ident)
      // Rewrite node
      val sym = Sym(symbol) withLoc ident.loc
      entity.copy(ref = sym) withLoc entity.loc withVariant entity.variant
    } followedBy {
      Scope.pop()
    }

    case Function(ident: Ident, body) => {
      // Lookup term (inserted in enter(Entity))
      val symbol = lookupTerm(ident)
      // Rewrite node
      val sym = Sym(symbol) withLoc ident.loc
      Function(sym, body) withLoc tree.loc
    } followedBy {
      Scope.pop()
    }

    case Instance(iIdent: Ident, eIdent: Ident, paramNames, paramExprs) => {
      // Lookup type
      val eSymbol = lookupType(eIdent)
      val eSym = Sym(eSymbol) withLoc eIdent.loc
      // Insert term
      val iSymbol = Scope.insert(cc.newTermSymbol(iIdent, TypeRef(eSym)))
      val iSym = Sym(iSymbol) withLoc iIdent.loc
      // Rewrite node
      Instance(iSym, eSym, paramNames, paramExprs) withLoc tree.loc
    }

    case node: StmtBlock => node followedBy {
      Scope.pop()
    }
    case node: StmtLoop => node followedBy {
      Scope.pop()
    }
    case node: StmtDo => node followedBy {
      Scope.pop()
    }
    case node: StmtWhile => node followedBy {
      Scope.pop()
    }
    case node: StmtFor => node followedBy {
      Scope.pop()
    }

    case StmtGoto(ident: Ident) => {
      // Lookup term
      val symbol = lookupTerm(ident)
      // Rewrite node
      val sym = Sym(symbol) withLoc ident.loc
      StmtGoto(sym) withLoc tree.loc
    }

    case Decl(ident: Ident, kind, init) => {
      // Lookup type
      val newKind = kind rewrite TypeNamer
      // Insert term
      val symbol = Scope.insert(cc.newTermSymbol(ident, newKind))
      // Rewrite node
      val sym = Sym(symbol) withLoc ident.loc
      Decl(sym, newKind, init) withLoc tree.loc
    }

    case ExprRef(ident: Ident) => {
      // Lookup term (or type if inside @bits)
      val symbol = if (!inAtBits) lookupTerm(ident) else lookupTermOrType(ident)
      // Rewrite node
      val sym = Sym(symbol) withLoc ident.loc
      ExprRef(sym) withLoc tree.loc
    }

    case ExprAtCall("bits", _) => tree followedBy {
      inAtBits = false
    }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    Scope.finalCheck()

    assert(!sawLet)
    assert(!inAtBits)

    // Check tree does not contain any Ident nodes anymore
    tree visit {
      case node: Ident => {
        cc.ice(node, s"Namer should have removed all identifiers, but '${node}' remains")
      }
    }

    // Collect symbols used in this file
    val symbols = (tree collectAll { case Sym(symbol) => symbol }).toSet

    // Check that denotations do not contain any Ident nodes anymore
    symbols filter { _ != ErrorSymbol } foreach  { symbol =>
      val denot = symbol.denot
      denot.kind visit {
        case node: Ident => {
          cc.ice(node, s"Namer should have removed all identifiers, but '${node}' remains in denotation '${denot}'")
        }
      }
    }
  }

}
