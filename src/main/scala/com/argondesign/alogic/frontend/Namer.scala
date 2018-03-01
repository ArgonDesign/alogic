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
// The Namer creates new symbols for declarations and type definitions and
// resolves Ident identifiers against their definitions, replacing them with
// Sym references referring to the symbol allocated for that definition.
// The Namer also discards all TypeDefinition nodes and replaces the Root
// with the root Entity.
// After the namer there should be none of these nodes left:
// - Ident
// - TypeDefinitionStruct
// - TypeDefinitoinTypedef
// - Root
// There should also not be any TypeRef(Ident(_)) types left anywhere.
// These are replaced with TypeRef(Sym(_)) after resolving the names.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.mutable

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees.Decl
import com.argondesign.alogic.ast.Trees.Entity
import com.argondesign.alogic.ast.Trees.ExprRef
import com.argondesign.alogic.ast.Trees.Function
import com.argondesign.alogic.ast.Trees.Ident
import com.argondesign.alogic.ast.Trees.Instance
import com.argondesign.alogic.ast.Trees.Root
import com.argondesign.alogic.ast.Trees.StmtBlock
import com.argondesign.alogic.ast.Trees.StmtDo
import com.argondesign.alogic.ast.Trees.StmtFor
import com.argondesign.alogic.ast.Trees.StmtGoto
import com.argondesign.alogic.ast.Trees.StmtLet
import com.argondesign.alogic.ast.Trees.StmtLoop
import com.argondesign.alogic.ast.Trees.StmtWhile
import com.argondesign.alogic.ast.Trees.Sym
import com.argondesign.alogic.ast.Trees.Tree
import com.argondesign.alogic.ast.Trees.TypeDefinition
import com.argondesign.alogic.ast.Trees.TypeDefinitionStruct
import com.argondesign.alogic.ast.Trees.TypeDefinitionTypedef
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Names.Name
import com.argondesign.alogic.core.Names.TermName
import com.argondesign.alogic.core.Names.TypeName
import com.argondesign.alogic.core.Symbols.ErrorSymbol
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types.TypeEntity
import com.argondesign.alogic.core.Types.TypeFunc
import com.argondesign.alogic.core.Types.TypeRef
import com.argondesign.alogic.core.Types.TypeStruct
import com.argondesign.alogic.util.FollowedBy

final class Namer(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy {

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

  private[this] var sawLet = false

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

    case _ => ()
  }

  override def transform(tree: Tree): Tree = tree match {
    case node: Root => {
      node.entity
    } followedBy {
      Scope.pop()
    }

    case TypeDefinitionTypedef(ident: Ident, kind) => {
      // Lookup target type
      val newKind = kind match {
        case TypeRef(ident: Ident) => TypeRef(Sym(lookupType(ident)) withLoc ident.loc)
        case _                     => kind
      }
      // Insert new type
      Scope.insert(cc.newTypeSymbol(ident, newKind))
      // Don't bother rewriting node, it will be removed when rewriting Root
      tree
    }

    case TypeDefinitionStruct(ident: Ident, fieldNames, fieldKinds) => {
      // Lookup field types
      val newFieldKinds = fieldKinds map {
        case TypeRef(ident: Ident) => TypeRef(Sym(lookupType(ident)) withLoc ident.loc)
        case other                 => other
      }
      val kind = TypeStruct(ListMap((fieldNames zip newFieldKinds): _*))
      // Insert new type
      Scope.insert(cc.newTypeSymbol(ident, kind))
      // Don't bother rewriting node, it will be removed when rewriting Root
      tree
    }

    case entity: Entity => {
      // Lookup type
      val Some(ident) = entity.children collectFirst {
        case ident: Ident => ident
      }
      val symbol = lookupType(ident)
      // Rewrite node
      val sym = Sym(symbol) withLoc ident.loc
      entity.copy(ref = sym) withLoc entity.loc
    } followedBy {
      Scope.pop()
    }

    case Function(ident: Ident, body) => {
      // Lookup term (inserted in enter(Entity)
      val sym = Sym(lookupTerm(ident)) withLoc ident.loc
      // Rewrite node
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
      // Rewrite bnode
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
      val sym = Sym(symbol) withLoc ident.loc
      // Rewrite node
      StmtGoto(sym) withLoc tree.loc
    }

    case Decl(ident: Ident, kind, init) => {
      // Lookup type
      val newKind = kind match {
        case TypeRef(ident: Ident) => TypeRef(Sym(lookupType(ident)) withLoc ident.loc)
        case _                     => kind
      }
      // Insert term
      val symbol = Scope.insert(cc.newTermSymbol(ident, kind))
      val sym = Sym(symbol) withLoc ident.loc
      // Rewrite node
      Decl(sym, newKind, init) withLoc tree.loc
    }

    case ExprRef(ident: Ident) => {
      // Lookup term
      val sym = Sym(lookupTerm(ident)) withLoc ident.loc
      // Rewrite node
      ExprRef(sym) withLoc tree.loc
    }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    Scope.finalCheck()

    assert(!sawLet)

    def errIdent(node: Tree, ident: Ident) = {
      cc.fatal(ident, s"Namer should have removed all identifiers, but '${ident}' remains in '${node}'")
    }

    tree visit {
      case node: Root                               => cc.fatal(node, "Namer hould have removed root node")
      case node: TypeDefinition                     => cc.fatal(node, "Namer should have removed type definitions")
      case node: Ident                              => errIdent(node, node)
      case node @ Decl(_, TypeRef(ident: Ident), _) => errIdent(node, ident)
      // TODO: type visitors
    }

  }

}
