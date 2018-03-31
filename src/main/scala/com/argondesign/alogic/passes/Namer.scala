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
// - Checks for unused variables
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
import com.argondesign.alogic.core.TreeInTypeTransformer
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.FollowedBy
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.mutable

final class Namer(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy { namer =>

  final private[this] object Scopes {
    private type SymTab = mutable.HashMap[Name, Symbol]

    private var scopes = List[SymTab]()

    private def current = scopes.head

    private val allSet = mutable.Set[Symbol]()

    private val usedSet = mutable.Set[Symbol]()

    def markUsed(symbol: Symbol) = usedSet add symbol

    def push() = {
      scopes = (new SymTab) :: scopes
    }

    def pop() = {
      allSet ++= current.values

      scopes = scopes.tail

      if (scopes.isEmpty) {
        val unusedSymbols = (allSet diff usedSet).toList sortBy { _.loc.line }

        for (symbol <- unusedSymbols) {
          val hint = symbol.denot.kind match {
            case _: TypeArray    => "Array"
            case _: TypeCtrlFunc => "Function"
            case _: TypeCombFunc => "Function"
            case _: TypeEntity   => "Entity"
            case _: TypeIn       => "Input port"
            case _: TypeOut      => "Output port"
            case _: TypeParam    => "Parameter"
            case _: TypeConst    => "Constant"
            case _: TypePipeline => "Pipeline variable"
            case TypeRef(Sym(symbol)) if symbol != ErrorSymbol =>
              symbol.denot.kind match {
                case _: TypeEntity => "Instance"
                case _             => "Variable"
              }
            case _ => "Variable"
          }
          cc.warning(symbol.loc, s"${hint} '${symbol.denot.name}' is unused")
        }
      }
    }

    // Lookup name in symbol table
    def lookup(name: Name): Option[Symbol] = {
      @tailrec
      def loop(name: Name, scopes: List[SymTab]): Option[Symbol] = scopes match {
        case Nil => cc.globalScope.get(name)
        case scope :: scopes =>
          scope.get(name) match {
            case opt @ Some(symbol) => opt
            case None               => loop(name, scopes)
          }
      }
      loop(name, scopes)
    }

    // Insert Symbol into current scope, and mark it unused
    def insert(symbol: Symbol): Symbol = {
      val name = symbol.denot.name

      lazy val flavour = if (symbol.isTermSymbol) "name" else "type"

      current.get(name) match {
        case Some(oldSymbol) => {
          cc.error(
            symbol.loc,
            s"Redefinition of ${flavour} '${name}' with previous definition at",
            oldSymbol.loc.prefix
          )
          current(name) = symbol
        }
        case None => {
          lookup(name) map { oldSymbol =>
            cc.warning(
              symbol.loc,
              s"Definition of ${flavour} '${name}' hides previous definition at",
              oldSymbol.loc.prefix
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
    Scopes.lookup(TermName(ident.name)) match {
      case Some(symbol) => symbol
      case None => {
        cc.error(ident, s"Name '${ident.name}' is not defined")
        ErrorSymbol
      }
    }
  }

  private[this] def lookupType(ident: Ident): Symbol = {
    Scopes.lookup(TypeName(ident.name)) match {
      case Some(symbol) => symbol
      case None => {
        cc.error(ident, s"Type '${ident.name}' is not defined")
        ErrorSymbol
      }
    }
  }

  private[this] def lookupTermOrType(ident: Ident): Symbol = {
    val termOpt = Scopes.lookup(TermName(ident.name))
    val typeOpt = Scopes.lookup(TypeName(ident.name))

    (termOpt, typeOpt) match {
      case (Some(termSymbol), None) => {
        termSymbol
      }
      case (None, Some(typeSymbol)) => {
        typeSymbol
      }
      case (Some(termSymbol), Some(typeSymbol)) => {
        cc.error(
          ident,
          s"Name '${ident.name}' in this context can resolve to either of",
          s"term '${ident.name}' defined at ${termSymbol.loc.prefix}",
          s"type '${ident.name}' defined at ${typeSymbol.loc.prefix}"
        )
        Scopes.markUsed(termSymbol)
        Scopes.markUsed(typeSymbol)
        ErrorSymbol
      }
      case (None, None) => {
        cc.error(ident, s"Name '${ident.name}' is undefined")
        ErrorSymbol
      }
    }
  }

  private[this] object TypeNamer extends TreeInTypeTransformer(this) {
    override def transform(kind: Type) = kind match {
      case TypeRef(ident: Ident) => TypeRef(Sym(lookupType(ident)) withLoc ident.loc)
      case _                     => super.transform(kind)
    }
  }

  private[this] var sawLet = false

  private[this] var atBitsEitherTypeOrTerm = false

  private[this] lazy val atBitsSymbol = cc.lookupGlobalTerm("@bits")

  override def enter(tree: Tree): Unit = tree match {
    case node: Root => {
      Scopes.push()
    }

    case node: Entity => {
      Scopes.push()

      // Insert function names before descending an entity so they can be in arbitrary order
      for (Function(ident: Ident, _) <- node.functions) {
        val attr = if (ident.hasAttr) ident.attr else Map.empty[String, Expr]
        val symbol = {
          cc.newTermSymbolWithAttr(ident.name, ident.loc, TypeCtrlFunc(Nil, TypeVoid), attr)
        }
        Scopes.insert(symbol)
        if (ident.name == "main") {
          // Always mark 'main' as used
          Scopes.markUsed(symbol)
          // Mark main as an entry point
          symbol addAttr ("entry" -> 1)
        }
      }

      // Insert nested entity names so instantiations can resolve them in arbitrary order
      for (Entity(ident: Ident, _, _, _, _, _, _, _, _) <- node.entities) {
        val symbol = cc.newTypeSymbol(ident, TypeEntity("", Nil, Nil, Nil, Nil))
        Scopes.insert(symbol)
      }
    }

    case node: Function => {
      Scopes.push()
    }
    case node: StmtBlock => {
      Scopes.push()
    }
    case node: StmtLet => {
      assert(!sawLet)
      sawLet = true
      Scopes.push()
    }
    case node: StmtLoop => {
      if (!sawLet) Scopes.push()
      sawLet = false
    }
    case node: StmtDo => {
      if (!sawLet) Scopes.push()
      sawLet = false
    }
    case node: StmtWhile => {
      if (!sawLet) Scopes.push()
      sawLet = false
    }
    case node: StmtFor => {
      if (!sawLet) Scopes.push()
      sawLet = false
    }

    case ExprCall(ExprRef(Ident("@bits")), arg :: _) if arg.isTypeExpr => {
      assert(!atBitsEitherTypeOrTerm)
      atBitsEitherTypeOrTerm = true
    }

    case _ => ()
  }

  override def transform(tree: Tree): Tree = tree match {
    case node: Root =>
      node followedBy {
        Scopes.pop()
      }

    case TypeDefinitionTypedef(ident: Ident, kind) => {
      // Lookup target type
      val newKind = kind rewrite TypeNamer
      // Insert new type
      val symbol = Scopes.insert(cc.newTypeSymbol(ident, newKind))
      // Don't check use of type symbols TODO: check types defined in same file
      Scopes.markUsed(symbol)
      // Rewrite node
      val sym = Sym(symbol) withLoc ident.loc
      TypeDefinitionTypedef(sym, newKind) withLoc tree.loc
    }

    case TypeDefinitionStruct(ident: Ident, fieldNames, fieldKinds) => {
      // Lookup field types
      val newFieldKinds = fieldKinds map {
        _ rewrite TypeNamer
      }
      val kind = TypeStruct(ident.name, fieldNames, newFieldKinds)
      // Insert new type
      val symbol = Scopes.insert(cc.newTypeSymbol(ident, kind))
      // Don't check use of type symbols TODO: check types defined in same file
      Scopes.markUsed(symbol)
      // Rewrite node
      val sym = Sym(symbol) withLoc ident.loc
      TypeDefinitionStruct(sym, fieldNames, newFieldKinds) withLoc tree.loc
    }

    case entity: Entity => {
      // Get Ident
      val ident @ Ident(name) = entity.ref
      // Lookup type
      val symbol = lookupType(ident) match {
        case symbol: TypeSymbol => {
          // Attach proper type
          val portSymbols = entity.declarations collect {
            case Decl(Sym(sym), _: TypeIn, _)  => sym
            case Decl(Sym(sym), _: TypeOut, _) => sym
          }
          val paramSymbols = entity.declarations collect {
            case Decl(Sym(sym), _: TypeParam, _) => sym
          }
          val portNames = portSymbols map {
            _.denot.name.str
          }
          val portTypes = portSymbols map {
            _.denot.kind
          }
          val paramNames = paramSymbols map {
            _.denot.name.str
          }
          val paramTypes = paramSymbols map {
            _.denot.kind
          }
          symbol withDenot symbol.denot.copy(
            kind = TypeEntity(name, portNames, portTypes, paramNames, paramTypes))
        }
        case _ => unreachable
      }
      // Mark all declarations used if verbatim entity
      if (entity.variant == "verbatim") {
        for (Decl(Sym(symbol), _, _) <- entity.declarations) {
          Scopes.markUsed(symbol)
        }
      }
      // Rewrite node
      val sym = Sym(symbol) withLoc ident.loc
      entity.copy(ref = sym) withLoc entity.loc withVariant entity.variant
    } followedBy {
      Scopes.pop()
    }

    case Function(ident: Ident, body) => {
      // Lookup term (inserted in enter(Entity))
      val symbol = lookupTerm(ident)
      // Rewrite node
      val sym = Sym(symbol) withLoc ident.loc
      Function(sym, body) withLoc tree.loc
    } followedBy {
      Scopes.pop()
    }

    case Instance(iIdent: Ident, eIdent: Ident, paramNames, paramExprs) => {
      // Lookup type
      val eSymbol = lookupType(eIdent)
      val eSym = Sym(eSymbol) withLoc eIdent.loc
      Scopes.markUsed(eSymbol)
      // Insert term
      val iSymbol = Scopes.insert(cc.newTermSymbol(iIdent, TypeRef(Sym(eSymbol))))
      val iSym = Sym(iSymbol) withLoc iIdent.loc
      // Rewrite node
      Instance(iSym, eSym, paramNames, paramExprs) withLoc tree.loc
    }

    case node: StmtBlock =>
      node followedBy {
        Scopes.pop()
      }
    case node: StmtLoop =>
      node followedBy {
        Scopes.pop()
      }
    case node: StmtDo =>
      node followedBy {
        Scopes.pop()
      }
    case node: StmtWhile =>
      node followedBy {
        Scopes.pop()
      }
    case node: StmtFor =>
      node followedBy {
        Scopes.pop()
      }

    case Decl(ident: Ident, kind, init) => {
      // Lookup type
      val newKind = kind rewrite TypeNamer
      // Insert term
      val symbol = Scopes.insert(cc.newTermSymbol(ident, newKind))
      // Rewrite node
      val sym = Sym(symbol) withLoc ident.loc
      Decl(sym, newKind, init) withLoc tree.loc
    }

    case ExprRef(ident: Ident) => {
      // Lookup term (or type if inside @bits)
      val symbol = if (!atBitsEitherTypeOrTerm) lookupTerm(ident) else lookupTermOrType(ident)
      Scopes.markUsed(symbol)
      // Rewrite node
      val sym = Sym(symbol) withLoc ident.loc
      ExprRef(sym) withLoc tree.loc
    }

    case ExprCall(ExprRef(Sym(symbol)), _) if symbol == atBitsSymbol => {
      tree followedBy {
        atBitsEitherTypeOrTerm = false
      }
    }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    Scopes.finalCheck()

    assert(!sawLet)
    assert(!atBitsEitherTypeOrTerm)

    // Check tree does not contain any Ident nodes anymore
    tree visit {
      case node: Ident => {
        cc.ice(node, s"Namer should have removed all identifiers, but '${node}' remains")
      }
    }

    // Collect symbols used in this file
    val symbols = (tree collectAll { case Sym(symbol) => symbol }).toSet

    // Check that denotations do not contain any Ident nodes anymore
    symbols filter { _ != ErrorSymbol } foreach { symbol =>
      val denot = symbol.denot
      denot.kind visit {
        case node: Ident => {
          cc.ice(
            node,
            s"Namer should have removed all identifiers, but '${node}' remains in denotation '${denot}'")
        }
      }
    }
  }

}
