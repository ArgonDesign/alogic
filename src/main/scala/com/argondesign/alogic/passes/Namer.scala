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
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.TreeInTypeTransformer
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.FollowedBy
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.mutable

final class Namer(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy { namer =>

  override val typed: Boolean = false

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

        for (symbol <- unusedSymbols if !(symbol.attr.unused.get contains true)) {
          val hint = symbol.kind match {
            case _: TypeArray    => "Array"
            case _: TypeCtrlFunc => "Function"
            case _: TypeCombFunc => "Function"
            case _: TypeEntity   => "Entity"
            case _: TypeIn       => "Input port"
            case _: TypeOut      => "Output port"
            case _: TypeParam    => "Parameter"
            case _: TypeConst    => "Constant"
            case _: TypePipeline => "Pipeline variable"
            case _: TypeInstance => "Instance"
            case _               => "Variable"
          }
          cc.warning(symbol, s"${hint} '${symbol.name}' is unused")
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
    private def insertSymbol(symbol: Symbol): Unit = {
      val name = symbol.uniqueName

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
          lookup(name) foreach { oldSymbol =>
            cc.warning(
              symbol.loc,
              s"Definition of ${flavour} '${name}' hides previous definition at",
              oldSymbol.loc.prefix
            )
          }
          current(name) = symbol
        }
      }
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
        Scopes.markUsed(termSymbol)
        Scopes.markUsed(typeSymbol)
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

  override def enter(tree: Tree): Unit = tree match {
    case node: Root => {
      Scopes.push()
    }

    case node: Entity => {
      Scopes.push()

      // Insert function names before descending an entity so they can be in arbitrary order
      for (Function(ident: Ident, _) <- node.functions) {
        val symbol = cc.newTermSymbol(ident, TypeCtrlFunc(Nil, TypeVoid))
        Scopes.insert(symbol)
        if (ident.name == "main") {
          // Always mark 'main' as used
          Scopes.markUsed(symbol)
          // Mark main as an entry point
          symbol.attr.entry set true
        }
      }

      // Insert nested entity names so instantiations can resolve them in arbitrary order
      for (Entity(ident: Ident, _, _, _, _, _, _, _, _) <- node.entities) {
        val symbol = cc.newTypeSymbol(ident, TypeEntity("", Nil, Nil))
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

    case ExprCall(ExprIdent("@bits"), arg :: _) if arg.isTypeExpr => {
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
            case Decl(symbol, _) if symbol.kind.isInstanceOf[TypeIn]  => symbol
            case Decl(symbol, _) if symbol.kind.isInstanceOf[TypeOut] => symbol
          }
          val paramSymbols = entity.declarations collect {
            case Decl(symbol, _) if symbol.kind.isInstanceOf[TypeParam] => symbol
          }
          symbol.kind = TypeEntity(name, portSymbols, paramSymbols)
          symbol
        }
        case _ => unreachable
      }
      // Some special behavior for verbatim entities only
      if (symbol.attr.variant.value == "verbatim") {
        // Mark all declarations used
        for (Decl(symbol, _) <- entity.declarations) {
          Scopes.markUsed(symbol)
        }
        // Always lift srams
        symbol.attr.liftSrams set true
      }
      // Rewrite node
      val sym = Sym(symbol) withLoc ident.loc
      entity.copy(ref = sym) withLoc entity.loc
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
      val iKind = eSymbol match {
        case ErrorSymbol        => TypeError
        case symbol: TypeSymbol => TypeInstance(symbol)
        case _                  => unreachable
      }
      val iSymbol = Scopes.insert(cc.newTermSymbol(iIdent, iKind))
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
      Scopes.markUsed(symbol)
      // Rewrite node
      ExprRef(symbol) withLoc tree.loc
    }

    case ExprCall(ExprRef(symbol), _) if symbol == atBitsSymbol => {
      tree followedBy {
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
