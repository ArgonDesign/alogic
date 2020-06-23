////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018-2019 Argon Design Ltd. All rights reserved.
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

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.SequenceNumbers
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable
import scala.util.chaining._

trait Symbols extends { self: CompilerContext =>

  // TODO: review this whole globalScope design..

  // The global scope only holds file level entity symbols
  final private[this] var _globalScope: Option[mutable.HashMap[String, Symbols.Symbol]] =
    Some(mutable.HashMap())

  // Can only hand out the final immutable copy
  final lazy val globalScope: Map[String, Symbols.Symbol] = {
    _globalScope.get.toMap
  } tap { _ =>
    _globalScope = None
  }

  // Add a symbol to the global scope, assuming it is still open
  final def addGlobalSymbol(symbol: Symbols.Symbol): Unit = synchronized {
    _globalScope match {
      case None => ice("Global scope is already sealed")
      case Some(scope) =>
        val name = symbol.name
        if (scope contains name) {
          ice(s"Global scope already contains '$name'")
        }
        scope(name) = symbol
    }
  }

  final def addGlobalDescs(descs: IterableOnce[Desc]): Unit = synchronized {
    for (desc <- descs.iterator) {
      addGlobalSymbol(newSymbol(desc.ref.asInstanceOf[Ident]))
    }

    // Force value to seal global scope
    globalScope
  }

  final def addGlobalDesc(desc: Desc): Unit = addGlobalDescs(List(desc))

  final def lookupGlobalTerm(name: String): Symbols.Symbol = synchronized {
    globalScope.get(name) match {
      case Some(symbol) => symbol
      case None         => ice(s"Cannot find global term '$name'")
    }
  }

  final def makeBuiltinCall(name: String, loc: Loc, args: List[Expr]): ExprCall = {
    val polySymbol = lookupGlobalTerm(name)
    assert(polySymbol.isBuiltin)
    assert(args exists { _.hasTpe })
    val argps = args map { a =>
      ArgP(a).regularize(a.loc)(this)
    }
    val symbol = polySymbol.kind(this).asPolyFunc.resolve(argps).get
    val call = ExprSym(symbol).call(argps)(this)
    call.regularize(loc)(this)
  }

  final private[this] val symbolSequenceNumbers = new SequenceNumbers

  //////////////////////////////////////////////////////////////////////////////
  // Creating Symbol instances
  //////////////////////////////////////////////////////////////////////////////

  final def newSymbol(name: String, loc: Loc): Symbols.Symbol = synchronized {
    new Symbols.Symbol(symbolSequenceNumbers.next, loc, name)
  }

  final def newSymbol(ident: Ident): Symbols.Symbol = {
    newSymbol(ident.name, ident.loc) tap { symbol =>
      symbol.attr.update(ident.attr)(this)
    }
  }

  final def newSymbolLike(symbol: Symbols.Symbol): Symbols.Symbol = {
    newSymbol(symbol.name, symbol.loc) tap { newSymbol =>
      newSymbol.kind = symbol.kind(this)
      newSymbol.attr.update(symbol.attr)
    }
  }

  final def newTemp(name: String, loc: Loc, kind: Type): Symbols.Symbol =
    newSymbol(name, loc) tap { symbol =>
      symbol.kind = kind
      symbol.attr.tmp set true
    }

}

object Symbols {

  final class Symbol(
      val id: Int,
      val loc: Loc,
      initialName: String) {
    var name: String = initialName

    val attr: SymbolAttributes = new SymbolAttributes()

    def isBuiltin: Boolean = this.attr.builtin.isSet

    override def hashCode: Int = id // TODO: review if this is still needed

    override def toString = s"$name@$id"

    ////////////////////////////////////////////////////////////////////////////
    // The following is the mechanism figuring out the type of the symbol
    ////////////////////////////////////////////////////////////////////////////

    // The Desc node of this symbol
    private[this] var _desc: Desc = _

    // The Decl node of this symbol
    private[this] var _decl: Decl = _

    // The Defn node of this symbol
    private[this] var _defn: Defn = _

    // The type of this symbol
    private[this] var _kind: Type = TypeUnknown

    // A symbol is considered specialized if it's Decl is known (or has a _kind)
    // TODO: make _kind non-writeable and drop condition on it
    def isSpecialized: Boolean = _decl != null || !(_kind.isUnknown || _kind.isParametrized)

    // Can only set desc on non-specialized symbols
    def desc_=(desc: Desc): Unit = {
      assert(!isSpecialized)
      _desc = desc
    }

    // Setting the Decl invalidates the type of the symbol and removes the Desc
    def decl_=(decl: Decl): Unit = {
      _desc = null
      _decl = decl
      _kind = TypeUnknown
    }

    // Setting the Defn does not change the type of the symbol
    def defn_=(defn: Defn): Unit = {
      assert(_desc == null)
      _defn = defn
    }

    // Explicitly setting the type removes the associated Desc/Decl node
    def kind_=(kind: Type): Unit = {
      _desc = null
      _decl = null
      _kind = kind
    }

    // Get Desc of symbol
    def desc: Desc = {
      assert(_desc != null && _decl == null && _defn == null)
      _desc
    }

    // Get Decl of symbol
    def decl: Decl = {
      assert(_decl != null && _desc == null)
      _decl
    }

    // Get Defn of symbol
    def defn: Defn = {
      assert(_defn != null && _desc == null)
      _defn
    }

    // Marker for detecting circular definitions
    private[this] var pending: Boolean = false

    // The type of this symbol. Retrieving this will attempt to compute the
    // type if it is not known, but it might still be unknown if referenced
    // choice symbols have not yet been resolved or if the definition is
    // circular.
    def kind(implicit cc: CompilerContext): Type = {
      // Attempt to compute the type if it is unknown, and we are not already
      // trying to compute it. The latter can arise from circular definitions.
      if (_kind == TypeUnknown && !pending) {
        // Mark pending
        pending = true
        // Compute the type of the symbol
        _kind = if (isSpecialized) {
          computeType
        } else if (isParametrized) {
          TypeParametrized(this)
        } else {
          cc.ice(loc, "Cannot compute type of un-specialized non-parametric symbol")
        }
        // Mark complete
        pending = false
      }
      _kind
    }

    private[this] def computeType(implicit cc: CompilerContext): Type = {
      // Figure out the type based on the declaration
      if (!cc.typeCheck(_decl)) {
        TypeError
      } else {
        _decl match {
          case DeclVar(_, spec)            => spec.tpe.asType.kind
          case DeclVal(_, spec)            => spec.tpe.asType.kind
          case DeclIn(_, spec, fc)         => TypeIn(spec.tpe.asType.kind, fc)
          case DeclOut(_, spec, fc, st)    => TypeOut(spec.tpe.asType.kind, fc, st)
          case DeclPipeline(_, spec)       => TypePipeline(spec.tpe.asType.kind)
          case DeclConst(_, spec)          => TypeConst(spec.tpe.asType.kind)
          case DeclGen(_, spec)            => TypeGen(spec.tpe.asType.kind)
          case DeclArray(_, elem, size)    => TypeArray(elem.tpe.asType.kind, size.value.get)
          case DeclSram(_, elem, size, st) => TypeSram(elem.tpe.asType.kind, size.value.get, st)
          case DeclStack(_, elem, size)    => TypeStack(elem.tpe.asType.kind, size.value.get)
          case DeclType(_, spec)           => TypeType(spec.tpe.asType.kind)
          case desc: DeclEntity            => TypeType(TypeEntity(this, desc.ports))
          case desc: DeclRecord            => TypeType(TypeRecord(this, desc.members))
          case DeclInstance(_, spec)       => spec.tpe.asType.kind
          case desc: DeclSingleton         => TypeEntity(this, desc.ports)
          case DeclFunc(_, variant, ret, args) =>
            val retType = ret.tpe.asType.kind
            val argTypes = args map { _.symbol.kind.asFund }
            variant match {
              case FuncVariant.Ctrl   => TypeCtrlFunc(this, retType, argTypes)
              case FuncVariant.Comb   => TypeCombFunc(this, retType, argTypes)
              case FuncVariant.Xeno   => TypeXenoFunc(this, retType, argTypes)
              case FuncVariant.Static => TypeStaticMethod(this, retType, argTypes)
              case FuncVariant.Method => TypeNormalMethod(this, retType, argTypes)
              case FuncVariant.None   => cc.ice(_decl, "Unknown function variant")
            }
          case _: DeclState => TypeState
        }
      }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Decl/Defn fabricators for compiler created symbols
    ////////////////////////////////////////////////////////////////////////////

    // Create a Decl node describing this symbol based on it's type
    def mkDecl: Decl = {
      assert(_decl == null)
      assert(_kind != TypeUnknown)
      def spec(kind: TypeFund): Expr = kind match {
        case TypeRecord(s, _) => ExprSym(s)
        case TypeEntity(s, _) => ExprSym(s)
        case k: TypeFund      => ExprType(k)
      }
      _kind match {
        case k: TypeEntity                    => DeclInstance(this, spec(k))
        case k: TypeFund                      => DeclVar(this, spec(k))
        case TypeIn(k, fc)                    => DeclIn(this, spec(k), fc)
        case TypeOut(k, fc, st)               => DeclOut(this, spec(k), fc, st)
        case TypeConst(k)                     => DeclConst(this, spec(k))
        case TypeArray(e, size)               => DeclArray(this, spec(e), ExprNum(false, size))
        case TypeSram(e, size, st)            => DeclSram(this, spec(e), ExprNum(false, size), st)
        case TypeStack(e, size)               => DeclStack(this, spec(e), ExprNum(false, size))
        case TypeType(TypeRecord(s, members)) => DeclRecord(s, members map { _.mkDecl })
        case TypeState                        => DeclState(this)
        // The rest should never be used by the compiler, but ones that make
        // sense can be added if required in the future
        case _ => unreachable
      }
    } ensuring { _decl != null }

    // Create a Defn node describing this symbol based on it's type
    def mkDefn(implicit cc: CompilerContext): Defn = {
      assert(_defn == null, this.toString)
      kind match {
        case _: TypeEntity => DefnInstance(this)
        case _: TypeFund   => DefnVar(this, None)
        case _: TypeIn     => DefnIn(this)
        case _: TypeOut    => DefnOut(this, None)
        case _: TypeArray  => DefnArray(this)
        case _: TypeSram   => DefnSram(this)
        case _: TypeStack  => DefnStack(this)
        case TypeType(TypeRecord(s, members)) =>
          DefnRecord(
            s,
            members map { symbol =>
              RecDefn(symbol.mkDefn)
            }
          )
        // The rest should never be used by the compiler, but ones that make
        // sense can be added if required in the future
        case _ => unreachable
      }
    } ensuring { _defn != null }

    // Create a Defn node describing this symbol based on it's type, using
    // the provided optional initializer expression
    def mkDefn(initOpt: Option[Expr])(implicit cc: CompilerContext): Defn = {
      assert(_defn == null)
      kind match {
        case _: TypeFund => DefnVar(this, initOpt)
        case _: TypeOut  => DefnOut(this, initOpt)
        // The rest should never be used by the compiler, but ones that make
        // sense can be added if required in the future
        case _ => unreachable
      }
    } ensuring { _defn != null }

    // Create a Defn node describing this symbol based on it's type, using
    // the provided initializer expression
    def mkDefn(init: Expr)(implicit cc: CompilerContext): Defn = {
      assert(_defn == null)
      kind match {
        case _: TypeEntity => unreachable
        case _: TypeFund   => DefnVar(this, Some(init))
        case _: TypeOut    => DefnOut(this, Some(init))
        case _: TypeConst  => DefnConst(this, init)
        // The rest should never be used by the compiler, but ones that make
        // sense can be added if required in the future
        case _ => unreachable
      }
    } ensuring { _defn != null }

    ////////////////////////////////////////////////////////////////////////////
    // Things derived from the Defn of the symbol
    ////////////////////////////////////////////////////////////////////////////

    // Extract the initializer value from the definition, if any
    def init(implicit cc: CompilerContext): Option[Expr] = {
      assert(_defn != null)
      if (_defn.initializer.isEmpty) {
        None
      } else if (!_defn.hasTpe && !cc.typeCheck(_defn)) {
        None
      } else {
        _defn.normalize.initializer map {
          // Ensure initializer has same signedness as the type
          _.simplify match {
            case e if e.tpe.isSigned == kind.isSigned => e
            case e if e.tpe.isSigned                  => e.castUnsigned
            case e                                    => e.castSigned
          }
        }
      }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Things derived from the Desc of the symbol
    ////////////////////////////////////////////////////////////////////////////

    def isParametrized: Boolean = !isSpecialized && {
      assert(_desc != null, _kind.toString + " " + toString)
      _desc.isParametrized
    }

    def isChoice: Boolean = !isSpecialized && {
      assert(_desc != null, _kind.toString + " " + toString)
      _desc.isInstanceOf[DescChoice]
    }

    ////////////////////////////////////////////////////////////////////////////
    // Clone symbol (create new symbol with same name, loc and attributes)
    ////////////////////////////////////////////////////////////////////////////

    def dup(implicit cc: CompilerContext): Symbol =
      cc.newSymbol(name, loc) tap { _.attr update attr }

  }

  object Symbol {
    // Extractor to pattern match against symbol name
    def unapply(arg: Symbol): Option[String] = Some(arg.name)
  }

}
