////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Symbol representation and creation
////////////////////////////////////////////////////////////////////////////////

// A Symbol is a unique handle to the definition of a Name

package com.argondesign.alogic.core

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.SequenceNumbers
import com.argondesign.alogic.util.unreachable

import scala.util.chaining._

trait Symbols { self: CompilerContext =>

  final private val symbolSequenceNumbers = new SequenceNumbers

  //////////////////////////////////////////////////////////////////////////////
  // Creating Symbol instances
  //////////////////////////////////////////////////////////////////////////////

  final def newSymbol(name: String, loc: Loc): Symbols.Symbol = synchronized {
    new Symbols.Symbol(name, loc, symbolSequenceNumbers.next)
  }

  final def newTemp(name: String, loc: Loc, kind: Type): Symbols.Symbol =
    newSymbol(name, loc) tap { symbol =>
      symbol.kind = kind
      symbol.attr.tmp set true
    }

}

object Symbols {

  final class Symbol(
      initialName: String,
      val loc: Loc = Loc.synthetic,
      val id: Int = -1) {
    var name: String = initialName
    var origName: String = name

    val attr: SymbolAttributes = new SymbolAttributes()

    override def hashCode: Int = id // TODO: review if this is still needed

    override def toString = s"$name@$id"

    var scopeName: String = ""

    def hierName: String = if (scopeName.nonEmpty) scopeName + "." + origName else origName

    def isDictName: Boolean = name contains '#'

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
    private[this] var _kind: Type = _

    // Setting the desc is only possibly before Decl/Defn have been set
    def desc_=(desc: Desc): Unit = {
      require(desc.ref == Sym(this), this)
      assert(_decl == null, this)
      assert(_defn == null, this)
      _desc = desc
    }

    // Setting the Decl removes the Desc and clears the type
    def decl_=(decl: Decl): Unit = {
      require(decl.symbol == this)
      _desc = null
      _decl = decl
      _kind = null
    }

    // Setting the Defn removes the Desc
    def defn_=(defn: Defn): Unit = {
      require(defn.symbol == this)
      _desc = null
      _defn = defn
    }

    // Get Desc of symbol
    def desc: Desc = _desc ensuring { _ != null }

    // Get Decl of symbol
    def decl: Decl = _decl ensuring { _ != null }

    // Get Defn of symbol
    def defn: Defn = _defn ensuring { _ != null }

    // Get Desc if exist (doesn't exist for builtins and compiler added fields,
    // or after elaboration)
    def descOption: Option[Desc] = Option(_desc)

    // Get Defn if exist (doesn't exist for builtins and compiler added fields,
    // or prior to elaboration)
    def defnOption: Option[Defn] = Option(_defn)

    // Set type of symbol
    def kind_=(kind: Type): Unit = _kind = kind

    // The type of this symbol.
    def kind: Type = {
      if (_kind == null) {
        assert(_decl != null)
        _kind = decl match {
          case DeclVar(_, spec)            => spec.tpe.asType.kind
          case DeclVal(_, spec)            => spec.tpe.asType.kind
          case DeclStatic(_, spec)         => spec.tpe.asType.kind
          case DeclIn(_, spec, fc)         => TypeIn(spec.tpe.asType.kind, fc)
          case DeclOut(_, spec, fc, st)    => TypeOut(spec.tpe.asType.kind, fc, st)
          case DeclPipeVar(_, spec)        => TypePipeVar(spec.tpe.asType.kind)
          case DeclPipeIn(_, fc)           => TypePipeIn(fc)
          case DeclPipeOut(_, fc, st)      => TypePipeOut(fc, st)
          case DeclConst(_, spec)          => TypeConst(spec.tpe.asType.kind)
          case DeclArray(_, elem, size)    => TypeArray(elem.tpe.asType.kind, size)
          case DeclSram(_, elem, size, st) => TypeSram(elem.tpe.asType.kind, size, st)
          case DeclStack(_, elem, size)    => TypeStack(elem.tpe.asType.kind, size)
          case DeclType(_, spec)           => TypeType(spec.tpe.asType.kind)
          case decl: DeclEntity            => TypeType(TypeEntity(this, decl.ports))
          case decl: DeclRecord            => TypeType(TypeRecord(this, decl.members))
          case DeclInstance(_, spec)       => spec.tpe.asType.kind
          case decl: DeclSingleton         => TypeEntity(this, decl.ports)
          case DeclFunc(_, variant, ret, args) =>
            val retType = ret.tpe.asType.kind
            val argTypes = args map { _.symbol.kind.asFund }
            variant match {
              case FuncVariant.Ctrl   => TypeCtrlFunc(this, retType, argTypes)
              case FuncVariant.Comb   => TypeCombFunc(this, retType, argTypes)
              case FuncVariant.Xeno   => TypeXenoFunc(this, retType, argTypes)
              case FuncVariant.Static => TypeStaticMethod(this, retType, argTypes)
              case FuncVariant.Method => TypeNormalMethod(this, retType, argTypes)
            }
          case _: DeclState => TypeState(this)
        }
      }
      _kind
    }

    def kindIsSet: Boolean = _kind != null

    ////////////////////////////////////////////////////////////////////////////
    // Decl/Defn fabricators for compiler created symbols
    ////////////////////////////////////////////////////////////////////////////

    // Create a Decl node describing this symbol based on it's type
    def mkDecl: Decl = {
      assert(_decl == null)
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
        case TypeArray(e, size)               => DeclArray(this, spec(e), size.toLong)
        case TypeSram(e, size, st)            => DeclSram(this, spec(e), size.toLong, st)
        case TypeStack(e, size)               => DeclStack(this, spec(e), size.toLong)
        case TypeType(TypeRecord(s, members)) => DeclRecord(s, members map { _.mkDecl })
        case TypeState(symbol) => assert(symbol == this); DeclState(this)
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
              RecSplice(symbol.mkDefn)
            }
          )
        // The rest should never be used by the compiler, but ones that make
        // sense can be added if required in the future
        case _ => unreachable
      }
    } ensuring { _defn != null }

    // Create a Defn node describing this symbol based on it's type, using
    // the provided optional initializer expression
    def mkDefn(initOpt: Option[Expr]): Defn = {
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
    def mkDefn(init: Expr): Defn = {
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
    // Clone symbol (create new symbol with same name, loc and attributes)
    ////////////////////////////////////////////////////////////////////////////

    def dup(implicit cc: CompilerContext): Symbol =
      cc.newSymbol(name, loc) tap { newSymbol =>
        newSymbol.attr update attr
        newSymbol.scopeName = scopeName
        newSymbol.origName = origName
      }

  }

  object Symbol {
    // Extractor to pattern match against symbol name
    def unapply(arg: Symbol): Option[String] = Some(arg.name)

    ////////////////////////////////////////////////////////////////////////////
    // Ordering for Symbol
    ////////////////////////////////////////////////////////////////////////////

    implicit val symbolOrdering: Ordering[Symbol] = (x: Symbol, y: Symbol) =>
      if (x eq y) {
        0
      } else {
        val compare1 = implicitly[Ordering[Loc]].compare(x.loc, y.loc)
        if (compare1 != 0) {
          compare1
        } else {
          val compare2 = x.name compare y.name
          if (compare2 != 0) {
            compare2
          } else {
            x.id compare y.id
          }
        }
      }

  }

}
