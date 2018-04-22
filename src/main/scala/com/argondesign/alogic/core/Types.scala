////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Representations of various internal types.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes._
import com.argondesign.alogic.core.StorageTypes.StorageType
import com.argondesign.alogic.core.Symbols.TermSymbol
import com.argondesign.alogic.core.Symbols.TypeSymbol
import com.argondesign.alogic.lib.StructuredTree

object Types {

  // The type system represents more than just  user facing types.
  // Inside the compiler, every tree node has a type.

  // Root of all types
  sealed trait Type extends StructuredTree with TypeOps

  ///////////////////////////////////////////////////////////////////////////////
  // All possible types
  ///////////////////////////////////////////////////////////////////////////////

  // Type of combinatorial statements
  case object TypeCombStmt extends Type
  // Type of control statements
  case object TypeCtrlStmt extends Type
  // Type of states
  case object TypeState extends Type

  sealed trait TypeInt extends Type

  // Simple signed integer types e.g.: i8 / int(N), analogous to Verilog packed arrays
  case class TypeSInt(size: Expr) extends TypeInt
  // Simple unsigned integer types e.g.: u2 / uint(N), analogous to Verilog packed arrays
  case class TypeUInt(size: Expr) extends TypeInt
  // Unsized numbers e.g: 1
  case class TypeNum(signed: Boolean) extends Type
  // Vector types (analogous to higher dimensions of SystemVerilog multi-dimensional packed arrays)
  case class TypeVector(elementType: Type, size: Expr) extends Type
  // Array types (analogous to verilog unpacked arrays)
  case class TypeArray(elementType: Type, size: Expr) extends Type
  // Stack types
  case class TypeStack(elementType: Type, size: Expr) extends Type with TypeStackImpl
  // Structure type
  case class TypeStruct(name: String, fieldNames: List[String], fieldTypes: List[Type])
      extends Type
      with TypeStructImpl
  // Void type
  case object TypeVoid extends Type
  // Named type e.g. 'foo_t foo;'
  case class TypeIdent(ident: Ident) extends Type
  // Combinatorial function type e.g. 'port.read'
  case class TypeCombFunc(argTypes: List[Type], retType: Type) extends Type
  // State function type e.g. 'void foo() {}'
  case class TypeCtrlFunc(argTypes: List[Type], retType: Type) extends Type
  // Entity type e.g. 'fsm foo {}'
  case class TypeEntity(name: String, portSymbols: List[TermSymbol], paramSymbols: List[TermSymbol])
      extends Type
      with TypeEntityImpl
  // Instance type
  case class TypeInstance(entitySymbol: TypeSymbol) extends Type with TypeInstanceImpl
  // Strings
  case object TypeStr extends Type

  // Input port type
  case class TypeIn(kind: Type, fct: FlowControlType) extends Type with TypeInImpl
  // Output port type
  case class TypeOut(kind: Type, fct: FlowControlType, st: StorageType)
      extends Type
      with TypeOutImpl
  // Pipeline variable type
  case class TypePipeline(kind: Type) extends Type
  // Parameter type
  case class TypeParam(kind: Type) extends Type
  // Constant type
  case class TypeConst(kind: Type) extends Type

  // Type of expressions that represent types, with underlying type kind e.g. ExprType(_)
  case class TypeType(kind: Type) extends Type

  // Type of other tree nodes where type is not important or meaningful
  case object TypeMisc extends Type

  // Placeholder if error happened and a valid type cannot be computed
  case object TypeError extends Type

  // Polymorphic function. Used to handle overloaded builtin functions
  case class TypePolyFunc(resolver: List[Expr] => CompilerContext => Option[TermSymbol])
      extends Type {

    // Resolve based on argument types to the correct overload of the symbol
    def resolve(argTypes: List[Expr])(implicit cc: CompilerContext): Option[TermSymbol] = {
      resolver(argTypes)(cc)
    }
  }

  ///////////////////////////////////////////////////////////////////////////////
  // base trait companions
  ///////////////////////////////////////////////////////////////////////////////

  object TypeInt {
    def apply(signed: Boolean, size: Expr): TypeInt = if (signed) TypeSInt(size) else TypeUInt(size)

    def unapply(expr: Type): Option[Expr] = expr match {
      case TypeSInt(size) => Some(size)
      case TypeUInt(size) => Some(size)
      case _              => None
    }
  }

  ///////////////////////////////////////////////////////////////////////////////
  // Implementations
  ///////////////////////////////////////////////////////////////////////////////

  private val oneExpr = Expr(1) withLoc Loc.synthetic

  // A base trait for types that have fields that can be looked up using dot notation
  trait CompoundType {
    // Apply returns the type of the field selected
    def apply(field: String): Option[Type]
  }

  // Base trait of types that add fields to existing types
  trait ExtensionType extends CompoundType {
    // The underlying type
    def kind: Type

    // The extensions
    def extensions: Map[String, Type]

    def apply(name: String): Option[Type] = {
      extensions.get(name) orElse {
        kind match {
          case comp: CompoundType => comp(name)
          case _                  => None
        }
      }
    }
  }

  trait TypeStructImpl extends CompoundType { this: TypeStruct =>

    lazy val fields = fieldNames zip fieldTypes

    private[this] lazy val fieldMap = fields.toMap

    def apply(name: String): Option[Type] = fieldMap.get(name)
  }

  trait TypeEntityImpl extends CompoundType { this: TypeEntity =>

    private[this] lazy val portMap = {
      val pairs = for (symbol <- portSymbols) yield {
        symbol.denot.name.str -> symbol
      }
      pairs.toMap
    }

    private[this] lazy val paramMap = {
      val pairs = for (symbol <- paramSymbols) yield {
        symbol.denot.name.str -> symbol
      }
      pairs.toMap
    }

    def apply(name: String): Option[Type] = portMap.get(name) map { _.denot.kind }

    def param(name: String): Option[Type] = paramMap.get(name) map { _.denot.kind }

    def portSymbol(name: String): Option[TermSymbol] = portMap.get(name)

    def paramSymbol(name: String): Option[TermSymbol] = paramMap.get(name)
  }

  trait TypeInstanceImpl extends CompoundType { this: TypeInstance =>

    private[this] def entityType = entitySymbol.denot.kind.asInstanceOf[TypeEntity]

    def portSymbols = entityType.portSymbols

    def apply(name: String): Option[Type] = entityType(name)

    def portSymbol(name: String): Option[TermSymbol] = entityType.portSymbol(name)
  }

  trait TypeInImpl extends ExtensionType { this: TypeIn =>
    lazy val extensions = fct match {
      case FlowControlTypeNone => {
        Map(
          "read" -> TypeCombFunc(Nil, kind)
        )
      }
      case FlowControlTypeValid | FlowControlTypeReady => {
        Map(
          "read" -> TypeCombFunc(Nil, kind),
          "valid" -> TypeCombFunc(Nil, TypeUInt(oneExpr)),
          "wait" -> TypeCombFunc(Nil, TypeVoid)
        )
      }
      case FlowControlTypeAccept => {
        Map(
          "read" -> TypeCombFunc(Nil, kind)
        )
      }
    }
  }

  trait TypeOutImpl extends ExtensionType { this: TypeOut =>
    lazy val extensions = {
      val writeFuncType = if (kind == TypeVoid) {
        TypeCombFunc(Nil, TypeVoid)
      } else {
        TypeCombFunc(List(kind), TypeVoid)
      }
      fct match {
        case FlowControlTypeNone => {
          Map(
            "write" -> writeFuncType
          )
        }
        case FlowControlTypeValid => {
          Map(
            "write" -> writeFuncType,
            "valid" -> TypeCombFunc(Nil, TypeUInt(oneExpr)),
            "flush" -> TypeCombFunc(Nil, TypeVoid)
          )
        }
        case FlowControlTypeReady => {
          Map(
            "write" -> writeFuncType,
            "valid" -> TypeCombFunc(Nil, TypeUInt(oneExpr)),
            "flush" -> TypeCombFunc(Nil, TypeVoid),
            "full" -> TypeCombFunc(Nil, TypeUInt(oneExpr)),
            "empty" -> TypeCombFunc(Nil, TypeUInt(oneExpr))
          )
        }
        case FlowControlTypeAccept => {
          Map(
            "write" -> writeFuncType,
          )
        }
      }
    }
  }

  trait TypeStackImpl extends CompoundType { this: TypeStack =>

    val fieldMap = Map(
      "push" -> TypeCombFunc(List(elementType), TypeVoid),
      "pop" -> TypeCombFunc(Nil, elementType),
      "set" -> TypeCombFunc(List(elementType), TypeVoid),
      "top" -> elementType,
      "full" -> TypeUInt(oneExpr),
      "empty" -> TypeUInt(oneExpr)
    )

    def apply(field: String): Option[Type] = fieldMap get field
  }
}

/*

// Generic stack

stack<i8>(10) s;

s.push(addr); // i8 => void
s.pop();      // void => i8
s.set(addr);  // i8 => void // Same as 's = addr' if s is an automatic variable
s.top;        // i8 // Same as 's' if s is an automatic variable
s.full;       // bool
s.empty;      // bool

restrictions:
 - Can only do a single push, pop, or set in the same cycle, compiler should err otherwise
 - .push() when full is error
 - .pop() when empty is error
 - .top when empty is error

// Function call/return handling ('rs' is for 'return-stack':

foo();    // maps to rs.push(return-state); goto foo;
goto foo; // nothing to do here, just goto foo;
return;   // maps to goto rs.pop();

// Function argument handling

At call site: arg.push(value)
At exit: arg.pop()

// Function local variable handling

At definition of variable: local.push(initializer)
At death/exit from function: local.pop()

// Hardware interface:

_d
_q

_push
_pop
_full
_empty

at beginning:
_d = _q

 */
