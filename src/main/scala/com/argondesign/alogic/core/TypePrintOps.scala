////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Peter de Rivaz/Geza Lore
//
// DESCRIPTION:
//
// Pretty printers for Type
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable

trait TypePrintOps { this: Type =>

  def toSource(implicit cc: CompilerContext): String = this.chase match {
    case TypeCombStmt                  => "type-comb-statement"
    case TypeCtrlStmt                  => "type-ctrl-statement"
    case TypeState                     => "type-state"
    case TypeSInt(ExprNum(_, value))   => s"i${value}"
    case TypeUInt(ExprNum(_, value))   => s"u${value}"
    case TypeSInt(size)                => s"int(${size.toSource})"
    case TypeUInt(size)                => s"uint(${size.toSource})"
    case TypeNum(true)                 => "int"
    case TypeNum(false)                => "uint"
    case TypeVector(elementType, size) => s"vec<${elementType.toString}>(${size.toSource})"
    case TypeArray(elementType, size)  => s"${elementType.toString}[${size.toSource}]"
    case TypeStack(elementType, size)  => s"stack<${elementType.toString}>(${size.toSource})"
    case TypeStruct(name, _, _)        => s"struct ${name}"
    case TypeVoid                      => "void"
    case TypeCombFunc(argTypes, retType) =>
      s"comb ${argTypes map { _.toSource } mkString ", "} -> ${retType.toSource}"
    case TypeCtrlFunc(argTypes, retType) =>
      s"ctrl ${argTypes map { _.toSource } mkString ", "} -> ${retType.toSource}"
    case TypeEntity(name, _, _, _, _)             => s"entity ${name}"
    case TypeStr                                  => "string"
    case TypeIn(kind, FlowControlTypeNone)        => s"in ${kind.toSource}"
    case TypeIn(kind, FlowControlTypeValid)       => s"in sync ${kind.toSource}"
    case TypeIn(kind, FlowControlTypeReady)       => s"in sync ready ${kind.toSource}"
    case TypeIn(kind, FlowControlTypeAccept)      => s"in sync accept ${kind.toSource}"
    case TypeOut(kind, FlowControlTypeNone, st)   => s"out ${st} ${kind.toSource}"
    case TypeOut(kind, FlowControlTypeValid, st)  => s"out sync ${st} ${kind.toSource}"
    case TypeOut(kind, FlowControlTypeReady, st)  => s"out sync ready ${st} ${kind.toSource}"
    case TypeOut(kind, FlowControlTypeAccept, st) => s"out sync accept ${st} ${kind.toSource}"
    case TypePipeline(kind)                       => s"pipeline ${kind.toSource}"
    case TypeParam(kind)                          => s"param ${kind.toSource}"
    case TypeConst(kind)                          => s"const ${kind.toSource}"
    case TypeType(kind)                           => s"type ${kind.toSource}"
    case TypeMisc                                 => "type-misc"
    case TypeError                                => "type-error"
    case _: TypePolyFunc                          => "type-poly-func"
    case _: TypeRef                               => unreachable
  }
}
