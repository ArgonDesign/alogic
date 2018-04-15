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
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable

trait TypePrintOps { this: Type =>

  final private[this] def fct2String(fct: FlowControlType): String = fct match {
    case FlowControlTypeNone   => ""
    case FlowControlTypeValid  => "sync "
    case FlowControlTypeReady  => "sync ready "
    case FlowControlTypeAccept => "sync accept "
  }

  final private[this] def st2String(st: StorageType): String = st match {
    case StorageTypeDefault => ""
    case StorageTypeReg     => "reg "
    case StorageTypeWire    => "wire "
    case StorageTypeSlices(slices) => {
      slices map {
        case StorageSliceFwd    => "fslice"
        case StorageSliceBwd    => "bslice"
        case StorageSliceBubble => "bubble"
      } mkString ("", " ", " ")
    }
  }

  final def toSource(implicit cc: CompilerContext): String = this.chase match {
    case TypeCombStmt                   => "type-comb-statement"
    case TypeCtrlStmt                   => "type-ctrl-statement"
    case TypeState                      => "type-state"
    case TypeSInt(ExprNum(_, value))    => s"i${value}"
    case TypeUInt(ExprNum(_, value))    => s"u${value}"
    case TypeSInt(ExprInt(_, _, value)) => s"i${value}"
    case TypeUInt(ExprInt(_, _, value)) => s"u${value}"
    case TypeSInt(size)                 => s"int(${size.toSource})"
    case TypeUInt(size)                 => s"uint(${size.toSource})"
    case TypeNum(true)                  => "int"
    case TypeNum(false)                 => "uint"
    case TypeVector(elementType, size)  => s"vec<${elementType.toSource}>(${size.toSource})"
    case TypeArray(elementType, size)   => s"${elementType.toSource}[${size.toSource}]"
    case TypeStack(elementType, size)   => s"stack<${elementType.toSource}>(${size.toSource})"
    case TypeStruct(name, _, _)         => s"struct ${name}"
    case TypeVoid                       => "void"
    case TypeCombFunc(argTypes, retType) =>
      s"comb ${argTypes map { _.toSource } mkString ", "} -> ${retType.toSource}"
    case TypeCtrlFunc(argTypes, retType) =>
      s"ctrl ${argTypes map { _.toSource } mkString ", "} -> ${retType.toSource}"
    case TypeEntity(name, _, _) => s"entity ${name}"
    case TypeStr                => "string"
    case TypeIn(kind, fct)      => s"in ${fct2String(fct)}${kind.toSource}"
    case TypeOut(kind, fct, st) => s"out ${fct2String(fct)}${st2String(st)}${kind.toSource}"
    case TypePipeline(kind)     => s"pipeline ${kind.toSource}"
    case TypeParam(kind)        => s"param ${kind.toSource}"
    case TypeConst(kind)        => s"const ${kind.toSource}"
    case TypeType(kind)         => s"type ${kind.toSource}"
    case TypeMisc               => "type-misc"
    case TypeError              => "type-error"
    case _: TypePolyFunc        => "type-poly-func"
    case _: TypeRef             => unreachable
  }
}
