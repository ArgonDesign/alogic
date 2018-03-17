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
import com.argondesign.alogic.core.Types._

trait TypePrintOps { this: Type =>

  def toSource: String = this match {
    case TypeCombStmt                       => "type-comb-statement"
    case TypeCtrlStmt                       => "type-ctrl-statement"
    case TypeSInt(size)                     => s"int(${size.toSource})"
    case TypeUInt(size)                     => s"uint(${size.toSource})"
    case TypeNum(true)                      => "signed-number"
    case TypeNum(false)                     => "unsigned-number"
    case TypeVector(elementType, size)      => s"vec(${elementType.toString}, ${size.toString})"
    case TypeArray(elementType, size)       => s"${elementType.toString}[${size.toString}]"
    case TypeStruct(fieldNames, fieldTypes) => "struct ???"
    case TypeVoid                           => "void"
    case TypeRef(ref: Ref)                  => s"ref ${ref.toSource}"
    case TypeCombFunc(argTypes, retType) =>
      s"comb ${argTypes map { _.toSource } mkString ", "} -> ${retType.toSource}"
    case TypeCtrlFunc(argTypes, retType) =>
      s"ctrl ${argTypes map { _.toSource } mkString ", "} -> ${retType.toSource}"
    case _: TypeEntity          => "entity ???"
    case TypeStr                => "string"
    case TypeIn(kind, fct)      => s"in ${fct} ${kind.toString}"
    case TypeOut(kind, fct, st) => s"out ${fct} ${st} ${kind.toString}"
    case TypePipeline(kind)     => s"pipeline ${kind.toString}"
    case TypeParam(kind)        => s"param ${kind.toString}"
    case TypeConst(kind)        => s"const ${kind.toString}"
    case TypeType(kind)         => s"type ${kind.toString}"
    case TypeMisc               => "type-misc"
    case TypeError              => "type-error"
    case TypeUnknown            => "type-unknown"
    case _: TypePolyFunc        => "type-poly-func"
  }
}
