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

import com.argondesign.alogic.core.FlowControlTypes._
import com.argondesign.alogic.core.StorageTypes._
import com.argondesign.alogic.core.Types._

// $COVERAGE-OFF$ debug code
trait TypePrintOps { this: Type =>

  final private[this] def fct2String(fct: FlowControlType): String = fct match {
    case FlowControlTypeNone  => ""
    case FlowControlTypeValid => "sync "
    case FlowControlTypeReady => "sync ready "
  }

  final private[this] def st2String(st: StorageType): String = st match {
    case StorageTypeDefault => ""
    case StorageTypeReg     => "reg "
    case StorageTypeWire    => "wire "
    case StorageTypeSlices(slices) =>
      slices map {
        case StorageSliceFwd => "fslice"
        case StorageSliceBwd => "bslice"
        case StorageSliceBub => "bubble"
      } mkString ("", " ", " ")
  }

  // format: off
  final def toSource: String = this match {
    case TypeSInt(sz)                         => s"i$sz"
    case TypeUInt(sz)                         => s"u$sz"
    case TypeNum(true)                        => "int"
    case TypeNum(false)                       => "uint"
    case TypeVector(elem, sz)                 => s"${elem.toSource}[$sz]"
    case TypeVoid                             => "void"
    case TypeStr                              => "string"
    case TypeRecord(symbol, _)                => s"struct ${symbol.name}"
    case TypeEntity(symbol, _)                => s"entity ${symbol.name}"
    case TypeIn(kind, fct)                    => s"in ${fct2String(fct)}${kind.toSource}"
    case TypeOut(kind, fct, st)               => s"out ${fct2String(fct)}${st2String(st)}${kind.toSource}"
    case TypePipeVar(kind)                   => s"pipeline ${kind.toSource}"
    case TypePipeIn(fct)                      => s"in ${fct2String(fct)}pipeline"
    case TypePipeOut(fct, st)                 => s"out ${fct2String(fct)}${st2String(st)}pipeline"
    case TypeParam(kind)                      => s"param ${kind.toSource}"
    case TypeConst(kind)                      => s"const ${kind.toSource}"
    case TypeGen(kind)                        => s"gen ${kind.toSource}"
    case TypeArray(elem, sz)                  => s"${elem.toSource} _[$sz]"
    case TypeSram(elem, sz, StorageTypeWire)  => s"sram wire ${elem.toSource} _[$sz]"
    case TypeSram(elem, sz, _)                => s"sram ${elem.toSource} _[$sz]"
    case TypeStack(elem, sz)                  => s"stack ${elem.toSource} _[$sz]"
    case TypeType(kind)                       => s"type ${kind.toSource}"
    case TypeNone(kind)                       => s"none ${kind.toSource}"
    case TypeParametrized(symbol)             => s"parametrized ${symbol.name}"
    case TypeCombStmt                         => this.toString
    case TypeCtrlStmt                         => this.toString
    case TypeCombFunc(s, r, as)               => s"comb ${r.toSource} ${s.name} ${as map { _.toSource } mkString ("(", ", ", ")")}"
    case TypeCtrlFunc(s, r, as)               => s"ctrl ${r.toSource} ${s.name} ${as map { _.toSource } mkString ("(", ", ", ")")}"
    case TypeXenoFunc(s, r, as)               => s"xeno ${r.toSource} ${s.name} ${as map { _.toSource } mkString ("(", ", ", ")")}"
    case TypeStaticMethod(s, r, as)           => s"static ${r.toSource} ${s.name} ${as map { _.toSource } mkString ("(", ", ", ")")}"
    case TypeNormalMethod(s, r, as)           => s"method ${r.toSource} ${s.name} ${as map { _.toSource } mkString ("(", ", ", ")")}"
    case _: TypePolyFunc                      => this.toString
    case TypeUnknown                          => this.toString
    case TypeState(symbol)                    => s"state ${symbol.name}"
    case TypeMisc                             => this.toString
    case TypeError                            => this.toString
  }
  // format: on
}
