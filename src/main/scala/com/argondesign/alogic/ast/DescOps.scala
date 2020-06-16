////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Symbols.Symbol

trait DescOps { this: Desc =>

  final def symbol: Symbol = ref.symbol

  final def name: String = ref.name

  def descs: List[Desc] = Nil

  def decls: List[Decl] = Nil

  final lazy val params: List[Desc] = descs collect {
    case desc: DescParam     => desc
    case desc: DescParamType => desc
  }

  final lazy val mayHaveGeneratedParam: Boolean = this match {
    case DescEntity(_, _, body) =>
      body exists { case EntGen(gen) => gen.mayGenerateParam; case _ => false }
    case DescRecord(_, body) =>
      body exists { case RecGen(gen) => gen.mayGenerateParam; case _ => false }
    case _ => false
  }

  final lazy val isParametrized: Boolean = params.nonEmpty || mayHaveGeneratedParam

  final lazy val initializer: Option[Expr] = this match {
    case DescVar(_, _, iOpt)       => iOpt
    case DescVal(_, _, i)          => Some(i)
    case DescOut(_, _, _, _, iOpt) => iOpt
    case DescParam(_, _, iOpt)     => iOpt
    case DescParamType(_, iOpt)    => iOpt
    case DescConst(_, _, i)        => Some(i)
    case DescGen(_, _, i)          => Some(i)
    case _                         => None
  }

  def cpy(ref: Ref): Desc = this match {
    // $COVERAGE-OFF$ Trivial to keep full, but not necessarily used
    case node: DescVar       => node.copy(ref = ref)
    case node: DescVal       => node.copy(ref = ref)
    case node: DescIn        => node.copy(ref = ref)
    case node: DescOut       => node.copy(ref = ref)
    case node: DescPipeline  => node.copy(ref = ref)
    case node: DescParam     => node.copy(ref = ref)
    case node: DescParamType => node.copy(ref = ref)
    case node: DescConst     => node.copy(ref = ref)
    case node: DescGen       => node.copy(ref = ref)
    case node: DescArray     => node.copy(ref = ref)
    case node: DescSram      => node.copy(ref = ref)
    case node: DescType      => node.copy(ref = ref)
    case node: DescEntity    => node.copy(ref = ref)
    case node: DescRecord    => node.copy(ref = ref)
    case node: DescInstance  => node.copy(ref = ref)
    case node: DescSingleton => node.copy(ref = ref)
    case node: DescFunc      => node.copy(ref = ref)
    case node: DescChoice    => node.copy(ref = ref)
    // $COVERAGE-ON$
  }

}

trait DescObjOps { self: Desc.type =>

  final def unapply(desc: Desc): Option[Ref] = Some(desc.ref)

}
