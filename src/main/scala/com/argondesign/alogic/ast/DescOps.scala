////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.util.unreachable

trait DescOps { this: Desc =>

  final def symbol: Symbol = ref match {
    case Sym(symbol) => symbol
    case _           => unreachable
  }

  final def name: String = symbol.name

  final lazy val initializer: Option[Expr] = this match {
    case DescVar(_, _, _, iOpt)       => iOpt
    case DescVal(_, _, _, i)          => Some(i)
    case DescStatic(_, _, _, iOpt)    => iOpt
    case DescOut(_, _, _, _, _, iOpt) => iOpt
    case DescParam(_, _, _, iOpt, _)  => iOpt
    case DescParamType(_, _, iOpt, _) => iOpt
    case DescConst(_, _, _, i)        => Some(i)
    case DescGenVar(_, _, _, i)       => Some(i)
    case _                            => None
  }

  private def hasParamDesc(body: IterableOnce[Tree], inGenOnly: Boolean): Boolean =
    body.iterator map {
      case PkgSplice(tree)  => tree
      case EntSplice(tree)  => tree
      case RecSplice(tree)  => tree
      case StmtSplice(tree) => tree
      case other            => other
    } exists {
      case _: DescParam     => !inGenOnly
      case _: DescParamType => !inGenOnly
      case DescGenIf(_, _, cases, defaults) =>
        hasParamDesc(cases, inGenOnly = false) || hasParamDesc(defaults, inGenOnly = false)
      case DescGenFor(_, _, _, _, _, body)   => hasParamDesc(body, inGenOnly = false)
      case DescGenRange(_, _, _, _, _, body) => hasParamDesc(body, inGenOnly = false)
      case GenCase(_, body)                  => hasParamDesc(body, inGenOnly = false)
      case _                                 => false
    }

  final lazy val isParametrized: Boolean = this match {
    case DescEntity(_, _, _, body) => hasParamDesc(body, inGenOnly = false)
    case DescRecord(_, _, body)    => hasParamDesc(body, inGenOnly = false)
    case DescPackage(_, _, body)   => hasParamDesc(body, inGenOnly = false)
    case _                         => false
  }

  final lazy val hasGeneratedParam: Boolean = this match {
    case DescEntity(_, _, _, body) => hasParamDesc(body, inGenOnly = true)
    case DescRecord(_, _, body)    => hasParamDesc(body, inGenOnly = true)
    case DescPackage(_, _, body)   => hasParamDesc(body, inGenOnly = true)
    case _                         => false
  }

  def copyRef(ref: Ref): Desc = this match {
    // $COVERAGE-OFF$ Trivial to keep full, but not necessarily used
    case node: DescVar          => node.copy(ref = ref)
    case node: DescVal          => node.copy(ref = ref)
    case node: DescStatic       => node.copy(ref = ref)
    case node: DescIn           => node.copy(ref = ref)
    case node: DescOut          => node.copy(ref = ref)
    case node: DescPipeVar      => node.copy(ref = ref)
    case node: DescPipeIn       => node.copy(ref = ref)
    case node: DescPipeOut      => node.copy(ref = ref)
    case node: DescParam        => node.copy(ref = ref)
    case node: DescParamType    => node.copy(ref = ref)
    case node: DescConst        => node.copy(ref = ref)
    case node: DescArray        => node.copy(ref = ref)
    case node: DescSram         => node.copy(ref = ref)
    case node: DescType         => node.copy(ref = ref)
    case node: DescEntity       => node.copy(ref = ref)
    case node: DescRecord       => node.copy(ref = ref)
    case node: DescInstance     => node.copy(ref = ref)
    case node: DescSingleton    => node.copy(ref = ref)
    case node: DescFunc         => node.copy(ref = ref)
    case node: DescPackage      => node.copy(ref = ref)
    case node: DescGenVar       => node.copy(ref = ref)
    case node: DescGenIf        => node.copy(ref = ref)
    case node: DescGenFor       => node.copy(ref = ref)
    case node: DescGenRange     => node.copy(ref = ref)
    case node: DescGenScope     => node.copy(ref = ref)
    case node: DescAlias        => node.copy(ref = ref)
    case node: DescParametrized => node.copy(ref = ref)
    // $COVERAGE-ON$
  }

  def copyAttr(attr: List[Attr]): Desc = this match {
    // $COVERAGE-OFF$ Trivial to keep full, but not necessarily used
    case node: DescVar          => node.copy(attr = attr)
    case node: DescVal          => node.copy(attr = attr)
    case node: DescStatic       => node.copy(attr = attr)
    case node: DescIn           => node.copy(attr = attr)
    case node: DescOut          => node.copy(attr = attr)
    case node: DescPipeVar      => node.copy(attr = attr)
    case node: DescPipeIn       => node.copy(attr = attr)
    case node: DescPipeOut      => node.copy(attr = attr)
    case node: DescParam        => node.copy(attr = attr)
    case node: DescParamType    => node.copy(attr = attr)
    case node: DescConst        => node.copy(attr = attr)
    case node: DescArray        => node.copy(attr = attr)
    case node: DescSram         => node.copy(attr = attr)
    case node: DescType         => node.copy(attr = attr)
    case node: DescEntity       => node.copy(attr = attr)
    case node: DescRecord       => node.copy(attr = attr)
    case node: DescInstance     => node.copy(attr = attr)
    case node: DescSingleton    => node.copy(attr = attr)
    case node: DescFunc         => node.copy(attr = attr)
    case node: DescPackage      => node.copy(attr = attr)
    case node: DescGenVar       => node.copy(attr = attr)
    case node: DescGenIf        => node.copy(attr = attr)
    case node: DescGenFor       => node.copy(attr = attr)
    case node: DescGenRange     => node.copy(attr = attr)
    case node: DescGenScope     => node.copy(attr = attr)
    case node: DescAlias        => node.copy(attr = attr)
    case node: DescParametrized => node.copy(attr = attr)
    // $COVERAGE-ON$
  }

}

trait DescObjOps { self: Desc.type =>

  final def unapply(desc: Desc): Some[Ref] = Some(desc.ref)

}
