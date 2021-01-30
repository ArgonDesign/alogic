////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.Types.TypePackage
import com.argondesign.alogic.util.unreachable

import scala.collection.parallel.CollectionConverters.IterableIsParallelizable

object DropPackageAndParametrizedDescsTransform extends StatelessTreeTransformer {

  override protected def enter(tree: Tree): Option[Tree] = tree match {
    case _: DescParametrized => Some(Stump) // Drop (there are no references at this point)
    case _                   => None
  }

  override protected def transform(tree: Tree): Tree = tree match {
    case ExprSel(tgt, name) =>
      tgt.tpe match {
        case kind: TypePackage => TypeAssigner(ExprSym(kind(name).get) withLocOf tree)
        case _                 => tree
      }
    case _ => tree
  }

}

object DropPackageAndParametrizedDescs
    extends SimplePass[Option[(DescPackage, Iterable[DescPackage])], Iterable[Desc]] {
  val name = "drop-package-and-parametrized-descs"

  override protected def dump(
      result: Iterable[Desc],
      tag: String
    )(
      implicit
      cc: CompilerContext
    ): Unit =
    result foreach { desc => cc.dump(desc, "." + tag) }

  override protected def process(
      input: Option[(DescPackage, Iterable[DescPackage])]
    )(
      implicit
      cc: CompilerContext
    ): Iterable[Desc] =
    input.toIterable
      .flatMap {
        case (root, dependencies) => Iterable(root) concat dependencies
      }
      .par
      .flatMap {
        case DescPackage(_, _, body) =>
          body.flatMap {
            case PkgSplice(d: Desc) =>
              DropPackageAndParametrizedDescsTransform(d) match {
                case Stump   => None
                case d: Desc => Some(d)
                case _       => unreachable
              }
            case _ => None // Drop everything else
          }
      }
      .seq

}
