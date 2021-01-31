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
import com.argondesign.alogic.core.ParOrSeqIterable
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.Types.TypePackage
import com.argondesign.alogic.util.unreachable

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
    extends SimplePass[
      Option[(DescPackage, ParOrSeqIterable[DescPackage])],
      ParOrSeqIterable[Desc]
    ] {
  val name = "drop-package-and-parametrized-descs"

  override protected def dump(
      result: ParOrSeqIterable[Desc],
      tag: String
    )(
      implicit
      cc: CompilerContext
    ): Unit =
    result.asPar foreach { desc => cc.dump(desc, "." + tag) }

  override protected def process(
      input: Option[(DescPackage, ParOrSeqIterable[DescPackage])]
    )(
      implicit
      cc: CompilerContext
    ): ParOrSeqIterable[Desc] =
    input
      .map {
        case (root, dependencies) => dependencies + root
      }
      .getOrElse(ParOrSeqIterable.empty)
      .asPar
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

}
