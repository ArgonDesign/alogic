////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext

object MarkTopLevels
    extends SimplePass[Option[(DescPackage, Iterable[DescPackage])], Option[
      (DescPackage, Iterable[DescPackage])
    ]] {
  val name = "mark-top-levels"

  override protected def dump(
      result: Option[(DescPackage, Iterable[DescPackage])],
      tag: String
    )(
      implicit
      cc: CompilerContext
    ): Unit = result foreach {
    case (input, dependencies) =>
      cc.dump(input, "." + tag)
      dependencies.foreach(cc.dump(_, "." + tag))
  }

  override protected def process(
      input: Option[(DescPackage, Iterable[DescPackage])]
    )(
      implicit
      cc: CompilerContext
    ): Option[(DescPackage, Iterable[DescPackage])] = input flatMap {
    case (desc, _) =>
      // First see if there are are any explicit 'compile' directives,
      // if so gather the target symbols
      val explicitTargets = desc.body collect {
        case PkgCompile(expr, _) => expr.tpe.asType.kind.asEntity.symbol
      }

      if (explicitTargets.nonEmpty) {
        // Mark explicit targets as toplevels
        explicitTargets.foreach(_.attr.topLevel set true)
        // No further processing needed
        input
      } else {
        // No explicit 'compile' directives.

        lazy val baseName = desc.loc.source.name

        // Gather entities defined at the top level (including parametrized
        // ones for error reporting)
        desc.body collect {
          case PkgSplice(DescEntity(Sym(symbol), _, _, _))                   => symbol
          case PkgSplice(DescParametrized(Sym(symbol), _, _: DescEntity, _)) => symbol
        } match {
          case Nil =>
            cc.error(
              desc,
              s"Input file '$baseName' does not contain any entity definitions nor 'compile' directives"
            )
            None
          case symbol :: Nil if symbol.kind.isParametrized =>
            cc.error(
              symbol,
              "Parametrized top level definition requires an explicit 'compile' directive"
            )
            None
          case symbol :: Nil =>
            // The lone top level entity, mark it as such
            symbol.attr.topLevel set true
            // No further processing needed
            input
          case symbols =>
            symbols foreach {
              cc.error(
                _,
                s"Input file '$baseName' contains multiple entity definitions",
                "please use explicit 'compile' directives to define top level compilation targets"
              )
            }
            None
        }
      }
  }

}
