////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

trait StmtCaseOps { this: StmtCase =>

  // Arbitrary limit avoid memory blow up
  final def smallCase: Boolean = expr.tpe.width <= 16

  final private def explicitlyCoveredValues: collection.BitSet = {
    require(smallCase) // Avoid blow up on large case. Use site should ensure.
    val set = mutable.BitSet()
    cases.foreach {
      case CaseRegular(cs, _) => cs.foreach(_.valueOption.foreach(set add _.toInt))
      case _: CaseDefault     => // Ignore
      case _                  => unreachable
    }
    set
  }

  // Covers all values without considering the default branch
  // Note: conservative for large case statements
  final lazy val coversAllWithoutDefault: Boolean =
    smallCase && (explicitlyCoveredValues.size == 1 << expr.tpe.width.toInt)

  // Has a default branch
  final lazy val hasDefault: Boolean = cases.exists(_.isInstanceOf[CaseDefault])

  // Note: conservative for large case statements
  final lazy val isFullCase: Boolean = hasDefault || coversAllWithoutDefault

}
