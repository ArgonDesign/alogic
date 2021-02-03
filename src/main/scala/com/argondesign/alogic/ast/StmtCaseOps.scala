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

  final lazy val isFullCase: Boolean =
    cases.exists(_.isInstanceOf[CaseDefault]) || {
      (expr.tpe.width <= 16) && {
        val set = mutable.BitSet()
        cases.foreach {
          case CaseRegular(cs, _) => cs.foreach(_.valueOption.foreach(set add _.toInt))
          case _                  => unreachable
        }
        set.size == 1 << expr.tpe.width.toInt
      }
    }

}
