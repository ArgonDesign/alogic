////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.util.unreachable

trait StmtOps { this: Stmt =>

  // Indicates if this statement always terminates execution of a sequence of
  // statements, i.e: always returns or throws/stops the world, i.e.: statements
  // following it in a sequence of statements will have no effect
  lazy val alwaysTerminates: Boolean = this match {
    case _: StmtReturn | StmtSplice(_: AssertionUnreachable) => true
    //
    case StmtIf(_, ts, es)      => ts.exists(_.alwaysTerminates) && es.exists(_.alwaysTerminates)
    case StmtCase(_, cases)     => cases.forall(_.stmts.exists(_.alwaysTerminates))
    case StmtBlock(ss)          => ss exists { _.alwaysTerminates }
    case StmtLoop(body)         => body.exists(_.alwaysTerminates)
    case StmtWhile(_, body)     => body.exists(_.alwaysTerminates)
    case StmtFor(_, _, _, body) => body.exists(_.alwaysTerminates)
    case StmtDo(_, body)        => body.exists(_.alwaysTerminates)
    case _: StmtLet             => unreachable
    case _                      => false
  }

  // Indicates if this statement never terminates execution of a sequence of
  // statements, i.e: never returns or throws/stops the world, i.e.: statements
  // following it in a sequence of statements will have effect
  lazy val neverTerminates: Boolean = this match {
    case _: StmtReturn | StmtSplice(_: AssertionUnreachable) => false
    //
    case StmtIf(_, ts, es)      => ts.forall(_.neverTerminates) && es.forall(_.neverTerminates)
    case StmtCase(_, cases)     => cases.forall(_.stmts.forall(_.neverTerminates))
    case StmtBlock(ss)          => ss.forall(_.neverTerminates)
    case StmtLoop(body)         => body.forall(_.neverTerminates)
    case StmtWhile(_, body)     => body.forall(_.neverTerminates)
    case StmtFor(_, _, _, body) => body.forall(_.neverTerminates)
    case StmtDo(_, body)        => body.forall(_.neverTerminates)
    case _: StmtLet             => unreachable
    case _                      => true
  }

}
