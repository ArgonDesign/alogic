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
import com.argondesign.alogic.util.unreachable

trait StmtOps { this: Stmt =>

  // Indicates if this statement always returns, i.e.: anything following it
  // in a sequence of statements will have no effect
  lazy val alwaysReturns: Boolean = this match {
    case _: StmtReturn          => true
    case StmtIf(_, ts, es)      => (ts exists { _.alwaysReturns }) && (es exists { _.alwaysReturns })
    case StmtCase(_, cases)     => cases forall { _.stmts exists { _.alwaysReturns } }
    case StmtBlock(ss)          => ss exists { _.alwaysReturns }
    case StmtLoop(body)         => body exists { _.alwaysReturns }
    case StmtWhile(_, body)     => body exists { _.alwaysReturns }
    case StmtFor(_, _, _, body) => body exists { _.alwaysReturns }
    case StmtDo(_, body)        => body exists { _.alwaysReturns }
    case _: StmtLet             => unreachable
    case _                      => false
  }

  // Indicates if this statement will never return, i.e.: anything following it
  // in a sequence of statements will have effect
  lazy val neverReturns: Boolean = this match {
    case _: StmtReturn          => false
    case StmtIf(_, ts, es)      => (ts forall { _.neverReturns }) && (es forall { _.neverReturns })
    case StmtCase(_, cases)     => cases forall { _.stmts forall { _.neverReturns } }
    case StmtBlock(ss)          => ss forall { _.neverReturns }
    case StmtLoop(body)         => body forall { _.neverReturns }
    case StmtWhile(_, body)     => body forall { _.neverReturns }
    case StmtFor(_, _, _, body) => body forall { _.neverReturns }
    case StmtDo(_, body)        => body forall { _.neverReturns }
    case _: StmtLet             => unreachable
    case _                      => true
  }

}
