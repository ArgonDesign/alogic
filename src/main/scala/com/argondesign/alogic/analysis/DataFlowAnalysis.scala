////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Generic date flow analysis framework
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.analysis

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.util.unreachable

import scala.collection.immutable
import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

// A data flow analysis is defined by:
//  1. A direction, which is either 'forward' or 'backward'
//  2. A set of data flow values 'S'
//  3. A binary operator '/\' (pronounced 'meet') over the elements of 'S',
//     such that ('S', '/\') form a lower semilattice.
//  4. A family of transfer functions from 'S' to 'S'
//  5. A constant value from 'S' representing the boundary condition
trait DataFlowAnalysis[S] {

  //////////////////////////////////////////////////////////////////////////////
  // Analysis direction, true for forward, false for backward
  //////////////////////////////////////////////////////////////////////////////

  protected val directionForward: Boolean

  //////////////////////////////////////////////////////////////////////////////
  // The meet operator
  //
  // In order for the meet operator to form a semilattice over 'S', it must
  // have the following properties. For all values 'x', 'y', 'z' from 'S', the
  // meet operator must be:
  //      1. Associative: x /\ (y /\ z) == (x /\ y) /\ z
  //      2. Commutative: x /\ y == y /\ x
  //      3. Idempotent: x /\ x == x
  // The meet operator with these properties induces a partial order '<=' over
  // the set 'S', with the following properties of interest:
  //      1. x /\ y <= x
  //      2. x /\ y <= y
  //      3. if z <= x and z <= y then z <= x /\ y
  // The last point says that 'x /\ y' is the greatest lower bound of
  // 'x' and 'y'.
  //
  // In Alogic, due to the sate system conversion and mandatory loop unrolling,
  // we have the luxury of not having cycles in the flow graph (i.e.: the flow
  // graph is always a DAG). This means that we can always complete the analysis
  // in a single pass over the flow graph. This also means that we need not have
  // a bounded data flow lattice, i.e.: we do not need a 'top' element.
  //////////////////////////////////////////////////////////////////////////////

  protected def meet(x: S, y: S): S

  //////////////////////////////////////////////////////////////////////////////
  // The transfer functions
  //
  // the transfer functions of a data flow analysis transform elements of the
  // set 'S' according to the semantics of control flow graph nodes (which are
  // statements). Note in particular, that for branching statements, only the
  // condition evaluation needs to be taken into account, as the statements
  // in the branches will be visited on their own right.
  //////////////////////////////////////////////////////////////////////////////

  protected def f(curr: S, stmt: StmtIf): S // Only needs to consider condition
  protected def f(curr: S, stmt: StmtCase): S // Only needs to consider scrutinee
  protected def f(curr: S, kase: CaseRegular): S // Only needs to consider labels
  protected def f(curr: S, stmt: StmtAssign): S
  protected def f(curr: S, stmt: StmtDelayed): S
  protected def f(curr: S, stmt: StmtOutcall): S
  protected def f(curr: S, stmt: StmtExpr): S

  //////////////////////////////////////////////////////////////////////////////
  // The default boundary condition
  //
  // This represents the data flow value at the entry node for a forward data
  // flow problem, or the data flow value at the exit node for a backward data
  // flow problem.
  //////////////////////////////////////////////////////////////////////////////

  protected def defaultBoundaryCondition: S

  //////////////////////////////////////////////////////////////////////////////
  // The type of the solution to the data flow problem
  //
  // The solution of the data flow analysis is returned as a map from statement
  // identity to a pair of values in 'S'. The first element of the pair holds
  // the data flow value just prior to the statement, and the second element
  // of the pair holds the data flow value just after the statement.
  //////////////////////////////////////////////////////////////////////////////

  final type Solution = immutable.Map[Int, (S, S)]

  //////////////////////////////////////////////////////////////////////////////
  // The entry point
  //
  // The flow graph itself can simply be represented as a statement, or a list
  // of statements. The boundary condition can be provided by the caller,
  // otherwise the default boundary condition will be used.
  //////////////////////////////////////////////////////////////////////////////

  final def apply(stmt: Stmt, boundaryCondition: S = defaultBoundaryCondition): Solution =
    if (directionForward) fwd(stmt, boundaryCondition) else bwd(stmt, boundaryCondition)

  final def apply(stmts: List[Stmt], boundaryCondition: S): Solution =
    apply(TypeAssigner(StmtBlock(stmts) withLoc Loc.synthetic), boundaryCondition)

  final def apply(stmts: List[Stmt]): Solution =
    apply(stmts, defaultBoundaryCondition)

  //////////////////////////////////////////////////////////////////////////////
  // The implementation of the analysis
  //////////////////////////////////////////////////////////////////////////////

  final private def fwd(stmt: Stmt, boundaryCondition: S): Solution = {

    // Building the solution as we go
    val solution = mutable.Map[Int, (S, S)]()

    def analyseStmt(in: S, stmt: Stmt): S = stmt pipe {
      // Branching statements
      case StmtBlock(stmts) => // 1-way branch
        stmts.foldLeft(in)(analyseStmt)
      case s @ StmtIf(_, thenStmts, elseStmts) => // 2-way branch
        val postCond = f(in, s)
        val postThen = thenStmts.foldLeft(postCond)(analyseStmt)
        val postElse = elseStmts.foldLeft(postCond)(analyseStmt)
        meet(postThen, postElse)
      case s @ StmtCase(_, cases) => // n-way branch
        val postScrutinee = f(in, s)
        val postCases = cases map { c => analyseCase(postScrutinee, c) }
        if (s.isFullCase) {
          postCases.reduce(meet)
        } else {
          postCases.foldLeft(postScrutinee)(meet)
        }

      // Simple non-branching statements
      case s: StmtAssign  => f(in, s)
      case s: StmtDelayed => f(in, s)
      case s: StmtOutcall => f(in, s)
      case s: StmtExpr    => f(in, s)
      case _: StmtComment => in

      // Statements removed by the compiler
      case _: StmtSplice | // Removed through various passes
          _: StmtLoop | // Removed by ConvertControl
          _: StmtWhile | // Removed by ConvertControl
          _: StmtFor | // Removed by ConvertControl
          _: StmtDo | // Removed by ConvertControl
          _: StmtLet | // Removed by Desugar
          _: StmtFence | // Removed by ConvertControl
          _: StmtBreak | // Removed by ConvertControl
          _: StmtContinue | // Removed by ConvertControl
          _: StmtGoto | // Removed by ConvertControl
          _: StmtReturn | // Removed by ConvertControl
          _: StmtUpdate | // Removed by Desugar
          _: StmtPost | // Removed by Desugar
          _: StmtWait // Removed by LowerWait
          =>
        unreachable
    } tap { out =>
      // Build the solution as we go
      solution(stmt.id) = (in, out)
    }

    def analyseCase(in: S, kase: Case): S = kase match {
      case c @ CaseRegular(_, stmts) =>
        val postCond = f(in, c)
        stmts.foldLeft(postCond)(analyseStmt)
      case CaseDefault(stmts) =>
        stmts.foldLeft(in)(analyseStmt)
      case _: CaseSplice =>
        unreachable // Removed by Elaboration
    }

    analyseStmt(boundaryCondition, stmt)

    solution.toMap
  }

  final private def bwd(stmt: Stmt, boundaryCondition: S): Solution = {

    // Building the solution as we go
    val solution = mutable.Map[Int, (S, S)]()

    def analyseStmt(stmt: Stmt, out: S): S = stmt pipe {
      // Branching statements
      case StmtBlock(stmts) => // 1-way branch
        stmts.foldRight(out)(analyseStmt)
      case s @ StmtIf(_, thenStmts, elseStmts) => // 2-way branch
        val preThen = thenStmts.foldRight(out)(analyseStmt)
        val preElse = elseStmts.foldRight(out)(analyseStmt)
        val postCond = meet(preThen, preElse)
        f(postCond, s)
      case s @ StmtCase(_, cases) => // n-way branch
        val preCases = cases map { c => analyseCase(c, out) }
        val postCond = if (s.isFullCase) {
          preCases.reduce(meet)
        } else {
          preCases.foldLeft(out)(meet)
        }
        f(postCond, s)

      // Simple non-branching statements
      case s: StmtAssign  => f(out, s)
      case s: StmtDelayed => f(out, s)
      case s: StmtOutcall => f(out, s)
      case s: StmtExpr    => f(out, s)
      case _: StmtComment => out

      // Statements removed by the compiler
      case _: StmtSplice | // Removed through various passes
          _: StmtLoop | // Removed by ConvertControl
          _: StmtWhile | // Removed by ConvertControl
          _: StmtFor | // Removed by ConvertControl
          _: StmtDo | // Removed by ConvertControl
          _: StmtLet | // Removed by Desugar
          _: StmtFence | // Removed by ConvertControl
          _: StmtBreak | // Removed by ConvertControl
          _: StmtContinue | // Removed by ConvertControl
          _: StmtGoto | // Removed by ConvertControl
          _: StmtReturn | // Removed by ConvertControl
          _: StmtUpdate | // Removed by Desugar
          _: StmtPost | // Removed by Desugar
          _: StmtWait // Removed by LowerWait
          =>
        unreachable
    } tap { in =>
      // Build the solution as we go
      solution(stmt.id) = (in, out)
    }

    def analyseCase(kase: Case, out: S): S = kase match {
      case c @ CaseRegular(_, stmts) =>
        val preStmts = stmts.foldRight(out)(analyseStmt)
        f(preStmts, c)
      case CaseDefault(stmts) =>
        stmts.foldRight(out)(analyseStmt)
      case _: CaseSplice =>
        unreachable // Removed by Elaboration
    }

    analyseStmt(stmt, boundaryCondition)

    solution.toMap
  }

}
