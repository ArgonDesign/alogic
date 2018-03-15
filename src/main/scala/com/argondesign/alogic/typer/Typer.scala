////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// The Typer:
// - Type checks all tree nodes
// - Infers widths of unsized constants
// - Assigns types to all nodes using the TypeAssigner
// - Remove TypeDefinition nodes
// - Replace the Root node with the root Entity node
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.typer

import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.transform.ConstantFold
import com.argondesign.alogic.util.unreachable

final class Typer(implicit cc: CompilerContext) extends TreeTransformer {

  val constantFold = new ConstantFold

  val reducingBinaryOps = Array(">", ">=", "<", "<=", "==", "!=", "&&", "||")

  private def hasError(tree: Tree) = {
    tree.children exists {
      case child: Tree => child.tpe == TypeError
      case _: Type     => false
      case _           => unreachable
    }
  }

  private def checkPacked(expr: Expr, msg: String): Option[ExprError] = {
    if (expr.tpe.isPacked) {
      None
    } else {
      cc.error(expr, s"${msg} is of non-packed type")
      Some(ExprError() withLoc expr.loc)
    }
  }

  private def checkNumeric(expr: Expr, msg: String): Option[ExprError] = {
    if (expr.tpe.isNumeric) {
      None
    } else {
      cc.error(expr, s"${msg} is of non-numeric type")
      Some(ExprError() withLoc expr.loc)
    }
  }

  private def checkBlock(stmts: List[Stmt]): Option[StmtError] = {
    val hasCtrl = stmts exists { _.tpe == TypeCtrlStmt }
    val lstCtrl = stmts.nonEmpty && stmts.last.tpe == TypeCtrlStmt
    if (hasCtrl && !lstCtrl) {
      cc.error(stmts.last,
               "Block must contain only combinatorial statements, or end with a control statement")
      Some(StmtError() withLoc stmts.last.loc)
    } else {
      None
    }
  }

  override def transform(tree: Tree): Tree = {
    // TODO: warn for field hiding by extension types

    val result: Tree = tree match {
      ////////////////////////////////////////////////////////////////////////////
      // Remove root node
      ////////////////////////////////////////////////////////////////////////////

      case node: Root => node.entity

      ////////////////////////////////////////////////////////////////////////////
      // Propagate errors
      ////////////////////////////////////////////////////////////////////////////

      case expr: Expr if hasError(expr) => ExprError() withLoc expr.loc
      case stmt: Stmt if hasError(stmt) => StmtError() withLoc stmt.loc

      ////////////////////////////////////////////////////////////////////////////
      // Infer width of of unsized literals in expressions
      ////////////////////////////////////////////////////////////////////////////

      case ExprBinary(lhs @ ExprNum(signed, value), op, rhs) => {
        require(rhs.hasTpe)

        // Type check and get expected type
        val kindOpt = rhs.tpe match {
          case kind if kind.isPacked => Some(kind)
          case other => {
            cc.error(tree, s"'${op}' expects packed value on the right hand side")
            None
          }
        }

        // Figure out width of rhs
        val widthOpt = kindOpt flatMap { _.width.value } map { _.toInt }

        if (kindOpt.isDefined && widthOpt.isEmpty) {
          cc.error(rhs, s"Cannot infer width of left hand operand of '${op}'")
        }

        // Convert lhs
        widthOpt map { width =>
          // TODO: Check value fits width ...
          val newLhs = ExprInt(signed, width, value) withLoc lhs.loc
          TypeAssigner(newLhs)
          ExprBinary(newLhs, op, rhs)
        } getOrElse {
          ExprError()
        } withLoc lhs.loc
      }

      case ExprBinary(lhs, op, rhs @ ExprNum(signed, value)) => {
        require(lhs.hasTpe)

        // Type check and get expected type
        val kindOpt = lhs.tpe match {
          case kind if kind.isPacked => Some(kind)
          case other => {
            cc.error(tree, s"'${op}' expects packed value on the left hand side")
            None
          }
        }

        // Figure out width of rhs
        val widthOpt = kindOpt flatMap { _.width.value } map { _.toInt }

        if (kindOpt.isDefined && widthOpt.isEmpty) {
          cc.error(rhs, s"Cannot infer width of right hand operand of '${op}'")
        }

        // Convert rhs
        widthOpt map { width =>
          // TODO: Check value fits width ...
          val newRhs = ExprInt(signed, width, value) withLoc rhs.loc
          TypeAssigner(newRhs)
          ExprBinary(lhs, op, newRhs)
        } getOrElse {
          ExprError()
        } withLoc lhs.loc
      }

      ////////////////////////////////////////////////////////////////////////////
      // Type check other nodes
      ////////////////////////////////////////////////////////////////////////////

      case entity: Entity => {
        if (entity.fenceStmts exists { _.tpe == TypeCtrlStmt }) {
          cc.error("'fence' block must contain only combinatorial statements")
          entity.copy(fenceStmts = Nil) withLoc tree.loc
        } else {
          tree
        }
      }

      case Function(ref, body) => {
        if (body.nonEmpty && body.last.tpe == TypeCtrlStmt) {
          tree
        } else {
          cc.error(tree, "Body of function must end in a control statement")
          val err = StmtError() withLoc tree.loc
          TypeAssigner(err)
          Function(ref, List(err)) withLoc tree.loc
        }
      }

      ////////////////////////////////////////////////////////////////////////////
      // Type check statements
      ////////////////////////////////////////////////////////////////////////////

      case StmtBlock(body) => {
        checkBlock(body) getOrElse tree
      }

      case StmtIf(_, thenStmt, Some(elseStmt)) => {
        (thenStmt.tpe, elseStmt.tpe) match {
          case (TypeCombStmt, TypeCombStmt) => tree
          case (TypeCtrlStmt, TypeCtrlStmt) => tree
          case _ => {
            cc.error(tree, "Either both or neither branches of if-else must be control statements")
            StmtError() withLoc tree.loc
          }
        }
      }

      case StmtCase(_, cases, default) => {
        checkBlock(default) getOrElse {
          val allCtrl = {
            val casesCtrl = cases forall { _.body.tpe == TypeCtrlStmt }
            val defaultCtrl = default.isEmpty || default.last.tpe == TypeCtrlStmt
            casesCtrl && defaultCtrl
          }
          val allComb = {
            val casesComb = cases forall { _.body.tpe == TypeCombStmt }
            val defaultComb = default.isEmpty || default.last.tpe == TypeCombStmt
            casesComb && defaultComb
          }
          if (!allComb && !allCtrl) {
            cc.error(tree, "Either all or no cases of a case statement must be control statements")
            StmtError() withLoc tree.loc
          } else {
            tree
          }
        }
      }

      case StmtLoop(body) => {
        if (body.nonEmpty && body.last.tpe == TypeCtrlStmt) {
          tree
        } else if (body exists { _.tpe == TypeCtrlStmt }) {
          cc.error(tree, "Body of 'loop' must end in a control statement")
          StmtError() withLoc tree.loc
        } else {
          cc.error(tree, "Body of 'loop' must be a control statement")
          StmtError() withLoc tree.loc
        }
      }

      ////////////////////////////////////////////////////////////////////////////
      // Type check expressions
      ////////////////////////////////////////////////////////////////////////////

      case ExprSelect(expr, selector) => {
        val field = expr.tpe match {
          case TypeType(kind: CompoundType) => kind(selector)
          case kind: CompoundType           => kind(selector)
          case _                            => None
        }

        if (field.isDefined) {
          tree
        } else {
          cc.error(tree, s"No field named '${selector}' in '${expr}'")
          ExprError() withLoc tree.loc
        }
      }

      case ExprCall(expr, args) => {
        require(expr.hasTpe)
        require(args forall { _.hasTpe })

        def checkArg(expected: Type, arg: Expr, i: Int): Tree = {
          assert(expected.isPacked)

          checkPacked(arg, s"Parameter ${i} to function call") getOrElse {
            val argWidthOpt = arg.tpe.width.value
            val expWidthOpt = expected.width.value
            // TODO: solve for parametrized
            val errOpt = for {
              argWidth <- argWidthOpt
              expWidth <- expWidthOpt
            } yield {
              if (argWidth != expWidth) {
                val cmp = if (argWidth > expWidth) "greater" else "less"
                cc.error(
                  arg,
                  s"Width ${argWidth} of parameter ${i} passed to function call is ${cmp} than expected width ${expWidth}")
                ExprError() withLoc expr.loc
              } else {
                tree
              }
            }
            errOpt getOrElse tree
          }
        }

        def checkFunc(argTypes: List[Type]) = {
          val eLen = argTypes.length
          val gLen = args.length
          if (eLen != gLen) {
            val cmp = if (eLen > gLen) "few" else "many"
            cc.error(tree, s"Too ${cmp} arguments to function call, expected ${eLen}, have ${gLen}")
            ExprError() withLoc tree.loc
          } else {
            val errOpt = {
              val tmp = for (((e, a), i) <- (argTypes zip args).zipWithIndex) yield {
                checkArg(e, a, i + 1)
              }
              tmp collectFirst {
                case e: ExprError => e
              }
            }
            errOpt getOrElse tree
          }
        }

        expr.tpe match {
          // Nothing to do for ordinary functions
          case tpe: TypeCombFunc => checkFunc(tpe.argTypes)
          case tpe: TypeCtrlFunc => checkFunc(tpe.argTypes)
          // Resolve calls to polymorphic builtins with the provided arguments
          // and rewrite as reference to the overloaded symbol
          case polyFunc: TypePolyFunc => {
            polyFunc.resolve(args) match {
              case Some(symbol) => {
                val sym = Sym(symbol) withLoc expr.loc
                TypeAssigner(sym)
                val ref = ExprRef(sym) withLoc expr.loc
                TypeAssigner(ref)
                ExprCall(ref, args) withLoc tree.loc
              }
              case None => {
                cc.error(tree,
                         s"Builtin function '${expr}' cannot be applied to arguments '${args}'")
                ExprError() withLoc tree.loc
              }
            }
          }
          // Anything else is not callable
          case _ => {
            cc.error(tree, s"'${expr}' is not callable")
            ExprError() withLoc tree.loc
          }
        }
      }

      case ExprCat(parts) => {
        val errors = for ((part, i) <- parts.zipWithIndex) yield {
          checkPacked(part, s"Part ${i + 1} of bit concatenation")
        }
        errors.flatten.headOption getOrElse tree
      }

      case ExprRep(count, expr) => {
        val errCount = checkNumeric(count, "Count of bit repetition")
        val errExpr = checkPacked(expr, "Value of bit repetition")
        errCount orElse errExpr getOrElse tree
      }

      case ExprIndex(expr, index) => {
        val errExpr = expr.tpe match {
          case _: TypeArray          => None
          case kind if kind.isPacked => None
          case _ => {}
          cc.error(expr, "Target of index is neither a packed value, nor an array")
          Some(ExprError() withLoc expr.loc)
        }

        val errIndex = checkNumeric(index, "Index")

        errExpr orElse errIndex getOrElse tree
      }

      case ExprSlice(expr, lidx, _, ridx) => {
        val errExpr = checkPacked(expr, "Target of slice")
        val errLidx = checkNumeric(lidx, "Left index of slice")
        val errRidx = checkNumeric(ridx, "Right index of slice")
        errExpr orElse errLidx orElse errRidx getOrElse tree
      }

      // unary ops
      case ExprUnary(op, expr) => {
        checkPacked(expr, s"Operand of unary '${op}'") getOrElse tree
      }

      // Binary ops
      case ExprBinary(lhs, op, rhs) => {
        require(lhs.hasTpe)
        require(rhs.hasTpe)

        // Check both sides are packed types
        val errLhs = checkPacked(lhs, s"Left hand operand of '${op}'")
        val errRhs = checkPacked(rhs, s"Right hand operand of '${op}'")

        errLhs orElse errRhs getOrElse {
          // TODO: handle parametrized widths by solving 'lhsWidth != rhsWidth' SAT
          val lhsWidthOpt = lhs.tpe.width.value
          val rhsWidthOpt = rhs.tpe.width.value

          for {
            lhsWidth <- lhsWidthOpt
            rhsWidth <- rhsWidthOpt
          } {
            if (lhsWidth != rhsWidth) {
              cc.warning(
                tree,
                s"'${op}' expects both operands to have the same width, but",
                s"left  operand is ${lhsWidth} bits wide, and",
                s"right operand is ${rhsWidth} bits wide"
              )
            }
          }

          tree
        }

      }

      case ExprTernary(cond, thenExpr, elseExpr) => {
        val errCondOpt = checkPacked(cond, "Condition of ternary operator ?:")
        val errThenOpt = checkPacked(thenExpr, "True part of ternary operator ?:")
        val errElseOpt = checkPacked(elseExpr, "False part of ternary operator ?:")
        errCondOpt orElse errThenOpt orElse errElseOpt getOrElse tree
      }

      case _ => tree
    }

    // Assign type if have not been assigned by now
    if (result.hasTpe) result else TypeAssigner(result)
  }

  override def finalCheck(tree: Tree): Unit = {
    val isEntity = tree match {
      case _: Entity => true
      case _         => false
    }

    if (isEntity) {
      tree visit {
        case _: Type => /* Don't recurse into types */
        case node: Tree if !node.hasTpe => {
          cc.ice(
            node,
            "Typer should have assigned type for all node, but the following is untyped",
            node.toString
          )
        }
        case node: Tree if node.tpe == TypeUnknown => {
          cc.ice(node, s"Could not compute type of ${node}")
        }
      }
    }

    tree visit {
      case node: TypeDefinition => {
        cc.ice(node, s"Typer should have removed type definitions, but '${node}' remains")
      }
      case node: Root => {
        cc.ice(node, s"Typer should have removed the Root node")
      }
    }
  }

}
