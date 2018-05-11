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

import com.argondesign.alogic.Config
import com.argondesign.alogic.Config.allowWidthInference
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.TreeInTypeTransformer
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.TreeLike
import com.argondesign.alogic.passes.TreeTransformerPass
import com.argondesign.alogic.util.FollowedBy
import com.argondesign.alogic.util.unreachable

final class Typer(implicit cc: CompilerContext) extends TreeTransformer with FollowedBy {

  override val typed: Boolean = false

  val mixedWidthBinaryOps = Set("<<", ">>", "<<<", ">>>", "&&", "||")

  private def hasError(tree: Tree) = {
    tree.children exists {
      case child: Tree => child.tpe == TypeError
      case _: Type     => false
      case _           => unreachable
    }
  }

  private def checkPacked(expr: Expr, msg: String): Option[Loc] = {
    if (expr.tpe.isPacked) {
      None
    } else {
      if (expr.tpe != TypeError) {
        cc.error(expr, s"${msg} is of non-packed type")
      }
      Some(expr.loc)
    }
  }

  private def checkNumeric(expr: Expr, msg: String): Option[Loc] = {
    if (expr.tpe.isNumeric) {
      None
    } else {
      if (expr.tpe != TypeError) {
        cc.error(expr, s"${msg} is of non-numeric type")
      }
      Some(expr.loc)
    }
  }

  private def checkNumericOrPacked(expr: Expr, msg: String): Option[Loc] = {
    if (expr.tpe.isNumeric || expr.tpe.isPacked) {
      None
    } else {
      if (expr.tpe != TypeError) {
        cc.error(expr, s"${msg} is of neither numeric nor packed type")
      }
      Some(expr.loc)
    }
  }

  private def checkWidth(kind: Type, expr: Expr, msg: String): Option[Loc] = {
    if (!Config.checkAssignWidth) {
      None
    } else {
      require(kind.isPacked)
      // TODO: solve for parametrized
      val kindWidthOpt = kind.width.value
      val exprWidthOpt = expr.tpe.width.value
      for {
        kindWidth <- kindWidthOpt
        exprWidth <- exprWidthOpt
        if kindWidth != exprWidth
      } yield {
        cc.error(expr, s"${msg} yields ${exprWidth} bits, ${kindWidth} bits are expected")
        expr.loc
      }
    }
  }

  private def checkBlock(stmts: List[Stmt]): Option[Loc] = {
    val hasCtrl = stmts exists { _.tpe == TypeCtrlStmt }
    val lstCtrl = stmts.nonEmpty && stmts.last.tpe == TypeCtrlStmt
    if (hasCtrl && !lstCtrl) {
      cc.error(stmts.last,
               "Block must contain only combinatorial statements, or end with a control statement")
      Some(stmts.last.loc)
    } else {
      None
    }
  }

  private[this] object TypeTyper extends TreeInTypeTransformer(this) {
    // TODO: implement checks
  }

  private var inConnect = false

  override def enter(tree: Tree): Unit = tree match {
    case _: Connect => {
      inConnect = true
    }
    case _ => ()
  }

  override def transform(tree: Tree): Tree = {
    // TODO: warn for field hiding by extension types
    // TODO: reduction of 1 bit value is error
    // TODO: Check bit select widths vs dimension
    // TODO: Warn for non power of 2 dimensions
    // TODO: check parameter assignments

    val result: Tree = tree match {
      ////////////////////////////////////////////////////////////////////////////
      // Remove root node
      ////////////////////////////////////////////////////////////////////////////

      case node: Root => node.entity

      ////////////////////////////////////////////////////////////////////////////
      // Propagate errors
      ////////////////////////////////////////////////////////////////////////////

      case _: Expr if hasError(tree)          => ExprError() withLoc tree.loc
      case _: Stmt if hasError(tree)          => StmtError() withLoc tree.loc
      case Function(ref, _) if hasError(tree) => Function(ref, Nil) withLoc tree.loc
      case _: Connect if hasError(tree) => {
        val err = ExprError() withLoc tree.loc
        TypeAssigner(err)
        Connect(err, List(err)) withLoc tree.loc
      } followedBy {
        inConnect = false
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
          val offender = if (body.isEmpty) {
            ref
          } else {
            val combStmts = body.last collect {
              case tree: Tree if tree.tpe == TypeCombStmt => tree
            }
            combStmts.toList.last
          }
          cc.error(offender, "Body of function must end in a control statement")
          val err = StmtError() withLoc offender.loc
          TypeAssigner(err)
          Function(ref, List(err)) withLoc tree.loc
        }
      }

      case conn @ Connect(lhs, rhss) => {
        if (ConnectChecks(conn)) {
          tree
        } else {
          val err = ExprError() withLoc tree.loc
          TypeAssigner(err)
          Connect(err, List(err)) withLoc tree.loc
        }
      } followedBy {
        inConnect = false
      }

      case decl @ Decl(symbol, Some(init)) => {
        val origKind = symbol.kind
        val kind = origKind rewrite TypeTyper
        if (kind ne origKind) {
          symbol.kind = kind
        }
        kind match {
          case _: TypeConst => symbol.attr.constValue set init
          case _            => ()
        }
        require(kind.isPacked)
        if (allowWidthInference && init.tpe.isNum) {
          // Infer width of init
          val newInit = kind.width.value map { CoerceWidth(init, _) } map {
            case e: Expr => e
            case _       => unreachable
          } getOrElse {
            cc.error(init, s"Cannot infer width of initializer")
            TypeAssigner(ExprError() withLoc tree.loc)
          }
          decl.copy(init = Some(newInit)) withLoc tree.loc
        } else {
          val packedErrOpt = checkPacked(init, "Initializer expression")
          lazy val widthErrOpt = checkWidth(kind, init, "Initializer expression")

          packedErrOpt orElse widthErrOpt map { errLoc =>
            val newInit = ExprError() withLoc errLoc
            TypeAssigner(newInit)
            decl.copy(init = Some(newInit)) withLoc tree.loc
          } getOrElse {
            decl
          }
        }
      }

      case decl @ Decl(symbol, None) => {
        val origKind = symbol.kind
        val kind = origKind rewrite TypeTyper
        if (kind ne origKind) {
          symbol.kind = kind
        }
        decl
      }

      ////////////////////////////////////////////////////////////////////////////
      // Type check statements
      ////////////////////////////////////////////////////////////////////////////

      case StmtBlock(body) => {
        checkBlock(body) map { ExprError() withLoc _ } getOrElse tree
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
        checkBlock(default) map { StmtError() withLoc _ } getOrElse {
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

      case stmt @ StmtAssign(lhs, rhs) => {
        lazy val errNotAssignable: Option[Loc] = {
          // TODO: Use WrittenSymbols
          def stripToRefs(expr: Expr): List[ExprRef] = expr match {
            case ref: ExprRef                => List(ref)
            case ExprIndex(expr, _)          => stripToRefs(expr)
            case ExprSlice(expr, _, _, _)    => stripToRefs(expr)
            case ExprSelect(expr, _)         => stripToRefs(expr)
            case ExprCat(parts)              => parts flatMap stripToRefs
            case other if other.isLValueExpr => cc.ice(expr)
            case _                           => unreachable
          }

          val locs = for (ref @ ExprRef(symbol) <- stripToRefs(lhs)) yield {
            symbol.kind match {
              case _: TypeParam => cc.error(ref, "Parameter cannot be assigned")
              case _: TypeConst => cc.error(ref, "Constant cannot be assigned")
              case _: TypeIn    => cc.error(ref, "Input port cannot be assigned")
              case TypeOut(_, fct, _) if fct != FlowControlTypeNone => {
                val msg = "Output port with flow control cannot be assigned directly, use '.write'"
                cc.error(ref, msg)
              }
              case _ => None
            }
          }

          locs.flatten.headOption
        }

        checkPacked(lhs, "Left hand side of assignment") orElse errNotAssignable map { errLoc =>
          StmtError() withLoc errLoc
        } getOrElse {
          if (allowWidthInference && rhs.tpe.isNum) {
            // Infer width of rhs
            lhs.tpe.width.value map {
              CoerceWidth(rhs, _)
            } map {
              case e: Expr => e
              case _       => unreachable
            } map { newRhs =>
              stmt.copy(rhs = newRhs) withLoc stmt.loc
            } getOrElse {
              cc.error(rhs, s"Cannot infer width of right hand side of assignment")
              StmtError() withLoc lhs.loc
            }
          } else {
            val rhsErrOpt = checkPacked(rhs, "Right hand side of assignment")
            lazy val widthErrOpt = checkWidth(lhs.tpe, rhs, "Right hand side of assignment")
            rhsErrOpt orElse widthErrOpt map {
              StmtError() withLoc _
            } getOrElse tree
          }
        }

      }

      // TODO: Some function call are pure e.g.: @zx(10, 1'b1);
      case StmtExpr(_: ExprCall) => tree

      case StmtExpr(expr) => {
        cc.warning(tree, "A pure expression in statement position does nothing")
        StmtBlock(Nil) withLoc tree.loc
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
          val thing = if (inConnect) "port" else "field"
          cc.error(
            tree,
            s"No ${thing} named '${selector}' in '${expr.toSource}' of type '${expr.tpe.toSource}'")
          ExprError() withLoc tree.loc
        }
      }

      case ExprCall(expr, args) => {
        require(expr.hasTpe)
        require(args forall { _.hasTpe })

        def checkArg(expected: Type, arg: Expr, i: Int): Tree = {
          assert(expected.isPacked)
          checkPacked(arg, s"Parameter ${i} to function call") map {
            ExprError() withLoc _
          } getOrElse {
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
              tmp collectFirst { case e: ExprError => e }
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
                val ref = ExprRef(symbol) withLoc expr.loc
                TypeAssigner(ref)
                ExprCall(ref, args) withLoc tree.loc
              }
              case None => {
                val funcStr = expr.toSource
                val argsStr = args map { _.toSource } mkString ", "
                val typeStr = args map { _.tpe.toSource } mkString ", "
                cc.error(
                  tree,
                  s"Builtin function '${funcStr}' cannot be applied to arguments '${argsStr}' of type '${typeStr}'")
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
        errors.flatten.headOption map { ExprError() withLoc _ } getOrElse tree
      }

      case ExprRep(count, expr) => {
        val errCount = checkNumeric(count, "Count of bit repetition")
        val errExpr = checkPacked(expr, "Value of bit repetition")
        errCount orElse errExpr map { ExprError() withLoc _ } getOrElse tree
      }

      case ExprIndex(expr, index) => {
        val errExpr = expr.tpe match {
          case _: TypeArray          => None
          case kind if kind.isPacked => None
          case _ => {
            cc.error(expr, "Target of index is neither a packed value, nor an array")
            Some(expr.loc)
          }
        }

        val errIndex = checkNumeric(index, "Index")

        errExpr orElse errIndex map { ExprError() withLoc _ } getOrElse tree
      }

      case ExprSlice(expr, lidx, _, ridx) => {
        val errExpr = checkPacked(expr, "Target of slice")
        val errLidx = checkNumeric(lidx, "Left index of slice")
        val errRidx = checkNumeric(ridx, "Right index of slice")
        errExpr orElse errLidx orElse errRidx map { ExprError() withLoc _ } getOrElse tree
      }

      // unary ops
      case ExprUnary(op, expr) => {
        if (expr.tpe.isNum) {
          tree // Do nothing, we will coerce later if possible
        } else {
          checkPacked(expr, s"Operand of unary '${op}'") map {
            ExprError() withLoc _
          } getOrElse tree
        }
      }

      // Binary ops
      case ExprBinary(lhs, op, rhs) => {
        lazy val errLhs = checkPacked(lhs, s"Left hand operand of '${op}'")
        lazy val errRhs = checkPacked(rhs, s"Right hand operand of '${op}'")

        (allowWidthInference && lhs.tpe.isNum, allowWidthInference && rhs.tpe.isNum) match {
          case (true, true) => tree // Do nothing, we will coerce later if possible
          case (false, true) => { // Infer with of rhs
            errLhs orElse {
              lhs.tpe.width.value map { CoerceWidth(rhs, _) }
            } map {
              case err: ExprError => err
              case rhs: Expr      => ExprBinary(lhs, op, rhs) withLoc tree.loc
              case _              => unreachable
            } getOrElse {
              cc.error(rhs, s"Cannot infer width of right hand operand of '${op}'")
              ExprError() withLoc tree.loc
            }
          }
          case (true, false) => {
            // Infer with of lhs
            errRhs orElse {
              rhs.tpe.width.value map { CoerceWidth(lhs, _) }
            } map {
              case err: ExprError => err
              case lhs: Expr      => ExprBinary(lhs, op, rhs) withLoc tree.loc
              case _              => unreachable
            } getOrElse {
              cc.error(lhs, s"Cannot infer width of left hand operand of '${op}'")
              ExprError() withLoc tree.loc
            }
          }
          case _ => {
            errLhs orElse errRhs map { ExprError() withLoc _ } getOrElse {
              if (Config.binaryOpWidthWarnings && !(mixedWidthBinaryOps contains op)) {
                // TODO: handle parametrized widths by solving 'lhsWidth != rhsWidth' SAT
                val lhsWidthOpt = lhs.tpe.width.value
                val rhsWidthOpt = rhs.tpe.width.value

                for {
                  lhsWidth <- lhsWidthOpt
                  rhsWidth <- rhsWidthOpt
                  if lhsWidth != rhsWidth
                } {
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
        }
      }

      case ExprTernary(cond, thenExpr, elseExpr) => {
        checkNumericOrPacked(cond, "Condition of '?:'") map { ExprError() withLoc _ } getOrElse {
          lazy val errThen = checkPacked(thenExpr, "'then' operand of '?:'")
          lazy val errElse = checkPacked(elseExpr, "'else' operand of '?:'")

          (allowWidthInference && thenExpr.tpe.isNum, allowWidthInference && elseExpr.tpe.isNum) match {
            case (true, true) => tree // Do nothing, we will coerce later if possible
            case (false, true) => { // Infer with of 'else' operand
              errThen map { ExprError() withLoc _ } orElse {
                thenExpr.tpe.width.value map {
                  CoerceWidth(elseExpr, _)
                }
              } map {
                case err: ExprError => err
                case elseExpr: Expr => ExprTernary(cond, thenExpr, elseExpr) withLoc tree.loc
                case _              => unreachable
              } getOrElse {
                cc.error(elseExpr, s"Cannot infer width of 'else' operand of '?:'")
                ExprError() withLoc tree.loc
              }
            }
            case (true, false) => { // Infer with of 'then' operand
              errElse map { ExprError() withLoc _ } orElse {
                elseExpr.tpe.width.value map {
                  CoerceWidth(thenExpr, _)
                }
              } map {
                case err: ExprError => err
                case thenExpr: Expr => ExprTernary(cond, thenExpr, elseExpr) withLoc tree.loc
                case _              => unreachable
              } getOrElse {
                cc.error(thenExpr, s"Cannot infer width of 'then' operand of '?:'")
                ExprError() withLoc tree.loc
              }
            }
            case _ => {
              errThen orElse errElse map { ExprError() withLoc _ } getOrElse {
                // TODO: handle parametrized widths by solving 'lhsWidth != rhsWidth' SAT
                val thenWidthOpt = thenExpr.tpe.width.value
                val elseWidthOpt = elseExpr.tpe.width.value

                for {
                  thenWidth <- thenWidthOpt
                  elseWidth <- elseWidthOpt
                  if thenWidth != elseWidth
                } {

                  cc.warning(
                    tree,
                    s"'?:' expects both the 'then' and 'else' operands to have the same width, but",
                    s"'then' operand is ${thenWidth} bits wide, and",
                    s"'else' operand is ${elseWidth} bits wide"
                  )

                }
                tree
              }
            }
          }
        }
      }

      case expr @ ExprType(origKind) => {
        val kind = origKind rewrite TypeTyper
        if (kind eq origKind) expr else expr.copy(kind = kind) withLoc tree.loc
      }

      case _ => tree
    }

    // Assign type if have not been assigned by now
    if (result.hasTpe) result else TypeAssigner(result)
  }

  override def finalCheck(tree: Tree): Unit = {

    def check(tree: TreeLike) {
      tree visitAll {
        case node: Tree if !node.hasTpe => {
          cc.ice(node, "Typer: untyped node remains", node.toString)
        }
        case node: TypeDefinition => {
          cc.ice(node, s"Typer: type definition remains", node.toString)
        }
        case node: Root => {
          cc.ice(node, s"Typer: root node remains")
        }
        case node: Tree if node.tpe.isInstanceOf[TypePolyFunc] => {
          cc.ice(node, s"Typer: node of type TypePolyFunc remains")
        }
        case Decl(symbol, _) => check(symbol.kind)
      }
    }

    check(tree)
  }

}

object Typer extends TreeTransformerPass {
  val name = "typer"
  def create(implicit cc: CompilerContext) = new Typer
}
