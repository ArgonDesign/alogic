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
// - Assigns types to all nodes using the TypeAssigner
// The typer is special and cannot rewrite any of the tree, unless there
// is a type error. The rest of the compiler relies on this property.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.typer

import com.argondesign.alogic.analysis.WrittenRefs
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.TreeInTypeTransformer
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.TreeLike
import com.argondesign.alogic.passes._
import com.argondesign.alogic.util.FollowedBy
import com.argondesign.alogic.util.unreachable

import com.argondesign.alogic.lib.Math.clog2

final class Typer(externalRefs: Boolean = false)(implicit cc: CompilerContext)
    extends TreeTransformer
    with FollowedBy {

  override val typed: Boolean = false

  private final val mixedWidthBinaryOps = Set("<<", ">>", "<<<", ">>>", "&&", "||")

  private final val addImplicitCasts = new AddImplicitCasts

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
    if (expr.tpe.underlying.isNumeric) {
      None
    } else {
      if (expr.tpe.underlying != TypeError) {
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

  private def checkWidth(width: Int, expr: Expr, msg: String): Option[Loc] = {
    val exprWidth = expr.tpe.width
    if (exprWidth != width) {
      cc.error(expr, s"${msg} yields ${exprWidth} bits, ${width} bits are expected")
    } else {
      None
    }
  }

  private def checkSign(sign: Boolean, expr: Expr, msg: String): Option[Loc] = {
    if (expr.tpe.isSigned != sign) {
      cc.error(expr, s"${msg} must be ${if (sign) "signed" else "unsigned"}")
    } else {
      None
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

  private def checkModifiable(expr: Expr): Option[Loc] = {
    val it = WrittenRefs(expr) flatMap {
      case ref @ ExprRef(symbol) =>
        symbol.kind match {
          case _: TypeParam => cc.error(ref, "Parameter cannot be modified")
          case _: TypeConst => cc.error(ref, "Constant cannot be modified")
          case _: TypeIn    => cc.error(ref, "Input port cannot be modified")
          case _: TypeArray => cc.error(ref, "Memory can only be modified using .write()")
          case TypeOut(_, fct, _) if fct != FlowControlTypeNone => {
            cc.error(ref, "Output port with flow control can only be modified using .write()")
          }
          case _ => None
        }
    }
    it.toList.headOption
  }

  private def checkNameHidingByExtensionType(decl: Decl): Unit = {
    decl.symbol.kind match {
      case eKind: ExtensionType => {
        eKind.kind match {
          case cKind: CompoundType => {
            for {
              (field, nKind) <- eKind.extensions
              pKind <- cKind(field)
            } {
              cc.error(
                decl.loc,
                s"Field '$field' of type '${pKind.toSource}' of symbol '${decl.symbol.name}'",
                s"defined in type '${cKind.toSource}'",
                s"is hidden by extension field of the same name of type '${nKind.toSource}'",
                s"defined by extension type '${eKind.toSource}'"
              )
            }
          }
          case _ => ()
        }
      }
      case _ => ()
    }
  }

  private def checkIndex(expectedWidth: Int, idx: Expr, msg: String) = {
    if (idx.tpe.isNum) { None } else {
      val errPacked = checkPacked(idx, msg)
      errPacked orElse {
        val errWidth = checkWidth(expectedWidth, idx, msg)
        val errSign = checkSign(false, idx, msg)
        errWidth orElse errSign
      }
    }
  }

  private[this] object TypeTyper extends TreeInTypeTransformer(this) {
    // TODO: implement checks
  }

  private var inConnect = false

  override def skip(tree: Tree): Boolean = tree match {
    case _: Entity  => false
    case _: Connect => !externalRefs
    case _          => externalRefs && !inConnect
  }

  override def enter(tree: Tree): Unit = tree match {
    case _: Connect => {
      inConnect = true
    }
    case _ => ()
  }

  override def transform(tree: Tree): Tree = {
    // TODO: reduction of 1 bit value is error
    // TODO: Warn for non power of 2 dimensions
    // TODO: check parameter assignments

    val result: Tree = tree match {
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

      case entity: EntityNamed => {
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
        require(kind.isPacked)
        checkNameHidingByExtensionType(decl)
        if (cc.postSpecialization && kind.underlying != TypeVoid && kind.width < 1) {
          cc.error(decl, s"Signal '${symbol.name}' has declared width ${kind.width}")
        }
        val newdecl = if (init.tpe.isNum) {
          decl
        } else {
          // Check initializer
          val packedErrOpt = checkPacked(init, "Initializer expression")
          lazy val widthErrOpt = checkWidth(kind.width, init, "Initializer expression")
          packedErrOpt orElse widthErrOpt map { errLoc =>
            val newInit = ExprError() withLoc errLoc
            TypeAssigner(newInit)
            decl.copy(init = Some(newInit)) withLoc tree.loc
          } getOrElse {
            decl
          }
        }

        // Attach initializer expression attribute to const declarations.
        // Add the with inference casts in the attribute only. This is required
        // as this initializer may be used in compile time computations
        // (e.g.: width) before the AddImplicitCasts pass is run on the whole
        // tree later
        if (symbol.kind.isConst && !newdecl.init.get.tpe.isError) {
          symbol.attr.init set {
            val init = (newdecl.init.get rewrite addImplicitCasts).asInstanceOf[Expr]
            if (symbol.kind.isPacked && init.tpe.isNum) {
              TypeAssigner(ExprCast(symbol.kind.underlying, init) withLoc init.loc)
            } else {
              init
            }
          }
        }

        // Yield actual declaration
        newdecl
      }

      case decl @ Decl(symbol, None) => {
        val origKind = symbol.kind
        val kind = origKind rewrite TypeTyper
        if (kind ne origKind) {
          symbol.kind = kind
        }
        checkNameHidingByExtensionType(decl)
        if (cc.postSpecialization && kind.isPacked && kind.underlying != TypeVoid && kind.width < 1) {
          cc.error(decl, s"Signal '${symbol.name}' has declared width ${kind.width}")
        }
        decl
      }

      ////////////////////////////////////////////////////////////////////////////
      // Type check statements
      ////////////////////////////////////////////////////////////////////////////

      case StmtBlock(body) => {
        checkBlock(body) map { StmtError() withLoc _ } getOrElse tree
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

      case StmtCase(_, cases) => {
        val allCtrl = cases forall { _.stmt.tpe == TypeCtrlStmt }
        val allComb = cases forall { _.stmt.tpe == TypeCombStmt }
        if (!allComb && !allCtrl) {
          cc.error(tree, "Either all or no cases of a case statement must be control statements")
          StmtError() withLoc tree.loc
        } else {
          tree
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
        // Check lhs
        checkPacked(lhs, "Left hand side of assignment") orElse checkModifiable(lhs) map {
          StmtError() withLoc _
        } getOrElse {
          if (rhs.tpe.isNum) {
            tree
          } else {
            // Type check rhs
            val rhsErrOpt = checkPacked(rhs, "Right hand side of assignment")
            lazy val widthErrOpt = checkWidth(lhs.tpe.width, rhs, "Right hand side of assignment")
            rhsErrOpt orElse widthErrOpt map { StmtError() withLoc _ } getOrElse tree
          }
        }
      }

      case stmt @ StmtUpdate(lhs, _, rhs) => {
        // Check lhs
        checkPacked(lhs, "Left hand side of assignment") orElse checkModifiable(lhs) map {
          StmtError() withLoc _
        } getOrElse {
          if (rhs.tpe.isNum) {
            tree
          } else {
            // Check rhs
            val rhsErrOpt = checkPacked(rhs, "Right hand side of assignment")
            lazy val widthErrOpt = checkWidth(lhs.tpe.width, rhs, "Right hand side of assignment")
            rhsErrOpt orElse widthErrOpt map { StmtError() withLoc _ } getOrElse tree
          }
        }
      }

      case StmtPost(expr, op) => {
        checkPacked(expr, s"Target of postfix '${op}'") orElse
          checkModifiable(expr) map { StmtError() withLoc _ } getOrElse tree
      }

      case StmtExpr(expr) => {
        val nonPure = expr exists {
          // TODO: Some function calls are pure e.g.: @zx(10, 1'b1);
          case _: ExprCall => true
        }
        if (!nonPure) {
          cc.error(tree, "A pure expression in statement position does nothing")
          StmtError() withLoc tree.loc
        } else {
          tree
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
          val thing = if (inConnect) "port" else "field"
          cc.error(
            tree,
            s"No ${thing} named '${selector}' in '${expr.toSource}' of type '${expr.tpe.toSource}'")
          ExprError() withLoc tree.loc
        }
      }

      case ExprCall(expr, args) => { // TODO: JIRA-152
        require(expr.hasTpe)
        require(args forall { _.hasTpe })

        def checkArg(expected: Type, arg: Expr, i: Int): Tree = {
          assert(expected.isPacked)
          checkPacked(arg, s"Parameter ${i} to function call") map {
            ExprError() withLoc _
          } getOrElse {
            val argWidth = arg.tpe.width
            val expWidth = expected.width
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

      case expr @ ExprIndex(tgt, idx) => {
        val shapeIter = tgt.tpe.shapeIter
        if (!shapeIter.hasNext) {
          cc.error(expr, "Target is not indexable")
          ExprError() withLoc expr.loc
        } else {
          val idxWidth = clog2(shapeIter.next) max 1
          val errTgt = if (tgt.tpe.isArray) None else checkPacked(tgt, "Target of index")
          val errIdx = checkIndex(idxWidth, idx, "Index")
          errTgt orElse errIdx map { ExprError() withLoc _ } getOrElse tree
        }
      }

      case expr @ ExprSlice(tgt, lidx, _, ridx) => {
        val shapeIter = tgt.tpe.shapeIter
        if (!shapeIter.hasNext || tgt.tpe.isArray) {
          cc.error(expr, "Target is not sliceable")
          ExprError() withLoc expr.loc
        } else {
          val idxWidth = clog2(shapeIter.next) max 1
          val errTgt = checkPacked(tgt, "Target of slice")
          val errLidx = checkIndex(idxWidth, lidx, "Left index")
          val errRidx = checkIndex(idxWidth, ridx, "Right index")
          errTgt orElse errLidx orElse errRidx map { ExprError() withLoc _ } getOrElse tree
        }
      }

      // Unary ops
      case ExprUnary(op, expr) => {
        if (expr.tpe.isNum) {
          if ("&|^" contains op) {
            cc.error(tree, s"Unary operator '${op}' cannot be applied to value of type num")
            ExprError() withLoc tree.loc
          } else tree
        } else {
          checkPacked(expr, s"Operand of unary operator '${op}'") map {
            ExprError() withLoc _
          } getOrElse tree
        }
      }

      // Binary ops
      case expr @ ExprBinary(lhs, op, rhs) => {
        val strictWidth = !(mixedWidthBinaryOps contains op)

        val check = if (strictWidth) checkPacked _ else checkNumericOrPacked _

        lazy val errLhs = check(lhs, s"Left hand operand of '${op}'")
        lazy val errRhs = check(rhs, s"Right hand operand of '${op}'")

        (lhs.tpe.isNum, rhs.tpe.isNum) match {
          case (true, true) => {
            // Do nothing, will be folded later
            tree
          }
          case (false, true) if strictWidth => {
            errLhs map { ExprError() withLoc _ } getOrElse tree
          }
          case (true, false) if strictWidth => {
            errRhs map { ExprError() withLoc _ } getOrElse tree
          }
          case _ => {
            errLhs orElse errRhs map { ExprError() withLoc _ } getOrElse {
              lazy val lWidth = lhs.tpe.width
              lazy val rWidth = rhs.tpe.width
              if (strictWidth && lWidth != rWidth) {
                cc.error(
                  tree,
                  s"Both operands of binary '${op}' must have the same width, but",
                  s"left  hand operand is ${lWidth} bits wide, and",
                  s"right hand operand is ${rWidth} bits wide",
                )
                ExprError() withLoc expr.loc
              } else tree
            }
          }
        }
      }

      case expr @ ExprTernary(cond, thenExpr, elseExpr) => {
        checkNumericOrPacked(cond, "Condition of '?:'") map { ExprError() withLoc _ } getOrElse {
          lazy val errThen = checkPacked(thenExpr, "'then' operand of '?:'")
          lazy val errElse = checkPacked(elseExpr, "'else' operand of '?:'")
          (thenExpr.tpe.isNum, elseExpr.tpe.isNum) match {
            case (true, true) => {
              // Do nothing, will be folded later
              tree
            }
            case (false, true) => {
              errThen map { ExprError() withLoc _ } getOrElse tree
            }
            case (true, false) => {
              errElse map { ExprError() withLoc _ } getOrElse tree
            }
            case _ => {
              errThen orElse errElse map { ExprError() withLoc _ } getOrElse {
                val thenWidth = thenExpr.tpe.width
                val elseWidth = elseExpr.tpe.width
                if (thenWidth != elseWidth) {
                  cc.error(
                    tree,
                    s"'then' and 'else' operands of ternary '?:' must have the same width, but",
                    s"'then' operand is ${thenWidth} bits wide, and",
                    s"'else' operand is ${elseWidth} bits wide"
                  )
                  ExprError() withLoc expr.loc
                } else tree
              }
            }
          }
        }
      }

      case expr @ ExprType(origKind) => {
        val kind = origKind rewrite TypeTyper
        if (kind eq origKind) expr else expr.copy(kind = kind) withLoc tree.loc
      }

      //////////////////////////////////////////////////////////////////////////
      // Type the types of TypeSymbols introduced by TypeDefinitions
      //////////////////////////////////////////////////////////////////////////

      case _ => tree
    }

    // There is a race between the Typer running on multiple trees in parallel.
    // The trees have already undergone parameter specialization and therefore
    // might share instances of isomorphic sub-trees. As the type can be
    // assigned only once, we apply synchronization on the tesult node for this
    // step. This is not a problem as all threads would assign the same type.
    result synchronized {
      // Assign type if have not been assigned by now
      if (result.hasTpe) result else TypeAssigner(result)
    }
  }

  override def finalCheck(tree: Tree): Unit = {
    def check(tree: TreeLike) {
      tree visitAll {
        case node: Tree if !node.hasTpe => {
          if (externalRefs) {
            cc.ice(node, "Typer: untyped node remains", node.toString)
          }
        }
        case node: Tree if node.tpe.isInstanceOf[TypePolyFunc] => {
          cc.ice(node, s"Typer: node of type TypePolyFunc remains")
        }
        case Decl(symbol, _) => check(symbol.kind)
        case Sym(symbol)     => check(symbol.kind)
//        case ExprRef(symbol) => check(symbol.kind) // TODO: fix this
        case ExprType(kind) => check(kind)
      }
    }

    check(tree)
  }
}

object Typer {
  class TyperPass(externalRefs: Boolean) extends TreeTransformerPass {
    val name = "typer"
    def create(implicit cc: CompilerContext) = new Typer(externalRefs)(cc)
  }

  def apply(externalRefs: Boolean): Pass = {
    new TyperPass(externalRefs)
  }
}
