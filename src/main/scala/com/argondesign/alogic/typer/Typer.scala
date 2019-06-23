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
// - Assigns types to all nodes
// The typer is special and cannot rewrite any of the tree, unless there
// is a type error. The rest of the compiler (and the typer itself) relies on
// this property.
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
import com.argondesign.alogic.lib.Math.clog2
import com.argondesign.alogic.lib.Stack
import com.argondesign.alogic.lib.TreeLike
import com.argondesign.alogic.passes._
import com.argondesign.alogic.util.FollowedBy
import com.argondesign.alogic.util.unreachable

final class Typer(externalRefs: Boolean = false)(implicit cc: CompilerContext)
    extends TreeTransformer
    with FollowedBy {

  override val typed: Boolean = false

  private final val mixedWidthBinaryOps = Set("<<", ">>", "<<<", ">>>", "&&", "||")

  private def hasError(node: TreeLike): Boolean = node.children exists {
    case child: Tree => child.hasTpe && child.tpe.isError
    case child: Type => child.children exists hasError
    case _           => unreachable
  }

  private var hadError = false

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
    if (idx.tpe.isNum) {
      checkKnownConst(idx)
    } else {
      val errPacked = checkPacked(idx, msg)
      errPacked orElse {
        val errWidth = checkWidth(expectedWidth, idx, msg)
        val errSign = checkSign(false, idx, msg)
        errWidth orElse errSign
      }
    }
  }

  private def checkKnownConst(expr: Expr): Option[Loc] = {
    if (!expr.isKnownConst) {
      cc.error(expr, s"Will not infer width of non-constant expression, use explicit sizing")
    } else {
      None
    }
  }

  private val contextKind = Stack[Type]()

  private val contextNode = Stack[Int]()

  private def pushContextWidth(node: Tree, kind: Type) = {
    contextNode push node.id
    contextKind push kind.underlying
  }

  private[this] object TypeTyper extends TreeInTypeTransformer(this) {
    override def transform(kind: Type): Type = {
      val result = super.transform(kind)
      if (hasError(result)) TypeError else result
    }
  }

  private var inConnect = false

  override def skip(tree: Tree): Boolean = tree match {
    case _: Entity  => false
    case _: Connect => !externalRefs
    case _: Expr    => tree.hasTpe
    case _          => externalRefs && !inConnect
  }

  override def enter(tree: Tree): Unit = tree match {
    case _: Connect => {
      inConnect = true
    }

    case EntityNamed(symbol, _, _, _, _, _, _, _, _) => {
      // Type the source attributes
      // TODO: do them all in a systematic way..
      symbol.attr.stackLimit.get foreach { symbol.attr.stackLimit set walk(_).asInstanceOf[Expr] }
    }

    case Function(Sym(symbol), _) => {
      // Type the source attributes
      // TODO: do them all in a systematic way..
      symbol.attr.recLimit.get foreach { symbol.attr.recLimit set walk(_).asInstanceOf[Expr] }
    }

    case decl @ Decl(symbol, initOpt) => {
      // Type check trees in the type of the symbol
      val origKind = symbol.kind
      val kind = origKind rewrite TypeTyper
      if (kind ne origKind) {
        symbol.kind = kind
      }

      checkNameHidingByExtensionType(decl)

      if (kind.isPacked && kind.underlying != TypeVoid && kind.width < 1) {
        cc.error(decl, s"'${symbol.name}' is declared with width ${kind.width}")
        decl withTpe TypeError
      }

      // Track unary tick result types
      if (initOpt.isDefined) {
        pushContextWidth(tree, symbol.kind)
      }
    }

    // Type check the lhs up front
    case StmtAssign(lhs, _) if !walk(lhs).tpe.isError => {
      val err = checkPacked(lhs, "Left hand side of assignment") orElse checkModifiable(lhs)
      if (err.isDefined) {
        tree withTpe TypeError
      } else {
        pushContextWidth(tree, lhs.tpe)
      }
    }

    // Type check the lhs up front
    case StmtUpdate(lhs, _, _) if !walk(lhs).tpe.isError => {
      val err = checkPacked(lhs, "Left hand side of assignment") orElse checkModifiable(lhs)
      if (err.isDefined) {
        tree withTpe TypeError
      } else {
        pushContextWidth(tree, lhs.tpe)
      }
    }

    // Type check target up front
    case ExprIndex(tgt, _) if !walk(tgt).tpe.isError => {
      val shapeIter = tgt.tpe.shapeIter
      if (!shapeIter.hasNext) {
        cc.error(tgt, "Target is not indexable")
        tree withTpe TypeError
      } else {
        pushContextWidth(tree, TypeUInt(Expr(clog2(shapeIter.next) max 1) regularize tgt.loc))
      }
    }

    // Type check target up front
    case ExprSlice(tgt, _, _, _) if !walk(tgt).tpe.isError => {
      val shapeIter = tgt.tpe.shapeIter
      if (!shapeIter.hasNext || tgt.tpe.isArray) {
        cc.error(tgt, "Target is not sliceable")
        tree withTpe TypeError
      } else {
        pushContextWidth(tree, TypeUInt(Expr(clog2(shapeIter.next) max 1) regularize tgt.loc))
      }
    }

    // Type check target up front
    case ExprCall(tgt, args) if !walk(tgt).tpe.isError => {
      def process(kinds: List[Type]): Unit = {
        if (kinds.length != args.length) {
          cc.error(tree, s"Function call expects ${kinds.length} arguments, ${args.length} given")
          tree withTpe TypeError
        } else {
          for ((kind, arg) <- (kinds zip args).reverse) {
            pushContextWidth(arg, kind)
          }
        }
      }
      tgt.tpe match {
        case TypeCombFunc(argTypes, _) => process(argTypes)
        case TypeCtrlFunc(argTypes, _) => process(argTypes)
        case _: TypePolyFunc           => ()
        case _ =>
          cc.error(tree, s"'${tgt.toSource}' is not callable")
          tree withTpe TypeError
      }
    }

    case _ => ()
  }

  override def transform(tree: Tree): Tree = {
    // TODO: reduction of 1 bit value is error
    // TODO: Warn for non power of 2 dimensions

    val result: Tree = if (tree.hasTpe) {
      // Some nodes have been typed int he the enter call, don't process them
      tree
    } else {
      tree match {
        ////////////////////////////////////////////////////////////////////////////
        // Propagate type errors
        ////////////////////////////////////////////////////////////////////////////

        case node if hasError(node) => tree withTpe TypeError

        ////////////////////////////////////////////////////////////////////////////
        // Type check other nodes
        ////////////////////////////////////////////////////////////////////////////

        case entity: EntityNamed => {
          if (entity.fenceStmts exists { _.tpe == TypeCtrlStmt }) {
            cc.error("'fence' block must contain only combinatorial statements")
            entity.copy(fenceStmts = Nil) withLoc tree.loc withTpe TypeError
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
          if (symbol.kind.underlying.isNum && init.tpe.isPacked) {
            if (symbol.kind.isParam) {
              cc.error(init, "Unsized integer parameter assigned a packed value")
            } else {
              cc.error(decl, "Unsized integer declaration has packed initializer")
            }
            decl withTpe TypeError
          } else {

            val newdecl = if (symbol.kind.underlying.isNum || init.tpe.isNum) {
              if (init.tpe.isNum) {
                checkKnownConst(init)
              }
              // TODO: check signed/unsigned somewhere?
              decl
            } else {
              // Check initializer
              val msg = if (symbol.kind.isParam) "Parameter value" else "Initializer expression"
              val packedErrOpt = checkPacked(init, msg)
              lazy val widthErrOpt = {
                checkWidth(symbol.kind.width, init, msg)
              }
              packedErrOpt orElse widthErrOpt map { errLoc =>
                decl.copy(init = Some(TypeAssigner(ExprError() withLoc errLoc))) withLoc tree.loc
              } getOrElse {
                decl
              }
            }

            // Attach initializer expression attribute to param/const declarations.
            if ((symbol.kind.isConst || symbol.kind.isParam) && !newdecl.init.get.tpe.isError) {
              symbol.attr.init set {
                val init = newdecl.init.get
                val expr = if (symbol.kind.isPacked && init.tpe.isNum) {
                  init cast TypeInt(init.tpe.isSigned, Expr(symbol.kind.width) regularize init.loc)
                } else {
                  init
                }
                expr.simplify
                // TODO: check it's a compile time constant
              }
            }

            // Yield actual declaration
            newdecl
          }
        }

        ////////////////////////////////////////////////////////////////////////////
        // Type check statements
        ////////////////////////////////////////////////////////////////////////////

        case StmtBlock(body) => checkBlock(body) map { StmtError() withLoc _ } getOrElse tree

        case StmtIf(_, thenStmt, Some(elseStmt)) => {
          (thenStmt.tpe, elseStmt.tpe) match {
            case (TypeCombStmt, TypeCombStmt) => tree
            case (TypeCtrlStmt, TypeCtrlStmt) => tree
            case _ => {
              cc.error(tree,
                       "Either both or neither branches of if-else must be control statements")
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
          // lhs have already been checked in enter
          if (rhs.tpe.isNum) {
            checkKnownConst(rhs)
            tree
          } else {
            // Type check rhs
            val rhsErrOpt = checkPacked(rhs, "Right hand side of assignment")
            lazy val widthErrOpt = checkWidth(lhs.tpe.width, rhs, "Right hand side of assignment")
            rhsErrOpt orElse widthErrOpt map {
              StmtError() withLoc _
            } getOrElse tree
          }
        }

        case stmt @ StmtUpdate(lhs, _, rhs) => {
          // lhs have already been checked in enter
          if (rhs.tpe.isNum) {
            checkKnownConst(rhs)
            tree
          } else {
            // Check rhs
            val rhsErrOpt = checkPacked(rhs, "Right hand side of assignment")
            lazy val widthErrOpt = checkWidth(lhs.tpe.width, rhs, "Right hand side of assignment")
            rhsErrOpt orElse widthErrOpt map {
              StmtError() withLoc _
            } getOrElse tree
          }
        }

        case StmtPost(expr, op) => {
          checkPacked(expr, s"Target of postfix '${op}'") orElse
            checkModifiable(expr) map {
            StmtError() withLoc _
          } getOrElse tree
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
              s"No $thing named '$selector' in '${expr.toSource}' of type '${expr.tpe.toSource}'")
            ExprError() withLoc tree.loc
          }
        }

        case ExprCall(expr, args) => {

          def checkArg(expected: Type, arg: Expr, i: Int): Tree = {
            if (expected.isType) {
              if (!arg.tpe.isType) {
                cc.error(arg, s"Argument $i expects a type")
                ExprError() withLoc expr.loc
              } else {
                tree
              }
            } else if (expected.underlying.isNum) {
              if (!expr.tpe.isNum) {
                cc.error(arg, s"Argument $i expects an unsized value")
                ExprError() withLoc expr.loc
              } else {
                // TODO: check signedness?
                checkKnownConst(arg)
                tree
              }
            } else if (arg.tpe.isNum) {
              checkKnownConst(arg)
              tree
            } else {
              checkPacked(arg, s"Argument $i to function call") map {
                ExprError() withLoc _
              } getOrElse {
                val argWidth = arg.tpe.width
                val expWidth = expected.width
                if (argWidth != expWidth) {
                  val cmp = if (argWidth > expWidth) "greater" else "less"
                  cc.error(
                    arg,
                    s"Width ${argWidth} of argument ${i} passed to function call is ${cmp} than expected width ${expWidth}")
                  ExprError() withLoc expr.loc
                } else {
                  tree
                }
              }
            }
          }

          def checkFunc(argTypes: List[Type]) = {
            val errOpt = {
              val tmp = for (((e, a), i) <- (argTypes zip args).zipWithIndex) yield {
                checkArg(e, a, i + 1)
              }
              tmp collectFirst { case e: ExprError => e }
            }
            errOpt getOrElse tree
          }

          expr.tpe match {
            // Nothing to do for ordinary functions
            case tpe: TypeCombFunc => checkFunc(tpe.argTypes)
            case tpe: TypeCtrlFunc => checkFunc(tpe.argTypes)
            // Get result type of polymorphic builtins
            case tpe: TypePolyFunc =>
              tpe.resolve(args) match {
                case Some(symbol) => tree withTpe symbol.kind.asInstanceOf[TypeCombFunc].retType
                case None =>
                  val err = args map { a =>
                    s"'${a.toSource}' of type ${a.tpe.toSource}"
                  }
                  cc.error(
                    tree,
                    s"Builtin function '${expr.toSource}' cannot be applied to arguments" :: err: _*)
                  ExprError() withLoc tree.loc
              }
            // Anything else is not callable
            case _ => unreachable // Handle in enter
          }
        }

        case ExprCat(parts) => {
          val errors = for ((part, i) <- parts.zipWithIndex) yield {
            checkPacked(part, s"Part ${i + 1} of bit concatenation")
          }
          errors.flatten.headOption map {
            ExprError() withLoc _
          } getOrElse tree
        }

        case ExprRep(count, expr) => {
          val errCount = checkNumeric(count, "Count of bit repetition")
          val errExpr = checkPacked(expr, "Value of bit repetition")
          errCount orElse errExpr map {
            ExprError() withLoc _
          } getOrElse tree
        }

        case expr @ ExprIndex(tgt, idx) => {
          val errTgt = if (tgt.tpe.isArray) None else checkPacked(tgt, "Target of index")
          val errIdx = checkIndex(contextKind.top.width, idx, "Index")
          errTgt orElse errIdx map {
            ExprError() withLoc _
          } getOrElse tree
        }

        case expr @ ExprSlice(tgt, lidx, _, ridx) => {
          val errTgt = checkPacked(tgt, "Target of slice")
          val errLidx = checkIndex(contextKind.top.width, lidx, "Left index")
          val errRidx = checkIndex(contextKind.top.width, ridx, "Right index")
          errTgt orElse errLidx orElse errRidx map {
            ExprError() withLoc _
          } getOrElse tree
        }

        // Unary ops
        case expr @ ExprUnary("'", op) => {
          if (hadError) {
            // If we had a type error, the context stack might be out of sync
            // so don't type check any further unary tick nodes
            tree withTpe TypeError
          } else if (contextKind.isEmpty) {
            cc.error(tree, "Unary ' operator used in invalid context")
            ExprError() withLoc tree.loc
          } else {
            checkPacked(op, "Operand of unary ' operator") map {
              ExprError() withLoc _
            } getOrElse {
              if (contextKind.top.isNum) {
                checkKnownConst(expr)
                tree withTpe TypeNum(op.tpe.isSigned)
              } else {
                val resWidth = contextKind.top.width
                val opWidth = op.tpe.width
                if (resWidth < opWidth) {
                  cc.error(
                    s"Result width ${resWidth} of unary ' operator is narrower than operand width ${opWidth}")
                  ExprError() withLoc tree.loc
                } else {
                  val widthExpr = Expr(resWidth) regularize tree.loc
                  tree withTpe TypeInt(op.tpe.isSigned, widthExpr)
                }
              }
            }
          }
        }

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
              val errRhs = checkKnownConst(rhs)
              errLhs orElse errRhs map {
                ExprError() withLoc _
              } getOrElse tree
            }
            case (true, false) if strictWidth => {
              val errLhs = checkKnownConst(lhs)
              errRhs orElse errLhs map {
                ExprError() withLoc _
              } getOrElse tree
            }
            case _ => {
              errLhs orElse errRhs map {
                ExprError() withLoc _
              } getOrElse {
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
          checkNumericOrPacked(cond, "Condition of '?:'") map {
            ExprError() withLoc _
          } getOrElse {
            lazy val errThen = checkPacked(thenExpr, "'then' operand of '?:'")
            lazy val errElse = checkPacked(elseExpr, "'else' operand of '?:'")
            (thenExpr.tpe.isNum, elseExpr.tpe.isNum) match {
              case (true, true) => {
                // Do nothing, will be folded later
                tree
              }
              case (false, true) => {
                val errElse = checkKnownConst(elseExpr)
                errThen orElse errElse map {
                  ExprError() withLoc _
                } getOrElse tree
              }
              case (true, false) => {
                val errThen = checkKnownConst(thenExpr)
                errElse orElse errThen map {
                  ExprError() withLoc _
                } getOrElse tree
              }
              case _ => {
                errThen orElse errElse map {
                  ExprError() withLoc _
                } getOrElse {
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

        case expr @ ExprCast(origKind, _) => {
          val kind = origKind rewrite TypeTyper
          if (kind eq origKind) expr else expr.copy(kind = kind) withLoc tree.loc
          // TODO: check preserves sign
        }

        //////////////////////////////////////////////////////////////////////////
        // Type the types of TypeSymbols introduced by TypeDefinitions
        //////////////////////////////////////////////////////////////////////////

        case _ => tree
      }
    }

    // Pop context stack if required. This is a loop as some nodes might have
    // been pushed multiple times, e.g.: port.write(array[index]), both the
    // ExprCall and ExprIndex would have pushed the ExprIndex node
    while (contextNode.nonEmpty && contextNode.top == tree.id) {
      contextNode.pop()
      contextKind.pop()
    }

    // There is a race between the Typer running on multiple trees in parallel.
    // The trees have already undergone parameter specialization and therefore
    // might share instances of isomorphic sub-trees. As the type can be
    // assigned only once, we apply synchronization on the tesult node for this
    // step. This is not a problem as all threads would assign the same type.

    result synchronized {
      // Assign type if have not been assigned by now
      if (result.hasTpe) result else TypeAssigner(result)
    } followedBy {
      assert(result.tpe.isError || (tree eq result), (result, result.tpe))
      hadError |= result.tpe.isError
    }
  }

  override def finalCheck(tree: Tree): Unit = {
    if (!tree.tpe.isError) {
      assert(contextKind.isEmpty, s"${contextKind.toList} ${contextNode.toList}")

      def check(tree: TreeLike) {
        tree visitAll {
          case node: Tree if !node.hasTpe => {
            if (externalRefs) {
              cc.ice(node, "Typer: untyped node remains", node.toString)
            }
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
