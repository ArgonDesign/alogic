////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// The type checker
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.analysis.WrittenSyms
import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeReady
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeValid
import com.argondesign.alogic.core.FuncVariant
import com.argondesign.alogic.core.Locatable
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Messages.Message
import com.argondesign.alogic.core.Messages.Note
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.TypeCompound
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Math.clog2
import com.argondesign.alogic.util.unreachable
import com.argondesign.alogic.util.BigIntOps._
import com.argondesign.alogic.util.BooleanOps._

import scala.annotation.tailrec
import scala.collection.immutable.Set
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.blocking

final private class TypeChecker(val root: Tree)(implicit cc: CompilerContext, fe: Frontend)
    extends StatelessTreeTransformer {
  override val typed: Boolean = false

  private val comparisonBinaryOps = Set(">", ">=", "<", "<=", "==", "!=")

  //////////////////////////////////////////////////////////////////////////////
  // Buffers used to track progress
  //////////////////////////////////////////////////////////////////////////////

  // Failure messages
  val mAcc = new ListBuffer[Message]
  // Unknown reasons
  val rAcc = new ListBuffer[Reason]

  // Predicate: Have we already encountered an error during the traversal?
  private def hadError: Boolean = mAcc.nonEmpty

  //////////////////////////////////////////////////////////////////////////////
  // Enclosing function tracker
  //////////////////////////////////////////////////////////////////////////////

  private object EnclosingFunction {
    private val stack = mutable.Stack[(Symbol, Int)]()

    def push(desc: DescFunc): Unit = {
      stack.push((desc.symbol, desc.id))
    }

    def pop(node: Tree): Unit = {
      if (stack.headOption.exists(_._2 == node.id)) {
        stack.pop()
      }
    }

    def top: Symbol = stack.top._1

    def get: Option[Symbol] = stack.headOption.map(_._1)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Unary ' context tracker
  //////////////////////////////////////////////////////////////////////////////

  private object UnaryTickContext {
    // The expected result type of unary ' operators, and node id to pop stack at
    private val stack = mutable.Stack[(Option[Type], Int)]()

    def pushType(popAtTree: Tree, kind: Type): Unit = {
      // Note: Anything that has a type set will be skipped, so only push
      // nodes which will actually be visited
      if (!popAtTree.hasTpe) {
        stack.push((Some(kind.underlying), popAtTree.id))
      }
    }

    def pushNoType(popAtTree: Tree): Unit = {
      // Note: Anything that has a type set will be skipped, so only push
      // nodes which will actually be visited
      if (!popAtTree.hasTpe) {
        stack.push((None, popAtTree.id))
      }
    }

    def pop(node: Tree): Unit = {
      // Pop context stack if required. This is a loop as some nodes might
      // have been pushed multiple times, e.g.: port.write(array[index]),
      // both the ExprCall and ExprIndex would have pushed the ExprIndex
      // node
      while (stack.headOption exists { _._2 == node.id }) {
        stack.pop()
      }
    }

    // An expected type is available based on context
    def hasExpected: Boolean = stack.nonEmpty && stack.top._1.nonEmpty

    // The expected type
    def expected: Type = stack.top._1.get

    def finalChecks(tree: Tree): Unit = {
      assert(stack.isEmpty, s"${tree.loc.prefix}\n$stack")
    }

  }

  //////////////////////////////////////////////////////////////////////////////
  // The following methods all take an implicit Tree parameter. This is always
  // the current tree being analyzed (as in the tree which was passed to 'enter'
  // and 'transform'), and is passed implicitly simply for convenience.
  //////////////////////////////////////////////////////////////////////////////

  // Assign TypeError to the Tree 'tree' being processed. Note this should only
  // be used if at least one error will eventually be reported, as every error
  // needs an explanation.
  private def error(implicit tree: Tree): Unit = if (tree.hasTpe) {
    assert(tree.tpe.isError, s"Tree with type error has non error type ${tree.tpe}")
  } else {
    tree withTpe TypeError
  }

  // Main error reporting method. The passed message will be reported as an
  // Error at the location of the 'mark' provided, which can be any type T with
  // has a Locatable[T] (i.e.: anything we can compute a source location for).
  private def error[T: Locatable](
      mark: T,
      msgHead: String,
      msgRest: String*
    )(
      implicit
      tree: Tree
    ): Unit = {
    error
    val locate = implicitly[Locatable[T]]
    mAcc.addOne(Error(locate(mark), msgHead +: msgRest: _*))
  }

  // Same as above but location provided from the 'tree' node.
  private def error(msgHead: String, msgRest: String*)(implicit tree: Tree): Unit =
    error(tree, msgHead, msgRest: _*)

  private def addNote(note: Note): Unit = {
    val newLast = mAcc.last match {
      case error: Error => error withNote note
      case _            => unreachable
    }
    mAcc.update(mAcc.size - 1, newLast)
  }

  // Check whether the Tree 'tree' currently being analyzed has been determined
  // to have a type error yet
  private def okSoFar(implicit tree: Tree): Boolean = {
    require(tree.hasTpe |-> tree.tpe.isError)
    !tree.hasTpe
  }

  // Evaluate Expression using the frontend, propagate unknowns/errors to the
  // type checking result. Return Some(_) if the value is computable.
  private def evaluate(expr: Expr, subject: => String)(implicit tree: Tree): Option[BigInt] =
    fe.evaluate(expr, subject) match {
      case Complete(v) => Some(v)
      case Unknown(rs) => rAcc addAll rs; None
      case Failure(ms) => mAcc addAll ms; error; None
    }

  // Wonders of the english language
  private def pluralize(value: BigInt, singular: String, plural: String): String = {
    if (value == 1) s"$value $singular" else s"$value $plural"
  }

  private def bitsPlur(value: BigInt): String = pluralize(value, "bit", "bits")

  //////////////////////////////////////////////////////////////////////////////
  // The following check* methods all set the Tree 'tree' being processed to
  // have type TypeError if the check fails. 'okSoFar' can be used later to see
  // if the checks performed prior have failed or not.
  //////////////////////////////////////////////////////////////////////////////

  private def checkPacked(expr: Expr, subject: => String)(implicit tree: Tree): Unit =
    if (!expr.tpe.isPacked) {
      error(expr, s"$subject is of non-packed type")
    } else if (expr.tpe.width == 0) {
      error(expr, s"$subject has 0 width")
    }

  private def checkNumeric(expr: Expr, subject: => String)(implicit tree: Tree): Unit =
    if (!expr.tpe.underlying.isNumeric) {
      error(expr, s"$subject is of non-numeric type")
    }

  private def checkNumericOrPacked(expr: Expr, subject: => String)(implicit tree: Tree): Unit =
    if (!expr.tpe.underlying.isNumeric && !expr.tpe.isPacked) {
      error(expr, s"$subject is of neither numeric nor packed type")
    } else if (expr.tpe.isPacked && expr.tpe.width == 0) {
      error(expr, s"$subject has 0 width")
    }

  private def checkWidth(width: BigInt, expr: Expr, subject: String)(implicit tree: Tree): Unit = {
    def complain(problem: String): Unit =
      error(expr, s"$subject $problem, a $width bit value is expected")
    if (expr.tpe.underlying.isNum) {
      complain("yields an unsized value")
    } else if (!expr.tpe.isPacked) {
      complain("is of non-packed type")
    } else if (expr.tpe.width != width) {
      complain(s"yields ${bitsPlur(expr.tpe.width)}")
    }
  }

  private def checkUnsigned(expr: Expr, subject: String)(implicit tree: Tree): Unit =
    if (expr.tpe.isSigned) {
      error(expr, s"$subject must be unsigned")
    }

  private def checkBlock(stmts: List[Stmt])(implicit tree: Tree): Unit = {
    def hasCtrl(trees: List[Tree]): Boolean =
      trees
        .exists {
          case StmtSplice(DescGenScope(_, _, body, _)) => hasCtrl(body)
          case stmt: Stmt                              => stmt.tpe.isCtrlStmt
          case _                                       => unreachable
        }
    def lastStmt(trees: List[Tree]): Option[Stmt] =
      trees.lastOption
        .flatMap {
          case StmtSplice(DescGenScope(_, _, body, _)) => lastStmt(body)
          case stmt: Stmt                              => Some(stmt)
          case _                                       => unreachable
        }
    val lastStmtOpt = lastStmt(stmts)
    val lastIsCtrl = lastStmtOpt.exists(_.tpe == TypeCtrlStmt)
    if (hasCtrl(stmts) && !lastIsCtrl) {
      error(
        lastStmtOpt.get,
        "Block must contain only combinational statements, or end with a control statement"
      )
    }
  }

  private def checkIndex(
      expectedWidthOpt: Option[BigInt],
      idx: Expr,
      hint: => String
    )(
      implicit
      tree: Tree
    ): Unit =
    if (idx.tpe.underlying.isNum) {
      checkUnsigned(idx, hint)
    } else {
      checkPacked(idx, hint)
      if (okSoFar) {
        expectedWidthOpt foreach { checkWidth(_, idx, hint) }
        checkUnsigned(idx, hint)
      }
    }

  private def checkInRange(
      value: BigInt,
      lo: BigInt,
      loIsInclusive: Boolean,
      hi: BigInt,
      hiIsInclusive: Boolean,
      subject: String,
      mark: Tree
    )(
      implicit
      tree: Tree
    ): Unit = {
    def complain(problem: String): Unit =
      error(mark, s"$subject is out of range ($value is $problem)")
    if (loIsInclusive && value < lo) {
      complain(s"< $lo")
    } else if (!loIsInclusive && value <= lo) {
      complain(s"<= $lo")
    } else if (hiIsInclusive && value > hi) {
      complain(s"> $hi")
    } else if (!hiIsInclusive && value >= hi) {
      complain(s">= $hi")
    }
  }

  private def checkPositive(value: BigInt, subject: String, mark: Tree)(implicit tree: Tree): Unit =
    if (value <= 0) {
      error(mark, s"$subject must be positive ($value is <= 0)")
    }

  private def checkModifiable(expr: Expr)(implicit tree: Tree): Unit =
    WrittenSyms(expr) foreach {
      case ref @ ExprSym(symbol) =>
        symbol.kind match {
          case _: TypeParam => error(ref, "Parameter cannot be modified")
          case _: TypeConst => error(ref, "Constant cannot be modified")
          case _: TypeIn    => error(ref, "Input port cannot be modified")
          case _: TypeArray => error(ref, "Memory can only be modified using .write()")
          case TypeOut(_, fct, _) =>
            if (fct != FlowControlTypeNone) {
              error(ref, "Output port with flow control can only be modified using .write()")
            }
          case _ =>
            symbol.desc match {
              case _: DescVal => error(ref, "'const' qualified variable cannot be modified")
              case _          =>
            }
        }
    }

  private def checkEntity(expr: Expr)(implicit tree: Tree): Unit =
    expr.tpe match {
      case TypeType(_: TypeEntity) => // OK
      case _                       => error(expr, "Expression does not name an entity")
    }

  // Other check* methods

  //        def checkNameHidingByExtensionType(decl: Decl): Unit = {
  //          decl.symbol.kind match {
  //            case eKind: ExtensionType =>
  //              eKind.kind match {
  //                case cKind: TypeCompound =>
  //                  for {
  //                    cSymbol <- cKind.publicSymbols
  //                    eSymbol <- eKind(cSymbol.name)
  //                    if cSymbol ne eSymbol
  //                  } {
  //                    cc.error(
  //                      decl.loc,
  //                      s"Field '${cSymbol.name}' of type '${cSymbol.kind.toSource}' of symbol '${decl.symbol.name}'",
  //                      s"defined in type '${cKind.toSource}'",
  //                      s"is hidden by extension field of the same name of type '${eSymbol.kind.toSource}'",
  //                      s"defined by extension type '${eKind.toSource}'"
  //                    )
  //                  }
  //                case _ => ()
  //              }
  //            case _ => ()
  //          }
  //        }

  // Called in pre-order (from 'enter') to check sub-trees ahead of the
  // traversal
  private def typeCheckUpFrontEter(
      subject: Tree
    )(
      result: => Option[Tree]
    )(
      implicit
      tree: Tree
    ): Option[Tree] = {
    // Type check the subject tree
    walk(subject)
    // If the type is not known, then we must have some unresolved dependency
    assert(!subject.hasTpe |-> rAcc.nonEmpty)
    // If subject is well typed, compute result, otherwise default,
    // but mark as error if type error.
    if (subject.hasTpe) {
      if (!subject.tpe.isError) {
        result
      } else {
        error
        Some(tree)
      }
    } else {
      Some(tree)
    }
  }

  // Same as above but with no result/fallback tree, called form 'preOrder'
  private def typeCheckUpFront(
      subject: Tree
    )(
      block: => Unit
    ): Unit = {
    walk(subject)
    assert(!subject.hasTpe |-> rAcc.nonEmpty)
    if (subject.hasTpe && !subject.tpe.isError) {
      block
    }
  }

  private def propagateChildErrorIfAny(tree: Tree, recursive: Boolean): Unit = {
    // If any of the children have a type error, propagate it upwards
    if (recursive) {
      tree.children.filterNot(_.hasTpe).foreach(propagateChildErrorIfAny(_, recursive = true))
    }
    if (tree.children.exists(child => child.hasTpe && child.tpe.isError)) {
      error(tree)
    }
  }

  override def enter(tree: Tree): Option[Tree] = {
    implicit val theTree: Tree = tree
    tree pipe {
      // Skip already typed sub-trees
      case _ if tree.hasTpe => Some(tree)

      case e: ExprIdent =>
        rAcc addOne ReasonUnresolved(e)
        Some(tree)
      case Desc(_: Ident) | _: DescGenIf | _: DescGenFor | _: DescGenRange | _: Using | _: Import =>
        rAcc addOne ReasonUnelaborated(tree)
        Some(tree)
      case DescParametrized(Sym(symbol), _, _, _) =>
        // Do not descend into parameterized definitions as they cannot be
        // type checked without actual parameter assignments. This is the
        // only case where we will have un-typed sub-trees under a typed tree.
        tree withTpe TypeParametrized(symbol)
        Some(tree)
      case ExprSym(symbol) =>
        // Compute and assign the type of the referenced symbol. We do this
        // via the frontend so we can spot circular dependencies.
        fe.typeOf(symbol, tree.loc) match {
          case Complete(kind) =>
            tree withTpe {
              kind match {
                case TypeConst(kind)   => kind
                case TypePipeVar(kind) => kind
                case _                 => kind
              }
            }
          case Unknown(rs) =>
            rAcc addAll rs
          case Failure(ms) =>
            mAcc addAll ms
            error
        }
        Some(tree)
      case ExprCall(tgt, args) =>
        typeCheckUpFrontEter(tgt) {
          def check(kinds: List[Type]): Option[tree.type] =
            if (kinds.length != args.length) {
              error(s"Function call expects ${kinds.length} arguments, ${args.length} given")
              Some(tree)
            } else {
              for ((kind, arg) <- (kinds zip args).reverse) {
                UnaryTickContext.pushType(arg, kind)
              }
              None
            }

          tgt match {
            case ExprType(_: TypeNum) => None // Will be resolved in postOrder
            case _ =>
              tgt.tpe match {
                case TypeCombFunc(_, _, argTypes)     => check(argTypes)
                case TypeCtrlFunc(_, _, argTypes)     => check(argTypes)
                case TypeXenoFunc(_, _, argTypes)     => check(argTypes)
                case TypeStaticMethod(_, _, argTypes) => check(argTypes)
                case TypeNormalMethod(_, _, argTypes) => check(argTypes)
                case TypeParametrized(symbol)         =>
                  // Specialize here without type checking the arguments as
                  // the arguments (parameter assignments) are not typeable
                  // without knowing the definitions (which are only known
                  // during specialization) due to possible unary ticks.
                  Some {
                    fe.specialize(symbol, args, tree.loc).flatMap { specialized =>
                      assert(specialized.descOption.isDefined)
                      fe.typeOf(specialized, tree.loc)
                    } match {
                      case Complete(kind) =>
                        tree withTpe kind
                      case Unknown(rs) =>
                        rAcc addAll rs
                        tree
                      case Failure(ms) =>
                        mAcc addAll ms
                        error
                        tree
                    }
                  }
                case TypeNone(_: TypeNormalMethod) =>
                  error(tgt, "Attempting to call non-static method via type")
                  Some(tree)
                case _: TypeType =>
                  error(tgt, s"Type does not take any parameters")
                  Some(tree)
                case _: TypePackage =>
                  error(tgt, s"Package does not take any parameters")
                  Some(tree)
                case _ =>
                  error(tgt, s"Expression is not callable")
                  Some(tree)
              }
          }
        }
      case _ =>
        // Apply pre-order checks
        preOrder(tree)
        // Stop if error or unresolved
        Option.when(!okSoFar || rAcc.nonEmpty) {
          propagateChildErrorIfAny(tree, recursive = true)
          tree
        }
    } tap {
      case Some(_) =>
        UnaryTickContext.pop(tree)
        EnclosingFunction.pop(tree)
      case None =>
        assert(!tree.hasTpe, tree.tpe)
    }
  }

  // TODO: Need to run this on the root node at the end
  private def childChecks(child: Tree, parametrizedOk: Boolean)(implicit tree: Tree): Unit =
    child match {
      case expr: Expr =>
        if (!parametrizedOk && expr.tpe.isParametrized) {
          val what = expr.tpe.asParametrized.symbol.desc.asInstanceOf[DescParametrized].desc match {
            case _: DescPackage => "package"
            case _: DescEntity  => "entity"
            case _: DescRecord  => "struct"
            case _              => unreachable
          }
          error(expr, s"Parametrized $what requires parameter list")
        }
        if (!tree.isInstanceOf[Expr]) {
          expr visit {
            case t if !t.hasTpe => // Stop descent
            case e: Expr if e.tpe.underlying.isNum =>
              evaluate(e, "Expression of unsized integer type")
          }
        }
      case _ =>
    }

  // Ensure all expressions referencing a parametrized type have an explicit
  // parameter list applied to them, except for the few cases where it's ok.
  // Ensure all expressions of unsized integer type are compile time constants
  private def checkChildren(implicit tree: Tree): Unit = {
    tree match {
      case _: ExprCall | _: DescAlias =>
        // Target of ExprCall can be a parametrized type,
        // and we can also alias a parametrized type
        tree.children.iterator.foreach(childChecks(_, parametrizedOk = true))
      case _ =>
        tree.children.iterator.foreach(childChecks(_, parametrizedOk = false))
    }
  }

  override def transform(tree: Tree): Tree = {
    require(!tree.hasTpe)
    implicit val theTree: Tree = tree
    val alreadyHadError = hadError

    propagateChildErrorIfAny(tree, recursive = false)

    // Bail quickly if we already had an unknown
    if (rAcc.isEmpty && okSoFar) {
      checkChildren
      if (okSoFar) {
        // Apply the post-order checks
        postOrder(tree)
      }
    }

    // Bail quickly if we already had an unknown or just discovered one
    if (rAcc.isEmpty) {
      // Check that if we have just found an error, then we have also
      // marked the tree with TypeError
      assert((!alreadyHadError && hadError) |-> tree.tpe.isError)
      // Track the unary ' context
      UnaryTickContext.pop(tree)
      // Track function context
      EnclosingFunction.pop(tree)
      // Assign type if it has not been assigned by now
      if (!tree.hasTpe) {
        TypeAssigner(tree)
      }
    }

    // Never re-write the tree during type checking
    tree
  }

  // Walks as many statements as necessary to determine whether ambiguous
  // 'unreachable' statements in the list are control statements or comb
  // statements. It is a control statement if there are any other control
  // statements in the list. Returns 'Some(true)' if the 'unreachable'
  // statement should be treated as a control statement, 'None' if currently
  // not determinable.
  private def computeTypeOfAmbiguousUnreachableStatements(stmts: List[Tree]): Option[Boolean] = {
    stmts.iterator
      .map {
        // Type of 'unreachable' statement might be already set when walking the
        // body of a StmtBlock or StmtLet contained in an outer statement.
        case stmt @ StmtSplice(AssertionUnreachable(None, _, _)) if !stmt.hasTpe =>
          Some(false)
        case StmtSplice(DescGenScope(_, _, body, _)) =>
          computeTypeOfAmbiguousUnreachableStatements(body)
        case StmtBlock(body) =>
          computeTypeOfAmbiguousUnreachableStatements(body)
        case StmtLet(_, body) =>
          computeTypeOfAmbiguousUnreachableStatements(body)
        case stmt: Stmt =>
          if (!stmt.hasTpe) {
            walk(stmt)
          }
          // walk might not assign a type if an error has occurred
          Option.when(stmt.hasTpe)(stmt.tpe.isCtrlStmt)
        case _ => unreachable
      }
      .foldLeft[Option[Boolean]](Some(false)) {
        case (None, _) | (_, None)             => None
        case (Some(true), _) | (_, Some(true)) => Some(true)
        case _                                 => Some(false)
      }
  }

  // Set the types of ambiguous 'unreachable' statements
  private def setTypeOfAmbiguousUnreachableStatements(stmts: List[Tree], ctrl: Boolean): Unit =
    stmts.foreach {
      // Type of 'unreachable' statement might be already set when walking the
      // body of a StmtBlock or StmtLet contained in an outer statement.
      case stmt @ StmtSplice(a @ AssertionUnreachable(None, _, _)) if !stmt.hasTpe =>
        // Note: 'a' might already have it's type assigned due to structural
        // sharing via 'gen' expansion and type checking of an earlier
        // containing tree
        if (!a.hasTpe) {
          TypeAssigner(a)
        }
        stmt.withTpe(if (ctrl) TypeCtrlStmt else TypeCombStmt)
      case StmtSplice(DescGenScope(_, _, body, _)) =>
        setTypeOfAmbiguousUnreachableStatements(body, ctrl)
      case StmtBlock(body) =>
        setTypeOfAmbiguousUnreachableStatements(body, ctrl)
      case StmtLet(_, body) =>
        setTypeOfAmbiguousUnreachableStatements(body, ctrl)
      case _: Stmt =>
      case _       => unreachable
    }

  private def preOrder(implicit tree: Tree): Unit = tree match {
    case desc: Desc =>
      // Compute type of symbol up front as we might need it for the
      // context width below. This in effect type checks the necessary
      // parts of the definition out of order
      fe.typeOf(desc.symbol, desc.symbol.loc) match {
        case Complete(kind) => // Ok
          // Allow unary ticks in initializers
          desc.initializer foreach { init =>
            UnaryTickContext.pushType(init, kind)
          }
        case Unknown(rs) =>
          rAcc addAll rs
        case Failure(ms) =>
          mAcc addAll ms
          error
      }

      desc match {
        case d @ DescFunc(_, _, variant, _, _, body) =>
          setTypeOfAmbiguousUnreachableStatements(body, variant == FuncVariant.Ctrl)
          EnclosingFunction.push(d)
        case _ =>
      }

    // Work out types of ambiguous 'unreachable' statements. An 'unrachable'
    // statement at this point is ambiguous if the 'comb' flag is false, as
    // it might still be a comb statement, if only comb statements are present
    // in the enclosing statement

    case StmtBlock(body) =>
      computeTypeOfAmbiguousUnreachableStatements(body) foreach { ctrl =>
        setTypeOfAmbiguousUnreachableStatements(body, ctrl)
      }
    case StmtIf(_, thenStmts, elseStmts) =>
      computeTypeOfAmbiguousUnreachableStatements(thenStmts) foreach { ctrlThen =>
        computeTypeOfAmbiguousUnreachableStatements(elseStmts) foreach { ctrlElse =>
          val ctrl = ctrlThen || ctrlElse
          setTypeOfAmbiguousUnreachableStatements(thenStmts, ctrl)
          setTypeOfAmbiguousUnreachableStatements(elseStmts, ctrl)
        }
      }
    case StmtCase(_, cases) =>
      cases.iterator
        .map {
          case CaseRegular(_, stmts) => computeTypeOfAmbiguousUnreachableStatements(stmts)
          case CaseDefault(stmts)    => computeTypeOfAmbiguousUnreachableStatements(stmts)
          case _: CaseSplice         => unreachable // Removed by Elaborate
        }
        .foldLeft[Option[Boolean]](Some(false)) {
          case (None, _) | (_, None)             => None
          case (Some(true), _) | (_, Some(true)) => Some(true)
          case _                                 => Some(false)
        }
        .foreach { ctrl =>
          cases foreach {
            case CaseRegular(_, stmts) => setTypeOfAmbiguousUnreachableStatements(stmts, ctrl)
            case CaseDefault(stmts)    => setTypeOfAmbiguousUnreachableStatements(stmts, ctrl)
            case _: CaseSplice         => unreachable // Removed by Elaborate
          }
        }
    case StmtLoop(body) =>
      computeTypeOfAmbiguousUnreachableStatements(body) foreach { ctrl =>
        setTypeOfAmbiguousUnreachableStatements(body, ctrl)
      }
    case StmtWhile(_, body) =>
      computeTypeOfAmbiguousUnreachableStatements(body) foreach { ctrl =>
        setTypeOfAmbiguousUnreachableStatements(body, ctrl)
      }
    case StmtFor(_, _, _, body) =>
      computeTypeOfAmbiguousUnreachableStatements(body) foreach { ctrl =>
        setTypeOfAmbiguousUnreachableStatements(body, ctrl)
      }
    case StmtDo(_, body) =>
      computeTypeOfAmbiguousUnreachableStatements(body) foreach { ctrl =>
        setTypeOfAmbiguousUnreachableStatements(body, ctrl)
      }
    case StmtLet(_, body) =>
      computeTypeOfAmbiguousUnreachableStatements(body) foreach { ctrl =>
        setTypeOfAmbiguousUnreachableStatements(body, ctrl)
      }

    // Type check the lhs up front
    case StmtAssign(lhs, _) =>
      typeCheckUpFront(lhs) {
        if (lhs.tpe.isGen && lhs.tpe.underlying.isNum) {
          // Assignment to unsized gen var is ok
        } else {
          checkModifiable(lhs)
          checkPacked(lhs, "Left hand side of assignment")
          if (okSoFar) {
            UnaryTickContext.pushType(tree, lhs.tpe)
          }
        }
      }

    // Type check the lhs up front
    case StmtUpdate(lhs, _, _) =>
      typeCheckUpFront(lhs) {
        if (lhs.tpe.isGen && lhs.tpe.underlying.isNum) {
          // Assignment to unsized gen var is ok
        } else {
          checkModifiable(lhs)
          checkPacked(lhs, "Left hand side of assignment")
          if (okSoFar) {
            UnaryTickContext.pushType(tree, lhs.tpe)
          }
        }
      }

    case _: StmtReturn =>
      EnclosingFunction.get foreach {
        _.kind match {
          case TypeCallable(_, retType, _) if retType != TypeVoid =>
            UnaryTickContext.pushType(tree, retType)
          case _ =>
        }
      }

    // Type check target up front
    case ExprIndex(tgt, _) =>
      typeCheckUpFront(tgt) {
        if (tgt.tpe.isType) {
          UnaryTickContext.pushNoType(tree)
        } else if (!tgt.tpe.underlying.isNum) {
          val shapeIter = tgt.tpe.shapeIter
          if (!shapeIter.hasNext && !tgt.tpe.underlying.isNum) {
            error(tgt, "Target is not indexable")
          } else {
            UnaryTickContext.pushType(tree, TypeUInt(clog2(shapeIter.next()) max 1))
          }
        }
      }

    // Type check target up front
    case ExprSlice(tgt, lIdx, op, rIdx) =>
      typeCheckUpFront(tgt) {
        if (!tgt.tpe.underlying.isNum) {
          val shapeIter = tgt.tpe.shapeIter
          if (!shapeIter.hasNext || tgt.tpe.isArray) {
            error(tgt, "Target is not sliceable")
          } else {
            val size = shapeIter.next()
            val lWidth = clog2(size) max 1
            val rWidth = if (op == ":") lWidth else clog2(size + 1)
            UnaryTickContext.pushType(rIdx, TypeUInt(rWidth))
            UnaryTickContext.pushType(lIdx, TypeUInt(lWidth))
          }
        }
      }

    case _ =>
  }

  // TODO: reduction of 1 bit value is error
  // TODO: Warn for non power of 2 dimensions
  private def postOrder(implicit tree: Tree): Unit = tree match {
    //////////////////////////////////////////////////////////////////////////
    // Desc
    //////////////////////////////////////////////////////////////////////////

    case desc: Desc =>
      // TODO: Add back checkNameHidingByExtensionType(decl)

      @tailrec
      def elem(kind: Type): Type = kind match {
        case TypeVector(e, _) => elem(e)
        case TypeArray(e, _)  => elem(e)
        case _                => kind
      }

      desc match {
        case DescArray(_, _, e, _) if elem(e.tpe.asType.kind).isRecord =>
          error(e, "Memory element must not have 'struct' type")
        case _ =>
      }

      desc match {
        case DescFunc(Sym(symbol), _, variant, ret, _, body) =>
          if (symbol.kind.isCtrlFunc) {
            if (body.isEmpty) {
              error(
                symbol.loc,
                "Body of control function must end in a control statement"
              )
            } else if (body.last.tpe != TypeCtrlStmt) {
              error(
                body.last,
                "Body of control function must end in a control statement"
              )
            }
          } else if (symbol.kind.isCombFunc || symbol.kind.isMethod) {
            body.iterator filter {
              _.tpe.isCtrlStmt
            } foreach { stmt =>
              error(
                stmt,
                "Body of combinational function must contain only combinational statements"
              )
            }
          } else if (!symbol.kind.isXenoFunc) {
            throw Ice(tree, "Unknown function definition")
          }
          symbol.kind.asCallable.retType match {
            case TypeVoid => // OK
            case kind if kind.isPacked && kind.width == 0 =>
              error(ret, s"return type of '${symbol.name}' has width 0")
            case _ => // Ok
          }

        case Desc(Sym(symbol)) if symbol.kind.isPacked && symbol.kind.width == 0 =>
          symbol.kind match {
            case TypeIn(TypeVoid, FlowControlTypeValid | FlowControlTypeReady)     => // Ok
            case TypeOut(TypeVoid, FlowControlTypeValid | FlowControlTypeReady, _) => // OK
            case TypeSnoop(TypeVoid)                                               => // OK
            case _ =>
              error(s"'${symbol.name}' has width 0")
          }

        case desc: DescParamType =>
          // Actual parameter must have been substituted by now
          val init = desc.initOpt.get
          if (!init.tpe.isType) {
            if (desc.loc contains init.loc) {
              error(init, "Type parameter default initializer does not name a type")
            } else {
              error(init, "Actual type parameter does not name a type")
              addNote(Note.definedHere(desc))
            }
          }

        case desc: DescParam =>
          // Actual parameter must have been substituted by now
          val init = desc.initOpt.get
          val symbol = desc.symbol
          if (symbol.kind.underlying.isNum && init.tpe.isPacked) {
            error(init, "Unsized integer parameter assigned a packed value")
          } else if (!symbol.kind.underlying.isNum && !init.tpe.underlying.isNum) {
            checkWidth(symbol.kind.width, init, "Parameter value")
          }
          if (!okSoFar && !(desc.loc contains init.loc)) {
            addNote(Note.definedHere(desc))
          }

        case desc: DescInstance =>
          if (desc.bind) {
            desc.symbol.kind.asEntity.members.foreach { s =>
              s.kind match {
                case _: TypeOut | _: TypePipeOut =>
                  error(desc, "Entity instantiated with 'bind' cannot have any outputs")
                  addNote(Note(s.desc, "Output is defined here"))
                case TypeIn(_, FlowControlTypeReady) | TypePipeIn(_, FlowControlTypeReady) =>
                  error(desc, "Entity instantiated with 'bind' cannot have a 'sync ready' input")
                  addNote(Note(s.desc, "'sync ready' input is defined here"))
                case _ => // OK
              }
            }
          }

        case desc: Desc =>
          desc.initializer foreach { init =>
            val symbol = desc.symbol
            if (symbol.kind.underlying.isNum && init.tpe.isPacked) {
              error("Unsized integer declaration has packed initializer")
            } else if (!symbol.kind.underlying.isNum && !init.tpe.underlying.isNum) {
              checkWidth(symbol.kind.width, init, "Initializer expression")
            }
          }
      }

    //////////////////////////////////////////////////////////////////////////
    // Assertion
    //////////////////////////////////////////////////////////////////////////

    case AssertionAssert(cond, _) =>
      checkNumericOrPacked(cond, "Condition of 'assert'")

    case AssertionStatic(cond, _) =>
      checkNumericOrPacked(cond, "Condition of 'static assert'")

    //////////////////////////////////////////////////////////////////////////
    // Pkg
    //////////////////////////////////////////////////////////////////////////

    case PkgCompile(expr, identOpt) =>
      assert(identOpt.isEmpty, "Should have been removed in elaboration")
      checkEntity(expr)

    //////////////////////////////////////////////////////////////////////////
    // Ent
    //////////////////////////////////////////////////////////////////////////

    case EntCombProcess(stmts) =>
      stmts filter { _.tpe == TypeCtrlStmt } foreach {
        error(_, "'fence' block must contain only combinational statements")
      }

    case conn: EntConnect =>
      val it = ConnectChecks(conn)
      if (it.hasNext) {
        mAcc.addAll(it)
        error
      }

    case EntConnectInputs(expr) =>
      if (!expr.tpe.isEntity) {
        error(expr, "Expression does not name an instance")
      }

    //////////////////////////////////////////////////////////////////////////
    // Branching statements
    //////////////////////////////////////////////////////////////////////////

    case StmtIf(cond, ts, es) =>
      checkWidth(1, cond, "Condition of 'if' statement")
      checkBlock(ts)
      checkBlock(es)
      if (okSoFar && es.nonEmpty) {
        if (ts.lastOption.exists(_.tpe.isCtrlStmt) != es.last.tpe.isCtrlStmt) {
          error("Either both or neither branches of 'if' statement must be control statements")
        }
      }

    case StmtCase(expr, cases) =>
      checkNumericOrPacked(expr, "'case' scrutinee expression")
      if (okSoFar) {
        // Check expressions are compatible
        val exprs = expr :: cases.flatMap {
          case CaseRegular(cond, _) => cond
          case _: CaseDefault       => Nil
          case _: CaseSplice        => unreachable
        }
        val sizedExprs = exprs.collect { case e if e.tpe.isPacked => e }
        if (sizedExprs.map(_.tpe.width).distinct.sizeIs > 1) {
          error("'case' statement expressions have mismatched width")
          if (expr.tpe.isPacked) {
            error(s"'case' scrutinee expression is ${bitsPlur(expr.tpe.width)} wide")
          }
          sizedExprs foreach { expr =>
            error(s"'case' item expression is ${bitsPlur(expr.tpe.width)} wide")
          }
        }
        // check branches are compatible
        val allComb = cases forall { _.stmts.lastOption.forall(_.tpe.isCombStmt) }
        val allCtrl = cases forall { _.stmts.lastOption.exists(_.tpe.isCtrlStmt) }
        if (!allComb && !allCtrl) {
          error("Either all or no cases of a 'case' statement must be control statements")
        }
      }

    case CaseRegular(cond, stmts) =>
      cond foreach { checkNumericOrPacked(_, "'case' item expression") }
      checkBlock(stmts)

    case CaseDefault(stmts) =>
      checkBlock(stmts)

    case _: CaseSplice => unreachable

    ////////////////////////////////////////////////////////////////////////////
    // Loops
    ////////////////////////////////////////////////////////////////////////////

    case StmtLoop(body) =>
      if (body.isEmpty) {
        error("Body of 'loop' must be a control statement")
      } else if (body.last.tpe != TypeCtrlStmt) {
        error("Body of 'loop' must end in a control statement")
      }

    case StmtWhile(cond, _) =>
      checkWidth(1, cond, "Condition of 'while' loop")

    case StmtFor(_, condOpt, _, _) =>
      condOpt.foreach(checkWidth(1, _, "Condition of 'for' loop"))

    case StmtDo(cond, _) =>
      checkWidth(1, cond, "Condition of 'do' loop")

    ////////////////////////////////////////////////////////////////////////////
    // Assignment like/updating statements
    ////////////////////////////////////////////////////////////////////////////

    case StmtAssign(lhs, rhs) =>
      if (rhs.tpe.underlying.isNum) {
        // Ok
      } else if (lhs.tpe.underlying.isNum && rhs.tpe.isPacked) {
        error(s"Unsized '${lhs.tpe.underlying.toSource}' variable assigned a packed value")
      } else {
        // lhs have already been checked in enter
        checkWidth(lhs.tpe.width, rhs, "Right hand side of assignment")
      }

    case StmtUpdate(lhs, op, rhs) =>
      op match {
        case ">>" | "<<" | ">>>" | "<<<" => // OK
        case _ =>
          if (rhs.tpe.underlying.isNum) {
            // Ok
          } else if (lhs.tpe.underlying.isNum && rhs.tpe.isPacked) {
            error(s"Unsized '${lhs.tpe.underlying.toSource}' variable assigned a packed value")
          } else {
            // lhs have already been checked in enter
            checkWidth(lhs.tpe.width, rhs, s"Right hand side of '$op=' assignment")
          }
      }

    case StmtPost(expr, op) =>
      if (expr.tpe.isGen && expr.tpe.underlying.isNum) {
        // Post op on unsized gen var is ok
      } else {
        checkModifiable(expr)
        checkPacked(expr, s"Target of postfix '$op'")
      }

    ////////////////////////////////////////////////////////////////////////////
    // Return
    ////////////////////////////////////////////////////////////////////////////

    case StmtReturn(_, exprOpt) =>
      EnclosingFunction.top.kind match {
        case TypeCallable(symbol, TypeVoid, _) =>
          exprOpt match {
            case Some(expr) => error(expr, s"void function '${symbol.name}' cannot return a value")
            case _          =>
          }
        case TypeCallable(symbol, retType, _) =>
          exprOpt match {
            case None => error(s"non-void function '${symbol.name}' must return a value")
            case Some(expr) =>
              if (!expr.tpe.underlying.isNum) {
                checkWidth(retType.width, expr, "Return value")
              }
          }
        case _ => error("'return' statement not inside function")
      }

    ////////////////////////////////////////////////////////////////////////////
    // Other simple statements
    ////////////////////////////////////////////////////////////////////////////

    case StmtBlock(body) =>
      checkBlock(body)

    case StmtLet(_, body) =>
      checkBlock(body)

    case StmtExpr(expr) if expr.isPure =>
      error("A pure expression in statement position does nothing")

    case StmtWait(cond) =>
      checkWidth(1, cond, "Condition of 'wait' statement")

    //////////////////////////////////////////////////////////////////////////
    // Dot
    //////////////////////////////////////////////////////////////////////////

    case ExprDot(expr, sel, idxs) =>
      assert(idxs.isEmpty) // Indices incorporated into selector by ResolveNames
      expr.tpe pipe {
        case TypeType(kind: TypeCompound) => kind(sel)
        case TypeNone(kind: TypeCompound) => kind(sel)
        case kind: TypeCompound           => kind(sel)
        case _                            => None
      } match {
        case None =>
          expr.tpe match {
            case TypeEntity(symbol, _) =>
              val msg = symbol.desc match {
                case _: DescEntity =>
                  s"No port named '$sel' on instance of entity '${symbol.name}'"
                case _: DescSingleton =>
                  s"No port named '$sel' on singleton instance '${symbol.name}'"
                case _ => unreachable
              }
              error(msg)
              addNote(Note.definedHere(symbol.desc))
            case kind =>
              error(s"No member named '$sel' in value of type '${kind.toSource}'")
          }
        case _ =>
      }

    ////////////////////////////////////////////////////////////////////////////
    // Call
    ////////////////////////////////////////////////////////////////////////////

    case ExprCall(expr, args) =>
      def check(kinds: List[Type]): Unit = {
        val as = args map {
          case ArgP(a) => a
          case _       => unreachable
        }
        for (((kind, arg), index) <- (kinds zip as).zipWithIndex) {
          val i = index + 1
          if (kind.isType) {
            if (!arg.tpe.isType) error(arg, s"Argument $i expects a type")
          } else if (kind.isNum) {
            if (!arg.tpe.underlying.isNum)
              error(arg, s"Argument $i expects an unsized value")
          } else if (!arg.tpe.underlying.isNum) {
            checkWidth(kind.width, arg, s"Argument $i of function call")
          }
        }
      }

      expr match {
        case ExprType(TypeNum(signed)) =>
          lazy val hint = if (signed) "Width of 'int'" else "Width of 'uint'"
          args match {
            case List(ArgP(width)) =>
              evaluate(width, hint) foreach {
                // Width is positive, assign the sized integer type
                case v if v > 0 => tree withTpe TypeType(TypeInt(signed, v.asLong))
                // Width is non-positive
                case v => error(width, s"$hint must be positive (not $v)")
              }
            case _ =>
              error(s"Bad parameter to '$hint', a single positional argument is expected.")
          }
        case _ =>
          expr.tpe match {
            case TypeCombFunc(_, _, argTypes)     => check(argTypes)
            case TypeCtrlFunc(_, _, argTypes)     => check(argTypes)
            case TypeXenoFunc(_, _, argTypes)     => check(argTypes)
            case TypeStaticMethod(_, _, argTypes) => check(argTypes)
            case TypeNormalMethod(_, _, argTypes) => check(argTypes)
            case _                                => unreachable // Handled in enter
          }
      }

    ////////////////////////////////////////////////////////////////////////////
    // Builtin
    ////////////////////////////////////////////////////////////////////////////

    case expr: ExprBuiltin =>
      expr.builtin.typeCheck(expr) match {
        case Complete(kind) => expr withTpe kind
        case Unknown(rs)    => rAcc addAll rs; None
        case Failure(ms)    => mAcc addAll ms; error; None
      }

    ////////////////////////////////////////////////////////////////////////////
    // Cat/Rep
    ////////////////////////////////////////////////////////////////////////////

    case ExprCat(parts) =>
      for ((part, i) <- parts.zipWithIndex) {
        checkPacked(part, s"Part ${i + 1} of bit concatenation")
      }

    case ExprRep(count, expr) =>
      checkPacked(expr, "Replicated value")
      val cHint = "Replication count"
      checkNumeric(count, cHint)
      if (okSoFar) {
        evaluate(count, cHint) filter { value =>
          { checkPositive(value, cHint, expr); okSoFar }
        } foreach { v =>
          tree withTpe TypeUInt(v.asLong * expr.tpe.width)
        }
      }

    ////////////////////////////////////////////////////////////////////////////
    // Index
    ////////////////////////////////////////////////////////////////////////////

    case ExprIndex(tgt, idx) =>
      if (tgt.tpe.isType) {
        val kindOpt = tgt.tpe.asType.kind match {
          case _: TypeRecord =>
            error(tgt, "Vector element must not have 'struct' type"); None
          case TypeVoid =>
            error(tgt, "Vector element must not have 'void' type"); None
          case kind if !kind.isPacked =>
            error(tgt, "Vector element must have a packed type"); None
          case kind => Some(kind)
        }
        val hint = "Size of vector"
        evaluate(idx, hint) filter { value =>
          { checkPositive(value, hint, idx); okSoFar }
        } foreach { v =>
          kindOpt foreach { kind =>
            tree withTpe TypeType(kind addVectorDimension v.asLong)
          }
        }
      } else if (tgt.tpe.underlying.isNum) {
        evaluate(idx, "Index of unsized integer value")
      } else if (tgt.tpe.isArray || { checkPacked(tgt, "Target of index"); okSoFar }) {
        // TODO: Is the conditional redundant due to the check already in
        //       enter? Should this be more like ExprSlice???
        checkIndex(Some(UnaryTickContext.expected.width), idx, "Index")
      }

    ////////////////////////////////////////////////////////////////////////////
    // Slice
    ////////////////////////////////////////////////////////////////////////////

    case ExprSlice(tgt, lIdx, op, rIdx) =>
      val tgtIsNum = tgt.tpe.underlying.isNum
      if (!tgtIsNum && { checkPacked(tgt, s"Target of '$op' slice"); !okSoFar }) {
        //
      } else {
        def mkType(width: Long): Type =
          tgt.tpe.underlying match {
            case _: TypeNum                              => TypeUInt(width)
            case TypeVector(kind, _)                     => TypeVector(kind, width)
            case kind if kind.isPacked && kind.width > 0 => TypeUInt(width)
            case _                                       => unreachable
          }
        val lHint = s"Left index of '$op' slice"
        val rHint = s"Right index of '$op' slice"
        val dimSize = tgt.tpe.shapeIter.nextOption() getOrElse 0L // Num has no shape
        if (op == ":") {
          val iWidth = clog2(dimSize) max 1
          checkIndex(Option.unless(tgtIsNum)(iWidth), lIdx, lHint)
          checkIndex(Option.unless(tgtIsNum)(iWidth), rIdx, rHint)
          if (okSoFar) {
            val lIdxValOpt = evaluate(lIdx, lHint) filter { value =>
              tgtIsNum || { checkInRange(value, 0, true, dimSize, false, lHint, lIdx); okSoFar }
            }
            val rIdxValOpt = evaluate(rIdx, rHint) filter { value =>
              tgtIsNum || { checkInRange(value, 0, true, dimSize, false, rHint, rIdx); okSoFar }
            }
            lIdxValOpt foreach { lIdxVal =>
              rIdxValOpt foreach { rIdxVal =>
                if (lIdxVal < rIdxVal) {
                  error("Left index of ':' slice must be >= than the right index")
                } else {
                  tree withTpe mkType(width = (lIdxVal - rIdxVal + 1).asLong)
                }
              }
            }
          }
        } else {
          val lWidth = clog2(dimSize) max 1
          val rWidth = clog2(dimSize + 1)
          checkIndex(Option.unless(tgtIsNum)(lWidth), lIdx, lHint)
          checkIndex(Option.unless(tgtIsNum)(rWidth), rIdx, rHint)
          if (okSoFar) {
            evaluate(rIdx, rHint) filter { value =>
              tgtIsNum || { checkInRange(value, 0, false, dimSize, true, rHint, rIdx); okSoFar }
            } foreach { value =>
              tree withTpe mkType(width = value.asLong)
            }
          }
        }
      }

    ////////////////////////////////////////////////////////////////////////////
    // Unary ops
    ////////////////////////////////////////////////////////////////////////////

    // Unary ' is special and the type of the node must be assigned here
    // based on context as it cannot be determined from the operand only.
    case ExprUnary("'", op) =>
      if (hadError) {
        // If we had a type error, the context stack might be out of sync
        // so don't type check any further unary tick nodes
        error
      } else if (!UnaryTickContext.hasExpected) {
        error("Unary ' operator used in invalid context")
      } else if ({ checkPacked(op, "Operand of unary ' operator"); okSoFar }) {
        val expected = UnaryTickContext.expected
        if (expected.isNum) {
          tree withTpe TypeNum(op.tpe.isSigned)
        } else {
          val resWidth = expected.width
          val opWidth = op.tpe.width
          if (resWidth < opWidth) {
            error(s"Unary ' causes narrowing of width from $opWidth to $resWidth")
          } else {
            tree withTpe TypeInt(op.tpe.isSigned, resWidth)
          }
        }
      }

    case ExprUnary("!", expr) =>
      checkWidth(1, expr, s"Operand of unary '!' operator")

    case ExprUnary(op, expr) =>
      if (expr.tpe.underlying.isNum) {
        // Operand must be constant, as it's unsized, so evaluate it here
        evaluate(expr, "Expression of unsized integer type") foreach { v =>
          def err(rest: String): Unit = error(s"Unary operator '$op' $rest")
          op match {
            case "-" if !expr.tpe.isSigned && v > 0 =>
              err(s"has 'uint' result type, but evaluates to a negative value")
            case "~" if !expr.tpe.isSigned =>
              err(s"has 'uint' result type, but evaluates to a negative value")
            case "^" if v < 0 =>
              err(s"is not well defined on a negative unsized value (${v}s)")
            case _ => // Rest are OK
          }
        }
      } else {
        checkPacked(expr, s"Operand of unary '$op' operator")
      }

    ////////////////////////////////////////////////////////////////////////////
    // Binary ops
    ////////////////////////////////////////////////////////////////////////////

    case ExprBinary(lhs, "'", rhs) =>
      val rHint = "Right hand side operand of binary ' operator"
      val lHint = "Left hand side operand of binary ' operator"
      if (!rhs.tpe.underlying.isNum) {
        checkPacked(rhs, rHint) // Evaluate eagerly up front
      }
      val rhsOk = okSoFar
      val s = rhs.tpe.isSigned
      evaluate(lhs, lHint)
        .filter(value => { checkPositive(value, lHint, lhs); okSoFar })
        .filter(_ => rhsOk)
        .foreach { lv =>
          if (rhs.tpe.underlying.isNum) {
            evaluate(rhs, "Expression of unsized integer type")
              .foreach { rv =>
                val lo = if (s) BigInt.iMin(lv.toInt) else BigInt.uMin(lv.toInt)
                val hi = if (s) BigInt.iMax(lv.toInt) else BigInt.uMax(lv.toInt)
                val ss = if (s) "signed" else "unsigned"
                if (rv < lo) {
                  error(
                    s"Right hand operand of binary ' cannot be represented as a $lv bit $ss value ($rv < $lo)"
                  )
                } else if (rv > hi) {
                  error(
                    s"Right hand operand of binary ' cannot be represented as a $lv bit $ss value ($rv > $hi)"
                  )
                } else {
                  tree withTpe TypeInt(s, lv.asLong)
                }
              }
          } else if (lv < rhs.tpe.width) {
            error("Binary ' operator causes narrowing")
          } else {
            tree withTpe TypeInt(s, lv.asLong)
          }
        }

    case ExprBinary(lhs, op @ ("<<" | ">>" | "<<<" | ">>>"), rhs) =>
      checkNumericOrPacked(lhs, s"Left hand operand of '$op'")
      checkNumericOrPacked(rhs, s"Right hand operand of '$op'")

      if ((op == "<<<" || op == ">>>") && !lhs.tpe.isSigned) {
        cc.warning(lhs, "Arithmetic shift used on unsigned left operand")
      } else if ((op == "<<" || op == ">>") && lhs.tpe.isSigned) {
        cc.warning(lhs, "Logical shift used on signed left operand")
      }

    case ExprBinary(lhs, op @ ("&&" | "||"), rhs) =>
      checkWidth(1, lhs, s"Left hand operand of '$op'")
      checkWidth(1, rhs, s"Right hand operand of '$op'")

    case ExprBinary(lhs, op, rhs) => // Arithmetic and comparison operators
      if (!lhs.tpe.underlying.isNum || !rhs.tpe.underlying.isNum) { // OK if both are unsized
        if (lhs.tpe.underlying.isNum) {
          // Left operand is unsized, the right must be packed
          checkPacked(rhs, s"Right hand operand of '$op'")
        } else if (rhs.tpe.underlying.isNum) {
          // Right operand is unsized, the left must be packed
          checkPacked(lhs, s"Left hand operand of '$op'")
        } else {
          // Neither operands are unsized, they both must be packed,
          // and have the same width
          checkPacked(lhs, s"Left hand operand of '$op'")
          checkPacked(rhs, s"Right hand operand of '$op'")
          if (okSoFar && lhs.tpe.width != rhs.tpe.width) {
            error(
              s"Both operands of binary '$op' must have the same width, but",
              s"left  hand operand is ${lhs.tpe.width} bits wide, and",
              s"right hand operand is ${rhs.tpe.width} bits wide"
            )
          }
        }

        if (okSoFar) {
          if (comparisonBinaryOps contains op) {
            if (lhs.tpe.isSigned && !rhs.tpe.isSigned) {
              cc.warning(
                tree,
                "Comparison between signed and unsigned operands is interpreted as a comparison between two unsigned values"
              )
            } else if (!lhs.tpe.isSigned && rhs.tpe.isSigned) {
              cc.warning(
                tree,
                "Comparison between signed and unsigned operands is interpreted as a comparison between two unsigned values"
              )
            }
          }
        }
      }

    ////////////////////////////////////////////////////////////////////////////
    // Ternary
    ////////////////////////////////////////////////////////////////////////////

    case ExprCond(cond, thenExpr, elseExpr) =>
      checkWidth(1, cond, "Condition of ternary '?:'")
      def thenHint = "'then' operand of ternary '?:'"
      def elseHint = "'else' operand of ternary '?:'"
      val thenIsNum = thenExpr.tpe.underlying.isNum
      val elseIsNum = elseExpr.tpe.underlying.isNum
      if (thenIsNum && elseIsNum) {
        // Both branches are Num, which is OK
      } else if (thenIsNum) {
        checkPacked(elseExpr, elseHint)
      } else if (elseIsNum) {
        checkPacked(thenExpr, thenHint)
      } else {
        checkPacked(thenExpr, thenHint)
        checkPacked(elseExpr, elseHint)
        if (okSoFar && thenExpr.tpe.width != elseExpr.tpe.width) {
          error(
            s"'then' and 'else' operands of ternary '?:' must have the same width, but",
            s"'then' operand is ${thenExpr.tpe.width} bits wide, and",
            s"'else' operand is ${elseExpr.tpe.width} bits wide"
          )
        }
      }

    //////////////////////////////////////////////////////////////////////////
    // Done
    //////////////////////////////////////////////////////////////////////////

    case _ => ()
  }

  override def finalCheck(tree: Tree): Unit = if (tree.hasTpe && !tree.tpe.isError) {
    UnaryTickContext.finalChecks(tree)

    // $COVERAGE-OFF$ Debug code
    tree visit {
      case _: DescParametrized                        => // Ok
      case ExprCall(tgt, _) if tgt.tpe.isParametrized => // Ok
      case n: Tree if !n.hasTpe                       => throw Ice(n, "Typer: untyped node remains", n.toString)
    }
    // $COVERAGE-ON$
  }

  // Run the actual traversal
  apply(root).ensuring(_ eq root, "Type checker must not modify the input tree")
}

private[frontend] object TypeChecker {

  def apply(tree: Tree)(implicit cc: CompilerContext, fe: Frontend): FinalResult[Type] =
    blocking {
      synchronized {
        if (tree.hasTpe) {
          // Tree already has it's type assigned, which means it has already been type checked
          if (tree.tpe.isError) {
            Unknown(ReasonEarlierTypeError(tree))
          } else {
            Complete(tree.tpe)
          }
        } else {
          // Run the checker
          val checker = new TypeChecker(tree)
          // If there are errors, the tree should have TypeError and vice versa
          assert(checker.mAcc.nonEmpty == (tree.hasTpe && tree.tpe.isError))
          if (checker.mAcc.nonEmpty) {
            Failure(checker.mAcc.toList)
          } else if (checker.rAcc.nonEmpty) {
            assert(!tree.hasTpe)
            Unknown(checker.rAcc.toList)
          } else {
            Complete(tree.tpe)
          }
        }
      }
    }

}
