package com.argondesign.alogic.frontend

import com.argondesign.alogic.analysis.WrittenSyms
import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._

import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Messages.Message
import com.argondesign.alogic.core.Messages.Note
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.ExtensionType
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.TypeCompound
import com.argondesign.alogic.lib.Math.clog2
import com.argondesign.alogic.util.unreachable

import scala.collection.immutable.Set
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

private[frontend] object TypeChecker {

  implicit class BoolHelpers(val value: Boolean) extends AnyVal {
    // Non short-circuiting &&
    def &&&(other: Boolean): Boolean = value && other
    // Non short-circuiting |||
    def |||(other: Boolean): Boolean = value || other

    // Run f if value is false, yield the value
    def ifFalse(f: => Unit): Boolean = { if (!value) f; value }

    // Implication
    def implies(other: => Boolean): Boolean = !value || other
  }

  final private val mixedWidthBinaryOps = Set("'", "<<", ">>", "<<<", ">>>", "&&", "||")
  final private val comparisonBinaryOps = Set(">", ">=", "<", "<=", "==", "!=")

  def apply(
      tree: Tree
    )(
      implicit
      cc: CompilerContext,
      fe: Frontend
    ): FinalResult[Type] = {
    if (tree.hasTpe) {
      if (tree.tpe.isError) Failure(Nil) else Complete(tree.tpe)
    } else {
      val mAcc = new ListBuffer[Message]
      val rAcc = new ListBuffer[Reason]

      var hadError = false

      object Checker extends StatefulTreeTransformer {
        override val typed: Boolean = false

        private val contextKind = mutable.Stack[Option[Type]]()

        private val contextNode = mutable.Stack[Int]()

        private def pushContextWidth(node: Tree, kind: Type): Unit =
          if (!node.hasTpe) {
            // Note: Anything that has a type set will be skipped, so only push
            // nodes with will actually be visited
            contextNode push node.id
            contextKind push Some(kind.underlying)
          }

        private def pushContextNoWidth(node: Tree): Unit =
          if (!node.hasTpe) {
            // Note: Anything that has a type set will be skipped, so only push
            // nodes with will actually be visited
            contextNode push node.id
            contextKind push None
          }

        private def popContextWidth(node: Tree): Unit =
          // Pop context stack if required. This is a loop as some nodes might
          // have been pushed multiple times, e.g.: port.write(array[index]),
          // both the ExprCall and ExprIndex would have pushed the ExprIndex
          // node
          while (contextNode.headOption contains node.id) {
            contextNode.pop()
            contextKind.pop()
          }

        // Utility that will issue the compiler error 'msg' at the 'loc' provided,
        // assign TypeError to the 'tree' node and set the 'hadError' flag
        def error(tree: Tree, loc: Loc, msg: String*): Unit = {
          if (msg.nonEmpty) {
            mAcc.addOne(Error(loc, msg: _*))
          }
          if (tree.hasTpe) {
            assert(tree.tpe.isError, tree.tpe)
          } else {
            tree withTpe TypeError
          }
          hadError = true
        }

        // Same as above but location provided from the 'mark' node.
        def error(tree: Tree, mark: Tree, msg: String*): Unit = error(tree, mark.loc, msg: _*)

        // Wrapper for above when 'tree' and 'mark' are the same
        def error(tree: Tree, msg: String*): Unit = error(tree, tree.loc, msg: _*)

        def evaluate(expr: Expr, hint: String)(implicit tree: Tree): Option[BigInt] =
          fe.evaluate(expr, hint) match {
            case Complete(v) => Some(v)
            case Unknown(rs) => rAcc addAll rs; None
            case Failure(ms) => mAcc addAll ms; error(tree); None
          }

        // The following check* methods all have the following uniform behaviour:
        // - Perform a method specific check
        // - If the check failed:
        //   - Issue an error message
        //   - Mark the implicitly provided tree node as having TypeError
        // - Return check result

        def checkPacked(expr: Expr, msg: String)(implicit tree: Tree): Boolean =
          expr.tpe.isPacked ifFalse {
            error(tree, expr, s"$msg is of non-packed type")
          }

        def checkNumeric(expr: Expr, msg: String)(implicit tree: Tree): Boolean =
          expr.tpe.underlying.isNumeric ifFalse {
            error(tree, expr, s"$msg is of non-numeric type")
          }

        def checkNumericOrPacked(expr: Expr, msg: String)(implicit tree: Tree): Boolean =
          (expr.tpe.underlying.isNumeric || expr.tpe.isPacked) ifFalse {
            error(tree, expr, s"$msg is of neither numeric nor packed type")
          }

        def pluralize(value: BigInt, singular: String, plural: String): String = {
          if (value == 1) s"$value $singular" else s"$value $plural"
        }

        def checkWidth(
            width: BigInt,
            expr: Expr,
            msg: String
          )(
            implicit
            tree: Tree
          ): Boolean = {
          val expected = s"${pluralize(width, "bit is", "bits are")} expected"
          if (expr.tpe.isNum) {
            error(tree, s"$msg yields an unsized value, $expected")
            false
          } else if (expr.tpe.width != width) {
            error(tree, expr, s"$msg yields ${pluralize(expr.tpe.width, "bit", "bits")}, $expected")
            false
          } else {
            true
          }
        }

        def checkSign(
            sign: Boolean,
            expr: Expr,
            msg: String
          )(
            implicit
            tree: Tree
          ): Boolean = {
          (expr.tpe.isSigned == sign) ifFalse {
            error(tree, expr, s"$msg must be ${if (sign) "signed" else "unsigned"}")
          }
        }

        def checkBlock(stmts: List[Stmt])(implicit tree: Tree): Boolean = {
          def hasCtrl(trees: List[Tree]): Boolean =
            trees
              .exists {
                case StmtSplice(DescGenScope(_, _, body)) => hasCtrl(body)
                case stmt: Stmt                           => stmt.tpe.isCtrlStmt
                case _                                    => unreachable
              }
          def lastStmt(trees: List[Tree]): Option[Stmt] =
            trees
              .filter {
                case StmtSplice(DescGenScope(_, _, Nil)) => false
                case _                                   => true
              }
              .lastOption
              .flatMap {
                case StmtSplice(DescGenScope(_, _, body)) => lastStmt(body)
                case stmt: Stmt                           => Some(stmt)
                case _                                    => unreachable
              }
          val lstCtrl = lastStmt(stmts).fold(false)(_.tpe == TypeCtrlStmt)
          (!hasCtrl(stmts) || lstCtrl) ifFalse {
            error(
              tree,
              stmts.last,
              "Block must contain only combinational statements, or end with a control statement"
            )
          }
        }

        def checkIndex(
            expectedWidthOpt: Option[BigInt],
            idx: Expr,
            hint: String
          )(
            implicit
            tree: Tree
          ): Boolean =
          idx.tpe.underlying.isNum || {
            checkPacked(idx, hint) && {
              (expectedWidthOpt forall { checkWidth(_, idx, hint) }) &&&
                checkSign(false, idx, hint)
            }
          }

        def checkInRange(
            value: BigInt,
            lo: BigInt,
            loIsInclusive: Boolean,
            hi: BigInt,
            hiIsInclusive: Boolean,
            hint: String,
            mark: Tree
          )(
            implicit
            tree: Tree
          ): Boolean =
          if (loIsInclusive && value < lo) {
            error(tree, mark, s"$hint is out of range ($value is < $lo)"); false
          } else if (!loIsInclusive && value <= lo) {
            error(tree, mark, s"$hint is out of range ($value is <= $lo)"); false
          } else if (hiIsInclusive && value > hi) {
            error(tree, mark, s"$hint is out of range ($value is > $hi)"); false
          } else if (!hiIsInclusive && value >= hi) {
            error(tree, mark, s"$hint is out of range ($value is >= $hi)"); false
          } else {
            true
          }

        def checkPositive(
            value: BigInt,
            hint: String,
            mark: Tree
          )(
            implicit
            tree: Tree
          ): Boolean =
          if (value <= 0) {
            error(tree, mark, s"$hint must be positive ($value is <= 0)"); false
          } else {
            true
          }

        def checkModifiable(expr: Expr)(implicit tree: Tree): Boolean = {
          WrittenSyms(expr) map {
            case ref @ ExprSym(symbol) =>
              symbol.kind match {
                case _: TypeParam => unreachable // Removed by elaboration
                case _: TypeConst => error(tree, ref, "Constant cannot be modified"); false
                case _: TypeIn    => error(tree, ref, "Input port cannot be modified"); false
                case _: TypeArray =>
                  error(tree, ref, "Memory can only be modified using .write()"); false
                case TypeOut(_, FlowControlTypeNone, _) =>
                  true
                case _: TypeOut =>
                  error(
                    tree,
                    ref,
                    "Output port with flow control can only be modified using .write()"
                  )
                  false
                case _ if symbol.desc.isInstanceOf[DescVal] =>
                  error(tree, ref, "'const' qualified variable cannot be modified")
                  false
                case _ => true
              }
          } forall identity
        }

        def checkEntity(expr: Expr)(implicit tree: Tree): Boolean = {
          expr.tpe.isType && expr.tpe.asType.kind.isEntity ifFalse {
            error(tree, expr, "Expression does not name an entity")
          }
        }

        // Other check* methods

        def checkNameHidingByExtensionType(decl: Decl): Unit = {
          decl.symbol.kind match {
            case eKind: ExtensionType =>
              eKind.kind match {
                case cKind: TypeCompound =>
                  for {
                    cSymbol <- cKind.publicSymbols
                    eSymbol <- eKind(cSymbol.name)
                    if cSymbol ne eSymbol
                  } {
                    cc.error(
                      decl.loc,
                      s"Field '${cSymbol.name}' of type '${cSymbol.kind.toSource}' of symbol '${decl.symbol.name}'",
                      s"defined in type '${cKind.toSource}'",
                      s"is hidden by extension field of the same name of type '${eSymbol.kind.toSource}'",
                      s"defined by extension type '${eKind.toSource}'"
                    )
                  }
                case _ => ()
              }
            case _ => ()
          }
        }

        def checkParametrizedSubExpression(tree: Tree): Boolean = {
          def check(trees: IterableOnce[Tree]): Boolean = trees.iterator.foldLeft(true) {
            case (ok, expr: Expr) =>
              if (expr.tpe.isParametrized) {
                mAcc.addOne(Error(expr, "Parametrized type requires parameter list"))
                false
              } else {
                ok
              }
            case (ok, _) => ok
          }
          tree match {
            case _: ExprCall         => true // OK (args checked when walking Arg*)
            case _: DescAlias        => true // Ok
            case _: DescParametrized => true // Ok
            case _                   => check(tree.children)
          }
        }

        // Called in pre-order (from 'enter') to check sub-trees ahead of the
        // traversal
        private def typeCheckUpFront(
            subject: Tree,
            tree: Tree
          )(
            result: => Option[Tree]
          ): Option[Tree] = {
          // Type check the subject tree
          walk(subject)
          // !subject.hasTpe implies rAcc.nonEmpty, i.e.: if the type is not
          // known, then we must have some undesolved dependency
          assert(subject.hasTpe || rAcc.nonEmpty);
          // If subject is well typed, compute result, otherwise default,
          // but mark as error if type error.
          if (subject.hasTpe) {
            if (!subject.tpe.isError) {
              result
            } else {
              error(tree)
              Some(tree)
            }
          } else {
            Some(tree)
          }
        }

        // Same as above but with no result/fallback tree
        private def typeCheckUpFront(
            subject: Tree
          )(
            block: => Unit
          ): Unit = {
          walk(subject)
          assert(subject.hasTpe || rAcc.nonEmpty);
          if (subject.hasTpe && !subject.tpe.isError) {
            block
          }
        }

        // Skip already typed sub-trees, bail if we have an unknown
        override def skip(tree: Tree): Boolean = tree.hasTpe || rAcc.nonEmpty

        override def enter(tree: Tree): Option[Tree] = tree pipe {
          case ExprIdent(ident) =>
            rAcc addOne ReasonUnresolved(ident)
            Some(tree)
          case Desc(_: Ident) | _: DescGenIf | _: DescGenFor | _: DescGenRange | _: Using |
              _: Import =>
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
              case Success(kind) =>
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
                error(tree)
            }
            Some(tree)
          case ExprCall(tgt, args) =>
            typeCheckUpFront(tgt, tree) {
              def check(kinds: List[Type]): Unit = {
                if (kinds.length != args.length) {
                  error(
                    tree,
                    s"Function call expects ${kinds.length} arguments, ${args.length} given"
                  )
                } else {
                  for ((kind, arg) <- (kinds zip args).reverse) {
                    pushContextWidth(arg, kind)
                  }
                }
              }

              tgt match {
                case ExprType(_: TypeNum) => None // Will be resolved in postOrder
                case _ =>
                  tgt.tpe match {
                    case TypeCombFunc(_, _, argTypes)     => check(argTypes); None
                    case TypeCtrlFunc(_, _, argTypes)     => check(argTypes); None
                    case TypeXenoFunc(_, _, argTypes)     => check(argTypes); None
                    case TypeStaticMethod(_, _, argTypes) => check(argTypes); None
                    case TypeNormalMethod(_, _, argTypes) => check(argTypes); None
                    case _: TypePolyFunc                  => None // Will be resolved in postOrder
                    case TypeParametrized(symbol)         =>
                      // Specialize here without type checking the arguments as
                      // the arguments (parameter assignments) are not typeable
                      // without knowing the definitions (which are only known
                      // during specialization) due to possible unary ticks.
                      Some {
                        fe.specialize(symbol, args, tree.loc) match {
                          case Complete(specialized) =>
                            tree withTpe {
                              fe.typeOf(specialized, tree.loc) match {
                                case Success(value) => value
                                case _              => unreachable
                              }
                            }
                          case Unknown(rs) =>
                            rAcc addAll rs
                            tree
                          case Failure(ms) =>
                            mAcc addAll ms
                            error(tree)
                            tree
                        }
                      }
                    case TypeNone(_: TypeNormalMethod) =>
                      error(tree, tgt, "Attempting to call non-static method via type")
                      Some(tree)
                    case _: TypeType =>
                      error(tree, tgt, s"Type does not take any parameters")
                      Some(tree)
                    case _ =>
                      error(tree, tgt, s"Expression is not callable")
                      Some(tree)
                  }
              }
            }
          case _ =>
            // Apply pre-order operations
            preOrder(tree)
            // Stop if error
            Option.when(tree.hasTpe || rAcc.nonEmpty) {
              assert(!tree.hasTpe || tree.tpe.isError)
              tree
            }
        } tap {
          case Some(_) => popContextWidth(tree)
          case None    =>
        }

        override def transform(tree: Tree): Tree = {
          val alreadyHadError = hadError

          // Bail quickly if we already had an unknown
          if (rAcc.isEmpty) {
            if (tree.children exists { _.tpe.isError }) {
              // If any of the children have a type error, propagate it upwards
              error(tree)
            } else if (!checkParametrizedSubExpression(tree)) {
              // Parametrized type in invalid context
              error(tree)
            } else {
              // Otherwise run the post order checks
              postOrder(tree)
            }
          }

          // Bail quickly if we already had an unknown or just discovered one
          if (rAcc.isEmpty) {
            // Check that if we have just found an error, then we have also
            // marked the tree with TypeError
            assert((!alreadyHadError && hadError) implies tree.tpe.isError)

            popContextWidth(tree)

            // Assign type if it has not been assigned by now
            if (!tree.hasTpe) {
              TypeAssigner(tree)
            }

            // TODO: Add back somewhere?
//            tree match {
//              case expr: Expr if expr.tpe.underlying.isNum && !expr.isKnownConst =>
//                error(expr, "Expression of unsized integer type must be compile time constant")
//              case _ =>
//            }

            hadError |= tree.tpe.isError
          }

          // Never re-write the tree during type checking
          tree
        }

        def preOrder(implicit tree: Tree): Unit = tree match {
          case desc: Desc =>
            // Compute type of symbol up front as we might need it for the
            // context width below. This in effect type checks the necessary
            // parts of the definition out of order
            fe.typeOf(desc.symbol, desc.symbol.loc) match {
              case Complete(kind) => // Ok
                // Allow unary ticks in initializers
                desc.initializer foreach { init =>
                  pushContextWidth(init, kind)
                }
              case Unknown(rs) =>
                rAcc addAll rs
              case Failure(ms) =>
                mAcc addAll ms
                error(tree)
            }

          // Type check the lhs up front
          case StmtAssign(lhs, _) =>
            typeCheckUpFront(lhs) {
              if (!(lhs.tpe.isGen && lhs.tpe.underlying.isNum)) {
                checkPacked(lhs, "Left hand side of assignment") && checkModifiable(lhs)
              }
              pushContextWidth(tree, lhs.tpe)
            }

          // Type check the lhs up front
          case StmtUpdate(lhs, _, _) =>
            typeCheckUpFront(lhs) {
              if (!(lhs.tpe.isGen && lhs.tpe.underlying.isNum)) {
                checkPacked(lhs, "Left hand side of assignment") && checkModifiable(lhs)
              }
              // TODO: this is not quite right for shift and compare etc
              pushContextWidth(tree, lhs.tpe)
            }

          case _: StmtReturn =>
            enclosingSymbols.headOption foreach {
              _.kind match {
                case TypeCallable(_, retType, _) if retType != TypeVoid =>
                  pushContextWidth(tree, retType)
                case _ =>
              }
            }

          // Type check target up front
          case ExprIndex(tgt, _) =>
            typeCheckUpFront(tgt) {
              if (tgt.tpe.isType) {
                pushContextNoWidth(tree)
              } else if (!tgt.tpe.underlying.isNum) {
                val shapeIter = tgt.tpe.shapeIter
                if (!shapeIter.hasNext && !tgt.tpe.underlying.isNum) {
                  error(tree, tgt, "Target is not indexable")
                } else {
                  pushContextWidth(tree, TypeUInt(clog2(shapeIter.next) max 1))
                }
              }
            }

          // Type check target up front
          case ExprSlice(tgt, lIdx, op, rIdx) =>
            typeCheckUpFront(tgt) {
              if (!tgt.tpe.underlying.isNum) {
                val shapeIter = tgt.tpe.shapeIter
                if (!shapeIter.hasNext || tgt.tpe.isArray) {
                  error(tree, tgt, "Target is not sliceable")
                } else {
                  val size = shapeIter.next
                  val lWidth = clog2(size) max 1
                  val rWidth = if (op == ":") lWidth else clog2(size + 1)
                  pushContextWidth(rIdx, TypeUInt(rWidth))
                  pushContextWidth(lIdx, TypeUInt(lWidth))
                }
              }
            }

          case _ =>
        }

        // TODO: reduction of 1 bit value is error
        // TODO: Warn for non power of 2 dimensions
        def postOrder(implicit tree: Tree): Unit = tree match {
          //////////////////////////////////////////////////////////////////////////
          // Desc
          //////////////////////////////////////////////////////////////////////////

          case desc: Desc =>
            // if (ok) {
            // TODO: Add these back
            //          checkNameHidingByExtensionType(decl)

            //          @tailrec
            //          def elem(kind: Type): Type = kind match {
            //            case TypeVector(e, _) => elem(e)
            //            case TypeArray(e, _)  => elem(e)
            //            case _                => kind
            //          }
            //
            //          decl match {
            //            case kind: TypeVector if elem(kind).underlying.isRecord =>
            //              cc.error(tree, "Vector element cannot have a struct type")
            //            case kind: TypeArray if elem(kind).underlying.isRecord =>
            //              cc.error(tree, "Memory element cannot have a struct type")
            //            case _ => ()
            //          }
            //}

            desc match {
              case DescFunc(Sym(symbol), _, _, _, _, body) =>
                if (symbol.kind.isCtrlFunc) {
                  if (body.isEmpty) {
                    error(
                      tree,
                      symbol.loc,
                      "Body of control function must end in a control statement"
                    )
                  } else if (body.last.tpe != TypeCtrlStmt) {
                    error(
                      tree,
                      body.last,
                      "Body of control function must end in a control statement"
                    )
                  }
                } else if (symbol.kind.isCombFunc || symbol.kind.isMethod) {
                  body.iterator filter {
                    _.tpe.isCtrlStmt
                  } foreach { stmt =>
                    error(
                      tree,
                      stmt,
                      "Body of combinational function must contain only combinational statements"
                    )
                  }
                } else if (!symbol.kind.isXenoFunc) {
                  throw Ice(tree, "Unknown function definition")
                }

              case desc: DescParamType =>
                // Actual parameter must have been substituted by now
                val init = desc.initOpt.get
                if (!init.tpe.isType) {
                  if (desc.loc contains init.loc) {
                    error(tree, init, "Type parameter default initializer does not name a type")
                  } else {
                    error(tree, init, "Actual type parameter does not name a type")
                    mAcc.addOne(Note.definedHere(desc))
                  }
                }

              case desc: DescParam =>
                // Actual parameter must have been substituted by now
                val init = desc.initOpt.get
                val symbol = desc.symbol
                if (symbol.kind.underlying.isNum && init.tpe.isPacked) {
                  error(tree, init, "Unsized integer parameter assigned a packed value")
                } else if (!symbol.kind.underlying.isNum && !init.tpe.underlying.isNum) {
                  val msg = "Parameter value"
                  if (!(checkPacked(init, msg) && checkWidth(symbol.kind.width, init, msg))) {
                    error(tree)
                  }
                }
                if (desc.hasTpe) {
                  assert(desc.tpe.isError)
                  if (!(desc.loc contains init.loc)) {
                    mAcc.addOne(Note.definedHere(desc))
                  }
                }

              case desc: Desc =>
                desc.initializer foreach { init =>
                  val symbol = desc.symbol
                  if (symbol.kind.underlying.isNum && init.tpe.isPacked) {
                    error(tree, "Unsized integer declaration has packed initializer")
                  } else if (!symbol.kind.underlying.isNum && !init.tpe.underlying.isNum) {
                    val msg = "Initializer expression"
                    if (!(checkPacked(init, msg) && checkWidth(symbol.kind.width, init, msg))) {
                      error(tree)
                    }
                  }
                }

              case _ => // ok
            }

          //////////////////////////////////////////////////////////////////////////
          // Assertion
          //////////////////////////////////////////////////////////////////////////

          case AssertionAssert(cond, _) => checkNumericOrPacked(cond, "Condition of 'assert'")

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

          // TODO: Skip scopes in stmt lists when checking comb/ctrl compatibility

          case EntCombProcess(stmts) =>
            stmts filter { _.tpe == TypeCtrlStmt } foreach {
              error(tree, _, "'fence' block must contain only combinational statements")
            }

          case conn: EntConnect => if (!ConnectChecks(conn)) error(tree)

          //////////////////////////////////////////////////////////////////////////
          // Stmt
          //////////////////////////////////////////////////////////////////////////

          case StmtBlock(body) if !checkBlock(body) => error(tree)

          case StmtIf(cond, ts, es) =>
            if (!checkNumericOrPacked(cond, "Condition of 'if' statement")) {
              error(tree)
            } else if (!checkWidth(1, cond, "Condition of 'if' statement")) {
              error(tree)
            }
            if (!(checkBlock(ts) &&& checkBlock(es))) {
              error(tree)
            } else if (es.nonEmpty) {
              if ((ts.isEmpty || ts.last.tpe.isCombStmt) != es.last.tpe.isCombStmt) {
                error(
                  tree,
                  "Either both or neither branches of 'if' statement must be control statements"
                )
              }
            }

          case StmtCase(expr, cases) =>
            if (!checkNumericOrPacked(expr, "'case' expression")) {
              error(tree)
            } else if (expr.tpe.isPacked && expr.tpe.width == 0) {
              error(tree, expr, "'case' expression has width zero")
            }
            val oks = cases map { _.stmts } map checkBlock
            if (oks exists { !_ }) {
              error(tree)
            } else {
              val allComb = cases forall { _.stmts forall { _.tpe.isCombStmt } }
              val allCtrl = cases forall { _.stmts exists { _.tpe.isCtrlStmt } }
              if (!allComb && !allCtrl) {
                error(
                  tree,
                  "Either all or no cases of a 'case' statement must be control statements"
                )
              }
            }

          case StmtLoop(body) =>
            if (body.isEmpty) {
              error(tree, "Body of 'loop' must be a control statement")
            } else if (body.last.tpe != TypeCtrlStmt) {
              error(tree, "Body of 'loop' must end in a control statement")
            }

          case StmtLet(_, body) if !checkBlock(body) => error(tree)

          case StmtAssign(lhs, rhs) if !rhs.tpe.underlying.isNum =>
            if (lhs.tpe.underlying.isNum && rhs.tpe.isPacked) {
              error(tree, "Unsized integer variable assigned a packed value")
            } else {
              // lhs have already been checked in enter
              val ok = checkPacked(rhs, "Right hand side of assignment") &&
                checkWidth(lhs.tpe.width, rhs, "Right hand side of assignment")
              if (!ok) error(tree)
            }

          // TODO: this is not right for shifts which can have any right hand side
          case StmtUpdate(lhs, _, rhs) if !rhs.tpe.underlying.isNum =>
            if (lhs.tpe.underlying.isNum && rhs.tpe.isPacked) {
              error(tree, "Unsized integer variable assigned a packed value")
            } else {
              // lhs have already been checked in enter
              val ok = checkPacked(rhs, "Right hand side of assignment") &&
                checkWidth(lhs.tpe.width, rhs, "Right hand side of assignment")
              if (!ok) error(tree)
            }

          case StmtPost(expr, op) =>
            val ok = expr.tpe.isGen && expr.tpe.underlying.isNum ||
              checkPacked(expr, s"Target of postfix '$op'") && checkModifiable(expr)
            if (!ok) error(tree)

          case stmt: StmtReturn =>
            enclosingSymbols.iterator.dropWhile(_.kind.isScope).next.kind match {
              case TypeCallable(symbol, TypeVoid, _) =>
                stmt.exprOpt match {
                  case Some(expr) =>
                    error(tree, expr, s"void function '${symbol.name}' cannot return a value")
                  case _ =>
                }
              case TypeCallable(symbol, retType, _) =>
                stmt.exprOpt match {
                  case None =>
                    error(tree, s"non-void function '${symbol.name}' must return a value")
                  case Some(expr) =>
                    if (!expr.tpe.underlying.isNum) {
                      val ok = checkPacked(expr, "Return value") &&
                        checkWidth(retType.width, expr, "Return value")
                      if (!ok) error(tree)
                    }
                }
              case _ => error(tree, "'return' statement not inside function")
            }

          case StmtExpr(expr) if expr.isPure =>
            error(tree, "A pure expression in statement position does nothing")

          case StmtWait(cond) =>
            if (!checkNumericOrPacked(cond, "Condition of 'stall' statement")) {
              error(tree)
            } else if (!checkWidth(1, cond, "Condition of 'stall' statement")) {
              error(tree)
            }

          //////////////////////////////////////////////////////////////////////////
          // Expr
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
                    error(tree, msg)
                    mAcc addOne Note.definedHere(symbol.desc)
                  case kind =>
                    error(tree, s"No member named '$sel' in value of type '${kind.toSource}'")
                }
              case _ =>
            }

          case ExprCall(expr, args) =>
            def check(kinds: List[Type]): Unit = {
              val as = args map {
                case ArgP(a) => a
                case _       => unreachable
              }
              for (((kind, arg), index) <- (kinds zip as).zipWithIndex) {
                val i = index + 1
                if (kind.isType) {
                  if (!arg.tpe.isType) error(tree, arg, s"Argument $i expects a type")
                } else if (kind.isNum) {
                  if (!arg.tpe.underlying.isNum)
                    error(tree, arg, s"Argument $i expects an unsized value")
                } else if (!arg.tpe.underlying.isNum) {
                  val ok = checkPacked(arg, s"Argument $i of function call") &&
                    checkWidth(kind.width, arg, s"Argument $i of function call")
                  if (!ok) error(tree)
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
                      case v if v > 0 => tree withTpe TypeType(TypeInt(signed, v))
                      // Width is non-positive
                      case v => error(tree, width, s"$hint must be positive (not $v)")
                    }
                  case _ =>
                    error(
                      tree,
                      s"Bad parameter to '$hint', a single positional argument is expected."
                    )
                }
              case _ =>
                expr.tpe match {
                  case TypeCombFunc(_, _, argTypes)     => check(argTypes)
                  case TypeCtrlFunc(_, _, argTypes)     => check(argTypes)
                  case TypeXenoFunc(_, _, argTypes)     => check(argTypes)
                  case TypeStaticMethod(_, _, argTypes) => check(argTypes)
                  case TypeNormalMethod(_, _, argTypes) => check(argTypes)
                  case tpe: TypePolyFunc =>
                    tpe.resolve(args, Some(fe)) match {
                      case Some(symbol) => tree withTpe symbol.kind.asCombFunc.retType
                      case None =>
                        val msg =
                          s"Builtin function '${expr.toSource}' cannot be applied to arguments" :: {
                            args map { arg => s"'${arg.toSource}' of type ${arg.tpe.toSource}" }
                          }
                        error(tree, msg: _*)
                    }
                  case _ => unreachable // Handled in enter
                }
            }

          case ExprCat(parts) =>
            for ((part, i) <- parts.zipWithIndex) {
              if (!checkPacked(part, s"Part ${i + 1} of bit concatenation")) {
                error(tree)
              }
            }

          case ExprRep(count, expr) =>
            val cHint = "Replication count"
            if (checkNumeric(count, cHint) &&& checkPacked(expr, "Replicated value")) {
              evaluate(count, cHint) filter { checkPositive(_, cHint, expr) } foreach { v =>
                tree withTpe TypeUInt(v * expr.tpe.width)
              }
            }

          case ExprIndex(tgt, idx) =>
            if (tgt.tpe.isType) {
              val kindOpt = tgt.tpe.asType.kind match {
                case _: TypeRecord =>
                  error(tree, tgt, "Vector element must not have 'struct' type"); None
                case TypeVoid => error(tree, tgt, "Vector element must not have 'void' type"); None
                case kind if !kind.isPacked =>
                  error(tree, tgt, "Vector element must have a packed type"); None
                case kind => Some(kind)
              }
              val hint = "Size of vector"
              evaluate(idx, hint) filter { checkPositive(_, hint, idx) } foreach { v =>
                kindOpt foreach { kind =>
                  tree withTpe TypeType(kind addVectorDimension v)
                }
              }
            } else if (tgt.tpe.underlying.isNum) {
              evaluate(idx, "Index of unsized integer value")
            } else if (tgt.tpe.isArray || checkPacked(tgt, "Target of index")) {
              // TODO: Is the conditional redundant due to the check already in
              //       enter? Should this be more like ExprSlice???
              checkIndex(Some(contextKind.top.get.width), idx, "Index")
            }

          case ExprSlice(tgt, lIdx, op, rIdx) =>
            val tgtIsNum = tgt.tpe.underlying.isNum
            if (!tgtIsNum && !checkPacked(tgt, s"Target of '$op' slice")) {
              error(tree)
            } else {
              def mkType(width: BigInt): Type =
                tgt.tpe.underlying match {
                  case _: TypeNum                              => TypeUInt(width)
                  case TypeVector(kind, _)                     => TypeVector(kind, width)
                  case kind if kind.isPacked && kind.width > 0 => TypeUInt(width)
                  case _                                       => unreachable
                }
              val lHint = s"Left index of '$op' slice"
              val rHint = s"Right index of '$op' slice"
              val dimSize = tgt.tpe.shapeIter.nextOption getOrElse BigInt(0) // Num has no shape
              if (op == ":") {
                val iWidth = clog2(dimSize) max 1
                if (
                  checkIndex(Option.unless(tgtIsNum)(iWidth), lIdx, lHint) &&&
                    checkIndex(Option.unless(tgtIsNum)(iWidth), rIdx, rHint)
                ) {
                  val lIdxValOpt = evaluate(lIdx, lHint) filter {
                    tgtIsNum || checkInRange(_, 0, true, dimSize, false, lHint, lIdx)
                  }
                  val rIdxValOpt = evaluate(rIdx, rHint) filter {
                    tgtIsNum || checkInRange(_, 0, true, dimSize, false, rHint, rIdx)
                  }
                  lIdxValOpt foreach { lIdxVal =>
                    rIdxValOpt foreach { rIdxVal =>
                      if (lIdxVal < rIdxVal) {
                        error(tree, "Left index of ':' slice must be >= than the right index")
                      } else {
                        tree withTpe mkType(width = lIdxVal - rIdxVal + 1)
                      }
                    }
                  }
                }
              } else {
                val lWidth = clog2(dimSize) max 1
                val rWidth = clog2(dimSize + 1)
                if (
                  checkIndex(Option.unless(tgtIsNum)(lWidth), lIdx, lHint) &&&
                    checkIndex(Option.unless(tgtIsNum)(rWidth), rIdx, rHint)
                ) {
                  evaluate(rIdx, rHint) filter { value =>
                    tgtIsNum || checkInRange(value, 0, false, dimSize, true, rHint, rIdx)
                  } foreach { value =>
                    tree withTpe mkType(width = value)
                  }
                }
              }
            }

          // Unary ops

          // Unary ' is special and the type of the node must be assigned here
          // based on context as it cannot be determined from the operand only.
          case ExprUnary("'", op) =>
            if (hadError) {
              // If we had a type error, the context stack might be out of sync
              // so don't type check any further unary tick nodes
              error(tree)
            } else if (contextKind.isEmpty || contextKind.top.isEmpty) {
              error(tree, "Unary ' operator used in invalid context")
            } else if (checkPacked(op, "Operand of unary ' operator")) {
              if (contextKind.top.get.isNum) {
                tree withTpe TypeNum(op.tpe.isSigned)
              } else {
                val resWidth = contextKind.top.get.width
                val opWidth = op.tpe.width
                if (resWidth < opWidth) {
                  error(tree, s"Unary ' causes narrowing of width from $opWidth to $resWidth")
                } else {
                  tree withTpe TypeInt(op.tpe.isSigned, resWidth)
                }
              }
            } else {
              error(tree)
            }

          case ExprUnary(op, expr) =>
            if (expr.tpe.underlying.isNum) {
              if ("&|^" contains op) {
                error(tree, s"Unary operator '$op' cannot be applied to unsized integer value")
              }
            } else if (!checkPacked(expr, s"Operand of unary operator '$op'")) {
              error(tree)
            }

          // Binary ops
          case ExprBinary(lhs, "'", rhs) =>
            val rHint = "Right hand side operand of binary ' operator"
            val lHint = "Left hand side operand of binary ' operator"
            val rhsOk = checkPacked(rhs, rHint) // Evaluate eagerly up front
            evaluate(lhs, lHint)
              .filter(checkPositive(_, lHint, lhs))
              .filter(_ => rhsOk)
              .foreach {
                case v if v < rhs.tpe.width =>
                  error(tree, "Binary ' operator causes narrowing")
                case v =>
                  tree withTpe TypeInt(rhs.tpe.isSigned, v)
              }

          case ExprBinary(lhs, op, rhs) =>
            if (!lhs.tpe.underlying.isNum || !rhs.tpe.underlying.isNum) {
              lazy val strictWidth = !(mixedWidthBinaryOps contains op)
              if (lhs.tpe.underlying.isNum && strictWidth) {
                if (!checkPacked(rhs, s"Right hand operand of '$op'")) {
                  error(tree)
                }
              } else if (rhs.tpe.underlying.isNum && strictWidth) {
                if (!checkPacked(lhs, s"Left hand operand of '$op'")) {
                  error(tree)
                }
              } else if (strictWidth) {
                if (
                  !checkPacked(lhs, s"Left hand operand of '$op'") |||
                    !checkPacked(rhs, s"Right hand operand of '$op'")
                ) {
                  error(tree)
                } else if (lhs.tpe.width != rhs.tpe.width) {
                  error(
                    tree,
                    s"Both operands of binary '$op' must have the same width, but",
                    s"left  hand operand is ${lhs.tpe.width} bits wide, and",
                    s"right hand operand is ${rhs.tpe.width} bits wide"
                  )
                }
              } else {
                if (
                  !checkNumericOrPacked(lhs, s"Left hand operand of '$op'") |||
                    !checkNumericOrPacked(rhs, s"Right hand operand of '$op'")
                ) {
                  error(tree)
                }
              }
            }

            if (!tree.hasTpe) {
              if ((op == "<<<" || op == ">>>") && !lhs.tpe.isSigned) {
                cc.warning(lhs, "Arithmetic shift used on unsigned left operand")
              } else if ((op == "<<" || op == ">>") && lhs.tpe.isSigned) {
                cc.warning(lhs, "Logical shift used on signed left operand")
              } else if (comparisonBinaryOps contains op) {
                if (lhs.tpe.isSigned && !rhs.tpe.isSigned) {
                  cc.warning(tree, "Comparison between signed and unsigned operands")
                } else if (!lhs.tpe.isSigned && rhs.tpe.isSigned) {
                  cc.warning(tree, "Comparison between unsigned and signed operands")
                }
              }
            }

          case ExprCond(cond, thenExpr, elseExpr) =>
            if (!checkNumericOrPacked(cond, "Condition of '?:'")) {
              error(tree)
            } else if (!thenExpr.tpe.underlying.isNum || !elseExpr.tpe.underlying.isNum) {
              lazy val okThen = checkPacked(thenExpr, "'then' operand of '?:'")
              lazy val okElse = checkPacked(elseExpr, "'else' operand of '?:'")
              if (thenExpr.tpe.underlying.isNum) {
                if (!okElse) error(tree)
              } else if (elseExpr.tpe.underlying.isNum) {
                if (!okThen) error(tree)
              } else if (okElse &&& okThen) {
                if (thenExpr.tpe.width != elseExpr.tpe.width) {
                  error(
                    tree,
                    s"'then' and 'else' operands of ternary '?:' must have the same width, but",
                    s"'then' operand is ${thenExpr.tpe.width} bits wide, and",
                    s"'else' operand is ${elseExpr.tpe.width} bits wide"
                  )
                }
              } else {
                error(tree)
              }
            }

          //////////////////////////////////////////////////////////////////////////
          // Done
          //////////////////////////////////////////////////////////////////////////

          case _ => ()
        }

        override def finalCheck(tree: Tree): Unit = {
          if (tree.hasTpe && !tree.tpe.isError) {
            assert(contextKind.isEmpty, s"${tree.loc.prefix}\n$contextKind\n$contextNode")

            // $COVERAGE-OFF$ Debug code
            tree visit {
              case _: DescParametrized                        => // Ok
              case ExprCall(tgt, _) if tgt.tpe.isParametrized => // Ok
              case n: Tree if !n.hasTpe                       => throw Ice(n, "Typer: untyped node remains", n.toString)
            }
            // $COVERAGE-ON$
          }
        }
      }

      (tree rewrite Checker).ensuring(_ eq tree, "Type checker must not modify the input tree")

      val ms = mAcc.result
      if (ms.nonEmpty || (tree.hasTpe && tree.tpe.isError)) {
        // TODO: Pass all errors out rather than using cc.error ...
        Failure(ms)
      } else {
        val rs = rAcc.result()
        if (rs.nonEmpty) {
          assert(!tree.hasTpe)
          Unknown(rs)
        } else {
          Complete(tree.tpe)
        }
      }
    }
  }

}
