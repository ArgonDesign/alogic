////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.analysis.StaticEvaluation
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Bindings
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Messages.Message
import com.argondesign.alogic.core.StorageTypes.StorageTypeDefault
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.TypeCompound
import com.argondesign.alogic.core.Types.TypeIn
import com.argondesign.alogic.core.Types.TypeOut
import com.argondesign.alogic.core.enums.EntityVariant
import com.argondesign.alogic.core.Messages.Note
import com.argondesign.alogic.core.Types.TypeNone
import com.argondesign.alogic.core.Types.TypeType
import com.argondesign.alogic.util.BigIntOps._
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.chaining.scalaUtilChainingOps

abstract class ListElaborable[T <: Tree] {
  def unapply(tree: Tree): Option[T]

  def description: String
}

object ListElaborable {

  implicit object ListElaborableDesc extends ListElaborable[Desc] { //
    def unapply(tree: Tree): Option[Desc] = tree match {
      case desc: Desc => Some(desc)
      case _          => unreachable
    } //
    val description: String = "Definition"
  }

  implicit object ListElaborableAttr extends ListElaborable[Attr] { //
    def unapply(tree: Tree): Option[Attr] = tree match {
      case attr: Attr => Some(attr)
      case _          => unreachable
    } //
    val description: String = "Attribute"
  }

  implicit object ListElaborablePkg extends ListElaborable[Pkg] { //
    def unapply(tree: Tree): Option[Pkg] = tree match {
      case t: Spliceable => Some(PkgSplice(t) withLocOf t)
      case pkg: Pkg      => Some(pkg)
      case _             => None
    } //
    val description: String = "Package content"
  }

  implicit object ListElaborableEnt extends ListElaborable[Ent] { //
    def unapply(tree: Tree): Option[Ent] = tree match {
      case t: Spliceable => Some(EntSplice(t) withLocOf t)
      case ent: Ent      => Some(ent)
      case _             => None
    } //
    val description: String = "Entity content"
  }

  implicit object ListElaborableRec extends ListElaborable[Rec] { //
    def unapply(tree: Tree): Option[Rec] = tree match {
      case t: Spliceable => Some(RecSplice(t) withLocOf t)
      case rec: Rec      => unreachable // Everything in a Rec is spliceable
      case _             => None
    } //
    val description: String = "Record content"
  }

  implicit object ListElaborableStmt extends ListElaborable[Stmt] { //
    def unapply(tree: Tree): Option[Stmt] = tree match {
      case t: Spliceable => Some(StmtSplice(t) withLocOf t)
      case stmt: Stmt    => Some(stmt)
      case _             => None
    } //
    val description: String = "Statement"
  }

  implicit object ListElaborableCase extends ListElaborable[Case] { //
    def unapply(tree: Tree): Option[Case] = tree match {
      // Allow 'gen' constructs only
      case g: DescGenIf    => Some(CaseSplice(g) withLocOf g)
      case g: DescGenFor   => Some(CaseSplice(g) withLocOf g)
      case g: DescGenRange => Some(CaseSplice(g) withLocOf g)
      // 'gen' constructs expand into gen vars + scopes + usings, so allow those too
      case g: DescGenVar   => Some(CaseSplice(g) withLocOf g)
      case g: DescGenScope => Some(CaseSplice(g) withLocOf g)
      case i: Using        => Some(CaseSplice(i) withLocOf i)
      // Using/from/import gets turned into aliases
      case d: DescAlias => Some(CaseSplice(d) withLocOf d)
      // Regular case clause
      case c: Case => Some(c)
      //
      case _ => None
    } //
    val description: String = "Case clause"
  }

  implicit object ListElaborableExpr extends ListElaborable[Expr] { //
    def unapply(tree: Tree): Option[Expr] = tree match {
      case expr: Expr => Some(expr)
      case _          => unreachable
    } //
    val description: String = "Expression"
  }

  implicit object ListElaborableArg extends ListElaborable[Arg] { //
    def unapply(tree: Tree): Option[Arg] = tree match {
      case arg: Arg => Some(arg)
      case _        => unreachable
    } //
    val description: String = "Argument expression"
  }

}

object Elaborate {

  private def assertProgressIsReal[T](input: T)(result: Result[T]): Unit = result match {
    case Complete(output) =>
      assert(output != input, s"Invalid Complete result in Elaboration: $input")
    case _ =>
  }

  private def definedSymbols(trees: Seq[Tree]): FinalResult[Seq[Symbol]] = {
    val newSymbols = trees collect {
      case Splice(Desc(Sym(symbol))) => symbol
      case Desc(Sym(symbol))         => symbol
    }

    val ms = newSymbols
      .groupBy(_.name)
      .filter(_._2.sizeIs > 1)
      .toSeq
      .sortBy {
        case (name, symbols) => (symbols.map(_.loc.start).min, name)
      }
      .flatMap {
        case (name, symbols) =>
          symbols
            .sortBy(_.loc.start)
            .map(symbol => Error(symbol, s"'$name' has multiple definitions"))
      }

    if (ms.nonEmpty) Failure(ms) else Complete(newSymbols)
  }

  // Given a symtab and a list of trees, return a symtab with a new scope added,
  // which includes the symbols introduced by definitions in the trees.
  private def expandSymtab(
      symtab: SymbolTable,
      trees: Seq[Tree]
    ): FinalResult[SymbolTable] =
    definedSymbols(trees) map { symbols =>
      symbols.foldLeft(symtab.push) {
        case (st, symbol: Symbol) => st + symbol
      }
    }

  // Check for progress in the given results. If the argument contains any
  // Failure instances, returns Left(the concatenated Seq of messages),
  // otherwise it returns true if at least one Complete or Progress instance
  // appears, indicating that the results contain at least incremental progress.
  private def checkProgress(results: IterableOnce[Result[_]]): Either[Seq[Message], Boolean] =
    results.iterator.foldLeft[Either[Seq[Message], Boolean]](Right(false)) {
      case (Right(_), Failure(ms))  => Left(ms) // First failure encountered
      case (Left(acc), Failure(ms)) => Left(acc concat ms) // Gather successive failure messages
      case (left @ Left(_), _)      => left // Failures are contagious
      case (_, Progress(_))         => Right(true) // We made some progress
      case (acc, _)                 => acc // No progress
    }

  // Gather all unknowns from the given results
  private def gatherReasons(results: IterableOnce[Result[_]]): Seq[Reason] =
    results.iterator.foldLeft(Seq.empty[Reason]) {
      case (acc, Unknown(us)) => acc concat us
      case (acc, _)           => acc
    }

  def list[T <: Tree](
      trees: List[Tree],
      symtab: SymbolTable,
      paramsOpt: Option[(Loc, List[Arg])] = None
    )(
      implicit
      cc: CompilerContext,
      fe: Frontend,
      le: ListElaborable[T]
    ): Result[List[T]] =
    if (trees.isEmpty && paramsOpt.isEmpty) {
      Finished(Nil)
    } else {
      // Create named parameter map
      paramsOpt
        .fold[List[Arg]](Nil)(_._2)
        .pipe { params =>
          assert(!params.exists(_.isInstanceOf[ArgD]))
          // Iterator yielding actual positional parameters
          val posParamIt = params.iterator collect { case ArgP(expr) => expr }
          // Named parameter list
          val namedParams = params collect { case a: ArgN => a }
          // Can't handle both positional and named parameters together
          assert(!posParamIt.hasNext || namedParams.isEmpty)

          // Un-splice and replace parameter initializers with positional parameter
          // (this works as we have either only positional or only named parameter
          // assignments, and when using positional assignment, the parameter
          // definitions are never inside a gen block).
          val inputs = trees map {
            case Splice(tree) => tree
            case tree         => tree
          } map {
            case d: DescParam =>
              val initOpt = posParamIt.nextOption() orElse d.initOpt
              if (d.finished) d else d.copy(initOpt = initOpt).withLocOf(d)
            case d: DescParamType =>
              val initOpt = posParamIt.nextOption() orElse d.initOpt
              if (d.finished) d else d.copy(initOpt = initOpt).withLocOf(d)
            case other => other
          }

          if (posParamIt.hasNext) {
            Failure(paramsOpt.get._1, "Too many positional parameters")
          } else {

            val namedParmsOpt = paramsOpt map { case (loc, _) => (loc, namedParams) }

            @tailrec
            def loop(
                trees: List[(Boolean, Tree)],
                hadProgress: Boolean
              ): Result[List[Tree]] =
              // Gather local symbols
              expandSymtab(symtab, trees.map(_._2)) match { // using match to allow tail recursion
                case failure: Failure => failure
                case unknown: Unknown => unknown
                case Complete(st)     =>
                  // Run one pass of elaboration
                  val processed: List[(Result[Tree], Tree)] = trees map {
                    // Parametrized definitions are special. We keep updating the associated
                    // symbol table by re-elaborating on every iteration. The symbol table
                    // is in turn used during specialization (invoked from type checking) to
                    // re-elaborate the definition with actual parameters. We also ignore
                    // parametrized definitions for progress checking as they will not be
                    // complete until the whole symbol table is known, which we will fix up
                    // at the end, but in the meantime the intermediate symbol tables stored
                    // are sufficient for resolving valid dependencies.
                    case (_, d: DescParametrized) => (elaborate(d, st, namedParmsOpt), d)
                    // Trees already completely elaborated on an earlier iteration
                    case (true, t) => (Finished(t), t)
                    // Trees not yet elaborated
                    case (false, d: Desc)      => (elaborate(d, st, namedParmsOpt), d)
                    case (false, a: Attr)      => (elaborate(a, st), a)
                    case (false, i: Import)    => (elaborate(i, st), i)
                    case (false, u: Using)     => (elaborate(u, st), u)
                    case (false, _: From)      => unreachable // Elminated by SyntaxNormalize
                    case (false, a: Assertion) => (elaborate(a, st), a)
                    case (false, p: Pkg)       => (elaborate(p, st), p)
                    case (false, e: Ent)       => (elaborate(e, st), e)
                    case (false, _: Rec) =>
                      unreachable // Desc/Import/Using/Assertion covers all
                    case (false, s: Stmt) => (elaborate(s, st), s)
                    case (false, c: Case) => (elaborate(c, st), c)
                    case (false, e: Expr) => (elaborate(e, st), e)
                    case (false, a: Arg)  => (elaborate(a, st), a)
                    case _                => unreachable // No other list elaborable node types
                  }

                  checkProgress(processed.iterator.map(_._1)) match {
                    case Left(messages) => Failure(messages)
                    case Right(true)    => // Made some progress
                      // Incorporate progress
                      val merged = processed map {
                        case (Finished(tree), _)   => (true, tree) // Completed result earlier
                        case (Complete(tree), _)   => (true, tree) // Completed result just now
                        case (Partial(tree, _), _) => (false, tree) // Partial progress
                        case (_: Unknown, tree)    => (false, tree) // No progress
                        case _                     => unreachable // Failures checked above
                      } flatMap {
                        case (flag, Thicket(trees)) => trees.iterator.map((flag, _))
                        case (_, Stump)             => Iterator.empty
                        case (flag, tree)           => Iterator.single((flag, tree))
                      }
                      if (merged.forall(_._1)) { // If all completed, we are done
                        Complete(merged.map(_._2))
                      } else { // Otherwise go again
                        loop(merged, hadProgress = true)
                      }
                    case Right(false) => // No progress
                      // Gather any unknown reasons
                      val rs = gatherReasons(processed.iterator.map(_._1))
                      if (rs.isEmpty) { // All were already finished
                        if (hadProgress) {
                          Complete(trees.map(_._2))
                        } else {
                          Finished(trees.map(_._2))
                        }
                      } else {
                        if (hadProgress) {
                          Partial(trees.map(_._2), rs)
                        } else { // No progress at all
                          Unknown(rs)
                        }
                      }
                  }
              }

            val simbolyfied: List[(Result[Tree], Tree)] = inputs map {
              case desc: Desc => (simbolify(desc, symtab), desc)
              case other      => (Finished(other), other)
            }

            checkProgress(simbolyfied.map(_._1)) match {
              case Left(messages) => Failure(messages)
              case Right(progress) =>
                val loopInput = simbolyfied map {
                  case (Success(tree), _) => (false, tree)
                  case (_, tree)          => (false, tree)
                }

                // Process tree list
                val processed = loop(loopInput, progress)

                // Check again for multiple definitions and update symbol tables in
                // DescParametrized definitions with the final symbol table
                // TODO: this needs to handle deeper nested parametrized stuff
                processed proceed { trees =>
                  expandSymtab(symtab, trees) flatMap { st =>
                    Finished {
                      trees map {
                        case desc: DescParametrized => desc.copy(symtab = st) withLocOf desc
                        case other                  => other
                      }
                    }
                  }
                } mapFailing { trees =>
                  // re-splice using the type-class or signal error
                  val (ms, ts) = trees partitionMap {
                    case le(tree) => Right(tree)
                    case other    => Left(Error(other, s"${le.description} expected"))
                  }
                  if (ms.nonEmpty) Left(ms) else Right(ts)
                }
            }
          }
        }
        .tap(assertProgressIsReal(trees))
    }

  private def resolveGenCaseConditions(
      cases: List[GenCase],
      symtab: SymbolTable
    )(
      implicit
      cc: CompilerContext,
      fe: Frontend
    ): Result[List[GenCase]] = {
    // Resolve names in conditions
    val processed: List[(GenCase, Result[GenCase])] = cases map { kase =>
      val resolved = elaborate(kase.cond, symtab) map { newCond =>
        kase.copy(cond = newCond) withLocOf kase
      }
      (kase, resolved)
    }

    checkProgress(processed.iterator.map(_._2)) match {
      case Left(messages) => Failure(messages)
      case Right(true)    => // Made some progress
        // Incorporate progress
        val merged = processed map {
          case (_, Finished(tree))    => (tree, Nil) // Completed result earlier
          case (_, Complete(tree))    => (tree, Nil) // Completed result just now
          case (_, Partial(tree, rs)) => (tree, rs) // Partial progress
          case (tree, Unknown(rs))    => (tree, rs) // No progress
          case _                      => unreachable // Failures checked above
        }
        val trees = merged.map(_._1)
        val rs = merged.flatMap(_._2)
        if (rs.isEmpty) { // If all completed, we are done
          Complete(trees)
        } else { // Otherwise we made partial progress
          Partial(trees, rs)
        }
      case Right(false) => // No progress
        // Gather any unknowns
        val unknowns = gatherReasons(processed.iterator.map(_._2))
        if (unknowns.nonEmpty) { // Had some unknowns
          Unknown(unknowns)
        } else { // Otherwise all were already finished
          Finished(cases)
        }
    }
  } tap assertProgressIsReal[List[Tree]](cases)

  private def evalGenCaseConditions(
      desc: DescGenIf
    )(
      implicit
      fe: Frontend
    ): Result[DescGenScope] = {
    val processed = desc.cases map {
      case GenCase(cond, body) =>
        fe.typeCheck(cond) flatMap { _ =>
          if (cond.tpe.underlying.isNum) {
            Failure(
              cond,
              "Condition of 'gen if' yields an unsized value, a 1 bit value is expected"
            )
          } else if (!cond.tpe.isPacked) {
            Failure(
              cond,
              "Condition of 'gen if' is of non-packed type, a 1 bit value is expected"
            )
          } else if (cond.tpe.width != 1) {
            Failure(
              cond,
              s"Condition of 'gen if' yields ${cond.tpe.width} bits, a 1 bit value is expected"
            )
          } else {
            fe.evaluate(cond, "'gen' condition") match {
              case Complete(v) if v == 0 => Complete(None)
              case Complete(_)           => Complete(Some(body))
              case unknown: Unknown      => unknown
              case failure: Failure      => failure
            }
          }
        }
    }

    checkProgress(processed) match {
      case Left(messages) => Failure(messages)
      case _              =>
        // Note: We only consider the cases resolved if we could compute the
        // value of all of them, even though we only care about the first taken
        // case. Doing it this way ensures that later cases are still well
        // formed, even though they are not used. Otherwise the compiler would
        // accept malformed code of the form:
        //   gen if (true) { ... } else if (nonsense) ...
        // Ensuring all branch conditions can be evaluated has the desirable
        // effect of rejecting these malformed cases.
        val unknowns = gatherReasons(processed.iterator)
        if (unknowns.nonEmpty) { // Had some unknowns
          Unknown(unknowns)
        } else { // All cases were evaluated
          Complete {
            // Pick the first taken branch, otherwise the defaults
            val body = processed collectFirst {
              case Complete(Some(body)) => body
            } getOrElse desc.defaults
            DescGenScope(desc.ref, desc.attr, body) withLocOf desc
          }
        }
    }
  }

  private def elabGenFor(
      desc: DescGenFor,
      symtab: SymbolTable
    )(
      implicit
      cc: CompilerContext,
      fe: Frontend
    ): Result[Desc] =
    // Elaborate the initializers
    list[Desc](desc.inits, symtab) map { inits =>
      desc.copy(inits = inits) withLocOf desc
    } proceed { desc =>
      // Add initializers to the symbol table
      expandSymtab(symtab, desc.inits) flatMap { st =>
        // Resolve names in condition
        elaborate(desc.cond, st) map { cond =>
          desc.copy(cond = cond) withLocOf desc
        } proceed { desc =>
          // Type check condition
          fe.typeCheck(desc.cond) flatMap { kind =>
            if (kind.underlying.isNum) {
              Failure(
                desc.cond,
                "Condition of 'gen for' yields an unsized value, a 1 bit value is expected"
              )
            } else if (!kind.isPacked) {
              Failure(
                desc.cond,
                "Condition of 'gen for' is of non-packed type, a 1 bit value is expected"
              )
            } else if (kind.width != 1) {
              Failure(
                desc.cond,
                s"Condition of 'gen for' yields ${kind.width} bits, a 1 bit value is expected"
              )
            } else {
              Complete(desc)
            }
          }
        } proceed { desc =>
          // Elaborate the step statements
          list[Stmt](desc.steps, st) map { steps =>
            desc.copy(steps = steps) withLocOf desc
          }
        } proceed { desc =>
          // Type check step statements
          (desc.steps map fe.typeCheck).distil map { _ => desc }
        } proceed {
          case DescGenFor(ref, attr, inits, cond, steps, body) =>
            // Evaluate the initial values of the loop variables
            inits
              .map(d =>
                fe.evaluate(d.symbol, d.loc, "'gen' variable initializer") map {
                  d.symbol -> _
                }
              )
              .distil
              .map(Bindings.from(_)) flatMap { bindings =>
              // Setup the step statements for evaluation, including if they are empty
              val step = StmtBlock(steps) withLoc steps.head.loc.copy(end = steps.last.loc.end)

              val iterationLimit = cc.settings.genLoopLimit

              // Function that computes the rest of the loop body given the
              // current loop variable bindings
              @tailrec
              def expandLoopBody(
                  bindings: Bindings,
                  count: Int,
                  acc: List[List[Tree]]
                ): Result[List[Tree]] =
                if (count >= iterationLimit) {
                  Failure(
                    cond,
                    s"'gen for' exceeds $iterationLimit iterations. Possibly an infinite loop,",
                    s"otherwise set --gen-loop-limit to more than $iterationLimit"
                  )
                } else {
                  // Evaluate the condition (using match to enable tail recursion)
                  fe.evaluate(cond substitute bindings, "'gen' loop condition") match {
                    case unknown: Unknown      => unknown
                    case failure: Failure      => failure
                    case Complete(v) if v == 0 => Complete(acc.reverse.flatten)
                    case Complete(_)           =>
                      // The current iteration expands into two things:
                      // - An intermediate scope containing a local copy of the initializers
                      //   and a copy of the body given in the input
                      // - A wildcard import of dictionary names from the intermediate scope
                      val currBody = {
                        // Introduce a new unique name for the temporary scope.
                        // At this point the bindings should be just numbers,
                        // so evaluate(_, ).get should be safe
                        val name = bindings.toSeq
                          .sortBy(_._1.loc)
                          .map {
                            case (symbol, value) =>
                              s"${symbol.name}_${fe.evaluate(value, unreachable).get}"
                          }
                          .mkString("``", cc.sep, "")
                        val ident = Ident(name, Nil) withLoc Loc.synth("d")
                        // Clone the loop variables, and replace replace initializers with
                        // their current bindings
                        val initClones = inits map {
                          case d @ DescGenVar(ref @ Sym(symbol), attr, spec, _) =>
                            DescGenVar(
                              Sym(symbol.dup) withLocOf ref,
                              attr,
                              spec,
                              bindings.get(symbol).get
                            ) withLocOf d
                          case _ => unreachable
                        }
                        // Create the expansion of the current iteration
                        List(
                          DescGenScope(ident, attr, initClones concat body) withLoc Loc.synth("a"),
                          UsingGenLoopBody(
                            ExprIdent(ident.base, ident.idxs) withLoc Loc.synth("b"),
                            Set.empty
                          ) withLoc {
                            Loc.synth("c")
                          }
                        )
                      }
                      // Apply the step to the bindings and expand
                      lazy val failure = Failure(step, "Cannot statically evaluate step statements")
                      StaticEvaluation(step, bindings) match {
                        case None => failure
                        case Some((_, nextBindings, _, _)) =>
                          if (inits forall { desc => nextBindings contains desc.symbol }) {
                            expandLoopBody(nextBindings, count + 1, currBody :: acc)
                          } else {
                            failure
                          }
                      }
                  }
                }

              // Compute the expanded loop body
              expandLoopBody(bindings, 0, Nil) map { newBody =>
                // Convert into a Gen scope
                DescGenScope(ref, attr, newBody) withLocOf desc
              }
            }
        }
      }
    } tap assertProgressIsReal(desc)

  private def elabGenRange(
      desc: DescGenRange,
      symtab: SymbolTable
    )(
      implicit
      cc: CompilerContext,
      fe: Frontend
    ): Result[Desc] =
    // Elaborate the initializer (type parameter is unused)
    elaborate[Desc](desc.init, symtab, None) map { init =>
      desc.copy(init = init) withLocOf desc
    } proceed { desc =>
      // Resolve names in the end value
      elaborate(desc.end, symtab) map { end =>
        desc.copy(end = end) withLocOf desc
      }
    } proceed {
      case DescGenRange(ref, attr, init, op, end, body) =>
        // Mark loop variable as used
        init.symbol.attr.wasUsed set true
        // Evaluate the end value
        fe.evaluate(end, "'gen' loop end value") map { value =>
          // Compute inclusive end value
          if (op == "<") value - 1 else value
        } flatMap { endValue =>
          // Compute type of loop variable
          fe.typeOf(init.symbol, init.symbol.loc) map { lKind => (endValue, lKind) }
        } flatMap {
          case (endValue, lKind) =>
            // Compute max value representable by the loop variable
            val maxValueOpt = if (lKind.underlying.isNum) {
              None
            } else if (lKind.isSigned) {
              Some(BigInt.mask(lKind.width - 1))
            } else {
              Some(BigInt.mask(lKind.width))
            }
            // Compute last value taken on by the iteration
            val lastValue = maxValueOpt map {
              _ min endValue
            } getOrElse endValue
            if (endValue > lastValue) {
              Failure(
                end,
                s"End value $endValue is out of range for variable '${init.symbol.name}' with type '${lKind.underlying.toSource}'"
              )
            } else {
              Complete(lastValue)
            }
        } flatMap { lastValue =>
          // Function that computes the rest of the loop body given the
          // current iteration index
          def expandLoopBody(currValue: BigInt, acc: List[List[Tree]]): Result[List[Tree]] =
            if (currValue > lastValue) {
              Complete(acc.reverse.flatten)
            } else {
              // The current iteration expands into two things:
              // - An intermediate scope containing a local copy of the loop
              //   variable and a copy of the body given in the input
              // - A wildcard import of dictionary names from the intermediate scope
              val currBody = {
                // Introduce a new unique name for the temporary scope
                val name = s"``${init.symbol.name}_$currValue"
                val ident = Ident(name, Nil) withLoc Loc.synth("e")
                // Clone the loop variables, and replace replace initializers with
                // their current bindings
                val initClone = init match {
                  case d @ DescGenVar(ref @ Sym(symbol), attr, spec, init) =>
                    DescGenVar(
                      Sym(symbol.dup) withLocOf ref,
                      attr,
                      spec,
                      ExprNum(symbol.kind.isSigned, currValue) withLocOf init
                    ) withLocOf d
                  case _ => unreachable
                }
                // Create the expansion of the current iteration
                List(
                  DescGenScope(ident, attr, initClone :: body) withLoc Loc.synth("f"),
                  UsingGenLoopBody(
                    ExprIdent(ident.base, ident.idxs) withLoc Loc.synth("g"),
                    Set.empty
                  ) withLoc {
                    Loc.synth("h")
                  }
                )
              }
              expandLoopBody(currValue + 1, currBody :: acc)
            }

          // Compute the expanded loop body
          expandLoopBody(0, Nil) map { newBody =>
            // Convert into a Gen scope
            DescGenScope(ref, attr, newBody) withLocOf desc
          }
        }
    } tap assertProgressIsReal(desc)

  def simbolify(
      desc: Desc,
      symtab: SymbolTable
    )(
      implicit
      cc: CompilerContext,
      fe: Frontend
    ): Result[Desc] = desc.ref match {
    case _: Sym =>
      // Already defining a symbol
      Finished(desc)
    case ident @ Ident(base, Nil) =>
      // Plain identifier. Create symbol
      val sym = Sym(cc.newSymbol(base, ident.loc)) withLocOf ident
      Complete(desc.copyRef(ref = sym) withLocOf desc)
    case ident @ Ident(_, idxs) =>
      // TODO: check shadowing
      // Dict identifier. Try to evaluate indices, then create symbol
      list[Expr](idxs, symtab) map { i =>
        desc.copyRef(ref = ident.copy(idxs = i) withLocOf ident) withLocOf desc
      } proceed {
        case Desc(Ident(base, idxs)) =>
          fe.nameFor(base, idxs) flatMap { name =>
            symtab.get(name) match {
              case SymbolTable.Outer(symbol) =>
                val err = Error(
                  desc.ref,
                  "Dictionary identifier hides definition with same name in outer scope"
                )
                val note = Note(symbol, "Outer definition is here:")
                Failure(Seq(err, note))
              case _ => Complete(name)
            }
          } map { name =>
            val sym = Sym(cc.newSymbol(name, ident.loc)) withLocOf ident
            desc.copyRef(ref = sym) withLocOf desc
          }
        case _ => unreachable
      }
  }

  def apply(
      desc: Desc,
      symtab: SymbolTable
    )(
      implicit
      cc: CompilerContext,
      fe: Frontend
    ): Result[Desc] = elaborate[Pkg](desc, symtab, None)

  private def elaborate[T <: Tree](
      desc: Desc,
      symtab: SymbolTable,
      namedParamsOpt: Option[(Loc, List[ArgN])]
    )(
      implicit
      cc: CompilerContext,
      fe: Frontend,
      le: ListElaborable[T]
    ): Result[Desc] =
    simbolify(desc, symtab) proceed { desc =>
      // Elaborate attributes
      list[Attr](desc.attr, symtab) map {
        desc.copyAttr(_) withLocOf desc
      }
    } proceed { desc =>
      desc.attr match {
        case Nil => // No attributes (or already processed)
          Finished(desc)
        case as => // We have some attributes, attach them to the symbol
          desc.symbol.attr.update(as)
          Complete(desc.copyAttr(Nil) withLocOf desc)
      }
    } proceed {
      case d: DescVar =>
        elaborate(d.spec, symtab) map { s =>
          d.copy(spec = s) withLocOf d
        } proceed { d =>
          d.initOpt map { init =>
            elaborate(init, symtab) map { i =>
              d.copy(initOpt = Some(i)) withLocOf d
            }
          } getOrElse Finished(d)
        }
      case d: DescVal =>
        elaborate(d.spec, symtab) map { s =>
          d.copy(spec = s) withLocOf d
        } proceed { d =>
          elaborate(d.init, symtab) map { i =>
            d.copy(init = i) withLocOf d
          }
        }
      case d: DescStatic =>
        elaborate(d.spec, symtab) map { s =>
          d.copy(spec = s) withLocOf d
        } proceed { d =>
          d.initOpt map { init =>
            elaborate(init, symtab) map { i =>
              d.copy(initOpt = Some(i)) withLocOf d
            }
          } getOrElse Finished(d)
        } proceed { d =>
          d.initOpt map { init =>
            fe.typeCheck(d) flatMap { _ =>
              fe.evaluate(
                Clarify(d).initOpt.get,
                "Initializer of 'static' variable definition"
              ) flatMap { value =>
                value.asExpr(d.symbol.kind) tap {
                  _ visitAll { case tree => tree withLocOf init }
                } match {
                  case `init`  => Finished(d)
                  case newInit => Complete(d.copy(initOpt = Some(newInit)) withLocOf d)
                }
              }
            }
          } getOrElse Finished(d)
        }
      case d: DescIn =>
        elaborate(d.spec, symtab) map { s =>
          d.copy(spec = s) withLocOf d
        }
      case d: DescOut =>
        elaborate(d.spec, symtab) map { s =>
          d.copy(spec = s) withLocOf d
        } proceed { d =>
          d.initOpt map { init =>
            elaborate(init, symtab) map { i =>
              d.copy(initOpt = Some(i)) withLocOf d
            }
          } getOrElse Finished(d)
        }
      case d: DescPipeVar =>
        elaborate(d.spec, symtab) map { s =>
          d.copy(spec = s) withLocOf d
        }
      case d: DescPipeIn =>
        Finished(d)
      case d: DescPipeOut =>
        Finished(d)
      case d: DescParam =>
        if (d.finished) {
          Finished(d)
        } else {
          elaborate(d.spec, symtab) map { s =>
            d.copy(spec = s) withLocOf d
          } proceed { d =>
            d.initOpt map { init =>
              elaborate(init, symtab) map { i =>
                d.copy(initOpt = Some(i)) withLocOf d
              }
            } getOrElse Finished(d)
          } proceed { d =>
            d.initOpt map { init =>
              fe.evaluate(
                d.symbol,
                d.symbol.loc,
                "Default initializer of 'param' definition",
                markUsed = false,
                paramCheck = true
              ) flatMap {
                case `init`  => Finished(d)
                case newInit => Complete(d.copy(initOpt = Some(newInit)) withLocOf d)
              }
            } getOrElse Finished(d)
          } proceed { d =>
            // namedParamOpt will be defined, as we can only reach here when
            // specializing a parametrized definition, which we can only do if
            // the specialization expression passed the type checker, which ensures
            // actual parameters were given.
            namedParamsOpt.get._2.collectFirst {
              case ArgN(name, expr) if name == d.symbol.name => expr
            } orElse d.initOpt match {
              case some @ Some(_) => Complete(d.copy(initOpt = some) withLocOf d)
              case None           => Unknown(ReasonNeedsParamValue(d.symbol, namedParamsOpt.get._1))
            }
          } proceed { d =>
            fe.evaluate(
              d.symbol,
              d.symbol.loc,
              "actual parameter value",
              markUsed = false,
              paramCheck = true
            ) map { init =>
              d.copy(initOpt = Some(init)) withLocOf d
            }
          } pipe {
            // The transformations above produce a Complete intermediate result
            // on success so the final result will never be marked as Finished
            case Finished(_) => unreachable
            // If the result is the same as the input, we are finished
            case Complete(r) if r == d => Finished(d)
            // Otherwise if success, we just completed it
            case Complete(r) => Complete(r)
            //
            case other => other
          } match {
            // Set 'finished' flag indicating this parameter has been
            // substituted already
            case Success(d) => Complete(d.copy(finished = true) withLocOf d)
            case other      => other
          }
        }
      case d: DescParamType =>
        if (d.finished) {
          Finished(d)
        } else {
          d.initOpt map { init =>
            elaborate(init, symtab) map { i =>
              d.copy(initOpt = Some(i)) withLocOf d
            }
          // TODO: check default initializer is TypeType
          } getOrElse Finished(d) proceed { d =>
            // namedParamOpt will be defined, as we can only reach here when
            // specializing a parametrized definition, which we can only do if
            // the specialization expression passed the type checker, which ensures
            // actual parameters were given.
            namedParamsOpt.get._2.collectFirst {
              case ArgN(name, expr) if name == d.symbol.name => expr
            } orElse d.initOpt match {
              case some @ Some(_) =>
                if (d.initOpt == some) Finished(d) else Complete(d.copy(initOpt = some) withLocOf d)
              case None =>
                Unknown(ReasonNeedsParamValue(d.symbol, namedParamsOpt.get._1))
            }
          } match {
            // Set 'finished' flag indicating this parameter has been
            // substituted already
            case Success(d) => Complete(d.copy(finished = true) withLocOf d)
            case other      => other
          }
        }
      case d: DescConst =>
        elaborate(d.spec, symtab) map { s =>
          d.copy(spec = s) withLocOf d
        } proceed { d =>
          elaborate(d.init, symtab) map { i =>
            d.copy(init = i) withLocOf d
          }
        } proceed { d =>
          fe.evaluate(
            d.symbol,
            d.symbol.loc,
            "Initializer of 'const' definition",
            markUsed = false
          ) flatMap { init =>
            if (init == d.init) Finished(d) else Complete(d.copy(init = init) withLocOf d)
          }
        }
      case d: DescArray =>
        elaborate(d.elem, symtab) map { e =>
          d.copy(elem = e) withLocOf d
        } proceed { d =>
          elaborate(d.size, symtab) map { s =>
            d.copy(size = s) withLocOf d
          }
        }
      case d: DescSram =>
        elaborate(d.elem, symtab) map { e =>
          d.copy(elem = e) withLocOf d
        } proceed { d =>
          elaborate(d.size, symtab) map { s =>
            d.copy(size = s) withLocOf d
          }
        }
      case d: DescType =>
        elaborate(d.spec, symtab) map { s =>
          d.copy(spec = s) withLocOf d
        }
      case d: DescEntity =>
        list[Ent](d.body, symtab) map { b =>
          d.copy(body = b) withLocOf d
        }
      case d: DescRecord =>
        list[Rec](d.body, symtab) map { b =>
          d.copy(body = b) withLocOf d
        }
      case d: DescInstance =>
        elaborate(d.spec, symtab) map { s =>
          d.copy(spec = s) withLocOf d
        }
      case d: DescSingleton =>
        list[Ent](d.body, symtab) map { b =>
          d.copy(body = b) withLocOf d
        }
      case d: DescFunc =>
        if (d.symbol.name == "main") {
          d.symbol.attr.entry set true
        }
        elaborate(d.ret, symtab) map { ret =>
          d.copy(ret = ret) withLocOf d
        } proceed { d =>
          list[Desc](d.args, symtab) map { args =>
            d.copy(args = args) withLocOf d
          }
        } proceed { d =>
          expandSymtab(symtab, d.args) flatMap { st =>
            list[Stmt](d.body, st) map { body =>
              d.copy(body = body) withLocOf d
            }
          }
        }
      case d: DescPackage =>
        list[Pkg](d.body, symtab) map { b =>
          d.copy(body = b) withLocOf d
        }
      case d: DescGenVar =>
        elaborate(d.spec, symtab) map { s =>
          d.copy(spec = s) withLocOf d
        } proceed { d =>
          elaborate(d.init, symtab) map { i =>
            d.copy(init = i) withLocOf d
          }
        }
      case d: DescGenIf =>
        resolveGenCaseConditions(d.cases, symtab) map { cases =>
          d.copy(cases = cases) withLocOf d
        } proceed {
          evalGenCaseConditions
        } proceed { d =>
          elaborate[T](d, symtab, namedParamsOpt)
        }
      case d: DescGenFor =>
        elabGenFor(d, symtab) proceed { newDesc =>
          // Elaborate result
          elaborate[T](newDesc, symtab, namedParamsOpt)
        }
      case d: DescGenRange =>
        elabGenRange(d, symtab) proceed { newDesc =>
          // Elaborate result
          elaborate[T](newDesc, symtab, namedParamsOpt)
        }
      case d: DescGenScope =>
        list[T](d.body, symtab, namedParamsOpt) map { b =>
          d.copy(body = b) withLocOf d
        }
      case d: DescAlias =>
        elaborate(d.expr, symtab) map { e =>
          d.copy(expr = e) withLocOf d
        }
      case d: DescParametrized =>
        Finished(d.copy(symtab = symtab) withLocOf d)
    } tap assertProgressIsReal(desc)

  private def elaborate(
      attr: Attr,
      symtab: SymbolTable
    )(
      implicit
      cc: CompilerContext,
      fe: Frontend
    ): Result[Tree] =
    attr pipe {
      case a: AttrBool => Finished(a)
      case a: AttrExpr =>
        elaborate(a.expr, symtab) proceed { expr =>
          fe.evaluate(
            expr,
            s"'${a.name}' attribute"
          ) map { value =>
            a.copy(expr = ExprNum(true, value) withLocOf expr) withLocOf a
          } match {
            case Complete(v) if v == a => Finished(a) // Mark as finished if unchanged
            case other                 => other
          }
        }
    } tap assertProgressIsReal(attr)

  private def elaborate(
      imprt: Import,
      symtab: SymbolTable
    )(
      implicit
      cc: CompilerContext,
      fe: Frontend
    ): Result[Tree] = {

    def finishImport(symbol: Symbol, ident: Ident): Result[Tree] = {
      val expr = ExprSym(symbol) withLocOf imprt
      val alias = DescAlias(ident, Nil, expr, exprt = false) withLocOf imprt
      // Type parameter is unused as DescAlias doesn't have a body...
      elaborate[Desc](alias, symtab, None)
    }

    imprt pipe {
      case ImportOne(path, ident) =>
        fe.imprt(path, imprt.loc) match {
          case Left(result) => result flatMap { finishImport(_, ident) }
          case Right(future) =>
            Partial(ImportPending(future, ident) withLocOf imprt, Seq(ReasonImportPending(imprt)))
        }
      case ImportPending(future, ident) =>
        Await.result(future, Duration.Inf) flatMap { finishImport(_, ident) }
    } tap assertProgressIsReal(imprt)
  }

  private def reExport(symbol: Symbol): Boolean = symbol.desc match {
    // Don't re-export aliases that should not be exported ..
    case DescAlias(_, _, _, exprt) => exprt
    // Can re-export anything else
    case _ => true
  }

  private def elaborate(
      usng: Using,
      symtab: SymbolTable
    )(
      implicit
      cc: CompilerContext,
      fe: Frontend
    ): Result[Tree] =
    elaborate(usng.expr, symtab) map { expr =>
      usng.cpy(expr = expr) withLocOf usng
    } proceed {
      case UsingOne(expr, Some(ident)) =>
        // Type parameter is unused as DescAlias doesn't have a body...
        elaborate[Desc](DescAlias(ident, Nil, expr, exprt = false) withLocOf usng, symtab, None)
      case UsingOne(_, None) =>
        unreachable // Eliminated by SyntaxNormalize
      case UsingAll(expr, exprt) =>
        fe.typeCheck(expr) pipe {
          // TODO: Clean up TypeType/TypeNone cases as they don't all make sense
          case Complete(t: TypeCompound)           => Complete(t.publicSymbols)
          case Complete(TypeType(t: TypeCompound)) => Complete(t.publicSymbols)
          case Complete(TypeNone(t: TypeCompound)) => Complete(t.publicSymbols)
          case Complete(_)                         => Failure(expr, "Target of 'using .*;' must be a compound type")
          case unknown: Unknown                    => unknown
          case failure: Failure                    => failure
        } flatMap { symbols =>
          val aliases: List[Desc] = symbols
            .filterNot(_.name startsWith "`")
            .map { symbol =>
              val eLoc = if (expr.loc.isSynthetic) symbol.desc.ref.loc else expr.loc
              DescAlias(
                Ident(symbol.name, Nil) withLocOf symbol.desc.ref,
                Nil,
                ExprSymSel(expr, symbol) withLoc eLoc,
                exprt = exprt && reExport(symbol)
              ) withLocOf symbol.desc
            }
          list[Desc](aliases, symtab) map { Thicket(_) }
        }
      case u @ UsingGenLoopBody(expr, exclude) =>
        // As opposed to the UsingAll directive, which works based on types,
        // this one works lexically and allows partial resolution. This in
        // turn enables loop carried dependencies among names.
        expr pipe {
          case ExprSym(symbol) => symbol.desc
          case _               => throw Ice("Malformed UsingGenLoopBody")
        } pipe {
          case DescGenScope(_, _, body) =>
            val (unresolved, resolved) = body map {
              case Splice(tree) => tree
              case other        => other
            } filter {
              // Keep things that may introduce a name
              case _: Desc | _: Import | _: Using => true
              case _                              => false
            } partitionMap {
              case d @ Desc(_: Sym) => Right(d.symbol)
              case other            => Left(other)
            } match {
              case (us, rs) =>
                (us, rs filter { _.name contains "#" } filterNot exclude)
            }
            val aliases: List[Desc] = resolved map { symbol =>
              DescAlias(
                Ident(symbol.name, Nil) withLocOf symbol.desc.ref,
                Nil,
                // Observer that here we use a direct reference (ExprSym)
                // rather than a hierarchical (ExprSymSel) reference. This is
                // ok, as we know the definition is inside a nearby lexical
                // scope, and using a direct reference allows us to type the
                // referenced symbol without typing the enclosing GenScope,
                // which might not be fully resolved due to loop carried
                // dependencies when the type is required
                ExprSym(symbol) withLocOf symbol.desc.ref,
                exprt = reExport(symbol)
              ) withLocOf symbol.desc.ref
            }
            if (unresolved.nonEmpty) {
              val rs = unresolved map ReasonUnelaborated.apply
              if (resolved.nonEmpty) {
                val newUsing = u.copy(exclude = exclude ++ resolved) withLocOf u
                Partial(Thicket(newUsing :: aliases), rs)
              } else {
                Unknown(rs)
              }
            } else {
              list[Desc](aliases, symtab) map { Thicket(_) }
            }
          case desc: DescGenRange => Unknown(ReasonUnelaborated(desc))
          case desc: DescGenFor   => Unknown(ReasonUnelaborated(desc))
          case _                  => throw Ice("Malformed UsingGenLoopBody")
        }
    } tap assertProgressIsReal(usng)

  private def elaborate(
      assertion: Assertion,
      symtab: SymbolTable
    )(
      implicit
      cc: CompilerContext,
      fe: Frontend
    ): Result[Tree] = assertion pipe {
    case a: AssertionAssert =>
      elaborate(a.cond, symtab) map { cond =>
        a.copy(cond = cond) withLocOf assertion
      }
    case a: AssertionStatic =>
      elaborate(a.cond, symtab) map { cond =>
        a.copy(cond = cond) withLocOf assertion
      } proceed { a =>
        fe.typeCheck(a) flatMap { _ =>
          fe.evaluate(a.cond, "Condition of static assertion") flatMap {
            case v if v != 0 => Complete(Stump)
            case _ =>
              val suffix = a.msgOpt map {
                ": " + _
              } getOrElse ""
              Failure(a.cond, s"Static assertion failure$suffix")
          }
        }
      }
    case a: AssertionUnreachable => Finished(a)
    case _: AssertionAssume      => unreachable // Only used in back-end
  } tap assertProgressIsReal(assertion)

  private def elaborate(
      pkg: Pkg,
      symtab: SymbolTable
    )(
      implicit
      cc: CompilerContext,
      fe: Frontend
    ): Result[Tree] =
    pkg pipe {
      case _: PkgSplice => unreachable
      case p: PkgCompile =>
        elaborate(p.expr, symtab) map { expr =>
          p.copy(expr = expr) withLocOf p
        } proceed { p =>
          p.identOpt match {
            case None =>
              // No wrapper needed
              Finished(p)
            case Some(ident) =>
              // Create wrapper
              fe.typeCheck(p.copy(identOpt = None) withLocOf p) flatMap { _ =>
                val portSymbols = p.expr.tpe.asType.kind.asEntity.portMembers
                val body = if (portSymbols.isEmpty) {
                  Nil
                } else {
                  val instName: String = {
                    @tailrec
                    def loop(name: String): String =
                      if (portSymbols exists { _.name == name }) loop(name + "_") else name
                    loop("inst")
                  }
                  val portsAndConnects: List[Ent] = portSymbols flatMap { symbol =>
                    val ident = Ident(symbol.name, Nil)
                    val portRef = ExprDot(ExprIdent(instName, Nil), ident.base, Nil)
                    symbol.kind match {
                      case TypeIn(kind, fc) =>
                        List(
                          EntSplice(
                            DescIn(ident, Nil, ExprType(kind), fc)
                          ),
                          EntConnect(ExprIdent(ident.base, ident.idxs), List(portRef))
                        )
                      case TypeOut(kind, fc, _) =>
                        List(
                          EntSplice(
                            DescOut(ident, Nil, ExprType(kind), fc, StorageTypeDefault, None)
                          ),
                          EntConnect(portRef, List(ExprIdent(ident.base, ident.idxs)))
                        )
                      case _ =>
                        // TypePipeIn and TypePipe out only appear inside nested
                        // entities, and those cannot be accessed from outside,
                        // so they cannot be subject to a 'compile' directive.
                        unreachable
                    }
                  }
                  EntSplice(DescInstance(Ident(instName, Nil), Nil, p.expr)) :: portsAndConnects
                }
                val wrapper = DescEntity(ident, Nil, EntityVariant.Net, body)
                wrapper.preOrderIterator foreach { tree => if (!tree.hasLoc) tree withLocOf p }
                val result: List[Pkg] = List(
                  PkgSplice(wrapper) withLocOf p,
                  PkgCompile(ExprIdent(ident.base, ident.idxs) withLocOf ident, None) withLocOf p
                )
                list[Pkg](result, symtab) map { Thicket(_) }
              }
          }
        }
    } tap assertProgressIsReal(pkg)

  private def elaborate(
      ent: Ent,
      symtab: SymbolTable
    )(
      implicit
      cc: CompilerContext,
      fe: Frontend
    ): Result[Ent] =
    ent pipe {
      case _: EntSplice => unreachable
      case e: EntConnect =>
        list[Expr](e.lhs :: e.rhs, symtab) map {
          case l :: r => e.copy(lhs = l, rhs = r) withLocOf e
          case Nil    => unreachable
        }
      case _: EntAssign => unreachable
      case e: EntCombProcess =>
        list[Stmt](e.stmts, symtab) map { stmts =>
          e.copy(stmts = stmts) withLocOf e
        }
      case _: EntClockedProcess => unreachable
      case _: EntVerbatim       => Finished(ent)
      case _: EntComment        => unreachable
    } tap assertProgressIsReal(ent)

  private def elaborate(
      stmt: Stmt,
      symtab: SymbolTable
    )(
      implicit
      cc: CompilerContext,
      fe: Frontend
    ): Result[Stmt] =
    stmt pipe {
      case _: StmtSplice => unreachable
      case s: StmtBlock =>
        list[Stmt](s.body, symtab) map { body =>
          s.copy(body = body) withLocOf s
        }
      case s: StmtIf =>
        elaborate(s.cond, symtab) map { cond =>
          s.copy(cond = cond) withLocOf s
        } proceed { s =>
          list[Stmt](s.thenStmts, symtab) map { thenStmts =>
            s.copy(thenStmts = thenStmts) withLocOf s
          }
        } proceed { s =>
          list[Stmt](s.elseStmts, symtab) map { elseStmts =>
            s.copy(elseStmts = elseStmts) withLocOf s
          }
        }
      case s: StmtCase =>
        elaborate(s.expr, symtab) map { expr =>
          s.copy(expr = expr) withLocOf s
        } proceed { s =>
          list[Case](s.cases, symtab) map { cases =>
            s.copy(cases = cases) withLocOf s
          }
        } match {
          // Child nodes were were already finished,
          // so the whole case statement is finished
          case finished @ Finished(_) => finished
          // Just completed the child nodes. Unwind the result of 'gen'
          // expansion
          case Complete(s) =>
            def gather(trees: List[Tree]): List[Tree] = trees map {
              case Splice(tree) => tree
              case tree         => tree
            } flatMap {
              case c: CaseDefault           => Iterator.single(c)
              case c: CaseRegular           => Iterator.single(c)
              case d: DescAlias             => Iterator.single(d)
              case d: DescGenVar            => Iterator.single(d)
              case DescGenScope(_, _, body) => gather(body)
              case _                        => unreachable
            }

            val (cases, descStmts) = gather(s.cases) partitionMap {
              case c: Case => Left(c)
              case d: Desc => Right(StmtSplice(d) withLocOf d)
              case _       => unreachable
            }

            val caseStmt = s.copy(cases = cases) withLocOf s

            Complete {
              if (descStmts.isEmpty) {
                caseStmt
              } else {
                // GenVar names created by multiple iterations can clash
                // once extracted from the GenScopes, so make them unique.
                // As we only do this after the cases have been completed,
                // there are only symbolic references left to these, so
                // this renaming operation is safe. Same applies to Aliases.
                val ord = LazyList.from(0).iterator
                descStmts foreach {
                  case StmtSplice(Desc(Sym(symbol))) =>
                    symbol.name = symbol.name + cc.sep + ord.next()
                  case _ => unreachable
                }
                StmtBlock(descStmts :+ caseStmt) withLocOf s
              }
            }
          // Propagate other result
          case other => other
        }
      case s: StmtLoop =>
        list[Stmt](s.body, symtab) map { body =>
          s.copy(body = body) withLocOf s
        }
      case s: StmtWhile =>
        elaborate(s.cond, symtab) map { cond =>
          s.copy(cond = cond) withLocOf s
        } proceed { s =>
          list[Stmt](s.body, symtab) map { body =>
            s.copy(body = body) withLocOf s
          }
        }
      case s: StmtFor =>
        list[Stmt](s.inits, symtab) map { inits =>
          s.copy(inits = inits) withLocOf s
        } proceed { s =>
          expandSymtab(symtab, s.inits) flatMap { st =>
            s.condOpt map { cond =>
              elaborate(cond, st) map { cond =>
                s.copy(condOpt = Some(cond)) withLocOf s
              }
            } getOrElse Finished(s) proceed { s =>
              list[Stmt](s.steps, st) map { steps =>
                s.copy(steps = steps) withLocOf s
              }
            } proceed { s =>
              list[Stmt](s.body, st) map { body =>
                s.copy(body = body) withLocOf s
              }
            }
          }
        }
      case s: StmtDo =>
        elaborate(s.cond, symtab) map { cond =>
          s.copy(cond = cond) withLocOf s
        } proceed { s =>
          list[Stmt](s.body, symtab) map { body =>
            s.copy(body = body) withLocOf s
          }
        }
      case s: StmtLet =>
        list[Stmt](s.inits, symtab) map { inits =>
          s.copy(inits = inits) withLocOf s
        } proceed { s =>
          expandSymtab(symtab, s.inits) flatMap { st =>
            list[Stmt](s.body, st) map { body =>
              s.copy(body = body) withLocOf s
            }
          }
        }
      case _: StmtFence =>
        Finished(stmt)
      case _: StmtBreak =>
        Finished(stmt)
      case _: StmtContinue =>
        Finished(stmt)
      case s: StmtGoto =>
        elaborate(s.expr, symtab) map { e =>
          s.copy(expr = e) withLocOf s
        }
      case s: StmtReturn =>
        s.exprOpt map { expr =>
          elaborate(expr, symtab) map { e =>
            s.copy(exprOpt = Some(e)) withLocOf s
          }
        } getOrElse Finished(s)
      case s: StmtAssign =>
        elaborate(s.lhs, symtab) map { l =>
          s.copy(lhs = l) withLocOf s
        } proceed { s =>
          elaborate(s.rhs, symtab) map { r =>
            s.copy(rhs = r) withLocOf s
          }
        }
      case s: StmtUpdate =>
        elaborate(s.lhs, symtab) map { l =>
          s.copy(lhs = l) withLocOf s
        } proceed { s =>
          elaborate(s.rhs, symtab) map { r =>
            s.copy(rhs = r) withLocOf s
          }
        }
      case s: StmtPost =>
        elaborate(s.expr, symtab) map { e =>
          s.copy(expr = e) withLocOf s
        }
      case _: StmtDelayed => unreachable
      case _: StmtOutcall => unreachable
      case s: StmtExpr =>
        elaborate(s.expr, symtab) map { e =>
          s.copy(expr = e) withLocOf s
        }
      case s: StmtWait =>
        elaborate(s.cond, symtab) map { c =>
          s.copy(cond = c) withLocOf s
        }
      case _: StmtError   => unreachable
      case _: StmtComment => unreachable
    } tap assertProgressIsReal(stmt)

  private def elaborate(
      kase: Case,
      symtab: SymbolTable
    )(
      implicit
      cc: CompilerContext,
      fe: Frontend
    ): Result[Case] =
    kase pipe {
      case _: CaseSplice => unreachable
      case c: CaseRegular =>
        list[Expr](c.cond, symtab) map { cond =>
          c.copy(cond = cond) withLocOf c
        } proceed { c =>
          list[Stmt](c.stmts, symtab) map { stmts =>
            c.copy(stmts = stmts) withLocOf c
          }
        }
      case c: CaseDefault =>
        list[Stmt](c.stmts, symtab) map { stmts =>
          c.copy(stmts = stmts) withLocOf c
        }
    } tap assertProgressIsReal(kase)

  def elaborate(
      expr: Expr,
      symtab: SymbolTable
    )(
      implicit
      cc: CompilerContext,
      fe: Frontend
    ): Result[Expr] = expr pipe {
    case e: ExprCall =>
      elaborate(e.expr, symtab) map { tgt =>
        e.copy(expr = tgt) withLocOf e
      } proceed { e =>
        list[Arg](e.args, symtab) map { args =>
          e.copy(args = args) withLocOf e
        }
      } map {
        case e @ ExprCall(ExprSym(symbol), _) =>
          cc.getBuiltin(symbol) map { ExprBuiltin(_, e.args) withLocOf e } getOrElse e
        case e => e
      }
    case e: ExprBuiltin =>
      list[Arg](e.args, symtab) map { args =>
        e.copy(args = args) withLocOf e
      }
    case e: ExprUnary =>
      elaborate(e.expr, symtab) map { op =>
        e.copy(expr = op) withLocOf e
      }
    case e: ExprBinary =>
      list[Expr](e.lhs :: e.rhs :: Nil, symtab) map {
        case lhs :: rhs :: Nil => e.copy(lhs = lhs, rhs = rhs) withLocOf e
        case _                 => unreachable
      }
    case e: ExprCond =>
      list[Expr](e.cond :: e.thenExpr :: e.elseExpr :: Nil, symtab) map {
        case c :: te :: ee :: Nil => e.copy(cond = c, thenExpr = te, elseExpr = ee) withLocOf e
        case _                    => unreachable
      }
    case e: ExprRep =>
      list[Expr](e.count :: e.expr :: Nil, symtab) map {
        case count :: expr :: Nil => e.copy(count = count, expr = expr) withLocOf e
        case _                    => unreachable
      }
    case e: ExprCat =>
      list[Expr](e.parts, symtab) map { parts =>
        ExprCat(parts) withLocOf e
      }
    case e: ExprIndex =>
      list[Expr](e.expr :: e.index :: Nil, symtab) map {
        case expr :: index :: Nil => e.copy(expr = expr, index = index) withLocOf e
        case _                    => unreachable
      }
    case e: ExprSlice =>
      list[Expr](e.expr :: e.lIdx :: e.rIdx :: Nil, symtab) map {
        case expr :: lIdx :: rIdx :: Nil =>
          e.copy(expr = expr, lIdx = lIdx, rIdx = rIdx) withLocOf e
        case _ => unreachable
      }
    case e: ExprDot =>
      list[Expr](e.expr :: e.idxs, symtab) map {
        case expr :: idxs => e.copy(expr = expr, idxs = idxs) withLocOf e
        case _            => unreachable
      } proceed { e =>
        fe.nameFor(e.selector, e.idxs) map { name =>
          e.copy(selector = name, idxs = Nil) withLocOf e
        }
      }
    case e: ExprIdent =>
      list[Expr](e.idxs, symtab) map { idxs =>
        e.copy(idxs = idxs) withLocOf e
      } proceed { e =>
        fe.nameFor(e.base, e.idxs) flatMap { name =>
          symtab.get(name) match {
            case SymbolTable.Defined(symbol) => Complete(ExprSym(symbol) withLocOf e)
            case SymbolTable.Undefined       => Unknown(ReasonUnresolved(e) :: Nil)
            case _                           => unreachable // Defined covers all
          }
        }
      }
    case e: ExprCast =>
      elaborate(e.expr, symtab) map { expr =>
        e.copy(expr = expr) withLocOf e
      }
    case _: ExprSym | _: ExprSymSel | _: ExprType | _: ExprInt | _: ExprNum | _: ExprStr =>
      Finished(expr)
    case _: ExprSel | _: ExprOld | _: ExprThis | _: ExprError =>
      unreachable // Introduced later
  } pipe {
    case Complete(result) if result == expr => Finished(expr)
    case other                              => other
  } tap assertProgressIsReal(expr)

  private def elaborate(
      arg: Arg,
      symtab: SymbolTable
    )(
      implicit
      cc: CompilerContext,
      fe: Frontend
    ): Result[Arg] = arg pipe {
    case a: ArgP =>
      elaborate(a.expr, symtab) map { e =>
        a.copy(expr = e) withLocOf a
      }
    case a: ArgN =>
      elaborate(a.expr, symtab) map { e =>
        a.copy(expr = e) withLocOf a
      }
    case a: ArgD =>
      list[Expr](a.idxs, symtab) map { idxs =>
        a.copy(idxs = idxs) withLocOf a
      } proceed { a =>
        fe.nameFor(a.base, a.idxs) map { name =>
          ArgN(name, a.expr) withLocOf a
        }
      } proceed {
        elaborate(_, symtab)
      }
  } tap assertProgressIsReal(arg)

}
