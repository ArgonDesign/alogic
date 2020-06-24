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
// Specialize parameters, process 'gen' constructs and split Desc to Decl/Defn
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.specialize

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types.Type
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.AnsiColor
import scala.util.chaining._

// The word "specialization" in this context means splitting a source level
// Desc definition into a pair of (Decl, Defn) definition which are fully
// describe the symbol. This means that the final resulting (Decl, Defn)
// will have the following properties:
//  - Have no more references to parametrized symbols (they will have been
//    replaced by references to the specialized symbols)
//  - All nested parametrized symbol definitions will have been replaced by
//    a collection of all their specializations
//  - All Gen instances have been expanded
//  - All dictionary identifier references have been resolved
//  - All dictionary select operations have been resolved

// The high level flow of the algorithm is described below. Note that the
// algorithm is recursive, proceeding in a depth-first traversal of the
// design hierarchy (or more precisely, of the symbol dependency graph). We
// rely on caching to avoid doing repeated specializations (memoization).
// Circular definitions are reported by detected the depth first traversal
// entering a node already on the current path. Note that great care has been
// taken to allow circular references, so long as they do not introduce
// definitions with circular types. This is achieved by splitting Desc nodes
// to Decl/Defn (and hence determining the type of the symbol) as soon as
// possible even if parts of the Defn still remain un-specialized (which can
// later depend on the current symbol). This is necessary for the support of
// recursive functions but also supports all coherent circular references
// (but not circular definitions).

// The algorithm is roughly as follows:
//
// Start out with a Desc, and some potentially empty set of parameter bindings
//
// 1. Substitute given parameters (if any)
// 2. Split into Decl/Defn if all of the following holds:
//    - There are no unbound parameters of the Desc
//    - There are no Gen that can generate parameters of the Desc
//    - All contents within the Desc that influence the type of the symbol
//      introduced by the Desc can be fully specialized
//    - There are no Gen in the Desc that can generate contents that influence
//      the type of the symbol introduced by the Desc
//    We call this step that splits the Desc to Decl/Defn "typing
//    specialization". If typing specialization is not possible due to some of
//    the conditions above, we proceed with the original Desc.
// 3. Attempt to specialize all non-parametrized Descs contained within the
//    Desc/Defn that are not inside a Gen body. Also specialize references to
//    non-specialized symbols in all remaining expressions within the Desc that
//    are not inside a Gen body or a nested Desc.
// 4. If any of the non-specialized references (outside Gen bodies) remaining
//    point to symbols not defined within this definition, (i.e.: they are to
//    an outer/external symbol), then we cannot proceed without specializing
//    those external symbols first so at this point the specialization is
//    unknown.
// 5. If a Gen exists in the body of this Desc/Defn (but not inside a nested
//    Desc, which will be expanded as part of specializing that Desc), process
//    Gen nodes.
//    - If no Gen was expand:
//      - If there are no unbound parameters, issue an error as the Gen
//        conditions/header must have a circular dependency.
//      - else return partial specialization
//    - else:
//      - If still working on Desc goto 1, with unused parameters as input
//      - else goto 3
//    If no Gen exists
//      - If left with a Desc then we must have unbound parameters, so return
//        a partial specialization result
//      - If left with a Decl/Defn, the specialization is complete. Rename the
//        specialized symbol based on the actual parameter values and return
//        the fully specialized result

// format: off
// Specialization result indicators
private[specialize] sealed trait DescSpecialization
// Specialization failed
private[specialize] sealed trait DescSpecializationError extends DescSpecialization
// Specialization failed due to named param bindings required
private[specialize] case object DescSpecializationErrorNeedsNamed extends DescSpecializationError
// Specialization failed due to other error
private[specialize] case object DescSpecializationErrorOther extends DescSpecializationError

// Specialization failed due to dependency on unresolved choice symbols
private[specialize] case class DescSpecializationUnknown(symbols: collection.Set[Symbol]) extends DescSpecialization {
  require(symbols forall { _.isChoice})
}
// Specialization is in progress but we already know the resulting Decl
private[specialize] case class DescSpecializationPending(decl: Decl) extends DescSpecialization
// Specialization successful with no more unbound parameters remaining,
// with the given final parameter values
private[specialize] case class DescSpecializationComplete(decl: Decl, defn: Defn, paramValues: Map[String, Either[BigInt, Type]]) extends DescSpecialization
// format: on

private[specialize] class SpecializeDesc(implicit cc: CompilerContext) {

  // To be passed to sub functions that might need to recursively specialize
  // other Desc nodes (i.e.: depth first traversal).
  implicit val self: SpecializeDesc = this

  // Cache of ('original symbol', 'param bindings', 'use defaults') -> 'specialization result'
  // Kept as an var to an immutable for easy reverting
  private[this] var cache: Map[(Symbol, ParamBindings, Boolean), DescSpecialization] = Map.empty

  // To detect circular and divergent definitions, we keep track of the pending
  // specializations (current active path in the depth first traversal) on a
  // stack. To avoid having to do a linear time search of the stack all the
  // time, and to provide better error messages, we also keep a map holding the
  // same keys as the stack, with the source location of the reference requiring
  // the specialization as the value.
  private[this] val pendingStack = mutable.Stack[(Symbol, (ParamBindings, Boolean))]()
  private[this] val pendingMap = mutable.HashMap[(Symbol, (ParamBindings, Boolean)), Loc]()

  // The below is just debug aid for the developer
  private[this] val dumpEnable = cc.settings.traceElaborate
  private[this] var prevDumped: Either[Desc, (Decl, Defn)] = _

  // $COVERAGE-OFF$ debug code
  private[this] def printWithPrefix(lines: String*): Unit = if (dumpEnable) {
    val prefix = pendingStack.reverse map {
      case (symbol, _) => s"${symbol.name}@${symbol.id}"
    } mkString (" ", AnsiColor.WHITE_B + " " + AnsiColor.RESET, " ")
    lines foreach { line =>
      println(prefix + line)
    }
  }

  private[this] def dump(label: String)(item: Either[Desc, (Decl, Defn)]): Unit =
    if (dumpEnable) {
      import AnsiColor._
      if (item != prevDumped) {
        printWithPrefix(BOLD + label + RESET)
        val text = item
          .fold(_.toSource, { case (decl, defn) => decl.toSource + "\n" + defn.toSource })
          .replaceAll("\\bparam\\b", BOLD + RED + "param" + RESET)
          .replaceAll("\\bconst\\b", BOLD + GREEN + "const" + RESET)
          .replaceAll("\\bgen\\b", BOLD + YELLOW + "gen" + RESET)
          .replaceAll("\\bchoice\\b", BOLD + YELLOW + "choice" + RESET)
          .replaceAll("\\bdesc\\b", BOLD + MAGENTA + "desc" + RESET)
          .replaceAll("\\bdecl\\b", BOLD + CYAN + "decl" + RESET)
          .replaceAll("\\bdefn\\b", BOLD + BLUE + "defn" + RESET)
        printWithPrefix(text.split("\n").toIndexedSeq: _*)
      } else {
        printWithPrefix(BOLD + label + WHITE + " SAME" + RESET)
      }

      if (cc.hasError) {
        cc.fatal("Stopping due to errors")
      }
      prevDumped = item
    }

  // $COVERAGE-ON$

  // Predicate to see if Desc has any Gen that might influence the type
  // of the introduced symbol
  private[this] def hasTypingGen(desc: Desc): Boolean = desc match {
    case DescEntity(_, _, body) =>
      body exists {
        case _: EntGen => true
        case _         => false
      }
    case DescRecord(_, body) =>
      body exists {
        case _: RecGen => true
        case _         => false
      }
    case DescSingleton(_, _, body) =>
      body exists {
        case _: EntGen => true
        case _         => false
      }
    case _ => false
  }

  // Entry point to the actual specialization algorithm
  private[this] def step1(
      desc: Desc,
      paramBindings: ParamBindings,
      useDefaultParameters: Boolean,
      loc: Loc
    ): DescSpecialization = {

    ////////////////////////////////////////////////////////////////////////////
    // Step 1: Substitute parameters with the given expression
    ////////////////////////////////////////////////////////////////////////////

    SubstituteParams(desc, paramBindings) match {
      case ParamSubstitutionUnbound(unbound) =>
        // Unbound parameter remains
        unbound foreach { d: Desc =>
          cc.error(loc, s"'${desc.name}' requires parameter '${d.name}'")
        }
        DescSpecializationErrorOther
      case ParamSubstitutionNeedsNamed =>
        // Named bindings are required but positional were given
        DescSpecializationErrorNeedsNamed
      case ParamSubstitutionComplete(desc, unusedBindings) =>
        // Substitution successful

        dump("Substituted")(Left(desc))

        ////////////////////////////////////////////////////////////////////////////
        // Step 2: Attempt typing specialization
        ////////////////////////////////////////////////////////////////////////////

        val typingSpecialized: Option[Either[Desc, (Decl, Defn)]] =
          if (hasTypingGen(desc) || desc.isParametrized) {
            Some(Left(desc))
          } else {
            SpecializeTyping(desc) match {
              case TypingSpecializationError                => None
              case TypingSpecializationUnknown              => Some(Left(desc))
              case TypingSpecializationComplete(decl, defn) =>
                // Put the DescSpecializationPending result into the cache so recursive
                // calls can pick up the decl if needed to resolve circular references
                val (symbol, (pb, udp)) = pendingStack.top
                val key = (symbol, pb, udp)
                assert(!(cache contains key))
                cache = cache + (key -> DescSpecializationPending(decl))
                Some(Right((decl, defn)))
            }
          }

        typingSpecialized foreach dump("Typing specialized")

        typingSpecialized match {
          case None       => DescSpecializationErrorOther
          case Some(item) => step3(item, unusedBindings, useDefaultParameters, loc)
        }
    }
  }

  @tailrec
  private[this] def step3(
      input: Either[Desc, (Decl, Defn)],
      unusedBindings: ParamBindings,
      useDefaultParameters: Boolean,
      loc: Loc
    ): DescSpecialization = {

    ////////////////////////////////////////////////////////////////////////////
    // Step 3: Specialize contained Descs
    ////////////////////////////////////////////////////////////////////////////

    val containedSpecialized = SpecializeContained(input)

    containedSpecialized foreach {
      case (item, _) => dump("Contained specialized")(item)
    }

    val checked = containedSpecialized flatMap {
      case (item, ucs) => CheckStaticAssertions(item) map { (_, ucs) }
    }

    checked foreach {
      case (item, _) => dump("Checked")(item)
    }

    checked match {
      case None                               => DescSpecializationErrorOther
      case Some((item, unknownChoiceSymbols)) =>
        ////////////////////////////////////////////////////////////////////////
        // Step 4: Stop if external un-specialized dependency remains
        ////////////////////////////////////////////////////////////////////////
        val definedChoiceSymbols = Set from {
          item.fold(identity, _._2) collect { case d: DescChoice => d.symbol }
        }
        val unresolvedChoiceSymbols = unknownChoiceSymbols diff definedChoiceSymbols

        if (unresolvedChoiceSymbols.nonEmpty) {
          DescSpecializationUnknown(unresolvedChoiceSymbols)
        } else {
          ////////////////////////////////////////////////////////////////////////////
          // Step 5: check for generates
          ////////////////////////////////////////////////////////////////////////////
          if (Generate.hasGen(item)) {
            // If the workpiece still has Gen nodes, process them
            val expanded = Generate(item)

            // TODO: error if dictident without gen...

            expanded foreach dump("Generate expanded")

            expanded match {
              // Propagate error
              case None => DescSpecializationErrorOther
              // If no gen nodes were expanded
              case Some(`item`) => cc.fatal("Circular 'gen'")
              // Otherwise go again
              case Some(Left(desc)) => step1(desc, unusedBindings, useDefaultParameters, loc)
              case Some(item)       => step3(item, unusedBindings, useDefaultParameters, loc)
            }
          } else {
            // If there are no Gen nodes left, we are done
            item match {
              case Left(_) =>
                unreachable // TODO: implement partial specialization
              case Right((decl, defn)) =>
                unusedBindings match {
                  case ParamBindingsPositional(bindings) if bindings.nonEmpty => unreachable
                  case ParamBindingsNamed(unused) if unused.nonEmpty =>
                    unused.iterator map {
                      case ((name, Nil), value) => (name, value)
                      case ((name, idxs), value) =>
                        (idxs.mkString(s"$name#[", ", ", "]"), value)
                    } foreach {
                      case (sourceName, value) =>
                        cc.error(value, s"'${decl.symbol.name}' has no parameter '$sourceName'")
                    }
                    DescSpecializationErrorOther
                  case _ =>
                    Finalize(decl, defn) match {
                      case None => DescSpecializationErrorOther
                      case Some((fDecl, fDefn, paramValues)) =>
                        dump("Finalized")(Right((fDecl, fDefn)))
                        DescSpecializationComplete(fDecl, fDefn, paramValues)
                    }
                }
            }
          }
        }
    }
  }

  // Entry point to the actual specialization algorithm
  private[this] def specialize(
      desc: Desc,
      paramBindings: ParamBindings,
      useDefaultParameters: Boolean,
      loc: Loc
    ): DescSpecialization = {
    require(desc.ref.asInstanceOf[Sym].idxs.isEmpty)
    require(useDefaultParameters) // TODO: implement partial specialization

    dump("Input " + paramBindings)(Left(desc))

    ////////////////////////////////////////////////////////////////////////////
    // Step 0: Check if the Desc has references to external choice symbols,
    // and bail early if it does, otherwise clone the input Desc so we have a
    // tree independent of the input Desc that we can work on iteratively
    // without affecting original symbols
    ////////////////////////////////////////////////////////////////////////////

    val definedChoiceSymbols = Set from {
      desc collect { case d: DescChoice => d.symbol }
    }
    val referencedChoiceSymbols = Set from {
      desc collect {
        case ExprSym(symbol) if symbol.isChoice && !symbol.attr.eliminated.isSet => symbol
        case Sym(symbol, _) if symbol.isChoice && !symbol.attr.eliminated.isSet  => symbol
      }
    }
    val externalChoiceRefs = referencedChoiceSymbols diff definedChoiceSymbols

    if (externalChoiceRefs.nonEmpty) {
      DescSpecializationUnknown(externalChoiceRefs)
    } else {
      val cloned = Clone(desc)

      dump("Cloned")(Left(cloned))

      step1(cloned, paramBindings, useDefaultParameters, loc)
    }
  }

  // Recursive entry point with circularity check
  private[this] def attempt(
      desc: Desc,
      paramBindings: ParamBindings,
      useDefaultParameters: Boolean,
      refLoc: Loc
    ): DescSpecialization = {
    //////////////////////////////////////////////////////////////////////////
    // Check for circular and divergent specialization
    //////////////////////////////////////////////////////////////////////////

    // TODO: detect divergence, currently it will just stack overflow

    val tag = (desc.symbol, (paramBindings, useDefaultParameters))

    pendingMap.get(tag) match {
      case Some(startLoc) =>
        // Already in progress. Report the cycle
        val color = cc.colorOpt(AnsiColor.GREEN + AnsiColor.BOLD)

        val msg = new ListBuffer[String]

        def bindingsNote(bindings: ParamBindings): Unit = bindings match {
          case ParamBindingsPositional(params) if params.nonEmpty =>
            msg += params map {
              _.toSource
            } mkString ("with parameter values: ", ", ", "")
          case ParamBindingsNamed(params) if params.nonEmpty =>
            msg += params map {
              case ((name, Nil), v)  => s"$name = ${v.toSource}"
              case ((name, idxs), v) => s"$name#[${idxs.mkString(", ")}] = ${v.toSource}"
            } mkString ("with parameter assignments: ", ", ", "")
          case _ =>
        }

        def sourceName(symbol: Symbol): String = symbol.attr.dictName.get match {
          case Some((name, Nil))  => name
          case Some((name, idxs)) => idxs mkString (s"$name#[", ", ", "]")
          case None               => symbol.name
        }

        msg += s"Definition of '${sourceName(desc.symbol)}' is circular:"
        bindingsNote(paramBindings)
        msg += s"defined at ${desc.symbol.loc.prefix}"
        msg ++= desc.symbol.loc.context(color).split("\\s*\n") map { "  " + _ }

        def addEntry(
            symbol: Symbol,
            bindings: ParamBindings,
            loc: Loc,
            last: Boolean = false
          ): Unit = {
          msg += s"depends on '${sourceName(symbol)}' via ${loc.prefix}"
          bindingsNote(bindings)
          msg ++= loc.context(color).split("\\s*\n") map { "  " + _ }
          if (!last) {
            msg += s"defined at ${symbol.loc.prefix}"
            msg ++= symbol.loc.context(color).split("\\s*\n") map { "  " + _ }
          }
        }

        (pendingStack takeWhile { _ != tag }).reverse filter {
          // Don't print dependencies via nested definitions
          case t @ (symbol, _) => symbol.loc != pendingMap(t)
        } foreach {
          case t @ (symbol, (bindings, _)) => addEntry(symbol, bindings, pendingMap(t))
        }

        addEntry(desc.symbol, paramBindings, refLoc, last = true)

        cc.error(startLoc, msg.toList: _*)

        DescSpecializationErrorOther

      case None =>
        // Good to go
        pendingMap(tag) = refLoc
        pendingStack push tag
        val result = specialize(desc, paramBindings, useDefaultParameters, refLoc)
        pendingMap.remove(tag)
        pendingStack.pop

        // Some sanity checks
        result match {
          case DescSpecializationComplete(_, defn, _) =>
            // Note: References remaining to non-specialized things will
            // be caught by the type checker later.
            defn visit {
              case desc: Desc if !desc.isParametrized =>
                cc.ice(desc, "Non-parametrized Desc remains in Defn")
              case _: Desc => // OK
              case EntDecl(decl) =>
                cc.ice(decl, "EntDecl remains in Defn")
              case RecDecl(decl) =>
                cc.ice(decl, "RecDecl remains in Defn")
              case node: AssertionStatic =>
                cc.ice(node, "AssertionStatic remains in Defn")
            }
          case _ =>
        }

        // Done
        result
    }
  }

  // Specialize Desc given bindings for parameters (given either as positional
  // parameter or a map of by name bindings), consulting cache
  def apply(
      desc: Desc,
      paramBindings: ParamBindings,
      useDefaultParameters: Boolean,
      refLoc: Loc
    ): DescSpecialization = {
    require(desc.symbol.desc eq desc, "Not canonical desc")
    require(useDefaultParameters, "Not yet implemented")
    require(!desc.isInstanceOf[DescChoice], "Cannot specialize DescChoice")

    val emptyParams = paramBindings match {
      case ParamBindingsPositional(params) => params.isEmpty
      case ParamBindingsNamed(params)      => params.isEmpty
    }

    assert(
      !(desc.isParametrized && emptyParams && !useDefaultParameters),
      "pointless specialization"
    )

    val simplifiedParamBindings = SimplifyParamBindings(paramBindings)

    // If we have already performed this specialization, return the result of
    // that, otherwise perform the work. Note that we kind of have to do double
    // caching of the results because the parameter values can contain ticks,
    // which cannot be fully evaluated until after we have performed the
    // complete specialization.
    val key = (desc.symbol, simplifiedParamBindings, useDefaultParameters)

    val savedCache = cache

    cache.get(key) pipe {
      case Some(result) => result
      case None         =>
        // Here is how to actually compute the result if it is not cached
        attempt(desc, simplifiedParamBindings, useDefaultParameters, refLoc) pipe {
          case result @ DescSpecializationComplete(_, _, paramValues) =>
            // The specialization is complete. Now we check if we have ended
            // up with these param values before and if so, we throw away what
            // we just did and return that equivalent specialization instead.
            // This is to avoid doing extra work in the remaining passes and
            // subsequently emitting identical code in the output.
            // TODO: could use a structure here that is faster to look up
            cache.iterator collectFirst {
              case ((symbol, _, _), cached: DescSpecializationComplete)
                  if symbol == desc.symbol && cached.paramValues == paramValues =>
                cached
            } getOrElse result
          case other =>
            // Other result
            other
        } tap {
          // $COVERAGE-OFF$ debug code
          case _ if !dumpEnable => // Nothing
          case DescSpecializationUnknown(symbols) =>
            printWithPrefix(s"Result: ${desc.ref.toSource} -> DescSpecializationUnknown")
            symbols foreach { symbol => printWithPrefix(s"Unknown: $symbol") }
          case r =>
            printWithPrefix(s"Result: ${desc.ref.toSource} -> ${r.getClass.getSimpleName}")
          // $COVERAGE-ON$
        } tap {
          // Update the cache
          case _: DescSpecializationUnknown =>
            // Result is unknown. Revert cache to initial state as we might have
            // added specialization dependent on this result (which is unknown).
            // Do not add this result so we can try again when other symbols
            // this desc depends on have been resolved.
            cache = savedCache
          case result: DescSpecializationError =>
            // Result is error. We will eventually fail anyway, so do not
            // revert the cache, as doing so might cause error messages to
            // be generated multiple times
            cache = cache + (key -> result)
          case _: DescSpecializationPending =>
            // We have just finished a specialization, so it will never be
            // pending...
            unreachable
          case result: DescSpecializationComplete =>
            // All good, add result to cache
            cache = cache + (key -> result)
        }
    } tap {
      case DescSpecializationErrorNeedsNamed =>
        cc.error(refLoc, "Type with more than one parameter requires named parameter assignment")
      case _ =>
    }
  }

  def apply(specs: Iterable[Expr]): Option[Set[(Decl, Defn)]] = {
    val results = specs filter {
      case expr @ ExprSym(symbol) if symbol.isParametrized =>
        cc.error(expr, s"Parametrized type requires parameter list")
        false
      case _ => true
    } map {
      SpecializeExpr(_)
    } flatMap {
      case ExprSpecializationComplete(ExprSym(symbol)) => Some(symbol)
      case ExprSpecializationError                     => None
      case _                                           => unreachable
    }

    if (results.sizeIs != specs.size) {
      // There was an error, so return None
      None
    } else {
      assert(results forall { _.isSpecialized })

      // Return all complete specializations of the given parametrized Descs
      Some {
        // All specialized dependencies of given definition, including itself
        def dependencies(decl: Decl, defn: Defn): Iterator[(Decl, Defn)] = {
          object ReplaceParametrizedWithSpecializations extends StatefulTreeTransformer {
            override protected val typed = false

            private val extraDecls = mutable.Map[Symbol, Iterator[Decl]]()

            def specializations(symbol: Symbol): List[(Decl, Defn)] = List from {
              val pairs = cache.iterator collect {
                case ((`symbol`, _, _), DescSpecializationComplete(decl, defn, _)) => (decl, defn)
              }
              // Keep only distinct results, as the above might have duplicates
              // due to different param assignments actually yielding the same
              // bindings.
              pairs.distinct
            }

            override def enter(tree: Tree): Option[Tree] = tree match {
              case EntDesc(desc) =>
                val (specialDecls, specialDefns) =
                  specializations(desc.symbol).unzip
                extraDecls.updateWith(enclosingSymbols.head) {
                  case None       => Some(specialDecls.iterator)
                  case Some(iter) => Some(iter concat specialDecls.iterator)
                }
                Some(Thicket(specialDefns map { EntDefn(_) withLoc tree.loc }))
              case RecDesc(desc) =>
                val (specialDecls, specialDefns) = specializations(desc.symbol).unzip
                extraDecls.updateWith(enclosingSymbols.head) {
                  case None       => Some(specialDecls.iterator)
                  case Some(iter) => Some(iter concat specialDecls.iterator)
                }
                Some(Thicket(specialDefns map { RecDefn(_) withLoc tree.loc }))
              case _ => None
            }

            override def transform(tree: Tree): Tree = tree match {
              case decl @ DeclEntity(symbol, decls) =>
                extraDecls.get(symbol) map { extra =>
                  val newDecls = List.from(decls.iterator concat extra)
                  decl.copy(decls = newDecls) withLoc tree.loc
                } getOrElse tree
              case decl @ DeclRecord(symbol, decls) =>
                extraDecls.get(symbol) map { extra =>
                  val newDecls = List.from(decls.iterator concat extra)
                  decl.copy(decls = newDecls) withLoc tree.loc
                } getOrElse tree
              case decl @ DeclSingleton(symbol, decls) =>
                extraDecls.get(symbol) map { extra =>
                  val newDecls = List.from(decls.iterator concat extra)
                  decl.copy(decls = newDecls) withLoc tree.loc
                } getOrElse tree
              case _ => tree
            }
          }

          // Replace parametrized members with their specializations
          val sDefn = defn rewrite ReplaceParametrizedWithSpecializations
          val sDecl = decl rewrite ReplaceParametrizedWithSpecializations

          prevDumped = null
          dump("Final")(Right((sDecl, sDefn)))

          // Set of symbols defined inside this definition
          val definedSymbols = Set from {
            Iterator(sDecl, sDefn) flatMap {
              _ collectAll { case d: Decl => d.symbol }
            }
          }

          // Set of symbols referenced but not defined inside this definition
          val requiredSymbols = Set from {
            Iterator(sDecl, sDefn) flatMap {
              _ flatCollect {
                case _: Desc                                                         => unreachable
                case ExprSym(symbol) if !definedSymbols(symbol) && !symbol.isBuiltin => Some(symbol)
              }
            }
          }

          // Iterator of all dependencies, including self note that references
          // to non-specialized symbols might remain in malformed programs.
          // These will be flagged by the type checker
          Iterator.single((sDecl, sDefn)) ++ {
            requiredSymbols.iterator filter { _.isSpecialized } flatMap { symbol =>
              dependencies(symbol.decl, symbol.defn)
            }
          }
        }

        Set from {
          results.iterator flatMap { symbol =>
            dependencies(symbol.decl, symbol.defn)
          }
        }
      }
    }
  } tap {
    case None          =>
    case Some(results) =>
      // Quick sanity check: All definitions must have been fully specialized
      def check(tree: Tree): Unit = tree visit {
        case Desc(ref) =>
          cc.ice(ref, "Unspecialized definition remains")
        case node: ExprRef =>
          cc.ice(node, "ExprRef remains")
        case node: DescChoice =>
          cc.ice(node, "DescChoice remains")
      }
      results tapEach {
        case (decl, defn) =>
          check(decl)
          check(defn)
      }
  }

}
