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
// Process 'gen' constructs in all unparametrized decls
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.specialize

import com.argondesign.alogic.analysis.StaticEvaluation
import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Bindings
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FuncVariant
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.util.BigIntOps._
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

private[specialize] object Generate {

  // Predicate to see if item has any Gen (but not inside a nested Desc/Defn)
  def hasGen(item: Either[Desc, (Decl, Defn)]): Boolean = {
    def checkStmts(stmts: List[Stmt]): Boolean = stmts exists {
      _ exists {
        case _: Gen  => true
        case _: Expr => false // Stop descent
      }
    }
    def checkEnts(ents: List[Ent]): Boolean = ents exists {
      case _: EntGen             => true
      case EntCombProcess(stmts) => checkStmts(stmts)
      case _                     => false
    }
    def checkRecs(recs: List[Rec]): Boolean = recs exists {
      case _: RecGen => true
      case _         => false
    }
    item.fold(identity, _._2) match {
      case DescEntity(_, _, body)     => checkEnts(body)
      case DefnEntity(_, _, body)     => checkEnts(body)
      case DescRecord(_, body)        => checkRecs(body)
      case DefnRecord(_, body)        => checkRecs(body)
      case DescSingleton(_, _, body)  => checkEnts(body)
      case DefnSingleton(_, _, body)  => checkEnts(body)
      case DescFunc(_, _, _, _, body) => checkStmts(body)
      case DefnFunc(_, _, body)       => checkStmts(body)
      case _                          => false
    }
  }

  // Type class used for handling context specific processing of generate
  private trait Generatable[T <: Tree] {
    // String describing what kind of content a generate needs to yield
    val description: String
    // Predicate indicating whether this tree is valid generate content in this context
    def isValid(tree: Tree): Boolean
    // Transform generate content to expected content
    def splice(tree: Tree): T
  }

  // Type class instance for Ent
  implicit private object generatableEnt extends Generatable[Ent] {
    val description = "entity content"

    def isValid(tree: Tree): Boolean = tree match {
      case _: Gen             => true
      case _: Ent             => true
      case _: Desc            => true
      case _: AssertionStatic => true
      case _                  => false
    }

    def splice(tree: Tree): Ent = tree match {
      case gen: Gen => EntGen(gen) withLoc gen.loc
      case ent: Ent => ent
      case desc: DescFunc if desc.variant == FuncVariant.None =>
        val newDesc = desc.copy(variant = FuncVariant.Ctrl) withLoc desc.loc
        EntDesc(newDesc) withLoc desc.loc
      case desc: Desc           => EntDesc(desc) withLoc desc.loc
      case assertion: Assertion => EntAssertion(assertion) withLoc assertion.loc
      case _                    => unreachable
    }

  }

  // Type class instance for Rec
  implicit private object generatableRec extends Generatable[Rec] {
    val description = "struct content"

    def isValid(tree: Tree): Boolean = tree match {
      case _: Gen             => true
      case _: Rec             => true
      case _: DescVar         => true
      case _: DescParam       => true
      case _: DescParamType   => true
      case _: DescConst       => true
      case _: DescType        => true
      case _: DescRecord      => true
      case _: DescFunc        => true
      case _: DescChoice      => true
      case _: AssertionStatic => true
      case _                  => false
    }

    def splice(tree: Tree): Rec = tree match {
      case gen: Gen => RecGen(gen) withLoc gen.loc
      case rec: Rec => rec
      case desc: DescFunc if desc.variant == FuncVariant.None =>
        val newDesc = desc.copy(variant = FuncVariant.Comb) withLoc desc.loc
        RecDesc(newDesc) withLoc desc.loc
      case desc: Desc           => RecDesc(desc) withLoc desc.loc
      case assertion: Assertion => RecAssertion(assertion) withLoc assertion.loc
      case _                    => unreachable
    }

  }

  // Type class instance for Stmt
  implicit private object generatableStmt extends Generatable[Stmt] {
    val description = "statement"

    def isValid(tree: Tree): Boolean = tree match {
      case _: Gen       => true
      case _: Stmt      => true
      case _: Desc      => true
      case _: Assertion => true
      case _            => false
    }

    def splice(tree: Tree): Stmt = tree match {
      case gen: Gen             => StmtGen(gen) withLoc gen.loc
      case stmt: Stmt           => stmt
      case DescConst(r, s, i)   => StmtDesc(DescVal(r, s, i) withLoc tree.loc) withLoc tree.loc
      case desc: Desc           => StmtDesc(desc) withLoc desc.loc
      case assertion: Assertion => StmtAssertion(assertion) withLoc assertion.loc
      case _                    => unreachable
    }

  }

  // Type class instance for Case
  implicit private object generatableCase extends Generatable[Case] {
    val description = "case clause"

    def isValid(tree: Tree): Boolean = tree match {
      case _: Gen  => true
      case _: Case => true
      case _       => false
    }

    def splice(tree: Tree): Case = tree match {
      case gen: Gen   => CaseGen(gen) withLoc gen.loc
      case kase: Case => kase
      case _          => unreachable
    }

  }

  def values(
      idxs: List[Expr],
      bindings: Bindings
    )(
      implicit
      cc: CompilerContext
    ): Option[List[BigInt]] = idxs match {
    case Nil => Some(Nil)
    case head :: tail =>
      if (!cc.typeCheck(head)) {
        values(tail, bindings) // Compute it anyway to emit all errors
        None
      } else {
        (head given bindings).value match {
          case Some(value) =>
            values(tail, bindings) map { value :: _ }
          case None =>
            cc.error(head, "Identifier index must be a compile time constant")
            values(tail, bindings) // Compute it anyway to emit all errors
            None
        }
      }
  }

  def apply(
      input: Either[Desc, (Decl, Defn)]
    )(
      implicit
      cc: CompilerContext
    ): Option[Either[Desc, (Decl, Defn)]] = {
    assert(input.left forall { _.params.isEmpty })
    assert(input.left forall { _.ref.asInstanceOf[Sym].idxs.isEmpty })

    // Map from symbol to cloned symbol of valid choices
    val validChoices = mutable.Map[Symbol, Symbol]()

    // Whether this choice has not been eliminated
    val potentialChoices = mutable.Set[Symbol]()

    final class Process[T <: Tree](
        // Bindings for symbols with known values (gen variables)
        bindings: Bindings,
        // The symbols already cloned externally (generated symbols that are used outside)
        symbolMap: Map[Symbol, Symbol]
      )(
        implicit
        cc: CompilerContext)
        extends StatefulTreeTransformer {

      override val typed: Boolean = false

      // Mark if we made any progress
      var progress = false

      // Latch errors
      var hadError = false

      private[this] def error(loc: Loc, msg: String*): Unit = {
        cc.error(loc, msg: _*)
        hadError = true
      }

      private[this] def error(tree: Tree, msg: String*): Unit = error(tree.loc, msg: _*)

      private def cloneSymbolsInDescs(
          descs: List[Desc],
          bindings: Bindings,
          suffix: String,
          inGenIf: Boolean
        ): List[(Symbol, Symbol)] = descs flatMap {
        // Scalar symbols
        case desc: Desc if desc.ref.idxs.isEmpty =>
          // Get the introduced symbol
          val symbol = desc.symbol
          symbolMap.get(symbol) match {
            case Some(cloneSymbol) =>
              // Symbol already cloned in outer scope. This can only happen
              // because it is a choice in an outer DescChoice (see recursive
              // cloning below). Nothing to do, just mark as a valid choice
              // now that it is actually being generated. Note that if this
              // has been cloned, then the outer choice reference will have
              // also been replaced by the clone, so map as identity
              validChoices(cloneSymbol) = cloneSymbol
              Nil
            case None =>
              // Clone the symbol
              val cloneSymbol = symbol.dup
              // Rename the clone by appending the suffix
              cloneSymbol.name = cloneSymbol.name + suffix
              if (inGenIf) {
                // Mark as valid choice, remember mapping to clone.
                validChoices(symbol) = cloneSymbol
              }
              // If this is a choice symbol, recursively clone all possible scalar
              // choices as well, as the choices are really introduced into this
              // scope, and they need to be unique symbols in case this choice
              // is inside a Gen loop itself. See the specialize_hard_02 test.
              def choiceClones(intr: Desc): List[(Symbol, Symbol)] = intr match {
                case DescChoice(_, choices) =>
                  choices flatMap {
                    case ExprSym(choice) if choice.desc.ref.idxs.isEmpty =>
                      // These cannot have been encountered yet
                      assert(!(symbolMap contains choice), "Already cloned in this context")
                      // Clone and rename
                      val choiceClone = choice.dup
                      choiceClone.name = choiceClone.name + suffix
                      (choice -> choiceClone) :: choiceClones(choice.desc)
                    case _ => Nil
                  }
                case _ => Nil
              }
              // Add to context local mapping
              (symbol -> cloneSymbol) :: choiceClones(desc)
          }

        // Dict symbols
        case desc: Desc =>
          // Get the introduced symbol
          val symbol = desc.symbol
          // Sanity check
          assert(!(symbolMap contains symbol), "Dict symbol must not be cloned")
          assert(!(validChoices contains symbol), "Dict symbol must not be in validChoices")
          assert(!desc.isInstanceOf[DescChoice], "Choice symbols are never dict")
          // At this point the indices must be known, or it's an error
          values(desc.ref.idxs, bindings) match {
            case None            => hadError = true
            case Some(idxValues) =>
              // Clone the symbol, this is the specific instance
              val newSymbol = symbol.dup
              // Clear attribute on clone, just in case it got inherited
              newSymbol.attr.dictResolutions.clear()

              // Rename based on value of indices
              newSymbol.name = idxValues
                .map(n => if (n < 0) s"n${-n}" else n)
                .mkString(symbol.name + cc.sep, "_", "")

              // Set the attribute used ot resolve external dict references
              newSymbol.attr.sourceName.set((symbol.name, idxValues))

              // Add to dictResolutions, error if already exists
              symbol.attr.dictResolutions
                .getOrElseUpdate(mutable.Map())
                .put(idxValues, newSymbol) match {
                case None => // OK, first definition with these indices
                case Some(_) =>
                  val name = idxValues.mkString(symbol.name + "#[", ", ", "]")
                  error(
                    desc.ref,
                    s"'$name' defined multiple times after processing 'gen' constructs"
                  )
              }
          }
          Nil
      }

      // Compute the generated tree from generate
      private def generate[U <: Tree: Generatable](gen: Gen): Option[List[Tree]] = {
        val dispatcher: Generatable[U] = implicitly[Generatable[U]]

        def process(bindings: Bindings, trees: List[Tree]): List[Tree] = {
          // Check for 'gen' content invalid in this context
          val (valid, invalid) = trees partition dispatcher.isValid

          // Issue error for any invalid contents
          lazy val hint = dispatcher.description
          invalid foreach {
            error(_, s"'gen' construct yields invalid syntax, $hint is expected")
          }

          // Clone symbols introduced in the scope of this Gen, and build the
          // symbol map of the gen context.
          val newSymbolMap = symbolMap ++ {
            // Gather Descs in the Gen scope, but not from nested Gens
            val (directScalarDescs, otherDescs) = {
              val direcScalars = mutable.ListBuffer[Desc]()
              val other = mutable.ListBuffer[Desc]()

              object GatherDescs extends StatelessTreeTransformer {
                override val typed = false
                var level = 0
                override def enter(tree: Tree): Option[Tree] = tree match {
                  case desc: Desc =>
                    if (level == 0 && desc.ref.idxs.isEmpty) {
                      direcScalars append desc
                    } else {
                      other append desc
                    }
                    level += 1
                    None
                  case gen: Gen =>
                    // Include Descs in Gen header, but not body
                    level += 1;
                    gen match {
                      case _: GenIf                 =>
                      case GenFor(inits, _, _, _)   => walk(inits);
                      case GenRange(inits, _, _, _) => walk(inits);
                    }
                    level -= 1
                    Some(tree)
                  case _ => None
                }
                override protected def transform(tree: Tree): Tree = tree match {
                  case _: Desc => level -= 1; tree
                  case _       => tree
                }
              }

              valid foreach GatherDescs

              (direcScalars.toList, other.toList)
            }

            // Rename non nested cloned scalar symbols based on bindings.
            def n(v: BigInt): String = if (v < 0) s"n${-v}" else s"$v"
            val suffix = bindings.toList sortBy { _._1.loc.start } map {
              case (symbol, expr) => s"${symbol.name}_${n(expr.value.get)}"
            } pipe {
              case Nil  => ""
              case list => list mkString (cc.sep, cc.sep, "")
            }

            val inGenIf = gen match {
              case _: GenIf => true
              case _        => false
            }

            cloneSymbolsInDescs(
              directScalarDescs,
              bindings,
              suffix,
              inGenIf
            ) concat cloneSymbolsInDescs(
              otherDescs,
              bindings,
              "",
              inGenIf
            )
          }

          // Create the recursive Generate transform
          val transform = new Process(bindings, newSymbolMap)

          // Recursively process valid trees, then splice them
          val results = valid map dispatcher.splice map transform

          // Propagate errors
          hadError |= transform.hadError

          // Flatten thickets
          results flatMap {
            case Thicket(ts) => ts
            case tree        => List(tree)
          }
        }

        def generateFor(
            bindings: Bindings,
            terminate: Bindings => Option[Boolean],
            loc: Loc,
            body: List[Tree],
            step: Stmt
          ): List[Tree] = {
          val buf = new ListBuffer[Tree]

          @tailrec
          def loop(bindings: Bindings): Unit = terminate(bindings) match {
            case None => error(loc, "Condition of 'gen for' is not a compile time constant")
            case Some(false) =>
              buf appendAll process(bindings, body)
              StaticEvaluation(step, bindings) match {
                case None                   => hadError = true
                case Some((_, newBindings)) => loop(newBindings)
              }
            case _ => ()
          }

          loop(bindings)

          buf.toList
        }

        // Check trees, yield None if trees cannot be typed, Some(Nil) if there
        // is a type error, and Some(result) otherwise.
        def typeCheck(trees: Tree*)(result: => List[Tree]): Option[List[Tree]] = {
          val cannotBeTyped = trees exists {
            _ exists {
              case ExprSym(symbol)      => !symbol.isSpecialized
              case ExprRef(Sym(_, idx)) => idx.nonEmpty
            }
          }

          if (cannotBeTyped) {
            None
          } else if (trees forall cc.typeCheck) {
            Some(result)
          } else {
            Some(Nil)
          }
        }

        val result = gen match {
          case GenIf(cond, thenItems, elseItems) =>
            typeCheck(cond) {
              (cond given bindings).simplify match {
                case ExprInt(_, 1, v) if v != 0 => process(bindings, thenItems)
                case ExprInt(_, 1, _)           => process(bindings, elseItems)
                case ExprInt(_, w, _) =>
                  error(cond, s"Condition of 'gen if' yields $w bits, 1 bit is expected")
                  Nil
                case _: ExprNum =>
                  error(cond, "Condition of 'gen if' yields an unsized value, 1 bit is expected")
                  Nil
                case _ =>
                  error(cond, "Condition of 'gen if' is not a compile time constant")
                  Nil
              }
            }

          case tree @ GenFor(gInits, cond, gSteps, body) =>
            typeCheck(gInits ::: cond :: gSteps: _*) {
              // normalize inits and steps so ticks and unsized constants are sized
              val inits = gInits map { _.normalize }
              val steps = gSteps map { _.normalize }
              val loopVars = Set from {
                inits.iterator collect {
                  case StmtDecl(Decl(symbol)) => symbol
                }
              }
              if (cond forall { case ExprSym(symbol) => !loopVars(symbol) }) {
                error(cond, "'gen for' condition does not depend on loop variable")
                Nil
              } else {
                val initBindings = bindings ++ {
                  inits collect {
                    case StmtDefn(DefnGen(symbol, init)) => symbol -> init.simplify
                  }
                }
                val stepStmt = TypeAssigner(StmtBlock(steps) withLoc tree.loc)
                val iteration = (LazyList from 0).iterator
                val limit = cc.settings.genLoopLimit
                val terminate = { bindings: Bindings =>
                  (cond given bindings).value map { value =>
                    if (value == 0) {
                      true
                    } else if (iteration.next() >= limit) {
                      error(
                        cond,
                        s"'gen for' exceeds $limit iterations. Possibly an infinite loop,",
                        s"otherwise set --gen-loop-limit to more than $limit"
                      )
                      true
                    } else {
                      false
                    }
                  }
                }
                generateFor(initBindings, terminate, cond.loc, body, stepStmt)
              }
            }

          case GenRange(StmtDecl(decl) :: StmtDefn(defn) :: Nil, op, end, body) =>
            val symbol = decl.symbol
            // Build a spoof condition node for type checking only
            val cond = {
              val expr = (ExprSym(symbol) withLoc symbol.loc) < end
              expr.copy() withLoc decl.loc.copy(start = symbol.loc.start)
            }
            typeCheck(decl, defn, end, cond) {
              // Some(Maximum inclusive value representable by symbol.kind) or None if infinite
              val maxValueOpt = if (symbol.kind.underlying.isNum) {
                None
              } else if (symbol.kind.isSigned) {
                Some(BigInt.mask(symbol.kind.width - 1))
              } else {
                Some(BigInt.mask(symbol.kind.width))
              }

              (end given bindings).value map { value =>
                if (op == "<") value - 1 else value // Inclusive end value
              } match {
                case None =>
                  error(end, "End value of range 'gen for' is not a compile time constant")
                  Nil
                case Some(endValue) =>
                  val lastValue = maxValueOpt map { _ min endValue } getOrElse endValue
                  if (endValue > lastValue) {
                    val v = (end given bindings).value.get
                    val t = symbol.kind.underlying.toSource
                    cc.warning(
                      decl,
                      s"End value $v is out of range for variable ${symbol.name} with type '$t',",
                      s"will iterate only up to ${symbol.name} == ${maxValueOpt.get}"
                    )
                  }
                  val initBindings = bindings + {
                    defn.normalize match {
                      case DefnGen(_, init) => symbol -> init.simplify
                      case _                => unreachable
                    }
                  }
                  val iter = (BigInt(0) to lastValue).iterator
                  val terminate = { _: Bindings =>
                    Some(if (iter.hasNext) { iter.next(); false }
                    else true)
                  }
                  val stepStmt = StmtPost(ExprSym(symbol), "++") regularize decl.loc
                  generateFor(initBindings, terminate, Loc.synthetic, body, stepStmt)
              }
            }

          case GenRange(List(_: StmtDesc), _, _, _) => None

          case _: GenRange => unreachable
        }

        if (hadError) Some(Nil) else result
      }

      // Stack of enclosing parametrized symbols. We use this to track if
      // we are inside a parametrized desc (i.e.: in a desc which is known
      // to have parameters, not including parameters that might be generated
      // by Gen). We use a stack as expanding generates might introduce new
      // parameters (i.e.: desc.params is not invariant to this transform)
      private val parametrizedSymbols = mutable.Stack[Symbol]()

      override def enter(tree: Tree): Option[Tree] = {
        // Only process Gen in non-parametrized context (note Gens themselves
        // might generate params, so we only check the already known params)
        tree match {
          case desc: Desc if desc.params.nonEmpty =>
            // Note the symbol might be cloned, so push the clone
            parametrizedSymbols push symbolMap.getOrElse(desc.symbol, desc.symbol)
          case _ =>
        }

        if (parametrizedSymbols.nonEmpty) {
          None
        } else {
          tree pipe {
            // Gen that must produce entity contents
            case EntGen(gen) => generate[Ent](gen) map Thicket

            // Gen that must produce record contents
            case RecGen(gen) => generate[Rec](gen) map Thicket

            // Gen that must produce statement
            case StmtGen(gen) => generate[Stmt](gen) map Thicket

            // Gen that must produce case clauses
            case CaseGen(gen) =>
              generate[Case](gen) map { results =>
                // Check we produced at most one default case
                val defaultCases = results collect { case c: CaseDefault => c }
                if (defaultCases.lengthIs > 1) {
                  for (loc <- (defaultCases map { _.loc }).distinct) {
                    error(loc, "'gen' yields multiple default cases")
                  }
                }
                Thicket(results)
              }

            // Keep going
            case _ => None
          } tap {
            case Some(_) => progress = true
            case None    =>
          }
        }
      }

      override def transform(tree: Tree): Tree = tree match {
        //////////////////////////////////////////////////////////////////////////
        // Replace refs to bound symbols with the value, and to cloned symbols
        // with a ref to the new symbol. Dict indices are handled by Resolve.
        //////////////////////////////////////////////////////////////////////////

        case ExprSym(symbol) =>
          bindings.get(symbol) orElse {
            symbolMap.get(symbol) map { ExprSym(_) withLoc tree.loc }
          } getOrElse tree

        case Sym(symbol, idxs) =>
          symbolMap.get(symbol) map { newSymbol =>
            Sym(newSymbol, idxs) withLoc tree.loc
          } getOrElse tree

        case decl: Decl =>
          symbolMap.get(decl.symbol) map { newSymbol =>
            potentialChoices add newSymbol
            decl.cpy(symbol = newSymbol) withLoc tree.loc
          } getOrElse tree

        case defn: Defn =>
          symbolMap.get(defn.symbol) map { newSymbol =>
            defn.cpy(symbol = newSymbol) withLoc tree.loc
          } getOrElse tree

        case desc: Desc =>
          potentialChoices add desc.symbol
          if (parametrizedSymbols.headOption contains desc.symbol) {
            parametrizedSymbols.pop()
          }
          tree

        ////////////////////////////////////////////////////////////////////////
        // Done
        ////////////////////////////////////////////////////////////////////////

        case _ => tree
      }

      override def finalCheck(tree: Tree): Unit = {
        if (!hadError) {
          assert(parametrizedSymbols.isEmpty, parametrizedSymbols)
        }
      }
    }

    final class Resolve(
        finished: Boolean
      )(
        implicit
        cc: CompilerContext)
        extends StatefulTreeTransformer {

      override val typed: Boolean = false

      // Latch errors
      var hadError = false

      private[this] def error(loc: Loc, msg: String*): Unit = {
        cc.error(loc, msg: _*)
        hadError = true
      }

      // Indicates whether we are in a nested parametrized Decl (> 0) or not ( == 0)
      private var parametrizedLevel = 0

      // Indicates whether we are in an un-expanded gen
      private var genLevel = 0

      private def resolveChoiceSymbol(symbol: Symbol, loc: Loc): Option[Symbol] = {
        if (!symbol.isChoice) {
          Some(symbol)
        } else {
          def extractValidChoices(symbol: Symbol): List[Symbol] = {
            symbol.desc.asInstanceOf[DescChoice].choices flatMap {
              // Dict symbol with valid any actual instances is valid is valid
              case ExprSym(choice) if choice.attr.dictResolutions.isSet => List(choice)
              case ExprSym(choice) =>
                validChoices.get(choice) match {
                  case Some(clone) if clone.isChoice =>
                    extractValidChoices(clone) match {
                      case Nil     => List(clone) // Not yet resolved
                      case choices => choices
                    }
                  case Some(clone) => List(clone)
                  case None        => Nil
                }
            }
          }

          extractValidChoices(symbol) match {
            case Nil =>
              if (parametrizedLevel == 0 && genLevel == 0 && finished) {
                // Only error when appears in the non-parametrized context. The
                // choice symbol might be introduced in a nested parametrized
                // definition, and if so will be resolved when that definition is
                // specialized.
                error(loc, s"'${symbol.name}' is not defined after processing 'gen' constructs")
              }
              None
            case resolution :: Nil => Some(resolution)
            case resolutions =>
              if (finished) {
                val msg =
                  s"'${symbol.name}' is ambiguous after processing 'gen' constructs. Active definitions at:" ::
                    (resolutions.reverse map { _.loc.prefix })
                error(loc, msg: _*)
              }
              None
          }
        }
      }

      private def resolveDict(symbol: Symbol, idxs: List[Expr], loc: Loc): Option[Symbol] =
        if (idxs exists { _ exists { case ExprSym(symbol) => !symbol.isSpecialized } }) {
          None
        } else {
          idxs match {
            case Nil => Some(symbol)
            case _ =>
              symbol.attr.dictResolutions.get match {
                case None => None // Gen not yet expanded (nested entity)
                case Some(dMap) =>
                  values(idxs, Bindings.empty) orElse {
                    hadError = true
                    None
                  } flatMap { idxValues =>
                    dMap.get(idxValues) orElse {
                      val expected = dMap.head._1.length
                      val provided = idxValues.length
                      if (expected != provided) {
                        error(
                          loc,
                          s"Wrong number of indices for name '${symbol.name}' (expected $expected, provided $provided)"
                        )
                      } else {
                        val srcName = idxValues.mkString(symbol.name + "#[", ", ", "]")
                        error(loc, s"'$srcName' is not defined after processing 'gen' constructs")
                      }
                      None
                    }
                  }
              }
          }
        }

      private var rootSymbol: Symbol = _

      override protected def start(tree: Tree): Unit = tree match {
        case Desc(ref)    => rootSymbol = ref.symbol
        case Decl(symbol) => rootSymbol = symbol
        case Defn(symbol) => rootSymbol = symbol
        case _            => unreachable
      }

      override def enter(tree: Tree): Option[Tree] = tree match {
        //////////////////////////////////////////////////////////////////////////
        // Update DescChoice
        //////////////////////////////////////////////////////////////////////////

        case desc @ DescChoice(_, choices) if parametrizedLevel == 0 =>
          Some {
            // If there are no Gen nodes left or if the choice is fully
            // resolved, drop the DescChoice node, otherwise remove eliminated
            // choices, possibly dropping the DescChoice if all choices have
            // been eliminated.
            if (finished || resolveChoiceSymbol(desc.symbol, desc.ref.loc).isDefined) {
              desc.symbol.attr.eliminated set true
              Stump
            } else {
              choices filter {
                case ExprSym(symbol) => potentialChoices(symbol)
              } match {
                case Nil =>
                  desc.symbol.attr.eliminated set true
                  Stump
                case remainingChoices => desc.copy(choices = remainingChoices) withLoc desc.loc
              }
            }
          }

        //
        case desc: Desc if desc.isParametrized && (desc.symbol != rootSymbol) =>
          parametrizedLevel += 1
          None

        //
        case gen: Gen =>
          Some {
            // Headers still belong to the outer scope, so resolve appropriately
            gen match {
              case GenIf(cond, thenItems, elseItems) =>
                val newCond = walk(cond).asInstanceOf[Expr]
                genLevel += 1
                val newThenItems = walk(thenItems)
                val newElseItems = walk(elseItems)
                genLevel -= 1
                GenIf(newCond, newThenItems, newElseItems) withLoc tree.loc
              case GenFor(inits, cond, steps, body) =>
                val newInits = walk(inits).asInstanceOf[List[Stmt]]
                val newCond = walk(cond).asInstanceOf[Expr]
                val newSteps = walk(steps).asInstanceOf[List[Stmt]]
                genLevel += 1
                val newBody = walk(body)
                genLevel -= 1
                GenFor(newInits, newCond, newSteps, newBody) withLoc tree.loc
              case GenRange(inits, op, end, body) =>
                val newInits = walk(inits).asInstanceOf[List[Stmt]]
                val newEnd = walk(end).asInstanceOf[Expr]
                genLevel += 1
                val newBody = walk(body)
                genLevel -= 1
                GenRange(newInits, op, newEnd, newBody) withLoc tree.loc
            }
          }

        //
        case _ => None
      }

      override def transform(tree: Tree): Tree = tree match {
        //////////////////////////////////////////////////////////////////////////
        // Resolve references to choice symbols
        //////////////////////////////////////////////////////////////////////////

        case ExprSym(symbol) =>
          resolveChoiceSymbol(symbol, tree.loc) map { symbol =>
            ExprSym(symbol) withLoc tree.loc
          } getOrElse tree

        //////////////////////////////////////////////////////////////////////////
        // Resolve dict references
        //////////////////////////////////////////////////////////////////////////

        // Only resolve the dict reference if not inside a Gen, as the
        // reference inside the Gen might be to an invalid index but would
        // be removed after processing the enclosing Gen
        case Sym(symbol, idxs) if genLevel == 0 =>
          resolveChoiceSymbol(symbol, tree.loc) map { rSymbol =>
            resolveDict(rSymbol, idxs, tree.loc) map {
              Sym(_, Nil) withLoc tree.loc
            } getOrElse {
              Sym(rSymbol, idxs) withLoc tree.loc
            }
          } getOrElse tree

        // Inside a gen, still attempt to resolve the choice symbol
        case Sym(symbol, idxs) =>
          resolveChoiceSymbol(symbol, tree.loc) map { rSymbol =>
            Sym(rSymbol, idxs) withLoc tree.loc
          } getOrElse tree

        //////////////////////////////////////////////////////////////////////////
        // Simplify now resolved ExprRefs
        //////////////////////////////////////////////////////////////////////////

        case ExprRef(Sym(symbol, Nil)) => ExprSym(symbol) withLoc tree.loc

        //////////////////////////////////////////////////////////////////////////
        // Update parametrizedLevel
        //////////////////////////////////////////////////////////////////////////

        case desc: Desc if desc.isParametrized && (desc.symbol != rootSymbol) =>
          parametrizedLevel -= 1
          tree

        ////////////////////////////////////////////////////////////////////////
        // Done
        ////////////////////////////////////////////////////////////////////////

        case _ => tree
      }

      override protected def finalCheck(tree: Tree): Unit = {
        assert(parametrizedLevel == 0)
        assert(genLevel == 0)
      }
    }

    def rewrite(
        input: Either[Desc, (Decl, Defn)],
        transform: StatefulTreeTransformer
      ): Either[Desc, (Decl, Defn)] = input match {
      case Left(desc) =>
        Left(desc rewrite transform ensuring { _.symbol eq desc.symbol })
      case Right((decl, defn)) =>
        Right(
          (
            decl rewrite transform ensuring { _.symbol eq decl.symbol },
            defn rewrite transform ensuring { _.symbol eq defn.symbol }
          )
        )
    }

    // Expand Gen nodes
    val process = new Process(Bindings.empty, Map.empty)
    val processed = rewrite(input, process)

    if (process.hadError) {
      None
    } else if (!process.progress) {
      // If no Gen was expanded, return the input
      Some(input)
    } else {
      // Otherwise proceed by resolving choice and dict symbols
      val resolve = new Resolve(!hasGen(processed))
      val res = rewrite(processed, resolve)
      if (resolve.hadError) None else Some(res)
    }
  }

}
