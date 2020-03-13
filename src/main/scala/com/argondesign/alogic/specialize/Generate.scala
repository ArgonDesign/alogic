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
  private implicit object generatableEnt extends Generatable[Ent] {
    val description = "entity content"
    def isValid(tree: Tree): Boolean = tree match {
      case _: Gen  => true
      case _: Ent  => true
      case _: Desc => true
      case _       => false
    }
    def splice(tree: Tree): Ent = tree match {
      case gen: Gen => EntGen(gen) withLoc gen.loc
      case ent: Ent => ent
      case desc: DescFunc =>
        assert(desc.variant == FuncVariant.None)
        val newDesc = desc.copy(variant = FuncVariant.Ctrl) withLoc desc.loc
        EntDesc(newDesc) withLoc desc.loc
      case desc: Desc => EntDesc(desc) withLoc desc.loc
      case _          => unreachable
    }
  }

  // Type class instance for Rec
  private implicit object generatableRec extends Generatable[Rec] {
    val description = "struct content"
    def isValid(tree: Tree): Boolean = tree match {
      case _: Gen        => true
      case _: Rec        => true
      case _: DescVar    => true
      case _: DescParam  => true
      case _: DescConst  => true
      case _: DescType   => true
      case _: DescRecord => true
      case _: DescFunc   => true
      case _: DescChoice => true
      case _             => false
    }
    def splice(tree: Tree): Rec = tree match {
      case gen: Gen => RecGen(gen) withLoc gen.loc
      case rec: Rec => rec
      case desc: DescFunc =>
        assert(desc.variant == FuncVariant.None)
        val newDesc = desc.copy(variant = FuncVariant.Comb) withLoc desc.loc
        RecDesc(newDesc) withLoc desc.loc
      case desc: Desc => RecDesc(desc) withLoc desc.loc
      case _          => unreachable
    }
  }

  // Type class instance for Stmt
  private implicit object generatableStmt extends Generatable[Stmt] {
    val description = "statement"
    def isValid(tree: Tree): Boolean = tree match {
      case _: Gen  => true
      case _: Stmt => true
      case _: Desc => true
      case _       => false
    }
    def splice(tree: Tree): Stmt = tree match {
      case gen: Gen   => StmtGen(gen) withLoc gen.loc
      case stmt: Stmt => stmt
      case desc: Desc => StmtDesc(desc) withLoc desc.loc
      case _          => unreachable
    }
  }

  // Type class instance for Case
  private implicit object generatableCase extends Generatable[Case] {
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

  def values(idxs: List[Expr])(implicit cc: CompilerContext): Option[List[BigInt]] = idxs match {
    case Nil => Some(Nil)
    case head :: tail =>
      val resOpt = values(tail) // Compute eagerly to emit all errors
      head.value match {
        case Some(value) => resOpt map { value :: _ }
        case None        => cc.error(head, "Identifier index must be a compile time constant"); None
      }
  }

  def apply(
      input: Either[Desc, (Decl, Defn)]
  )(
      implicit cc: CompilerContext
  ): Option[Either[Desc, (Decl, Defn)]] = {
    assert(input.left forall { !_.isParametrized })
    assert(input.left forall { _.ref.asInstanceOf[Sym].idxs.isEmpty })

    // Whether this choice has been generated
    val validChoices = mutable.Set[Symbol]()

    // Whether this choice has not been eliminated
    val potentialChoices = mutable.Set[Symbol]()

    final class Process[T <: Tree](
        // Bindings for symbols with known values (gen variables)
        bindings: Bindings,
        // The symbols already cloned externally (generated symbols that are used outside)
        newSymbols: Map[Symbol, Symbol],
        // This is inside a gen
        inGen: Boolean
    )(
        implicit cc: CompilerContext
    ) extends StatefulTreeTransformer {

      override val typed: Boolean = false

      // The cloned symbol map
      private val symbolMap = mutable.Map from newSymbols

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
          inGen: Boolean
      ): List[(Symbol, Symbol)] = descs flatMap {
        // Don't clone parametrized symbols outside Gen. These will be
        // specialized and replaced
        case desc: Desc if desc.isParametrized && !inGen => Nil
        // Scalar symbols
        case desc: Desc if desc.ref.idxs.isEmpty =>
          symbolMap.get(desc.symbol) match {
            // A scalar symbol may already be cloned if referenced by an outer
            // DescChoice. We just need to mark it valid at this point.
            case Some(cloneSymbol) =>
              // This choice will now be active, so mark as such
              validChoices add cloneSymbol
              // Rename the clone by appending the suffix
              cloneSymbol.name = cloneSymbol.name + suffix
              // Already in the symbol map, no need to add it
              Nil

            // If a scalar symbol was not already cloned, we must clone it
            case None =>
              // Clone the symbol
              val newSymbol = desc.symbol.dup
              // Rename the clone by appending the suffix
              newSymbol.name = newSymbol.name + suffix
              // If this is a choice symbol, recursively clone all referenced
              // choices as well. We clone recursively to ensure only one
              // clone exists, even if a referenced choice is inside a loop.
              def choiceClones(intr: Desc): List[(Symbol, Symbol)] = intr match {
                case DescChoice(_, choices) =>
                  choices flatMap {
                    case ExprSym(choice) =>
                      // These could not have been seen before,
                      assert(!(symbolMap contains choice))
                      (choice -> choice.dup) :: choiceClones(choice.desc)
                  }
                case _ => Nil
              }
              // Add this clone and the choice clones (if any)
              (desc.symbol -> newSymbol) :: choiceClones(desc)
          }
        // Dict symbols
        case desc: Desc =>
          // Choice symbols are never dict
          assert(!desc.isInstanceOf[DescChoice])
          // At this point the indices must be known, or it's an error
          values(desc.ref.idxs map { _ given bindings }) match {
            case None =>
              hadError = true
              Nil
            case Some(idxValues) =>
              // Get the introduced symbol
              val symbol = desc.symbol

              // Dict symbols will have always either been cloned as they can only
              // be defined in a 'gen' loop, and they are always injected into the
              // enclosing scope via a choice symbol, or have been cloned in another
              // call to generate and subsequently resolved to the clone, so just get
              // the clone (if introduced in the generate being processed, or use
              // the already resolved symbol).
              val dictSymbol = symbolMap.getOrElse(symbol, symbol)

              // We will now have at least one instance of the dict decl,
              // so mark the dictSymbol as a valid choice
              validChoices add dictSymbol

              // Clone the symbol, this is the specific instance
              val newSymbol = symbol.dup

              // Rename based on value of indices
              newSymbol.name = idxValues.mkString(symbol.name + cc.sep, "_", "")

              // Set the attribute used ot resolve external dict references
              newSymbol.attr.sourceName.set((symbol.name, idxValues))

              // Add to dictResolutions, error if already exists
              dictSymbol.attr.dictResolutions
                .getOrElseUpdate(mutable.Map())
                .put(idxValues, newSymbol) match {
                case Some(_) =>
                  val ref = desc.ref
                  val srcName = idxValues.mkString(symbol.name + "#[", ", ", "]")
                  error(ref, s"'$srcName' defined multiple times after processing 'gen' constructs")
                case None => // OK, first definition with these indices
              }

              // The dict clone is already in the symbolMap, no need to add it
              Nil
          }
      }

      // Compute the generated tree from generate
      private def generate[U <: Tree: Generatable](gen: Gen): Option[List[Tree]] = {
        val dispatcher: Generatable[U] = implicitly[Generatable[U]]

        def process(bindings: Bindings, trees: List[Tree]): List[Tree] = {
          // Check for 'gen' content invalid in this context
          val (valid, invalid) = trees partition dispatcher.isValid

          // Issue error for any invalid contents
          invalid foreach { t =>
            error(
              t,
              s"'gen' construct yields invalid syntax, ${dispatcher.description} is expected (${t.toString})")
          }

          // Gather definitions in the 'gen' scope
          val descs = valid collect { case desc: Desc => desc }

          // Clone symbols introduced in the scope of this Gen, and build the new
          // starting symbol map. Rename cloned scalar symbols based on bindins.
          val sm = {
            val suffix = if (bindings.isEmpty) {
              ""
            } else {
              bindings.toList sortBy {
                _._1.loc.start
              } map {
                case (symbol, expr) => s"${symbol.name}_${expr.value.get}"
              } mkString (cc.sep, cc.sep, "")
            }

            Map from {
              symbolMap.iterator concat cloneSymbolsInDescs(descs, bindings, suffix, true)
            }
          }

          // Create the recursive Generate transform
          val transform = new Process(bindings, sm, true)

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

        def generateFor(bindings: Bindings,
                        terminate: Bindings => Option[Boolean],
                        loc: Loc,
                        body: List[Tree],
                        step: Stmt): List[Tree] = {
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
              (cond given bindings).value match {
                case None =>
                  error(cond, "Condition of 'gen if' is not a compile time constant")
                  Nil
                case Some(v) if v != 0 => process(bindings, thenItems)
                case _                 => process(bindings, elseItems)
              }
            }

          case tree @ GenFor(gInits, cond, gSteps, body) =>
            typeCheck(gInits ::: cond :: gSteps: _*) {
              // normalize inits and steps so ticks and unsized constants are sized
              val inits = gInits map { _.normalize }
              val steps = gSteps map { _.normalize }
              if (cond.value.isDefined) {
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
                    Some(if (iter.hasNext) { iter.next(); false } else true)
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

      // Indicates whether we are in a parametrized Desc (> 0) or not ( == 0)
      private var parametrizedLevel = 0

      override def enter(tree: Tree): Option[Tree] = {
        tree match {
          case desc: Desc if desc.isParametrized => parametrizedLevel += 1
          case _                                 =>
        }

        // Only process Gen in non-parametrized context
        if (parametrizedLevel > 0) {
          None
        } else {
          // Clone un-specialized symbols introduced in the scope of this tree
          symbolMap ++= {
            def stmtDescs(stmts: List[Stmt]): List[Desc] = stmts collect {
              case StmtDesc(desc) => desc
            }

            val descs = tree match {
              case desc: Desc                      => desc.descs
              case defn: Defn                      => defn.descs
              case EntCombProcess(stmts)           => stmtDescs(stmts)
              case StmtBlock(body)                 => stmtDescs(body)
              case StmtIf(_, thenStmts, elseStmts) => stmtDescs(thenStmts) ::: stmtDescs(elseStmts)
              case StmtLoop(body)                  => stmtDescs(body)
              case StmtWhile(_, body)              => stmtDescs(body)
              case StmtFor(inits, _, _, body)      => stmtDescs(inits) ::: stmtDescs(body)
              case StmtDo(_, body)                 => stmtDescs(body)
              case StmtLet(inits, body)            => stmtDescs(inits) ::: stmtDescs(body)
              case CaseRegular(_, stmts)           => stmtDescs(stmts)
              case CaseDefault(stmts)              => stmtDescs(stmts)
              case _                               => Nil
            }

            cloneSymbolsInDescs(descs, bindings, "", inGen)
          }

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
          if (desc.isParametrized) {
            parametrizedLevel -= 1
          }
          tree

        ////////////////////////////////////////////////////////////////////////
        // Done
        ////////////////////////////////////////////////////////////////////////

        case _ => tree
      }

      override def finalCheck(tree: Tree): Unit = {
        if (!hadError) {
          assert(parametrizedLevel == 0)
        }
      }
    }

    final class Resolve(
        finished: Boolean
    )(implicit cc: CompilerContext)
        extends StatefulTreeTransformer {

      override val typed: Boolean = false

      // Latch errors
      var hadError = false

      private[this] def error(loc: Loc, msg: String*): Unit = {
        cc.error(loc, msg: _*)
        hadError = true
      }

      // Indicates whether we are in a parametrized Decl (> 0) or not ( == 0)
      private var parametrizedLevel = 0

      // Indicates whether we are in an un-expanded gen
      private var genLevel = 0

      private def resolveChoiceSymbol(symbol: Symbol, loc: Loc): Option[Symbol] = {
        if (!symbol.isChoice) {
          Some(symbol)
        } else {
          def extractValidChoices(symbol: Symbol): List[Symbol] = {
            symbol.desc.asInstanceOf[DescChoice].choices flatMap { choice =>
              if (!(validChoices contains choice.symbol)) {
                Nil
              } else if (choice.symbol.isChoice) {
                extractValidChoices(choice.symbol) match {
                  case Nil     => List(choice.symbol)
                  case choices => choices
                }
              } else {
                List(choice.symbol)
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
                val msg = s"'${symbol.name}' is ambiguous after processing 'gen' constructs. Active definitions at:" ::
                  (resolutions.reverse map {
                  _.loc.prefix
                })
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
                  values(idxs) orElse {
                    hadError = true
                    None
                  } flatMap { idxValues =>
                    dMap.get(idxValues) orElse {
                      val expected = dMap.head._1.length
                      val provided = idxValues.length
                      if (expected != provided) {
                        error(
                          loc,
                          s"Wrong number of indices for name '${symbol.name}' (expected $expected, provided $provided)")
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
        case desc: Desc if desc.isParametrized =>
          parametrizedLevel += 1
          None

        //
        case _: Gen =>
          genLevel += 1
          None

        //
        case _ => None
      }

      override def transform(tree: Tree): Tree = tree match {
        //////////////////////////////////////////////////////////////////////////
        // Resolve references to choice symbols
        //////////////////////////////////////////////////////////////////////////

        case ExprSym(symbol) =>
          resolveChoiceSymbol(symbol, tree.loc) map { symbol =>
            assert(!symbol.attr.dictResolutions.isSet)
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
        // Update parametrizedLevel/genLevel
        //////////////////////////////////////////////////////////////////////////

        case desc: Desc if desc.isParametrized =>
          parametrizedLevel -= 1
          tree

        case _: Gen =>
          genLevel -= 1
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
          (decl rewrite transform ensuring { _.symbol eq decl.symbol },
           defn rewrite transform ensuring { _.symbol eq defn.symbol })
        )
    }

    // Expand Gen nodes
    val process = new Process(Bindings.empty, Map.empty, false)
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
