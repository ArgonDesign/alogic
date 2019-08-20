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
// Process 'gen' constructs in the top level entity
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.transform

import com.argondesign.alogic.analysis.StaticEvaluation
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Symbols._
import com.argondesign.alogic.core.Bindings
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.TreeInTypeTransformer
import com.argondesign.alogic.core.Types.TypeChoice
import com.argondesign.alogic.core.Types.TypeInstance
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.typer.Typer
import com.argondesign.alogic.util.BigIntOps._
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

private trait Generatable {
  def isExpectedType(tree: Tree): Boolean
  val description: String
  def convertDecl(gen: GenDecl): Tree = unreachable
}

private object generatableStmt extends Generatable {
  def isExpectedType(tree: Tree): Boolean = tree.isInstanceOf[Stmt]
  val description = "statement"
  override def convertDecl(gen: GenDecl): StmtDecl = StmtDecl(gen.decl) withLoc gen.loc
}

private object generatableCase extends Generatable {
  def isExpectedType(tree: Tree): Boolean = tree.isInstanceOf[Case]
  val description = "case clause"
}

private object generatableEnt extends Generatable {
  def isExpectedType(tree: Tree): Boolean = tree.isInstanceOf[Ent]
  val description = "entity content"
  override def convertDecl(gen: GenDecl): EntDecl = EntDecl(gen.decl) withLoc gen.loc
}

private final class Generate(
    bindings: Bindings,
    newSymbols: Map[Symbol, Symbol],
    dispatcher: Option[Generatable], // Some, if already inside a 'gen' else None
    initialEntityLevel: Int
)(
    implicit cc: CompilerContext,
    typer: Typer,
    choiceMap: mutable.Map[Symbol, Symbol]
) extends TreeTransformer {

  override val typed: Boolean = false
  override val checkRefs: Boolean = false

  // The symbols produced by the current gen instance
  private val symbolMap = mutable.Map from newSymbols

  // Latch errors
  var hadError = false

  private[this] def error(loc: Loc, msg: String*): Unit = {
    cc.error(loc, msg: _*)
    hadError = true
  }

  private[this] def error(tree: Tree, msg: String*): Unit = error(tree.loc, msg: _*)

  // Type check trees, yield Nil if type error, otherwise yield result
  private def typeCheck(trees: Tree*)(result: => List[Tree]): List[Tree] = {
    if (trees map typer exists { _.tpe.isError }) {
      hadError = true
      Nil
    } else result
  }

  // Used to clone function, instance and entity symbols up front to use by
  // forward references
  private def cloneForwardSymbols(trees: List[Tree]): List[(Symbol, Symbol)] = {
    trees flatMap {
      case EntFunction(Sym(symbol: TermSymbol), _) =>
        List(symbol -> cc.newSymbolLike(symbol))
      case EntInstance(Sym(symbol: TermSymbol), _, _, _) =>
        List(symbol -> cc.newSymbolLike(symbol))
      case EntEntity(Entity(Sym(symbol: TypeSymbol), _)) =>
        List(symbol -> cc.newSymbolLike(symbol))
      case _ => Nil
    }
  }

  // Compute the generated tree from generate
  private def generate(gen: Gen, dispatcher: Generatable): List[Tree] = {

    def process(bindings: Bindings, trees: List[Tree]): List[Tree] = {
      // Clone forward symbols
      val clones = cloneForwardSymbols(trees)

      // Add them to the choice map
      clones foreach { choiceMap += _ }

      // Add forward symbol clones
      val sm = Map from { symbolMap.iterator concat clones }

      // Generate all trees
      val subGen = new Generate(bindings, sm, Some(dispatcher), entityLevel)

      // Apply subGen
      val results = trees map subGen

      // Propagate errors
      hadError |= subGen.hadError

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
          loop(StaticEvaluation(step, bindings)._2)
        case _ => ()
      }

      loop(bindings)

      buf.toList
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

      case tree @ GenFor(gInits, Some(cond), gSteps, body) =>
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
                case StmtDecl(Decl(symbol, Some(init))) => symbol -> init
              }
            }
            val StepStmt = TypeAssigner(StmtBlock(steps) withLoc tree.loc)
            val iteration = (LazyList from 0).iterator
            val limit = cc.settings.genLoopLimit
            val terminate = { bindings: Bindings =>
              (cond given bindings).value map { value =>
                if (value == 0) {
                  true
                } else if (iteration.next() >= limit) {
                  error(
                    cond,
                    s"'gen for' exceeds ${limit} iterations. Possibly an infinite loop,",
                    s"otherwise set --gen-loop-limit to more than ${limit}"
                  )
                  true
                } else {
                  false
                }
              }
            }
            generateFor(initBindings, terminate, cond.loc, body, StepStmt)
          }
        }

      case tree @ GenRange(decl @ Decl(symbol, None), op, end, body) =>
        // Build a spoof condition node for type checking only
        val cond = {
          val expr = (ExprRef(symbol) withLoc symbol.loc) < end
          expr.copy() withLoc decl.loc.copy(start = symbol.loc.start)
        }
        typeCheck(decl, end, cond) {
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
                val v = end.value.get
                val t = symbol.kind.underlying.toSource
                cc.warning(
                  decl,
                  s"End value $v is out of range for variable ${symbol.name} with type '$t',",
                  s"will iterate only up to ${symbol.name} == ${maxValueOpt.get}"
                )
              }
              val init = if (symbol.kind.underlying.isNum) {
                ExprNum(symbol.kind.isSigned, 0) regularize symbol.loc
              } else {
                ExprInt(symbol.kind.isSigned, symbol.kind.width, 0) regularize symbol.loc
              }
              val initBindings = bindings + (symbol -> init)
              val iter = (BigInt(0) to lastValue).iterator
              val terminate = { _: Bindings =>
                Some(if (iter.hasNext) { iter.next(); false } else true)
              }
              val stepStmt = StmtPost(ExprRef(symbol), "++") regularize decl.loc
              generateFor(initBindings, terminate, Loc.synthetic, body, stepStmt)
          }
        }

      case GenFor(_, None, _, _)               => unreachable
      case GenRange(Decl(_, Some(_)), _, _, _) => unreachable
      case GenRange(_: DeclIdent, _, _, _)     => unreachable
      case _: GenDecl                          => unreachable // Handled in transform
    }

    if (hadError) Nil else result
  }

  // Result of the outermost encountered Gen construct
  private var generated: Option[List[Tree]] = None

  // Tree level
  private var level = 0

  // Entity level
  private var entityLevel = initialEntityLevel

  // To rewrite refs in types
  private[this] object TypeGenerate extends TreeInTypeTransformer(this)

  // Only go one Gen deep
  override def skip(tree: Tree): Boolean = generated.nonEmpty

  private def invalidGenSyntax(tree: Tree): Unit = {
    val desc = dispatcher.get.description
    error(tree, s"'gen' construct yields invalid syntax, $desc is expected")
    generated = Some(Nil) // To prevent further descent
  }

  override def enter(tree: Tree): Unit = {
    assert(!tree.isInstanceOf[Gen] || generated.isEmpty, "should have stopped descent")

    tree match {
      case entity: Entity =>
        // If entering an entity inside a Gen, clone forward symbols
        if (dispatcher.isDefined) {
          cloneForwardSymbols(entity.body) foreach { pair =>
            symbolMap += pair
            choiceMap += pair
          }
        }
        entityLevel += 1
      case _ =>
    }

    // Only process Gen in the root entity
    if (entityLevel <= 1) {
      tree match {
        // Can't do parametrized entities..
        case Decl(symbol, _) if symbol.kind.isParam =>
          cc.ice(symbol, "Attempting to process Gen on entity with unbound parameter")

        // Root Gen that must produce statement
        case StmtGen(gen) =>
          generated = Some(generate(gen, generatableStmt))

        // Root Gen that must produce case clauses
        case CaseGen(gen) =>
          generated = Some(generate(gen, generatableCase))

        // Root Gen that must produce entity contents
        case EntGen(gen) =>
          generated = Some(generate(gen, generatableEnt))

        // Declaration inside gen block
        case _: GenDecl =>
          if (dispatcher.get eq generatableCase) {
            invalidGenSyntax(tree)
          }

        // Gen nested inside outer Gen node, must produce whatever it's nested inside
        case gen: Gen =>
          assert(dispatcher.nonEmpty)
          generated = Some(generate(gen, dispatcher.get))

        // Keep going if not inside a Gen
        case _ if dispatcher.isEmpty => ()

        // Die if the wrong kind of tree is generated
        case _ if level == 0 && !dispatcher.get.isExpectedType(tree) => invalidGenSyntax(tree)

        case _ => ()
      }
    }

    level += 1
  }

  override def transform(tree: Tree): Tree = {
    tree match {
      //////////////////////////////////////////////////////////////////////////
      // Replace Gen nodes with the result
      //////////////////////////////////////////////////////////////////////////

      // Replace outermost gen with the generate result
      case _: StmtGen if generated.isDefined => Thicket(generated.get) withLoc tree.loc

      // Replace outermost gen with the generate result
      case _: CaseGen if generated.isDefined =>
        val result = generated.get
        // Check we produced at most one default case
        val defaultCases = result collect { case c: CaseDefault => c }
        if (defaultCases.length > 1) {
          for (loc <- (defaultCases map { _.loc }).distinct) {
            error(loc, "'gen' yields multiple default cases")
          }
        }
        Thicket(result) withLoc tree.loc

      // TODO: Check no defs after contents
      case _: EntGen if generated.isDefined => Thicket(generated.get) withLoc tree.loc

      // Convert GenDecl
      case node: GenDecl if entityLevel <= 1 => dispatcher.get.convertDecl(node)

      // Replace nested gen with the generated result
      case _: Gen if generated.isDefined => Thicket(generated.get) withLoc tree.loc

      //////////////////////////////////////////////////////////////////////////
      // Clone variable symbols which are declared inside a gen
      //////////////////////////////////////////////////////////////////////////

      case decl @ Decl(symbol, _) if dispatcher.nonEmpty =>
        // Rewrite references in types
        val newKind = symbol.kind rewrite TypeGenerate
        // Clone symbol
        val newSymbol = cc.newTermSymbol(symbol.name, symbol.loc, newKind)
        newSymbol.attr update symbol.attr
        symbolMap(symbol) = newSymbol
        choiceMap(symbol) = newSymbol
        // Rewrite decl
        decl.copy(symbol = newSymbol) withLoc tree.loc

      //////////////////////////////////////////////////////////////////////////
      // Replace refs to bound symbols with the value, and
      // to generated symbols with a ref to the new symbol
      //////////////////////////////////////////////////////////////////////////

      case ExprRef(symbol: TermSymbol) =>
        bindings.get(symbol) orElse {
          symbolMap.get(symbol) map { ExprRef(_) withLoc tree.loc }
        } getOrElse tree

      case ExprRef(symbol: TypeSymbol) =>
        symbolMap.get(symbol) map { ExprRef(_) withLoc tree.loc } getOrElse tree

      case Sym(symbol) =>
        symbolMap.get(symbol) map { Sym(_) withLoc tree.loc } getOrElse tree

      //////////////////////////////////////////////////////////////////////////
      // Update symbol types
      //////////////////////////////////////////////////////////////////////////

      case EntInstance(Sym(iSymbol), Sym(eSymbol: TypeSymbol), _, _) =>
        iSymbol.kind = TypeInstance(eSymbol)
        tree

      case EntEntity(entity: Entity) =>
        entity.symbol.kind = entity.typeBasedOnContents
        tree

      //////////////////////////////////////////////////////////////////////////
      // Rename entities based on active gen vars
      //////////////////////////////////////////////////////////////////////////

      case entity: Entity => {
        if (bindings.nonEmpty) {
          // Note we only need to re-name entities early, everything else is
          // taken care of by the late RenameSymbols pass. We prefer to rename
          // here as it helps compiler debugging if all entities ultimately
          // have unique names, plus we can make up a more meaningful name here.
          entity.symbol.rename {
            entity.symbol.name :: {
              bindings.toList sortBy {
                _._1.loc.start
              } map {
                case (symbol, expr) => s"${symbol.name}_${expr.value.get}"
              }
            } mkString cc.sep
          }
        }
        entity.symbol.kind = entity.typeBasedOnContents
        tree
      } tap { _ =>
        entityLevel -= 1
      }

      //////////////////////////////////////////////////////////////////////////
      // Rewrite types
      //////////////////////////////////////////////////////////////////////////

      case ExprType(kind) =>
        val newKind = kind rewrite TypeGenerate
        if (kind eq newKind) tree else ExprType(newKind) withLoc tree.loc

      case cast @ ExprCast(kind, _) =>
        val newKind = kind rewrite TypeGenerate
        if (kind eq newKind) tree else cast.copy(kind = newKind) withLoc tree.loc

      //////////////////////////////////////////////////////////////////////////
      // Done
      //////////////////////////////////////////////////////////////////////////

      case _ => tree
    }
  } tap { _ =>
    generated = None
    level -= 1
  }

  override def finalCheck(tree: Tree): Unit = {
    if (!hadError) {
      assert(generated.isEmpty)
      assert(level == 0)
      assert(entityLevel == initialEntityLevel)
      tree visit {
        case _: EntEntity  => // Stop descent at nested entity
        case node: Gen     => cc.ice(node, s"Gen remains")
        case node: StmtGen => cc.ice(node, s"StmtGen remains")
        case node: CaseGen => cc.ice(node, s"CaseGen remains")
      }
    }
  }
}

private final class Disambiguate(
    choiceMap: mutable.Map[Symbol, Symbol]
)(
    implicit cc: CompilerContext
) extends TreeTransformer {

  override val typed: Boolean = false
  override val checkRefs: Boolean = false

  private var inEntity = false

  override def skip(tree: Tree): Boolean = tree match {
    case _: Entity => inEntity
    case _         => false
  }

  override def enter(tree: Tree): Unit = tree match {
    case _: Entity => inEntity = true
    case _         =>
  }

  private def resolveChoice(symbol: Symbol, loc: Loc): Option[Symbol] = {
    val TypeChoice(choices) = symbol.kind.asChoice
    val resolutions = choices flatMap choiceMap.get
    if (resolutions.isEmpty) {
      cc.error(loc, s"'${symbol.name}' is not defined after processing 'gen' constructs")
      None
    } else if (resolutions.lengthIs == 1) {
      resolutions.headOption
    } else {
      val msg = s"'${symbol.name}' is ambiguous after processing 'gen' constructs. Active declarations:" ::
        (resolutions.reverse map { _.loc.prefix })
      cc.error(loc, msg: _*)
      None
    }
  }

  override def transform(tree: Tree): Tree = tree match {
    //////////////////////////////////////////////////////////////////////////
    // Replace refs to TypeChoice symbols with the resolved value
    //////////////////////////////////////////////////////////////////////////

    case ExprRef(symbol) if symbol.kind.isChoice =>
      resolveChoice(symbol, tree.loc) map { ExprRef(_) withLoc tree.loc } getOrElse tree

    case Sym(symbol) if symbol.kind.isChoice =>
      resolveChoice(symbol, tree.loc) map { Sym(_) withLoc tree.loc } getOrElse tree

    case _: Entity =>
      inEntity = false
      tree

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(!inEntity)
  }
}

object Generate {

  def apply(tree: Tree)(implicit cc: CompilerContext): Option[Tree] = {
    val choiceMap = mutable.Map[Symbol, Symbol]()
    val generate = new Generate(Bindings.empty, Map.empty, None, 0)(cc, new Typer, choiceMap)
    val result = tree rewrite generate
    if (generate.hadError) None else Some(result rewrite new Disambiguate(choiceMap))
  }
}
