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
import com.argondesign.alogic.core.Types.Type
import com.argondesign.alogic.core.Types.TypeChoice
import com.argondesign.alogic.core.Types.TypeInstance
import com.argondesign.alogic.lib.TreeLike
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
  def convertDefn(gen: GenDefn): Tree = unreachable
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
  override def convertDefn(gen: GenDefn): EntDefn = EntDefn(gen.defn) withLoc gen.loc
}

object DictIdxValues {
  def apply(idxs: List[Expr])(implicit cc: CompilerContext): Option[List[BigInt]] = {
    assert(idxs.nonEmpty)
    val listBuffer = ListBuffer[BigInt]()

    @tailrec
    def loop(list: List[Expr], ok: Boolean): Boolean = list match {
      case Nil => ok
      case head :: tail =>
        head.value match {
          case Some(value) =>
            listBuffer append value
            loop(tail, ok)
          case None =>
            cc.error(head, "Identifier index must be a compile time constant")
            loop(tail, false)
        }
    }

    if (loop(idxs, true)) Some(listBuffer.toList) else None
  }
}

private final class Generate(
    bindings: Bindings,
    newSymbols: Map[Symbol, Symbol],
    dispatcher: Option[Generatable], // Some, if already inside a 'gen' else None
    initialEntityLevel: Int
)(
    implicit cc: CompilerContext,
    typer: Typer,
    choiceMap: mutable.Map[Symbol, Symbol],
    dictMap: mutable.Map[Symbol, mutable.Map[List[BigInt], Symbol]]
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
      case EntFunction(Sym(symbol: TermSymbol, Nil), _) =>
        List(symbol -> cc.newSymbolLike(symbol))
      case EntInstance(Sym(symbol: TermSymbol, Nil), _, _, _) =>
        List(symbol -> cc.newSymbolLike(symbol))
      case EntEntity(Entity(Sym(symbol: TypeSymbol, Nil), _)) =>
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
          val expr = (ExprSym(symbol) withLoc symbol.loc) < end
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
              val stepStmt = StmtPost(ExprSym(symbol), "++") regularize decl.loc
              generateFor(initBindings, terminate, Loc.synthetic, body, stepStmt)
          }
        }

      case GenFor(_, None, _, _)               => unreachable
      case GenRange(Decl(_, Some(_)), _, _, _) => unreachable
      case GenRange(_: DeclRef, _, _, _)       => unreachable
      case _: GenDecl                          => unreachable // Handled in transform
      case _: GenDefn                          => unreachable // Handled in transform
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
          val clones = cloneForwardSymbols(entity.body)
          symbolMap ++= clones
          choiceMap ++= clones
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

        // Definition inside gen block
        case _: GenDefn =>
          if (dispatcher.get ne generatableEnt) {
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

  private[this] def cloneDictSymbol[T <: Symbol](sym: Sym,
                                                 newKind: Type,
                                                 newTSymbol: (String, Loc, Type) => T,
                                                 hint: String): Option[T] = {
    val Sym(symbol, idxs) = sym
    assert(idxs.nonEmpty)
    DictIdxValues(idxs) map { idxValues =>
      // Clone symbol
      val newName = idxValues.mkString(symbol.name + cc.sep, "_", "")
      newTSymbol(newName, symbol.loc, newKind) tap { newSymbol =>
        newSymbol.attr update symbol.attr
        newSymbol.attr.sourceName.set((symbol.name, idxValues))
        // Add to dictMap, error if already exists
        dictMap.getOrElseUpdate(symbol, mutable.Map()).put(idxValues, newSymbol) match {
          case Some(_) =>
            val srcName = idxValues.mkString(symbol.name + "#[", ", ", "]")
            error(sym,
                  s"${hint} '$srcName' defined multiple times after processing 'gen' constructs")
          case _ =>
        }
        // Mark original decl with identity for choice resolution
        choiceMap(symbol) = symbol
      }
    } orElse {
      hadError = true
      None
    }
  }

  private[this] def renameBasedOnBindings(symbol: Symbol, bindings: Bindings): Unit = {
    if (bindings.nonEmpty) {
      // We prefer to rename here as it helps compiler debugging if symbols
      // ultimately have unique names, plus we can make up a more meaningful
      // name here.
      symbol.rename {
        symbol.name :: {
          bindings.toList sortBy {
            _._1.loc.start
          } map {
            case (symbol, expr) => s"${symbol.name}_${expr.value.get}"
          }
        } mkString cc.sep
      }
    }
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

      // Replace outermost gen with the generate result
      case _: EntGen if generated.isDefined => Thicket(generated.get) withLoc tree.loc

      // Convert GenDecl
      case node: GenDecl if entityLevel <= 1 => dispatcher.get.convertDecl(node)

      // Convert GenDefn
      case node: GenDefn if entityLevel <= 1 => dispatcher.get.convertDefn(node)

      // Replace nested gen with the generated result
      case _: Gen if generated.isDefined => Thicket(generated.get) withLoc tree.loc

      //////////////////////////////////////////////////////////////////////////
      // Clone scalar term symbols which are declared inside a gen
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
      // Clone dict term symbol declarations
      //////////////////////////////////////////////////////////////////////////

      case DeclRef(sym: Sym, _, init) if dispatcher.nonEmpty =>
        // Rewrite references in type
        val newKind = sym.symbol.kind rewrite TypeGenerate
        // Clone symbol
        cloneDictSymbol(sym, newKind, cc.newTermSymbol, "Name") map { newSymbol =>
          // Rewrite decl
          Decl(newSymbol, init) withLoc tree.loc
        } getOrElse {
          tree
        }

      //////////////////////////////////////////////////////////////////////////
      // Clone scalar type symbols which are defined inside a gen
      //////////////////////////////////////////////////////////////////////////

      case Defn(symbol) if dispatcher.nonEmpty =>
        // Rewrite references in types
        val newKind = symbol.kind rewrite TypeGenerate
        // Clone symbol
        val newSymbol = cc.newTypeSymbol(symbol.name, symbol.loc, newKind)
        newSymbol.attr update symbol.attr
        symbolMap(symbol) = newSymbol
        choiceMap(symbol) = newSymbol
        // Rewrite defn
        Defn(newSymbol) withLoc tree.loc

      //////////////////////////////////////////////////////////////////////////
      // Clone dict type symbol declarations
      //////////////////////////////////////////////////////////////////////////

      case DefnRef(sym: Sym, _) if dispatcher.nonEmpty =>
        // Rewrite references in type
        val newKind = sym.symbol.kind rewrite TypeGenerate
        // Clone symbol
        cloneDictSymbol(sym, newKind, cc.newTypeSymbol, "Type") map { newSymbol =>
          // Rewrite defn
          Defn(newSymbol) withLoc tree.loc
        } getOrElse {
          tree
        }

      //////////////////////////////////////////////////////////////////////////
      // Rename scalar entities (already cloned in enter)
      //////////////////////////////////////////////////////////////////////////

      case Entity(Sym(symbol, Nil), _) if dispatcher.nonEmpty =>
        entityLevel -= 1
        renameBasedOnBindings(symbol, bindings)
        tree

      //////////////////////////////////////////////////////////////////////////
      // Clone dict entity definitions
      //////////////////////////////////////////////////////////////////////////

      case entity @ Entity(sym: Sym, _) if dispatcher.nonEmpty =>
        entityLevel -= 1
        // Compute new type
        val newKind = entity.typeBasedOnContents
        // Clone symbol
        cloneDictSymbol(sym, newKind, cc.newTypeSymbol, "Entity") map { newSymbol =>
          // Rewrite
          val newSym = Sym(newSymbol, Nil) withLoc sym.loc
          entity.copy(ref = newSym) withLoc entity.loc
        } getOrElse {
          tree
        }

      // Just update the entity level while outside a gen
      case _: Entity =>
        entityLevel -= 1
        tree

      //////////////////////////////////////////////////////////////////////////
      // Rename scalar instances (already cloned in enter)
      //////////////////////////////////////////////////////////////////////////

      case EntInstance(Sym(symbol, Nil), _, _, _) if dispatcher.nonEmpty =>
        renameBasedOnBindings(symbol, bindings)
        tree

      //////////////////////////////////////////////////////////////////////////
      // Clone dict instance definitions
      //////////////////////////////////////////////////////////////////////////

      case instance @ EntInstance(sym: Sym, Sym(eSymbol: TypeSymbol, _), _, _)
          if dispatcher.nonEmpty =>
        // Compute new type
        val newKind = TypeInstance(eSymbol)
        // Clone symbol
        cloneDictSymbol(sym, newKind, cc.newTermSymbol, "Instance") map { newSymbol =>
          // Rewrite
          val newSym = Sym(newSymbol, Nil) withLoc sym.loc
          instance.copy(instance = newSym) withLoc instance.loc
        } getOrElse {
          tree
        }

      //////////////////////////////////////////////////////////////////////////
      // Rename scalar functions (already cloned in enter)
      //////////////////////////////////////////////////////////////////////////

      case EntFunction(Sym(symbol, Nil), _) if dispatcher.nonEmpty =>
        renameBasedOnBindings(symbol, bindings)
        tree

      //////////////////////////////////////////////////////////////////////////
      // Clone dict function definitions
      //////////////////////////////////////////////////////////////////////////

      case func @ EntFunction(sym: Sym, _) if dispatcher.nonEmpty =>
        // Compute new type
        val newKind = sym.symbol.kind rewrite TypeGenerate
        // Clone symbol
        cloneDictSymbol(sym, newKind, cc.newTermSymbol, "Function") map { newSymbol =>
          // Rewrite
          val newSym = Sym(newSymbol, Nil) withLoc sym.loc
          func.copy(ref = newSym) withLoc func.loc
        } getOrElse {
          tree
        }

      //////////////////////////////////////////////////////////////////////////
      // Replace scalar refs to bound symbols with the value, and to generated
      // symbols with a ref to the new symbol. Dict refs handled in Resolve
      //////////////////////////////////////////////////////////////////////////

      case ExprSym(symbol: TermSymbol) =>
        bindings.get(symbol) orElse {
          symbolMap.get(symbol) map { ExprSym(_) withLoc tree.loc }
        } getOrElse tree

      case ExprSym(symbol: TypeSymbol) =>
        symbolMap.get(symbol) map { ExprSym(_) withLoc tree.loc } getOrElse tree

      case Sym(symbol, Nil) =>
        symbolMap.get(symbol) map { newSymbol =>
          Sym(newSymbol, Nil) withLoc tree.loc
        } getOrElse tree

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
        case node: EntGen  => cc.ice(node, s"EntGen remains")
      }
    }
  }
}

private final class Resolve(
    choiceMap: mutable.Map[Symbol, Symbol],
    dictMap: mutable.Map[Symbol, mutable.Map[List[BigInt], Symbol]]
)(
    implicit cc: CompilerContext
) extends TreeTransformer {

  override val typed: Boolean = false
  override val checkRefs: Boolean = false

  // Latch errors
  var hadError = false

  private[this] val refCounts = mutable.Map[Symbol, Int]()

  private[this] def error(loc: Loc, msg: String*): Unit = {
    cc.error(loc, msg: _*)
    hadError = true
  }

  // Entity level
  private var entityLevel = 0

  override def enter(tree: Tree): Unit = tree match {
    case _: Entity => entityLevel += 1
    case _         =>
  }

  private def resolveChoice(kind: Type, loc: Loc): Option[Symbol] = kind match {
    case TypeChoice(choices) =>
      val name = choices.head.name
      val resolutions = choices flatMap choiceMap.get
      if (resolutions.isEmpty) {
        if (entityLevel == 1) {
          // Only error when appears in root entity. The choice symbol might be
          // introduced in the lower entity, and if so will be resolved later
          // when gen are processed in the nested entity, or will error then
          error(loc, s"'$name' is not defined after processing 'gen' constructs")
        }
        None
      } else if (resolutions.lengthIs == 1) {
        resolutions.headOption
      } else {
        val msg = s"'$name' is ambiguous after processing 'gen' constructs. Active declarations:" ::
          (resolutions.reverse map { _.loc.prefix })
        error(loc, msg: _*)
        None
      }
    case _ => None
  }

  private def resolveChoiceSymbol(symbol: Symbol, loc: Loc): Option[Symbol] = symbol.kind match {
    case kind: TypeChoice => resolveChoice(kind, loc)
    case _                => Some(symbol)
  }

  private def resolveDict(symbol: Symbol, idxs: List[Expr], loc: Loc): Option[Symbol] =
    idxs match {
      case Nil => Some(symbol)
      case _ =>
        dictMap.get(symbol) match {
          case None => None // Gen not yet expanded (nested entity)
          case Some(dMap) =>
            DictIdxValues(idxs) orElse {
              hadError = true
              None
            } flatMap { idxValues =>
              dMap.get(idxValues) orElse {
                val expected = dictMap(symbol).head._1.length
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

  // To rewrite refs in types
  private[this] final class TypeResolve(loc: Loc) extends TreeInTypeTransformer(this) {
    override def transform(kind: Type): Type = kind match {
      case choice: TypeChoice => resolveChoice(choice, loc) map { _.kind } getOrElse kind
      case _                  => super.transform(kind)
    }
  }

  override def transform(tree: Tree): Tree = {
    val result = tree match {
      //////////////////////////////////////////////////////////////////////////
      // Replace refs to TypeChoice symbols with the resolved value
      //////////////////////////////////////////////////////////////////////////

      case ExprSym(symbol) =>
        resolveChoiceSymbol(symbol, tree.loc) map { symbol =>
          assert(!(dictMap contains symbol))
          ExprSym(symbol) withLoc tree.loc
        } getOrElse tree

      case Sym(symbol, idxs) =>
        resolveChoiceSymbol(symbol, tree.loc) flatMap {
          resolveDict(_, idxs, tree.loc)
        } map {
          Sym(_, Nil) withLoc tree.loc
        } getOrElse tree

      //////////////////////////////////////////////////////////////////////////
      // simplify now resolved ExprRefs
      //////////////////////////////////////////////////////////////////////////

      case ExprRef(Sym(symbol, Nil)) => ExprSym(symbol) withLoc tree.loc

      //////////////////////////////////////////////////////////////////////////
      // Resolve in Type
      //////////////////////////////////////////////////////////////////////////

      case Decl(symbol, _) =>
        symbol.kind = symbol.kind rewrite new TypeResolve(tree.loc)
        tree

      case Defn(symbol) =>
        symbol.kind = symbol.kind rewrite new TypeResolve(tree.loc)
        tree

      case ExprType(kind) =>
        val newKind = kind rewrite new TypeResolve(tree.loc)
        if (kind eq newKind) tree else ExprType(newKind) withLoc tree.loc

      case cast @ ExprCast(kind, _) =>
        val newKind = kind rewrite new TypeResolve(tree.loc)
        if (kind eq newKind) tree else cast.copy(kind = newKind) withLoc tree.loc

      //////////////////////////////////////////////////////////////////////////
      // Update symbol types
      //////////////////////////////////////////////////////////////////////////

      case EntInstance(Sym(iSymbol, Nil), Sym(eSymbol: TypeSymbol, Nil), _, _) =>
        iSymbol.kind = TypeInstance(eSymbol)
        tree

      case entity: Entity => {
        if (entityLevel > 1 || entity.symbol.attr.variant.contains("verbatim")) {
          entity
        } else {
          // Remove const declarations with no more references
          @tailrec
          def loop(body: List[Ent]): List[Ent] = {
            if (refCounts.valuesIterator contains 0) {
              loop {
                body filter {
                  case EntDecl(Decl(symbol, initOpt)) if refCounts get symbol contains 0 =>
                    refCounts remove symbol
                    symbol.kind.visit { case ExprSym(s)         => refCounts(s) -= 1 }
                    initOpt foreach { _ visit { case ExprSym(s) => refCounts(s) -= 1 } }
                    false
                  case _ => true
                }
              }
            } else {
              body
            }
          }

          entity.copy(body = loop(entity.body)) withLoc entity.loc
        }
      } tap { result =>
        result.symbol.kind = result.typeBasedOnContents
        entityLevel -= 1
      }

      case _ => tree
    }

    result match {
      case ExprSym(symbol: Symbol) =>
        refCounts.updateWith(symbol) { _ map { _ + 1 } }
      case Decl(symbol, _) if symbol.kind.isConst && entityLevel == 1 =>
        refCounts(symbol) = 0
      case EntInstance(_, _, _, paramExprs) =>
        // Ignore references in parameter assignments. These will be stripped
        // by the end of Specialize.
        paramExprs foreach {
          _ visit {
            case ExprSym(symbol: Symbol) => refCounts.updateWith(symbol) { _ map { _ - 1 } }
          }
        }
      case _ =>
    }

    result
  }

  override def finalCheck(tree: Tree): Unit = {
    if (!hadError) {
      assert(entityLevel == 0)
      def check(tree: TreeLike): Unit = {
        tree visit {
          case _: EntEntity                         => // Stop visit
          case node: DeclRef                        => cc.error(node, "DeclRef remains")
          case node: DefnRef                        => cc.error(node, "DefnRef remains")
          case node @ Sym(_, idxs) if idxs.nonEmpty => cc.error(node, "Sym with indices remains")
          case node: TypeChoice                     => cc.ice(s"$node remains")
          case Decl(symbol, _)                      => check(symbol.kind)
          case Defn(symbol)                         => check(symbol.kind)
          case Sym(symbol, Nil)                     => check(symbol.kind)
          case ExprSym(symbol)                      => check(symbol.kind)
        }
      }
      check(tree)
    }
  }
}

object Generate {

  def apply(tree: Tree)(implicit cc: CompilerContext): Option[Tree] = {
    val cMap = mutable.Map[Symbol, Symbol]()
    val dMap = mutable.Map[Symbol, mutable.Map[List[BigInt], Symbol]]()
    val generate = new Generate(Bindings.empty, Map.empty, None, 0)(cc, new Typer, cMap, dMap)
    val generated = tree rewrite generate
    if (generate.hadError) {
      None
    } else {
      val resolve = new Resolve(cMap, dMap)
      val resolved = generated rewrite resolve
      if (resolve.hadError) None else Some(resolved)
    }
  }
}
