////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Remove all wholly unused symbols if possible
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.analysis.ReadSymbols
import com.argondesign.alogic.analysis.WrittenSymbols
import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.Expr.InstancePortSel
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.enums.EntityVariant
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.util.unreachable
import com.argondesign.alogic.util.IteratorOps._

import scala.collection.immutable.HashSet
import scala.collection.mutable

// The transform that removes all symbols not in the given set
final private class RemoveSymbols(retained: Set[Symbol]) extends StatelessTreeTransformer {

  private def emptyStmt(stmt: Stmt): Boolean = stmt match {
    case StmtBlock(body)         => body forall emptyStmt
    case StmtIf(_, eBody, tBody) => (eBody forall emptyStmt) && (tBody forall emptyStmt)
    case StmtCase(_, cases) =>
      cases forall {
        case CaseRegular(_, stmts) => stmts forall emptyStmt
        case CaseDefault(stmts)    => stmts forall emptyStmt
        case _: CaseSplice         => unreachable
      }
    case _: StmtComment => true
    case _              => false
  }

  override def enter(tree: Tree): Option[Tree] = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Remove connects driving only dropped symbols
    ////////////////////////////////////////////////////////////////////////////

    case EntAssign(InstancePortSel(iSymbol, pSymbol), _) =>
      Option.unless(retained(iSymbol) && retained(pSymbol))(Stump)

    case EntAssign(lhs, _) =>
      Option.unless(WrittenSymbols(lhs) exists retained)(Stump)

    ////////////////////////////////////////////////////////////////////////////
    // Remove assignments to only dropped symbols
    ////////////////////////////////////////////////////////////////////////////

    case StmtAssign(lhs, _)  => Option.unless(WrittenSymbols(lhs) exists retained)(Stump)
    case StmtDelayed(lhs, _) => Option.unless(WrittenSymbols(lhs) exists retained)(Stump)

    ////////////////////////////////////////////////////////////////////////////
    // Drop whole foreign function Decl/Defn if unused
    ////////////////////////////////////////////////////////////////////////////

    case d: DeclFunc => Some(if (retained(d.symbol)) tree else Stump)
    case d: DefnFunc => Some(if (retained(d.symbol)) tree else Stump)

    ////////////////////////////////////////////////////////////////////////////
    // Remove Decl/Defn of symbol not being retained
    ////////////////////////////////////////////////////////////////////////////

    case Decl(symbol) =>
      Option.unless(retained(symbol)) {
        // If we are removing a _q, drop the suffix from the _d
        symbol.attr.flop.get foreach { dSymbol =>
          assert(dSymbol.name endsWith "_d")
          dSymbol.name = dSymbol.name.dropRight(2)
          dSymbol.attr.combSignal set true
        }
        Stump
      }

    case Defn(symbol) => Option.unless(retained(symbol))(Stump)

    //
    case _ => None
  }

  override def transform(tree: Tree): Tree = tree match {

    ////////////////////////////////////////////////////////////////////////////
    // Keep comments on their own
    ////////////////////////////////////////////////////////////////////////////

    case _: StmtComment => tree

    ////////////////////////////////////////////////////////////////////////////
    // Fold empty statements as we go to get rid of unused branch conditions
    ////////////////////////////////////////////////////////////////////////////

    case stmt: Stmt if emptyStmt(stmt) => Stump

    ////////////////////////////////////////////////////////////////////////////
    // Fold empty clocked processes as we go to get rid of unused sensitivities
    ////////////////////////////////////////////////////////////////////////////

    case ent: EntClockedProcess if ent.stmts.forall(emptyStmt) => Stump

    ////////////////////////////////////////////////////////////////////////////
    // Replace reference to removed symbol with constant zero
    ////////////////////////////////////////////////////////////////////////////

    case ExprSym(symbol) if !retained(symbol) =>
      assert(symbol.kind.isPacked)
      TypeAssigner(ExprInt(symbol.kind.isSigned, symbol.kind.width.toInt, 0) withLocOf tree)

    //
    case _ => tree
  }

}

object RemoveUnused extends PairsTransformerPass {
  val name = "remove-unused"

  // Comptues dependencies within an entity
  private def computeDependencies(
      defn: DefnEntity
    ): (
      // Dependencies of local symbols on other local symbols
      collection.Map[Symbol, Set[Symbol]],
      // Dependencies of instance inputs on local symbols
      collection.Map[(Symbol, Symbol), Set[Symbol]],
      // Dependencies of local symbols on instance outputs
      collection.Map[Symbol, Set[(Symbol, Symbol)]],
      // Local symbol dependencies of impure statements
      collection.Set[Symbol]
  ) = {

    // The accumulators
    val localDeps = mutable.Map[Symbol, Set[Symbol]]()
    val iputDeps = mutable.Map[(Symbol, Symbol), Set[Symbol]]()
    val oputDeps = mutable.Map[Symbol, Set[(Symbol, Symbol)]]()
    val impureDeps = mutable.Set[Symbol]()

    def addLocalDeps(dependencies: Set[Symbol])(symbol: Symbol): Unit =
      localDeps.updateWith(symbol) {
        case Some(ds) => Some(ds union dependencies)
        case None     => Some(dependencies)
      }

    def walkStmt(controlFlowDependencies: Set[Symbol])(stmt: Stmt): Unit = stmt match {
      case StmtAssign(lhs, rhs) =>
        val dependencies =
          controlFlowDependencies concat ReadSymbols.lval(lhs) concat ReadSymbols.rval(rhs)
        WrittenSymbols(lhs).foreach(addLocalDeps(dependencies))
      case StmtDelayed(lhs, rhs) =>
        val dependencies =
          controlFlowDependencies concat ReadSymbols.lval(lhs) concat ReadSymbols.rval(rhs)
        WrittenSymbols(lhs).foreach(addLocalDeps(dependencies))
      case StmtOutcall(lhs, f, rhss) =>
        val dependencies =
          controlFlowDependencies concat ReadSymbols.lval(lhs) concat (f :: rhss).flatMap(
            ReadSymbols.rval
          )
        WrittenSymbols(lhs).foreach(addLocalDeps(dependencies))
        // Assume 'f' is impure
        impureDeps.addAll(dependencies)
      case StmtIf(cond, thenStmts, elseStmts) =>
        val dependencies = controlFlowDependencies concat ReadSymbols.rval(cond)
        thenStmts.foreach(walkStmt(dependencies))
        elseStmts.foreach(walkStmt(dependencies))
      case StmtCase(expr, cases) =>
        // Note: Assumes the default case is last in 'cases'
        cases.foldLeft(controlFlowDependencies concat ReadSymbols.rval(expr)) {
          case (controlFlowDependencies, CaseRegular(cond, stmts)) =>
            val dependencies = controlFlowDependencies concat cond.flatMap(ReadSymbols.rval)
            stmts.foreach(walkStmt(dependencies))
            dependencies
          case (controlFlowDependencies, CaseDefault(stmts)) =>
            stmts.foreach(walkStmt(controlFlowDependencies))
            controlFlowDependencies
          case _ => unreachable
        }
      case StmtSplice(spliceable) =>
        spliceable match {
          case AssertionAssert(cond, _) =>
            impureDeps.addAll(controlFlowDependencies)
            impureDeps.addAll(ReadSymbols.rval(cond))
          case AssertionUnreachable(_, condOpt, _) =>
            impureDeps.addAll(controlFlowDependencies)
            condOpt.foreach(cond => impureDeps.addAll(ReadSymbols.rval(cond)))
          case _ => println(spliceable); unreachable
        }
      case StmtExpr(expr) =>
        expr match {
          case _: ExprBuiltin | _: ExprCall =>
            // Assume function is impure
            impureDeps.addAll(controlFlowDependencies)
            impureDeps.addAll(ReadSymbols.rval(expr))
          case _ => println(expr); unreachable
        }
      case _: StmtComment => //
      case _              => println(stmt); unreachable
    }

    // enumerate body
    defn.body.foreach {
      case EntAssign(lhs, InstancePortSel(iSymbol, pSymbol)) =>
        val dependencies = ReadSymbols.lval(lhs).toSet
        val portDep = (iSymbol, pSymbol)
        WrittenSymbols(lhs) foreach { symbol =>
          addLocalDeps(dependencies)(symbol)
          oputDeps.updateWith(symbol) {
            case Some(ds) => Some(ds incl portDep)
            case None     => Some(Set(portDep))
          }
        }
      case EntAssign(InstancePortSel(iSymbol, pSymbol), rhs) =>
        val dependencies = ReadSymbols.rval(rhs).toSet
        iputDeps.updateWith((iSymbol, pSymbol)) {
          case Some(ds) => Some(ds union dependencies)
          case None     => Some(dependencies)
        }
      case EntAssign(lhs, rhs) =>
        val dependencies = Set from {
          ReadSymbols.lval(lhs) concat ReadSymbols.rval(rhs)
        }
        WrittenSymbols(lhs).foreach(addLocalDeps(dependencies))
      case EntCombProcess(stmts) =>
        stmts.foreach(walkStmt(Set.empty))
      case EntClockedProcess(clk, rstOpt, stmts) =>
        val dependencies = Set from {
          ReadSymbols.rval(clk) concat rstOpt.iterator.flatMap(ReadSymbols.rval)
        }
        stmts.foreach(walkStmt(dependencies))
      case _: EntVerbatim | _: EntComment | _: EntSplice => Iterator.empty
      case _: EntConnect | _: EntConnectInputs           => unreachable
    }

    // In verbatim entities, assume everything depends on everything
    if (defn.variant == EntityVariant.Ver) {
      val localSymbols = defn.defns.map(_.symbol).toSet
      localSymbols.foreach(addLocalDeps(localSymbols))
    }

    (localDeps, iputDeps, oputDeps, impureDeps)
  }

  def process(pairs: Pairs)(implicit cc: CompilerContext): Pairs = {

    // We can do a lot in parallel
    val parPairs = pairs.asPar

    // Compute all dependencies. See 'computeDependencies' for what all these
    // mean...
    val (localDeps, iputDeps, oputDeps, impureDeps) =
      parPairs
        .aggregate(
          (
            Map.empty[Symbol, Set[Symbol]],
            Map.empty[(Symbol, Symbol), Set[Symbol]],
            Map.empty[Symbol, Set[(Symbol, Symbol)]],
            Map.empty[Symbol, collection.Set[Symbol]]
          )
        )(
          {
            case ((accL, accI, accO, accP), (_, defn: DefnEntity)) =>
              val (currL, currI, currO, currP) = computeDependencies(defn)
              (
                accL concat currL,
                accI concat currI,
                accO concat currO,
                accP.updated(defn.symbol, currP)
              )
            case _ => unreachable
          },
          {
            case ((aL, aI, aO, aP), (bL, bI, bO, bP)) =>
              (aL concat bL, aI concat bI, aO concat bO, aP concat bP)
          }
        )

    // Prepare for effectively a depth first traversal of the flattened
    // hierarchy.

    // Graph nodes to visit next. We start from outputs of top level entities.
    // Note we also need to include symbols used by impure statements in the
    // top levels, and all symbols used by impure statements in any instance.
    val pending = mutable.Stack.from {
      def enumerateInstances(decl: DeclEntity, hier: List[Symbol]): Iterator[List[Symbol]] = {
        decl.instances.iterator
          .map(_.symbol)
          .flatMap { symbol =>
            val sub = symbol :: hier
            Iterator.single(sub) concat
              enumerateInstances(symbol.kind.asEntity.symbol.decl.asInstanceOf[DeclEntity], sub)
          }
      }

      parPairs.iterator.collect {
        case (decl @ DeclEntity(symbol, decls), _) if symbol.attr.topLevel.isSet =>
          decls.iterator
            .collect { // Outputs of top-levels
              case DeclOut(symbol, _, _, _) => symbol
            }
            .concat( // Symbols used by impure statements in top-levels
              impureDeps.get(symbol).iterator.flatten
            )
            .map(_ :: Nil)
            .concat( // Symbols used by impure statements in any instance under top-levels
              enumerateInstances(decl, Nil).flatMap { instance =>
                impureDeps
                  .get(instance.head.kind.asEntity.symbol)
                  .iterator
                  .flatten
                  .map(_ :: instance)
              }
            )
      }.flatten
    }

    // Set of nodes already visited
    val visited = mutable.Set[List[Symbol]]()

    def enqueue(node: List[Symbol]): Unit = if (!visited(node)) { pending.push(node) }

    // Traverse graph
    while (pending.nonEmpty) {
      val node = pending.pop()
      if (!visited(node)) {
        visited.add(node)
        val symbol = node.head
        val instance = node.tail
        localDeps
          .get(symbol)
          .iterator
          .flatten
          .foreach(dependency => enqueue(dependency :: instance))
        oputDeps
          .get(symbol)
          .iterator
          .flatten
          .foreach { case (iSymbol, pSymbol) => enqueue(pSymbol :: iSymbol :: instance) }
        instance match {
          case parent :: rest =>
            iputDeps
              .get((parent, symbol))
              .iterator
              .flatten
              .foreach(dependency => enqueue(dependency :: rest))
            impureDeps
              .get(parent.kind.asEntity.symbol)
              .iterator
              .flatten
              .foreach(dependency => enqueue(dependency :: instance))
          case _ =>
        }
      }
    }

    // Gather all symbols that need to be kept
    val keep = HashSet from {
      // Keep all nodes visited, including instantiated entities
      visited.iterator.flatten.flatMap { s =>
        if (s.kind.isEntity) Iterator(s, s.kind.asEntity.symbol) else Iterator.single(s)
      } concat {
        // Some additional symbols must be kept, even if they have not been
        // reached during the dependency search.
        parPairs.iterator.flatMap {
          case (DeclEntity(symbol, decls), defn: DefnEntity) =>
            Iterator.when(symbol.attr.topLevel.isSet) thenIterator {
              // Keep the top level entities themselves
              Iterator.single(symbol) concat {
                // Keep input ports of top level entities (except for ones added
                // by the compiler), in order to preserve the user defined
                // interface
                decls.iterator.collect {
                  case DeclIn(s, _, _) if !s.attr.clk.isSet && !s.attr.rst.isSet => s
                }
              }
            } concat {
              // Gather symbols that must be kept because they are used as a
              // placeholder on the left hand side of an assignment
              defn flatCollect {
                case EntAssign(lhs: ExprCat, _)   => WrittenSymbols(lhs)
                case StmtAssign(lhs: ExprCat, _)  => WrittenSymbols(lhs)
                case StmtDelayed(lhs: ExprCat, _) => WrittenSymbols(lhs)
                case StmtOutcall(lhs, _, _)       => WrittenSymbols(lhs)
                case _: Expr                      => Iterator.empty // Stop descent
              }
            }
          case _ => unreachable
        }
      }
    }

    // The pruning transform
    val prune = new RemoveSymbols(keep)

    // Drop all entities not marked for retention, then apply the pruning
    // transform to the rest.
    val processedPairs = parPairs.collect {
      case (decl, defn) if keep(decl.symbol) => (decl rewrite prune, defn rewrite prune)
    }

    // Need to update types of entities and instances as ports might have
    // been removed.
    object UpdateTypes extends StatefulTreeTransformer {
      override def replace(symbol: Symbol): Boolean = symbol.kind match {
        case TypeType(_: TypeEntity) => true
        case _: TypeEntity           => true
        case _                       => false
      }
    }

    // This we need to do sequentially as it's a global mapping of symbols
    processedPairs.asSeq.map {
      case (decl, defn) => (decl rewrite UpdateTypes, defn rewrite UpdateTypes)
    }
  }

}
