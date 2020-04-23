////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Peter de Rivaz/Geza Lore
//
// DESCRIPTION:
//
// Analyse call graph and:
//  - Check reclimit attributes
//  - Ensure reclimit attributes exist on all functions
//  - Check stacklimit attributes
//  - Allocate return stack with required depth
//  - Remove unused functions
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.enums.EntityVariant
import com.argondesign.alogic.lib.Matrix
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.mutable

final class AnalyseCallGraph(implicit cc: CompilerContext) extends StatefulTreeTransformer {

  //////////////////////////////////////////////////////////////////////////
  // State for collecting information in the enter section
  //////////////////////////////////////////////////////////////////////////

  // Current function we are inside of
  private var currentFunction: Symbol = _

  // Set of caller -> callee call arcs. Caller is option, None represents
  // the fictional caller that called the entry point.
  private val calls = mutable.Set.empty[(Option[Symbol], Symbol)]

  // Set of caller -> callee goto arcs
  private val gotos = mutable.Set.empty[(Symbol, Symbol)]

  // Count of number of times each function is called
  private val callCounts = mutable.Map[Symbol, Int]().withDefaultValue(0)

  // All control function symbols
  private val functionSymbols = mutable.LinkedHashSet[Symbol]()

  ////////////////////////////////////////////////////////////////////////////
  // All call arcs in the entity
  //
  // Goto arcs are treated as proper tail calls, i.e.: as calls from the
  // caller of the function executing the goto to the target of the goto.
  // To do this we transitively propagate goto arcs as call arcs from all
  // functions that call the source of the goto to the target of the goto:
  //   a --call--> b --goto--> c
  // is treated as
  //   a --call--> {b, c}
  // or in a more general case:
  //   {a, b} --call--> {c, d} --goto--> {e, f}
  // is treated as
  //   {a, b} --call--> {c, d, e, f}
  // We store each arc as a triple, (caller, returnee, callee), where returnee
  // is the function whose call site we need to return to from the callee,
  // which might be different due to goto, e.g.:
  //   a --call--> b             is stored as (a, b, b)
  //   a --call--> b --goto--> c is stored as (a, b, c)
  //////////////////////////////////////////////////////////////////////////////

  type CallArc = (Option[Symbol], Symbol, Symbol)

  private def computeCallArcSet: Set[CallArc] = {

    @tailrec
    def loop(arcs: Set[CallArc]): Set[CallArc] = {
      // Add one batch of transitive goto arcs
      val extra = gotos.iterator flatMap {
        case (intermediate, callee) =>
          arcs.iterator collect {
            case (caller, returnee, `intermediate`) => (caller, returnee, callee)
          }
      }

      val expanded = extra.foldLeft(arcs)(_ + _)

      // Keep going until settled
      if (expanded == arcs) arcs else loop(expanded)
    }

    loop(Set from {
      calls.iterator map { case (caller, callee) => (caller, callee, callee) }
    })
  }

  //////////////////////////////////////////////////////////////////////////////
  // Adjacency matrix of the call graph
  //////////////////////////////////////////////////////////////////////////////
  private def computeAdjacentyMatrix(callArcs: Set[CallArc]): Matrix[Int] = Matrix {
    val calleeMap = callArcs.groupMap(_._1)(_._3) withDefaultValue Set.empty
    List from {
      functionSymbols.iterator map { caller =>
        val calleeSet = calleeMap(Some(caller))
        List from {
          functionSymbols.iterator map { symbol =>
            if (calleeSet contains symbol) 1 else 0
          }
        }
      }
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Indirect connectivity matrix of the call graph
  //
  // Compute the indirect matrix by summing over all powers of the adjacency
  // matrix between 2 and nFunctions. This will have a non-zero on the diagonal
  // if the function is recursively entered through a cycle of any length >= 2
  //////////////////////////////////////////////////////////////////////////////
  private def computeIndirectCallMatrix(adjMat: Matrix[Int]): Matrix[Int] =
    if (adjMat.size._1 == 1) {
      Matrix.zeros[Int](1)
    } else {
      @tailrec
      def loop(pow: Int, curr: Matrix[Int], acc: Matrix[Int]): Matrix[Int] = {
        if (pow == 2) {
          acc
        } else {
          val prod = curr * adjMat
          loop(pow - 1, prod, prod + acc)
        }
      }

      val square = adjMat * adjMat
      loop(adjMat.size._1, square, square)
    }

  private def warnIgnoredStacklimitAttribute(eSymbol: Symbol): Unit =
    if (eSymbol.attr.stackLimit.isSet) {
      val loc = eSymbol.loc
      val name = eSymbol.name
      cc.warning(loc, s"'stacklimit' attribute ignored on entity '$name'")
    }

  //////////////////////////////////////////////////////////////////////////////
  // Collect 'reclimit' values and check that they are sensible
  //////////////////////////////////////////////////////////////////////////////
  private def computeRecLimits(
      callArcs: Set[CallArc],
      adjMat: Matrix[Int],
      indMat: Matrix[Int]
    ): Option[List[Int]] = {
    val directlyRecursive = adjMat.diagonal map { _ != 0 }
    val indirectlyRecursive = indMat.diagonal map { _ != 0 }

    val list = List from {
      for {
        (symbol, isRecD, isRecI) <-
          functionSymbols lazyZip directlyRecursive lazyZip indirectlyRecursive
      } yield {
        lazy val loc = symbol.loc
        lazy val name = symbol.name
        val exprOpt = symbol.attr.recLimit.get
        if (!isRecD && !isRecI) {
          if (exprOpt.isDefined) {
            cc.warning(loc, s"'reclimit' attribute ignored on function '$name'")
          }
          1
        } else {
          exprOpt match {
            case None =>
              val hint = if (isRecD) "Recursive" else "Indirectly recursive"
              cc.error(loc, s"$hint function '$name' requires 'reclimit' attribute")
              0
            case Some(expr) =>
              expr.value match {
                case Some(value) if value < 2 =>
                  cc.error(
                    loc,
                    s"Recursive function '$name' has 'reclimit' attribute equal to $value"
                  )
                  0
                case None =>
                  symbol.attr.recLimit set Expr(2)
                  cc.error(loc, s"Cannot compute value of 'reclimit' attribute of function '$name'")
                  0
                case Some(value) => value.toInt
              }
          }
        }
      }
    }

    // 0 is only possible if there was an error so yield None in that case
    if (list contains 0) None else Some(list)
  }

  //////////////////////////////////////////////////////////////////////////////
  // Computes the length of the longest static path in the call graphs
  //////////////////////////////////////////////////////////////////////////////
  private def computeLongestPathLength(
      callArcs: Set[CallArc],
      costOfCalling: Map[Symbol, Int]
    ): Int = {
    // Find the longest simple (non-cyclic) path in the call graph
    // Note strictly this is only an upper bound on the required
    // return stack depth if there is recursion

    // Longest path from 'node' that goes through only vertices that are in 'nodes'
    def longestPathFrom(node: Option[Symbol], nodes: Set[Symbol]): (Int, List[Symbol]) = {
      assert(!(node exists nodes.contains))
      // Candidate successors are the vertices which are callees of this node and are in 'nodes'
      val candidates = callArcs.iterator collect {
        case (`node`, returnee, callee) if nodes contains callee => (returnee, callee)
      }
      // Find longest paths from each of these candidates
      val paths = for ((returnee, callee) <- candidates) yield {
        val (l, p) = longestPathFrom(Some(callee), nodes - callee)
        (returnee, l, p)
      }

      if (paths.isEmpty) {
        // If no further paths, we are done
        (0, node.toList)
      } else {
        // Find the one that is the longest among all candidates
        val (returnee, len, longest) = paths maxBy {
          case (fc, l, _) => costOfCalling(fc) + l
        }
        // Prepend this node
        (costOfCalling(returnee) + len, node.fold(longest)(_ :: longest))
      }
    }

    // Get the longest path originating from "None", which represents the
    // original call to the entry point function.
    longestPathFrom(None, functionSymbols.toSet)._1
  }

  // If the return point of this function is known at compile time, then
  // Some(function-the-call-site-of-whom-to-return-to).  Otherwise, None. E.g.
  // If the only call arc ending at c is (a,b,c), (and b is only called once
  // within the body of a) then the unique return point is Some(b) as c needs
  // to return to the call site of b. If no call arcs end at c then return
  // to the top of main.
  private def uniqueReturnPoint(symbol: Symbol, callArcs: Set[CallArc]): Option[Symbol] = {
    val it = callArcs.iterator collect {
      case (_, returnee, `symbol`) => returnee
    }
    val returnee = it.next
    // If the returnee is unique and is called only once, then we have an urp
    Option.when(!it.hasNext && callCounts(returnee) == 1) {
      returnee
    }
  }

  // When this function is called, should we push an entry to the return stack?
  // True if and only if every call arc with this function as returnee
  // ends at a function with uniqueReturnPoint.
  private def pushStackOnCall(symbol: Symbol, callArcs: Set[CallArc]): Boolean =
    // We can skip checking this if the function has multiple call sites, as it
    // will then definitely need to push
    (callCounts(symbol) > 1) || {
      // This function is indeed called only once, figure out if we need a push
      val arcsWithSymbolAsReturnee = callArcs filter { _._2 == symbol }
      // Since callCounts(symbol) == 1, all tuples should have identical callers
      assert((arcsWithSymbolAsReturnee map { _._1 }).sizeIs <= 1)
      arcsWithSymbolAsReturnee exists {
        case (_, _, callee) =>
          uniqueReturnPoint(callee, callArcs) match {
            case None           => true // Must push stack if callee does not have an urp
            case Some(`symbol`) => false // The unique return point should be exactly this symbol
            case _              => unreachable
          }
      }
    }

  //////////////////////////////////////////////////////////////////////////////
  // The actual return stack depth required
  //////////////////////////////////////////////////////////////////////////////
  private def returnStackDepth(
      eSymbol: Symbol,
      callArcs: Set[CallArc],
      adjMat: Matrix[Int],
      indMat: Matrix[Int],
      recLimits: List[Int]
    ): Int = {
    // Compute if the entity uses any recursion
    val hasRecursion = adjMat.diagonal.sum + indMat.diagonal.sum != 0

    // Issue ignored attribute waring if required
    if (!hasRecursion) {
      warnIgnoredStacklimitAttribute(eSymbol)
    }

    // Compute the value of the 'stacklimit' attribute of the entity
    lazy val stackLimit: Option[Int] = {
      eSymbol.attr.stackLimit.get flatMap { expr =>
        lazy val loc = eSymbol.loc
        lazy val name = eSymbol.name
        expr.value match {
          case Some(value) if value < 1 =>
            cc.error(loc, s"Entity '$name' has 'stacklimit' attribute equal to $value")
            None
          case None =>
            cc.error(loc, s"Cannot compute value of 'stacklimit' attribute of entity '$name'")
            None
          case Some(value) => Some(value.toInt)
        }
      }
    }

    // Compute the maximum number of pushes to stack
    if (hasRecursion && stackLimit.isDefined) {
      // If the entity uses recursion and has a sane
      // 'stacklimit' attribute, use the value of that.
      // We subtract one as the root function does not return anywhere
      stackLimit.get - 1
    } else {
      // Map from symbol -> max stack pushes for this function
      val costOfCalling: Map[Symbol, Int] = Map from {
        (functionSymbols lazyZip recLimits) map {
          case (returnee, recLimit) =>
            returnee -> (if (returnee.attr.pushStackOnCall.value) recLimit else 0)
        }
      }

      // Find the longest path in the static call graph
      computeLongestPathLength(callArcs, costOfCalling)
    }
  }

  override protected def skip(tree: Tree): Boolean = tree match {
    case decl: DeclEntity                                               => decl.functions.isEmpty
    case DefnEntity(symbol, variant, _) if variant != EntityVariant.Fsm =>
      // Warn early if there are no functions at all, as
      // we will not have an opportunity to do it later
      warnIgnoredStacklimitAttribute(symbol)
      true
    case _: DefnEntity => false
    case _: EntDefn    => false
    case _: Ent        => true
    case _: DefnFunc   => false
    case _: Defn       => true
    case _: Decl       => true
    case _             => false
  }

  override def enter(tree: Tree): Option[Tree] = tree match {

    //////////////////////////////////////////////////////////////////////////
    // Gather all function symbols defined in the
    //////////////////////////////////////////////////////////////////////////

    case defn: DefnEntity =>
      assert(functionSymbols.isEmpty)
      functionSymbols ++= defn.functions.iterator map { _.symbol } filter { _.kind.isCtrlFunc }
      val entryPoints = functionSymbols filter { _.attr.entry.isSet }
      Option.when(entryPoints.sizeIs != 1) {
        if (entryPoints.isEmpty) {
          cc.error(defn.symbol, "No 'main' function in fsm.")
        } else {
          val locations = List from { entryPoints.iterator map { _.loc.prefix } }
          cc.error(defn.symbol, "Multiple 'main' functions in fsm at:" :: locations: _*)
        }
        tree
      }

    //////////////////////////////////////////////////////////////////////////
    // Note current function we are processing
    //////////////////////////////////////////////////////////////////////////

    case defn: DefnFunc =>
      currentFunction = defn.symbol
      if (currentFunction.attr.entry.isSet) {
        // Add the initial call to the entry point function.
        calls add (None -> currentFunction)
        callCounts(currentFunction) += 1
      }
      None

    //////////////////////////////////////////////////////////////////////////
    // Collect the call graph edges as we go
    //////////////////////////////////////////////////////////////////////////

    case ExprCall(ref, _) if ref.tpe.isCtrlFunc =>
      ref match {
        case ExprSym(callee) =>
          calls add (Some(currentFunction) -> callee)
          callCounts(callee) += 1
        case _ => unreachable
      }
      None

    case StmtGoto(ExprSym(callee)) =>
      gotos add (currentFunction -> callee)
      None

    //
    case _ => None

  }

  override def transform(tree: Tree): Tree = tree match {
    case defn: DefnEntity =>
      val allCallArcs = computeCallArcSet
      // Compute which functions are actually used
      val usedFunctions = {
        def loop(used: Set[Symbol]): Set[Symbol] = {
          val extra = allCallArcs collect {
            case (None, _, callee) if !used(callee)                         => callee
            case (Some(caller), _, callee) if used(caller) && !used(callee) => callee
          }
          if (extra.isEmpty) used else loop(used union extra)
        }
        loop(Set.empty)
      }

      // Filter call arcs and function symbols to the used functions
      val callArcs = allCallArcs filter {
        case (Some(caller), _, _) => usedFunctions(caller)
        case _                    => true
      }
      functionSymbols filterInPlace usedFunctions

      val adjMat = computeAdjacentyMatrix(callArcs)
      val indMat = computeIndirectCallMatrix(adjMat)

      // Ensure 'reclimit' attributes exist on all functions
      val recLimtis =
        computeRecLimits(callArcs, adjMat, indMat).getOrElse(List.fill(functionSymbols.size)(1))
      for ((symbol, value) <- functionSymbols lazyZip recLimtis) {
        symbol.attr.recLimit set Expr(value)
      }

      // Set function attributes
      for { symbol <- functionSymbols } {
        symbol.attr.pushStackOnCall set pushStackOnCall(symbol, callArcs)
        symbol.attr.staticReturnPoint set uniqueReturnPoint(symbol, callArcs)
      }

      for { symbol <- functionSymbols } {
        symbol.attr.popStackOnReturn set {
          symbol.attr.staticReturnPoint.value match {
            case Some(returnee) => returnee.attr.pushStackOnCall.value
            case None           => true
          }
        }
      }

      // Drop unused functions
      val newBody = defn.body filter {
        case EntDefn(DefnFunc(symbol, _, _)) if symbol.kind.isCtrlFunc => usedFunctions(symbol)
        case _                                                         => true
      }

      val stackDepth = returnStackDepth(defn.symbol, callArcs, adjMat, indMat, recLimtis)

      if (stackDepth == 0) {
        TypeAssigner(defn.copy(body = newBody) withLoc defn.loc)
      } else {
        // Allocate the return stack with TypeVoid entries and with the right
        // depth. The elementType will be refined in a later pass when the
        // state numbers are allocated
        val name = if (stackDepth > 1) "return_stack" else "return_state"
        val symbol = cc.newSymbol(name, defn.loc) tap { s =>
          s.kind = TypeStack(TypeVoid, stackDepth)
          s.attr.returnStack set true
        }
        // Add th Defn of the return stack. ConvertControl relies on it being
        // added to the front so it can be picked up in 'transform' early.
        val stackDefn = EntDefn(symbol.mkDefn) regularize defn.loc
        TypeAssigner(defn.copy(body = stackDefn :: newBody) withLoc defn.loc)
      }

    case decl: DeclEntity =>
      // Drop unused functions
      val newDecls = decl.decls filter {
        case DeclFunc(symbol, _, _, _) if symbol.kind.isCtrlFunc => functionSymbols(symbol)
        case _                                                   => true
      }

      decl.symbol.defn.defns collectFirst {
        // Add the Decl of the return stack
        case Defn(symbol) if symbol.attr.returnStack.isSet =>
          val stackDecl = symbol.mkDecl regularize decl.loc
          TypeAssigner(decl.copy(decls = stackDecl :: newDecls) withLoc decl.loc)
      } getOrElse {
        TypeAssigner(decl.copy(decls = newDecls) withLoc decl.loc)
      }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = tree visit {
    case node: Tree if !node.hasTpe => cc.ice(node, "Lost tpe of", node.toString)
  }

}

object AnalyseCallGraph extends EntityTransformerPass(declFirst = false) {
  val name = "analyse-call-graph"

  def create(symbol: Symbol)(implicit cc: CompilerContext) = new AnalyseCallGraph
}
