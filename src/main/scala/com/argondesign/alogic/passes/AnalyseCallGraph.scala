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
  private[this] var currentFunction: Symbol = _

  // Set of caller -> callee call arcs
  private[this] val callArcs = mutable.Set.empty[(Symbol, Symbol)]

  // Set of caller -> callee goto arcs
  private[this] val gotoArcs = mutable.Set.empty[(Symbol, Symbol)]

  // All function symbols
  private[this] var functionSymbols: List[Symbol] = _

  //////////////////////////////////////////////////////////////////////////
  // Lazy vals used for computation in the transform section
  //////////////////////////////////////////////////////////////////////////

  // number of function symbols
  private[this] lazy val nFunctions = functionSymbols.length

  //////////////////////////////////////////////////////////////////////////////
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
  //   {a,b} --call--> {c, d} --goto--> {e, f}
  // is treated as
  //   {a, b} --call--> {c, d, e, f}
  //////////////////////////////////////////////////////////////////////////////
  private[this] lazy val callArcSet = if (callArcs.isEmpty) {
    // As an optimization, if we know that there are no calls, then
    // we know that there won't be any after goto conversion either
    Set.empty[(Symbol, Symbol)]
  } else {
    // Set of all callers of callee based on the current callArcs
    // This is forced to an immutable set to avoid iterating callArcs
    // while it is being modified in the for expression below
    def callers(callee: Symbol): Set[Symbol] = {
      (callArcs filter { _._2 == callee } map { _._1 }).toSet
    }

    for {
      (intermediate, callee) <- gotoArcs
      caller <- callers(intermediate)
    } {
      callArcs add (caller -> callee)
    }

    callArcs.toSet
  }

  // Map from caller -> Set(callee)
  private[this] lazy val calleeMap = {
    callArcSet.groupMap(_._1)(_._2) withDefaultValue Set.empty
  }

  //////////////////////////////////////////////////////////////////////////////
  // Adjacency matrix of the call graph
  //////////////////////////////////////////////////////////////////////////////
  private[this] lazy val adjMat: Matrix[Int] = Matrix {
    for (caller <- functionSymbols) yield {
      val calleeSet = calleeMap.getOrElse(caller, Set())
      for (symbol <- functionSymbols) yield {
        if (calleeSet contains symbol) 1 else 0
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
  private[this] lazy val indMat: Matrix[Int] = {
    if (nFunctions == 1) {
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
      loop(nFunctions, square, square)
    }
  }

  private[this] def warnIgnoredStacklimitAttribute(eSymbol: Symbol): Unit = {
    if (eSymbol.attr.stackLimit.isSet) {
      val loc = eSymbol.loc
      val name = eSymbol.name
      cc.warning(loc, s"'stacklimit' attribute ignored on entity '${name}'")
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  // Collect 'reclimit' values and check that they are sensible
  //////////////////////////////////////////////////////////////////////////////
  private[this] lazy val recLimits: Option[List[Int]] = {
    val directlyRecursive = adjMat.diagonal map { _ != 0 }
    val indirectlyRecursive = indMat.diagonal map { _ != 0 }

    val list = List from {
      for {
        (symbol, isRecD, isRecI) <- functionSymbols lazyZip directlyRecursive lazyZip indirectlyRecursive
      } yield {
        lazy val loc = symbol.loc
        lazy val name = symbol.name
        val exprOpt = symbol.attr.recLimit.get
        if (!isRecD && !isRecI) {
          if (exprOpt.isDefined) {
            cc.warning(loc, s"'reclimit' attribute ignored on function '${name}'")
          }
          1
        } else {
          exprOpt match {
            case None => {
              val hint = if (isRecD) "Recursive" else "Indirectly recursive"
              cc.error(loc, s"${hint} function '${name}' requires 'reclimit' attribute")
              0
            }
            case Some(expr) => {
              expr.value match {
                case Some(value) if value < 2 => {
                  cc.error(
                    loc,
                    s"Recursive function '${name}' has 'reclimit' attribute equal to ${value}"
                  )
                  0
                }
                case None => {
                  symbol.attr.recLimit set Expr(2)
                  cc.error(loc,
                           s"Cannot compute value of 'reclimit' attribute of function '${name}'")
                  0
                }
                case Some(value) => value.toInt
              }
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
  private[this] def computeLongestPathLength(cost: Map[Symbol, Int]): Int = {
    // Find the longest simple (non-cyclic) path in the call graph
    // Note strictly this is only an upper bound on the required
    // return stack depth if there is recursion
    val (length, _) = {
      // Longest path from 'node' that goes through only vertices that are in 'nodes'
      def longestPathFrom(node: Symbol, nodes: Set[Symbol]): (Int, List[Symbol]) = {
        assert(!(nodes contains node))
        // Candidate successors are the vertices which are callees of this node and are in 'nodes'
        val candidates = calleeMap(node) intersect nodes
        // Find longest paths from each of these candidates
        val paths = for (cand <- candidates) yield longestPathFrom(cand, nodes - cand)

        if (paths.isEmpty) {
          // If no further paths, we are done
          (cost(node), node :: Nil)
        } else {
          // Find the one that is the longest among all candidates
          val (len, longest) = paths maxBy { _._1 }
          // Prepend this node
          (cost(node) + len, node :: longest)
        }
      }

      val funcSet = functionSymbols.toSet

      // Get the longest of the longest paths originating from any function
      functionSymbols map { symbol =>
        longestPathFrom(symbol, funcSet - symbol)
      } maxBy { _._1 }
    }

    length
  }

  //////////////////////////////////////////////////////////////////////////////
  // The actual return stack depth required
  //////////////////////////////////////////////////////////////////////////////
  private[this] def returnStackDepth(eSymbol: Symbol): Int = {
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

    // Compute the maximum number of active functions
    val maxActive = if (hasRecursion && stackLimit.isDefined) {
      // If the entity uses recursion and has a sane
      // 'stacklimit' attribute, use the value of that
      stackLimit.get
    } else {
      recLimits map { weights =>
        // Map from symbol -> reclimit
        val cost = (functionSymbols zip weights).toMap

        // Find the longest path in the static call graph
        computeLongestPathLength(cost)
      } getOrElse {
        // If we don't have sane recLimits, emit a return stack of size 1
        // (by using maxActive == 2) to allow later passes to proceed
        2
      }
    }

    // The actual stack depth required for handling return from N active
    // functions is N - 1 as the root function does not return anywhere
    maxActive - 1
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
      assert(functionSymbols == null)
      functionSymbols = defn.functions map { _.symbol }
      val entryPoints = functionSymbols filter { _.attr.entry.isSet }
      Option.when(entryPoints.lengthIs != 1) {
        if (entryPoints.isEmpty) {
          cc.error(defn.symbol, "No 'main' function in fsm.")
        } else {
          val locations = entryPoints map { _.loc.prefix }
          cc.error(defn.symbol, "Multiple 'main' functions in fsm at:" :: locations: _*)
        }
        tree
      }

    //////////////////////////////////////////////////////////////////////////
    // Note current function we are processing
    //////////////////////////////////////////////////////////////////////////

    case defn: DefnFunc =>
      currentFunction = defn.symbol
      None

    //////////////////////////////////////////////////////////////////////////
    // Collect the call graph edges as we go
    //////////////////////////////////////////////////////////////////////////

    case ExprCall(ref, _) if ref.tpe.isCtrlFunc =>
      ref match {
        case ExprSym(callee) => callArcs add (currentFunction -> callee)
        case _               => unreachable
      }
      None

    case StmtGoto(ExprSym(callee)) =>
      gotoArcs add (currentFunction -> callee)
      None

    //
    case _ => None

  }

  override def transform(tree: Tree): Tree = tree match {
    case defn: DefnEntity =>
      // Ensure 'reclimit' attributes exist on all functions
      val values = recLimits.getOrElse(List.fill(nFunctions)(1))
      for ((symbol, value) <- functionSymbols zip values) {
        symbol.attr.recLimit set Expr(value)
      }

      val stackDepth = returnStackDepth(defn.symbol)

      if (stackDepth == 0) {
        defn
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
        TypeAssigner(defn.copy(body = stackDefn :: defn.body) withLoc defn.loc)
      }

    case decl: DeclEntity =>
      decl.symbol.defn.defns collectFirst {
        // Add the Decl of the return stack
        case Defn(symbol) if symbol.attr.returnStack.isSet =>
          val stackDecl = symbol.mkDecl regularize decl.loc
          TypeAssigner(decl.copy(decls = stackDecl :: decl.decls) withLoc decl.loc)
      } getOrElse tree

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
