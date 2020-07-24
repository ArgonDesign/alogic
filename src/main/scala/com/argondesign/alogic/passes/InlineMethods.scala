////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.analysis.ReadSymbols
import com.argondesign.alogic.analysis.WrittenSymbols
import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Messages.Fatal
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types.TypeRecord
import com.argondesign.alogic.core.Types.TypeVoid
import com.argondesign.alogic.transform.StatementFilter
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class InlineMethods(implicit cc: CompilerContext) extends StatelessTreeTransformer { self =>

  private val extraStmts = mutable.Stack[ListBuffer[Stmt]]()

  private val recDepth = mutable.Map[Symbol, Int]() withDefaultValue 0
  private val rootLoc = mutable.Map[Symbol, Loc]()

  // Perform local simplification: InlineKnownVars + Fold
  private def simplify(stmts: IterableOnce[Stmt]): List[Stmt] = {
    // Build a block for processing
    val it = stmts.iterator
    if (it.isEmpty) {
      Nil
    } else {
      val block = StmtBlock(it.toList)
      TypeAssigner(block withLoc block.body.head.loc)
      // Simplify it
      cc.fold(block rewrite new RemoveStructuralSharing rewrite new InlineKnownVars) match {
        case Thicket(trees) => trees.asInstanceOf[List[Stmt]]
        case tree: Stmt     => List(tree)
        case _              => unreachable
      }
    }
  }

  // Given a function symbol, a list of actual arguments, and an optional
  // 'this' symbol, return an iterator holding the statements to be emitted
  // to inline the function, and the symbol holding the return value, if the
  // function has non-void return type.
  private def inline(
      symbol: Symbol,
      args: List[Arg],
      thisOpt: Option[Symbol],
      loc: Loc
    ): Option[(Iterator[Stmt], Option[Symbol])] = {

    if (recDepth(symbol) == 0) {
      rootLoc(symbol) = loc
    }

    val recursionError = recDepth(symbol) >= cc.settings.combRecLimit

    if (recursionError) {
      cc.error(
        loc,
        s"Combinational function call exceeds static recursion limit of ${cc.settings.combRecLimit}",
        "this can be increased with the '--comb-rec-limit' option",
        s"root call is at: ${rootLoc(symbol).prefix}"
      )
    }

    Option.unless(recursionError) {
      val decl = symbol.decl.asInstanceOf[DeclFunc]
      val defn = symbol.defn.asInstanceOf[DefnFunc]

      // Formal arguments
      val fSymbols = decl.args map { _.symbol }

      // Actual arguments
      val aExprs = args map {
        case ArgP(expr) => expr
        case argn: ArgN =>
          // For now it does not work, not hard though..
          throw Fatal(argn, "Function call arguments must be positional")
        case _: ArgD => unreachable
      }

      // Local temporary symbols holding actual arguments
      val aSymbols: List[Symbol] = List from {
        (aExprs lazyZip fSymbols) map {
          case (aExpr, fSymbol) =>
            cc.newTemp(
              s"_${symbol.name}${cc.sep}arg${cc.sep}${fSymbol.name}",
              aExpr.loc,
              fSymbol.kind
            ) tap { _.attr.combSignal set true }
        }
      }

      // Temporary symbol holding return value, if any
      val rSymbolOpt: Option[Symbol] = symbol.kind.asCallable.retType match {
        case TypeVoid => None
        case kind =>
          Some {
            cc.newTemp(s"_${symbol.name}${cc.sep}ret", loc, kind) tap { _.attr.combSignal set true }
          }
      }

      // Statements introducing the temporaries
      val temporaries = {
        // Decl/Defn for non-constant actual arguments
        val aDecls = aSymbols.iterator map { aSymbol =>
          StmtDecl(aSymbol.mkDecl) regularize aSymbol.loc
        }
        val aDefns = (aSymbols.iterator zip aExprs.iterator) map {
          case (aSymbol, aExpr) =>
            // Make defn then normalize it to resolve unary ticks
            (StmtDefn(aSymbol.mkDefn(Some(aExpr))) regularize aSymbol.loc).normalize
        }

        // Decl/Defn for optional return value
        val rDeclOpt = rSymbolOpt map { rSymbol => StmtDecl(rSymbol.mkDecl) regularize rSymbol.loc }
        val rDefnOpt = rSymbolOpt map { rSymbol => StmtDefn(rSymbol.mkDefn) regularize rSymbol.loc }

        aDecls concat aDefns concat rDeclOpt concat rDefnOpt
      }

      // Transform the function body
      val stmts = {
        // formal to actual argument substitution, will later be expanded with
        // cloned local symbol substitutions.
        val substitution = mutable.Map from {
          fSymbols.iterator zip aSymbols.iterator
        }

        // - Do not replace return statements yet!
        // - Substitute formal argument references with the actual argument symbol
        // - Clone local definitions
        // - Substitute ExprThis
        object Transform extends StatelessTreeTransformer {
          override def transform(tree: Tree): Tree = tree match {
            case decl @ Decl(symbol) =>
              // Declarations in function bodies (statements) are in-order with
              // the decl first, so clone the local when we encountered it.
              // As these are all combinational, we can mark them as combSingals
              assert(!(substitution contains symbol))
              val newSymbol = symbol.dup tap { s =>
                s.kind = symbol.kind
                s.attr.combSignal set true
              }
              substitution(symbol) = newSymbol
              TypeAssigner(decl.cpy(symbol = newSymbol) withLoc tree.loc)

            case defn @ Defn(symbol) =>
              TypeAssigner(defn.cpy(symbol = substitution(symbol)) withLoc tree.loc)

            case ExprSym(symbol) =>
              substitution.get(symbol) match {
                case Some(aSymbol) => ExprSym(aSymbol) regularize tree.loc
                case None          => tree
              }

            case expr: ExprThis =>
              thisOpt match {
                case Some(symbol) =>
                  val thisSymbol = symbol.kind.underlying match {
                    case TypeRecord(s, _) => s
                    case _                => throw Ice(expr, "Strange 'this' instance type")
                  }
                  val exprSymbol = expr.tpe match {
                    case TypeRecord(s, _) => s
                    case _                => throw Ice(expr, "Strange 'this' reference type")
                  }
                  if (thisSymbol == exprSymbol) {
                    ExprSym(symbol) regularize tree.loc
                  } else {
                    throw Ice(expr, "Incompatible 'this' reference")
                  }
                case None => throw Ice(expr, "Missing instance for 'this' reference")
              }

            case _ => tree
          }
        }

        defn.body.iterator map { _ rewrite Transform }
      }

      // Simplify the result so we have a hope to terminate recursion.
      val simplified = simplify(temporaries concat stmts)

      // Drop any trailing statements after a statement that always returns
      val reachable = {
        val (init, tail) = simplified.iterator.span(!_.alwaysReturns)
        init concat tail.nextOption
      }

      // Now convert return statements
      val result = {
        object Transform extends StatelessTreeTransformer {
          override def transform(tree: Tree): Tree = tree match {
            case StmtReturn(_, exprOpt) =>
              exprOpt match {
                case Some(expr) => StmtAssign(ExprSym(rSymbolOpt.get), expr) regularize tree.loc
                case None       => Stump
              }
            case _ => tree
          }
        }
        reachable flatMap { stmt =>
          Transform(stmt) match {
            case result: Stmt => Some(result)
            case Stump        => None
            case _            => unreachable
          }
        }
      }

      // Walk the result recursively so we inline calls within function bodies,
      // simplify after to reduce the number of temporaries.
      recDepth(symbol) += 1
      val inlined = simplify {
        result flatMap { stmt =>
          self.walk(stmt) match {
            case s: Stmt        => Iterator.single(s)
            case Thicket(trees) => trees.asInstanceOf[List[Stmt]].iterator
            case Stump          => Iterator.empty // Only when error below
            case _              => unreachable
          }
        }
      }
      recDepth(symbol) -= 1

      // A quick pass to drop any unused temporaries so we don't have to carry
      // them around just to be dropped at the end of compilation.
      val pruned = {
        val referenced = Set from {
          rSymbolOpt.iterator concat {
            inlined.iterator flatMap {
              // TODO: factor this out with RemoveUnused?
              case stmt @ StmtAssign(lhs, rhs) =>
                lhs match {
                  case _: ExprCat => stmt collect { case ExprSym(symbol) => symbol }
                  case _          => ReadSymbols.lval(lhs) ++ ReadSymbols.rval(rhs)
                }
              case stmt => stmt collect { case ExprSym(symbol) => symbol }
            }
          }
        }
        val unused = Set from {
          inlined.iterator collect {
            case StmtDecl(Decl(symbol)) if !referenced(symbol) => symbol
          }
        }
        val filter = StatementFilter {
          case StmtDecl(Decl(symbol)) => !unused(symbol)
          case StmtDefn(Defn(symbol)) => !unused(symbol)
          case StmtAssign(lhs, _)     => !(WrittenSymbols(lhs) forall unused)
        }
        inlined.iterator flatMap {
          filter(_) match {
            case s: Stmt        => Iterator.single(s)
            case Thicket(trees) => trees.asInstanceOf[List[Stmt]].iterator
            case Stump          => Iterator.empty
            case _              => unreachable
          }
        }
      }

      // We are done!
      (pruned, rSymbolOpt)
    }
  }

  private def getReceiver(tgt: Expr): Option[Symbol] = {
    require(tgt.tpe.isMethod)
    Option.unless(tgt.tpe.isStaticMethod) {
      tgt match {
        case ExprSel(ExprSym(symbol), _, _) => symbol
        case _                              => throw Ice(tgt, "Don't know how to translate that method call")
      }
    }
  }

  private def processCallInStatementPosition(
      call: ExprCall
    ): Option[(Iterator[Stmt], Option[Symbol])] = {
    require(call.expr.tpe.isMethod)
    // Walk args up front so they are expanded in the same order as calls
    // not in statement position
    extraStmts.push(new ListBuffer[Stmt])
    val tgt = walk(call.expr).asInstanceOf[Expr]
    val args = walk(call.args).asInstanceOf[List[Arg]]
    if (tgt.tpe.isError || (args exists { _.tpe.isError })) {
      None
    } else {
      inline(tgt.tpe.asCallable.symbol, args, getReceiver(tgt), call.loc) map {
        case (extra, retOpt) => (extraStmts.top.iterator concat extra, retOpt)
      }
    }
  } tap { _ =>
    extraStmts.pop
  }

  override protected def enter(tree: Tree): Option[Tree] = tree match {
    // Calls in statement position are replaced straight as they might not
    // have a return value, and even if they do it is not needed
    case StmtExpr(call: ExprCall) if call.expr.tpe.isMethod =>
      processCallInStatementPosition(call) map {
        case (stmts, _) => Thicket(stmts.toList)
      } orElse Some(Stump)

    case _: Stmt =>
      extraStmts.push(new ListBuffer[Stmt])
      None

    // Drop Method Decl/Defn
    case Decl(symbol) if symbol.kind.isMethod => Some(Stump)
    case Defn(symbol) if symbol.kind.isMethod => Some(Stump)

    case _ => None
  }

  override def transform(tree: Tree): Tree = tree match {
    case ExprCall(tgt, args) if tgt.tpe.isMethod =>
      inline(tgt.tpe.asCallable.symbol, args, getReceiver(tgt), tree.loc) map {
        case (extra, resultOpt) =>
          extraStmts.top.addAll(extra)
          TypeAssigner(ExprSym(resultOpt.get) withLoc tree.loc)
      } getOrElse TypeAssigner(ExprError() withLoc tree.loc)

    case stmt: Stmt =>
      val extra = extraStmts.pop
      extra append stmt
      Thicket(extra.toList)

    case _ =>
      tree
  }

}

object InlineMethods extends PairTransformerPass {
  val name = "inline-methods"

  final protected def transform(
      decl: Decl,
      defn: Defn
    )(
      implicit
      cc: CompilerContext
    ): (Tree, Tree) = {
    require(decl.symbol eq defn.symbol)
    val transformer = new InlineMethods
    (transformer(decl), transformer(defn))
  }

}
