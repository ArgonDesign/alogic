////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.analysis.ReadSymbols
import com.argondesign.alogic.analysis.WrittenSymbols
import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Messages.Fatal
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.Types.Type
import com.argondesign.alogic.core.Types.TypeCombFunc
import com.argondesign.alogic.core.Types.TypeNormalMethod
import com.argondesign.alogic.core.Types.TypeStaticMethod
import com.argondesign.alogic.core.Types.TypeVoid
import com.argondesign.alogic.transform.StatementFilter
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

final class InlineMethods(implicit cc: CompilerContext) extends StatefulTreeTransformer { self =>

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
      block rewrite new RemoveStructuralSharing rewrite new InlineKnownVars pipe {
        new Fold
      } match {
        case Thicket(trees) => trees.asInstanceOf[List[Stmt]]
        case tree: Stmt     => List(tree)
        case _              => unreachable
      }
    }
  }

  // Given a function symbol, a list of actual arguments, and optional
  // 'this' expression, and an optional symbol the return value should be
  // assigned to, return an iterator holding the statements to be emitted
  // to inline the function.
  private def inlineBody(
      symbol: Symbol,
      args: List[Arg],
      thisOpt: Option[Expr],
      rSymbolOpt: Option[Symbol],
      loc: Loc
    ): Option[Iterator[Stmt]] = {

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
            Symbol.temp(
              s"_${symbol.name}${cc.sep}arg${cc.sep}${fSymbol.name}",
              aExpr.loc,
              fSymbol.kind
            ) tap { _.attr.combSignal set true }
        }
      }

      // Statements introducing the temporaries
      val temporaries = {
        // Decl/Defn for non-constant actual arguments
        val aDecls = aSymbols.iterator map { aSymbol =>
          StmtSplice(aSymbol.mkDecl) regularize aSymbol.loc
        }
        val aDefns = (aSymbols.iterator zip aExprs.iterator) map {
          case (aSymbol, aExpr) => StmtSplice(aSymbol.mkDefn(Some(aExpr))) regularize aSymbol.loc
        }

        // Decl/Defn for optional return value
        val rDeclOpt = rSymbolOpt map { rSymbol =>
          StmtSplice(rSymbol.mkDecl) regularize rSymbol.loc
        }
        val rDefnOpt = rSymbolOpt map { rSymbol =>
          StmtSplice(rSymbol.mkDefn) regularize rSymbol.loc
        }

        aDecls concat aDefns concat rDeclOpt concat rDefnOpt
      }

      // Transform the function body
      val stmts = {
        // formal to actual argument substitution, will later be expanded with
        // cloned local symbol substitutions.
        val substitution = mutable.Map from {
          fSymbols.iterator zip aSymbols.iterator
        }

        // - Substitute formal argument references with the actual argument symbol
        // - Rewrite return statements as assignments to the return symbol
        // - Clone local definitions
        // - Substitute ExprThis
        object Transform extends StatelessTreeTransformer {
          // Need to keep track of whether we are in an inner function
          var innerFunction: Int = 0

          override def enter(tree: Tree): Option[Tree] = {
            tree match {
              case _: DefnFunc => innerFunction += 1
              case _           =>
            }
            None
          }

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
              defn match {
                case _: DefnFunc => innerFunction -= 1
                case _           =>
              }
              TypeAssigner(defn.cpy(symbol = substitution(symbol)) withLoc tree.loc)

            case StmtReturn(_, exprOpt) if innerFunction == 0 =>
              exprOpt match {
                case Some(expr) =>
                  rSymbolOpt map { rSymbol =>
                    StmtAssign(ExprSym(rSymbol), expr) regularize tree.loc
                  } getOrElse Stump
                case None => Stump
              }

            case ExprSym(symbol) =>
              substitution.get(symbol) match {
                case Some(aSymbol) => ExprSym(aSymbol) regularize tree.loc
                case None          => tree
              }

            case expr: ExprThis =>
              thisOpt match {
                case Some(thisExpr) =>
                  assert(expr.tpe.isRecord)
                  assert(thisExpr.tpe.underlying.isRecord)
                  assert(expr.tpe.asRecord.symbol == thisExpr.tpe.underlying.asRecord.symbol)
                  // TODO: should do deep copy here to preserve all locations
                  TypeAssigner(thisExpr.cpy() withLocOf tree)
                case None => throw Ice(expr, "Missing instance for 'this' reference")
              }

            case _ => tree
          }

          override def finalCheck(tree: Tree): Unit = {
            assert(innerFunction == 0)
          }
        }

        // Transform the body, then drop function definitions from the
        // transformed body (these will be inlined just below and the
        // definitions are still attached to the reference symbols)
        defn.body.iterator flatMap {
          Transform(_) match {
            case Stump      => None
            case stmt: Stmt => Some(stmt)
            case _          => unreachable
          }
//        } filter {
//          case StmtSplice(_: DeclFunc) | StmtSplice(_: DefnFunc) => false
//          case _                                                 => true
        }
      }

      // Simplify the result so we have a hope to terminate recursion.
      val simplified = simplify(temporaries concat stmts)

      // Drop any trailing statements after a statement that always returns
      val result = {
        val (init, tail) = simplified.iterator.span(!_.alwaysTerminates)
        init concat tail.nextOption()
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

      // A quick pass to drop any unused temporaries so we don't have to
      // carry them around just to be dropped at the end of compilation,
      // but otherwise we are done!
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
          case StmtSplice(Decl(symbol)) if !referenced(symbol) => symbol
        }
      }
      val filter = StatementFilter {
        case StmtSplice(Decl(symbol)) => !unused(symbol)
        case StmtSplice(Defn(symbol)) => !unused(symbol)
        case StmtAssign(lhs, _)       => !(WrittenSymbols(lhs) forall unused)
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
  }

  private def getReceiver(tgt: Expr): Option[Expr] = Option.when(tgt.tpe.isNormalMethod) {
    tgt match {
      case ExprSel(expr, _) => expr
      case _                => unreachable
    }
  }

  private def processCallInStatementPosition(
      call: ExprCall
    ): Option[Iterator[Stmt]] = {
    // Walk args up front so they are expanded in the same order as calls
    // not in statement position
    extraStmts.push(new ListBuffer[Stmt])
    val tgt = walk(call.expr).asInstanceOf[Expr]
    val args = walk(call.args).asInstanceOf[List[Arg]]
    if (tgt.tpe.isError || (args exists { _.tpe.isError })) {
      None
    } else {
      inlineBody(tgt.tpe.asCallable.symbol, args, getReceiver(tgt), None, call.loc) map {
        case extra => (extraStmts.top.iterator concat extra)
      }
    }
  } tap { _ =>
    extraStmts.pop()
  }

  override def enter(tree: Tree): Option[Tree] = tree match {
    // Do not descend into the definitions ve are inlining
    case Decl(symbol) if InlineMethods.mustInline(symbol.kind) => Some(tree)
    case Defn(symbol) if InlineMethods.mustInline(symbol.kind) => Some(tree)

    // Calls in statement position are replaced straight as they might not
    // have a return value, and even if they do it is not needed
    case StmtExpr(call: ExprCall) if InlineMethods.mustInline(call.expr.tpe) =>
      processCallInStatementPosition(call) map { stmts =>
        Thicket(stmts.toList)
      } orElse Some(Stump)

    case _: Stmt =>
      extraStmts.push(new ListBuffer[Stmt])
      None

    case _ => None
  }

  override def transform(tree: Tree): Tree = tree match {
    case ExprCall(tgt, args) if InlineMethods.mustInline(tgt.tpe) =>
      // The function symbol
      val fSymbol = tgt.tpe.asCallable.symbol

      // Temporary symbol holding return value, if any
      val rSymbol: Symbol = tgt.tpe.asCallable.retType match {
        case TypeVoid =>
          // void functions can only be called in statement position,
          // which is handled in enter
          unreachable
        case kind =>
          Symbol.temp(s"_${fSymbol.name}${cc.sep}ret", tree.loc, kind) tap {
            _.attr.combSignal set true
          }
      }

      // The replacement
      inlineBody(fSymbol, args, getReceiver(tgt), Some(rSymbol), tree.loc) map { extra =>
        extraStmts.top.addAll(extra)
        TypeAssigner(ExprSym(rSymbol) withLoc tree.loc)
      } getOrElse {
        tree // Only used if reached recursion limit. Leave it be, will stop due to error.
      }

    case stmt: Stmt =>
      val extra = extraStmts.pop()
      extra append stmt
      Thicket(extra.toList)

    case _ =>
      tree
  }

}

private object DropDefs extends StatelessTreeTransformer {

  override def enter(tree: Tree): Option[Tree] = tree match {
    case Decl(symbol) if InlineMethods.mustInline(symbol.kind) => Some(Stump)
    case Defn(symbol) if InlineMethods.mustInline(symbol.kind) => Some(Stump)

    case _: Expr => Some(tree)

    case _ => None
  }

}

object InlineMethods {

  def mustInline(kind: Type): Boolean = kind match {
    case _: TypeNormalMethod | _: TypeStaticMethod => true
    case TypeCombFunc(symbol, _, _) =>
      symbol.defnOption.nonEmpty // False for extension methods on ports/rams etc
    case _ => false
  }

  def apply(): Pass[Pairs, Pairs] = {
    new EntityTransformerPass(declFirst = true, parallel = true) {
      val name = "inline-methods"

      def create(symbol: Symbol)(implicit cc: CompilerContext): TreeTransformer = new InlineMethods
    } andThen new PairTransformerPass(parallel = true) {
      val name = "drop-comb-function-definitions"

      def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) =
        (DropDefs(decl), DropDefs(defn))
    }
  }

}
