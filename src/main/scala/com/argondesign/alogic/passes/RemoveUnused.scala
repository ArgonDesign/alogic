////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Remove all unused symbols if possible, and sign off the rest
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.analysis.ReadSymbolBits
import com.argondesign.alogic.analysis.ReadSymbols
import com.argondesign.alogic.analysis.SymbolBitSet
import com.argondesign.alogic.analysis.WrittenSymbols
import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.Expr.InstancePortSel
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.core.enums.EntityVariant
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.util.unreachable
import com.argondesign.alogic.util.BitSetOps._
import com.argondesign.alogic.util.IteratorOps._

import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.immutable.HashSet
import scala.collection.parallel.CollectionConverters.IterableIsParallelizable
import scala.collection.parallel.ParIterable

// The transform that removes all symbols for which the given predicate fails
final private class RemoveSymbols(
    retainedInternal: Symbol => Boolean, // Used to check local symbols
    retainedExternal: Map[Symbol, Symbol => Boolean] // used ot check assignment to instance ports
  )(
    implicit
    cc: CompilerContext)
    extends StatefulTreeTransformer {

  private def emptyStmt(stmt: Stmt): Boolean = stmt match {
    case StmtBlock(body)         => body forall emptyStmt
    case StmtIf(_, eBody, tBody) => (eBody forall emptyStmt) && (tBody forall emptyStmt)
    case StmtCase(_, cases) =>
      cases forall {
        case CaseRegular(_, stmts) => stmts forall emptyStmt
        case CaseDefault(stmts)    => stmts forall emptyStmt
        case _: CaseSplice         => unreachable
      }
    case _ => false
  }

  override def enter(tree: Tree): Option[Tree] = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Remove connects driving only dropped symbols
    ////////////////////////////////////////////////////////////////////////////

    case EntAssign(InstancePortSel(iSymbol, pSymbol), _) =>
      Some {
        if (retainedInternal(iSymbol) && retainedExternal(iSymbol.kind.asEntity.symbol)(pSymbol)) {
          tree
        } else {
          Stump
        }
      }
    case EntAssign(lhs, _) =>
      Some(if (WrittenSymbols(lhs) exists retainedInternal) tree else Stump)

    ////////////////////////////////////////////////////////////////////////////
    // Remove assignments to only dropped symbols
    ////////////////////////////////////////////////////////////////////////////

    case StmtAssign(lhs, _) if !(WrittenSymbols(lhs) exists retainedInternal)  => Some(Stump)
    case StmtDelayed(lhs, _) if !(WrittenSymbols(lhs) exists retainedInternal) => Some(Stump)

    ////////////////////////////////////////////////////////////////////////////
    // Skip foreign function Decl/Defn (which are the only functions left)
    ////////////////////////////////////////////////////////////////////////////

    case _: DeclFunc | _: DefnFunc => Some(tree)

    ////////////////////////////////////////////////////////////////////////////
    // Skip expressions
    ////////////////////////////////////////////////////////////////////////////

    case _: Expr => Some(tree)

    //
    case _ => None
  }

  override def transform(tree: Tree): Tree = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Fold empty statements as we go to get rid of unused branch conditions
    ////////////////////////////////////////////////////////////////////////////

    case stmt: Stmt if emptyStmt(stmt) => Stump

    ////////////////////////////////////////////////////////////////////////////
    // Remove Decl/Defn of symbol not being retained
    ////////////////////////////////////////////////////////////////////////////

    case Decl(symbol) if !retainedInternal(symbol) =>
      // If we are removing a _q, drop the suffix from the _d
      symbol.attr.flop.get foreach { dSymbol =>
        assert(dSymbol.name endsWith "_d")
        dSymbol.name = dSymbol.name.dropRight(2)
        dSymbol.attr.combSignal set true
      }
      Stump

    case Defn(symbol) if !retainedInternal(symbol) => Stump

    //
    case _ => tree
  }

}

object RemoveUnused extends PairsTransformerPass {
  val name = "remove-unused"

  private def signOffUnused(
      decl: DeclEntity,
      defn: DefnEntity,
      usedSymbolBits: SymbolBitSet
    )(
      implicit
      cc: CompilerContext
    ): (Decl, Defn) = {

    // Turns (6, 5, 4, 2, 1, 0) into ((6, 4), (2, 1), (0, 0)), etc.
    def collapseConsecutiveValues(it: Iterator[Int]): Iterator[(Int, Int)] = {
      require(it.hasNext)

      def f(hi: Int, lo: Int): Iterator[(Int, Int)] = it.nextOption() match {
        case None                   => Iterator.single((hi, lo))
        case Some(n) if n == lo - 1 => f(hi, n)
        case Some(n)                => Iterator.single((hi, lo)) concat f(n, n)
      }

      val first = it.next()
      f(first, first)
    }

    val unusedTerms = decl.decls.iterator map {
      _.symbol
    } flatMap {
      case symbol if !symbol.kind.isPacked => Iterator.empty
      case symbol =>
        val usedBits = usedSymbolBits.getOrElse(symbol, BitSet.empty)
        if (usedBits.size == symbol.kind.width) {
          Iterator.empty
        } else if (usedBits.isEmpty) {
          Iterator.single(TypeAssigner(ExprSym(symbol) withLoc Loc.synthetic))
        } else {
          val unusedBits = Iterator.range(symbol.kind.width.toInt - 1, -1, -1).filterNot(usedBits)

          def ref: ExprSym = TypeAssigner(ExprSym(symbol) withLoc Loc.synthetic)

          collapseConsecutiveValues(unusedBits) map {
            case (hi, lo) if hi == lo => ref index hi
            case (hi, lo)             => ref.slice(hi, ":", lo)
          }
        }
    }

    if (!unusedTerms.hasNext) {
      (decl, defn)
    } else {
      val unusedSymbol = cc.newSymbol("_unused", Loc.synthetic)
      unusedSymbol.kind = TypeUInt(1)
      unusedSymbol.attr.combSignal set true
      val unusedDecl = unusedSymbol.mkDecl regularize Loc.synthetic
      val unusedDefn = EntSplice(unusedSymbol.mkDefn) regularize Loc.synthetic
      val reduction = ExprUnary("&", ExprCat(ExprInt(false, 1, 0) :: unusedTerms.toList))
      val unusedAssign = EntAssign(ExprSym(unusedSymbol), reduction) regularize Loc.synthetic
      val newDecl = TypeAssigner(decl.copy(decls = unusedDecl :: decl.decls) withLocOf decl)
      val newDefn = TypeAssigner(
        defn.copy(body = unusedDefn :: unusedAssign :: defn.body) withLocOf defn
      )
      (newDecl, newDefn)
    }
  }

  def process(
      pairs: Iterable[(Decl, Defn)]
    )(
      implicit
      cc: CompilerContext
    ): Iterable[(Decl, Defn)] = {
    // TODO: Could prune every entity completely that has only inputs left
    //       unless it has non-pure contents like foreign function calls,
    //       assumption is that this is very rare, so don't bother for now.

    // We can do a lot in parallel
    val parPairs = pairs.par

    // Gather (by entity) all symbols that must be kept, even if they are
    // unused.
    val keptSymbols: Map[Symbol, HashSet[Symbol]] = Map from {
      parPairs.iterator map {
        case (DeclEntity(symbol, decls), defn: DefnEntity) =>
          symbol -> HashSet.from {
            Iterator.when(symbol.attr.topLevel.isSet) thenIterator {
              // Keep input ports of top level entities (except for ones added
              // by the compiler), in order to preserve the user defined
              // interface
              decls.iterator
                .map(_.symbol)
                .filter(_.kind.isIn)
                .filterNot { symbol =>
                  symbol.attr.clk.isSet || symbol.attr.rst.isSet
                }
            } concat {
              // Gather symbols that must be kept because they are used as a
              // placeholder in a concatenation on the left hand side of an
              // assignment
              defn flatCollect {
                case EntAssign(lhs: ExprCat, _)  => WrittenSymbols(lhs)
                case StmtAssign(lhs: ExprCat, _) => WrittenSymbols(lhs)
                case _: Expr                     => Iterator.empty // Stop descent
              }
            }
          }
        case (decl, _) => decl.symbol -> HashSet.empty
      }
    }

    @tailrec
    def loop(parPairs: ParIterable[(Decl, Defn)]): Iterable[(Decl, Defn)] = {
      // Gather (by entity) all used symbols (excluding symbols not defined in
      // the entity). A symbol is used if it's value is consumed. This can happen
      // when it is read in an rvalue, read in an lvalue, or is a top level
      // output.
      val usedInternalSymbols: Map[Symbol, HashSet[Symbol]] = Map from {
        parPairs.iterator.map {
          case (DeclEntity(symbol, decls), defn: DefnEntity) =>
            symbol -> HashSet.from {
              Iterator.when(symbol.attr.topLevel.isSet) thenIterator {
                // Assume all output ports in top level entities are used
                decls.iterator.map(_.symbol).filter(_.kind.isOut)
              } concat {
                Iterator.when(defn.variant == EntityVariant.Ver) thenIterator {
                  // Assume everything is used in verbatim entities as signals
                  // might be used in verbatim blocks
                  decls.iterator.map(_.symbol)
                }
              } concat {
                defn.collect {
                  case EntAssign(lhs, InstancePortSel(iSymbol, _)) =>
                    ReadSymbols.lval(lhs) concat Iterator.single(iSymbol)
                  case EntAssign(_: ExprSel, rhs) =>
                    ReadSymbols.rval(rhs)
                  case EntAssign(lhs, rhs) =>
                    ReadSymbols.lval(lhs) concat ReadSymbols.rval(rhs)
                  case StmtAssign(lhs, rhs) =>
                    ReadSymbols.lval(lhs) concat ReadSymbols.rval(rhs)
                  case StmtDelayed(lhs, rhs) =>
                    ReadSymbols.lval(lhs) concat ReadSymbols.rval(rhs)
                  case StmtOutcall(lhs, f, rhss) =>
                    ReadSymbols.lval(lhs) concat (f :: rhss).iterator.flatMap(ReadSymbols.rval)
                  case expr: Expr =>
                    ReadSymbols.rval(expr)
                }.flatten
              }
            }
          case (decl, _) => decl.symbol -> HashSet.empty
        }
      }

      // Gather external symbols referenced in entities
      val usedExternalSymbols = HashSet from {
        parPairs.iterator.flatMap {
          case (DeclEntity(_, decls), DefnEntity(_, _, body)) =>
            decls.iterator.collect {
              // Instantiated entities
              case DeclInstance(_, ExprSym(symbol)) => symbol
            } concat {
              body.iterator.collect {
                // Read ports of instances
                case EntAssign(_, InstancePortSel(_, pSymbol)) => pSymbol
              }
            }
          case _ => Iterator.empty
        }
      }

      // Predicate function for symbols referenced externally, including top
      // level entities
      val externallyUsed: Symbol => Boolean = { symbol =>
        usedExternalSymbols(symbol) || symbol.attr.topLevel.isSet
      }

      // First drop all entities not marked for retention, then apply the pruning
      // transform to the rest.
      val processedPairs =
        parPairs
          .withFilter(pair => externallyUsed(pair._1.symbol)) // Keep only used entities
          .map {
            case (decl, defn) =>
              val usedInternal = usedInternalSymbols(decl.symbol)
              val keptInternal = keptSymbols(decl.symbol)
              // Predicate for symbols retained in this entity on this iteration
              val retained: Symbol => Boolean = { symbol =>
                usedInternal(symbol) || keptInternal(symbol) || externallyUsed(symbol)
              }
              // Apply the pruning transform
              val transform = new RemoveSymbols(retained, usedInternalSymbols)
              (decl rewrite transform, defn rewrite transform)
          }

      if ( // Check if we removed any symbol at all
        (processedPairs.iterator zip parPairs.iterator).exists {
          case ((declA, defnA), (declB, defnB)) => (declA ne declB) || (defnA ne defnB)
        }
      ) {
        // We removed some symbols. Try again to see if anything have in turn
        // became unused as a result of the removals.
        loop(processedPairs)
      } else {
        // No more removals were possible. Finish up by signing off any unused
        // signal bits.

        // Need to update types of entities and instances as ports might have
        // been removed
        val updatedPairs = {
          object UpdateTypes extends StatefulTreeTransformer {
            override def replace(symbol: Symbol): Boolean = symbol.kind match {
              case TypeType(_: TypeEntity) => true
              case _: TypeEntity           => true
              case _                       => false
            }
          }
          // This we need to do sequentially as it's a global mapping of symbols
          processedPairs.seq.map {
            case (decl, defn) => (decl rewrite UpdateTypes, defn rewrite UpdateTypes)
          }.par
        }

        // Gather all used symbol bits. This is almost the same as the above
        // gathering of used symbols, but we don't include non-packed symbols,
        // and we used bitwise precision. This is a lot slower than working on
        // whole symbol granularity, so we only use bitwise precision at the
        // end to generate signoffs. Also note that we need not gather output
        // symbols here, as all wholly unused ports would have been removed
        // during the iteration, and unused sub ranges of output ports will be
        // signed off where the port is used at the instantiating side.
        val usedLocalSymbolBits: Map[Symbol, SymbolBitSet] = Map from {
          updatedPairs.iterator
            .map {
              case (DeclEntity(symbol, decls), defn: DefnEntity) =>
                symbol ->
                  // Assume all output ports are used
                  decls.iterator
                    .map(_.symbol)
                    .filter(_.kind.isOut)
                    .map(symbol => SymbolBitSet(symbol, BitSet.whole(symbol)))
                    .concat {
                      Iterator.when(defn.variant == EntityVariant.Ver) thenIterator {
                        // Assume everything is used in verbatim entities as signals
                        // might be used in verbatim blocks
                        decls.iterator.map(decl =>
                          SymbolBitSet(decl.symbol, BitSet.whole(decl.symbol))
                        )
                      }
                    }
                    .concat {
                      defn collect {
                        case EntAssign(lhs, _: ExprSel) =>
                          ReadSymbolBits.lval(lhs)
                        case EntAssign(_: ExprSel, rhs) =>
                          ReadSymbolBits.rval(rhs)
                        case EntAssign(lhs, rhs) =>
                          ReadSymbolBits.lval(lhs) union ReadSymbolBits.rval(rhs)
                        case StmtAssign(lhs, rhs) =>
                          ReadSymbolBits.lval(lhs) union ReadSymbolBits.rval(rhs)
                        case StmtDelayed(lhs, rhs) =>
                          ReadSymbolBits.lval(lhs) union ReadSymbolBits.rval(rhs)
                        case StmtOutcall(lhs, f, rhss) =>
                          (f :: rhss).foldLeft(ReadSymbolBits.lval(lhs))(
                            _ union ReadSymbolBits.rval(_)
                          )
                        case expr: Expr => ReadSymbolBits.rval(expr)
                      }
                    }
                    .foldLeft(SymbolBitSet.empty)(_ union _)
              case _ => unreachable
            }
        }

        updatedPairs.map {
          case (decl: DeclEntity, defn: DefnEntity) =>
            // Update types then add unused signoffs
            signOffUnused(decl, defn, usedLocalSymbolBits(decl.symbol))
          case other => other
        }.seq
      }
    }
    loop(parPairs)
  }

}
