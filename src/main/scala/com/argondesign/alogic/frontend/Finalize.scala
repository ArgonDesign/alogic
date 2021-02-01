////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.ParOrSeqIterable
import com.argondesign.alogic.core.ParOrSeqIterable.ImmutableIterableToParOrSeqIterable
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.Set
import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.chaining.scalaUtilChainingOps

private[frontend] object Finalize {

  def apply(
      desc: DescPackage
    )(
      implicit
      cc: CompilerContext,
      fe: Frontend
    ): (DescPackage, ParOrSeqIterable[DescPackage]) = {

    // Map used to ensure each package is processed only once once
    // (think of diamonds in the dependency DAG)
    val processedParallel = TrieMap[Symbol, () => Future[Set[DescPackage]]]()
    val processedSerial = TrieMap[Symbol, Unit]()

    def process(
        desc: DescPackage
      )(
        implicit
        cc: CompilerContext,
        fe: Frontend
      ): (DescPackage, Set[DescPackage]) = {

      // Gather referenced packages
      val referencedPackageSymbols = Set from {
        def gatherReferencePackages(tree: Tree): Iterator[Symbol] = tree flatCollect {
          case DescParametrized(_, attr, _, _) => // The desc was not type checked
            attr.iterator.flatMap(gatherReferencePackages)
          case ExprCall(expr, _) if expr.tpe.isParametrized => // Arguments are not type checked
            gatherReferencePackages(expr)
          case expr @ ExprSym(symbol) =>
            assert(expr.hasTpe) // Must have been type checked
            symbol.desc match {
              case _: DescPackage =>
                // Non parametrized package
                Iterator.single(symbol)
              case DescParametrized(_, _, _: DescPackage, _) =>
                // Pick up all specializations of the referenced package
                symbol.attr.specializations.getOrElse(Map.empty).valuesIterator
              case _ =>
                // Not a package
                Iterator.empty
            }
        }
        gatherReferencePackages(desc)
      }

      // Finalize the definitions of all referenced packages
      val dependencies = if (cc.settings.parallel) {
        // Launch recursion in parallel
        val futures = referencedPackageSymbols map { symbol =>
          val frontend = fe.fork
          lazy val recursiveResult = Future {
            val (self, transitiveDependencies) =
              process(symbol.desc.asInstanceOf[DescPackage])(cc, frontend)
            transitiveDependencies + self
          }
          processedParallel.putIfAbsent(symbol, () => recursiveResult) match {
            case Some(f) => f() map { _ => Set.empty[DescPackage] }
            case None    => recursiveResult
          }
        }
        // Reduce the results
        val result = Future.foldLeft(futures)(Set.empty[DescPackage])(_ union _)
        // Wait for the computation to complete
        Await.result(result, Duration.Inf)
      } else {
        // Work sequentially
        referencedPackageSymbols flatMap { symbol =>
          lazy val recursiveResult = {
            val (self, transitiveDependencies) =
              process(symbol.desc.asInstanceOf[DescPackage])(cc, fe)
            transitiveDependencies + self
          }
          processedSerial.putIfAbsent(symbol, ()) match {
            case Some(()) => Set.empty
            case None     => recursiveResult
          }
        }
      }

      // Finalize this definition
      val self = Clarify(desc) rewrite new StatelessTreeTransformer {
        // Add all specializations next to the parametrized definitions.
        // Drop scope resolution selects.

        ////////////////////////////////////////////////////////////////////////
        // Add actual specializations next to the parametrized definitions.
        // We keep the original for now for later error reporting.
        ////////////////////////////////////////////////////////////////////////

        override protected def enter(tree: Tree): Option[Tree] = tree match {
          case DescParametrized(Sym(symbol), _, _, _) =>
            Some {
              Thicket {
                symbol.attr.specializations
                  .getOrElse(Map.empty)
                  .valuesIterator
                  .map(symbol => walk(Clarify(symbol.desc)))
                  .concat(Iterator.single(tree))
              }
            }

          case expr @ ExprCall(tgt, _) if tgt.tpe.isParametrized =>
            // Need to preserve type of specialization expression as it's type
            // is not computable by TypeAssigner, but is also not fixed By
            // Clarify. Also, do not descend into args as they cannot be
            // evaluated out of the context of the parametrized definition, and
            // thus might be malformed. This is ok as these calls are dropped
            // in the next transform.
            Some {
              expr.copy(expr = walkSame(tgt)) withLocOf tree withTpe tree.tpe
            }

          case _ => None
        }

        ////////////////////////////////////////////////////////////////////////
        // Drop scope resolution selects
        ////////////////////////////////////////////////////////////////////////

        override protected def transform(tree: Tree): Tree = tree match {
          // TODO: Select from scope selected in a non-scope should turn into
          //       ExprSymSel from the same, not ExprSym, but we can't input this yet
          case ExprSymSel(tgt, symbol) if tgt.tpe.isScope && !tree.tpe.isScope =>
            TypeAssigner(ExprSym(symbol) withLocOf tree)
          case _ => tree
        }

      } rewrite new StatelessTreeTransformer {
        // Inline DescVarScope bodies, remove Aliases. This also requires that
        // we re-compute the type of every symbol whose definition we modify in
        // order to keep the types consistent.

        private val hierarcy = mutable.Stack[Symbol]()

        private val modified = mutable.Stack[Boolean](false)

        private def inlineScope(scope: DescGenScope): Some[Thicket] = {
          // Attach scope suffix to definitions inside LoopScopes
          if (scope.name startsWith "``") {
            val suffix = cc.sep + scope.name.drop(2)
            scope.body foreach {
              case Splice(d: Desc) if d.name startsWith "``" => // LoopScope in LoopScope
                d.symbol.name = scope.name + cc.sep + d.name.drop(2)
              case Splice(d: Desc) if d.name startsWith "`" => // IfScope in LoopScope
                d.symbol.name = scope.name
              case Splice(d: Desc) if !(d.symbol.origName contains "#") =>
                d.symbol.name = d.symbol.name + suffix
              case _: Desc => unreachable
              case _       =>
            }
          }
          Some(Thicket(walk(scope.body)))
        }

        override def enter(tree: Tree): Option[Tree] = tree tap {
          case DescPackage(Sym(symbol), _, _)          => hierarcy.push(symbol)
          case Splice(DescGenScope(Sym(symbol), _, _)) => hierarcy.push(symbol)
          case _: DescGenScope                         => unreachable // See below why
          case Desc(Sym(symbol))                       =>
            // Attach the hierarchical scope name
            symbol.scopeName = hierarcy.reverse
              .dropWhile(_.kind.isPackage) // Drop leading package
              .filterNot(_.name startsWith "`") // Keep only named scopes
              .map(_.name)
              .mkString(".")
            // We will need this (directly enclosing 'gen' scopes)
            val directScopes = hierarcy.takeWhile(_.kind.isScope)
            // Attach the named scope prefix, if this is a symbol directly
            // inside some sequence of named 'gen' scopes
            val prefix = directScopes
              .filterNot(_.name startsWith "`") // Keep only named scopes
              .map(_.name)
              .reverse
              .mkString(cc.sep)
            if (prefix.nonEmpty) {
              symbol.attr.localName set symbol.name
              symbol.name = prefix + cc.sep + symbol.name
            }
            hierarcy.push(symbol)
          case _ =>
        } pipe {
          case _: DescParametrized => Some(tree)

          //////////////////////////////////////////////////////////////////////
          // Inline all scopes
          //////////////////////////////////////////////////////////////////////

          // Replace scopes with their body. Note the bodies are already spliced
          // where needed to the right type by Elaborate
          case PkgSplice(d: DescGenScope)  => inlineScope(d)
          case EntSplice(d: DescGenScope)  => inlineScope(d)
          case RecSplice(d: DescGenScope)  => inlineScope(d)
          case StmtSplice(d: DescGenScope) => inlineScope(d)

          // Should be no other left (includes CaseSplice which was eliminated
          // by Elaborate)
          case Splice(_: DescGenScope) => unreachable

          // 'gen' in a 'gen', will have been spliced in by Elaborate and
          // we catch all splices above, so we should never see this
          case _: DescGenScope => unreachable

          //////////////////////////////////////////////////////////////////////
          // Drop aliases
          //////////////////////////////////////////////////////////////////////

          case DescAlias(Sym(symbol), _, expr, _) =>
            // Propagate wasUsed attribute
            if (symbol.attr.wasUsed contains true) {
              expr visitAll {
                case ExprSym(symbol)       => symbol.attr.wasUsed set true
                case ExprSymSel(tgt, name) => symbol.attr.wasUsed set true
              }
            }
            Some(Stump)

          //////////////////////////////////////////////////////////////////////
          // Allocate flag on stack
          //////////////////////////////////////////////////////////////////////

          case _: Desc => modified push false; None

          //////////////////////////////////////////////////////////////////////
          // Redirect aliases
          //////////////////////////////////////////////////////////////////////

          case expr @ ExprCall(tgt, _) if tgt.tpe.isParametrized =>
            // Need to preserve type of specialization expression as it's type
            // is not computable by TypeAssigner, but is also not fixed By
            // Clarify. Also, do not descend into args as they cannot be
            // evaluated out of the context of the parametrized definition, and
            // thus might be malformed. This is ok as these calls are dropped
            // in the next transform.
            Some {
              expr.copy(expr = walkSame(tgt)) withLocOf tree withTpe tree.tpe
            }

          case ExprSym(symbol) if !symbol.kind.isParametrized =>
            symbol.desc match {
              case DescAlias(_, _, expr, _) =>
                // We assign the type of the tree, as the result might be a
                // specialization expression which the TypeAssigner cannot handle
                Some(walkSame(expr).cpy() withLocOf tree withTpe tree.tpe)
              case _ => None
            }

          case ExprSymSel(tgt, symbol) if symbol.kind.isPackage =>
            tgt visitAll { case ExprSym(symbol) => symbol.attr.wasUsed set true }
            Some(walkSame(TypeAssigner(ExprSym(symbol) withLocOf tree)))

          case ExprSymSel(tgt, symbol) if !tree.tpe.isParametrized =>
            symbol.descOption collect {
              // TODO: if the alias target is a specialization expression, the
              //       TypeAssigner will fail... See ExprSym case above
              case DescAlias(_, _, expr, _) =>
                def reselect(stem: Expr, name: Expr): Expr = name match {
                  case ExprSym(symbol) if symbol.kind.isPackage =>
                    stem visitAll { case ExprSym(symbol) => symbol.attr.wasUsed set true }
                    TypeAssigner(ExprSym(symbol) withLocOf stem)
                  case ExprSym(symbol) =>
                    TypeAssigner(ExprSymSel(stem, symbol) withLocOf stem)
                  case ExprSymSel(expr, sel) =>
                    TypeAssigner(ExprSymSel(reselect(stem, expr), sel) withLocOf stem)
                  case _ => unreachable
                }

                reselect(walkSame(tgt), walkSame(expr))
            }

          case _ => None
        } tap {
          case Some(_) =>
            // Mark the enclosing definition as modified
            modified(0) = true
            // Pop hierarchy
            tree match {
              case Splice(_: DescGenScope) | _: Desc => hierarcy.pop()
              case _                                 =>
            }
          case None =>
        }

        override protected def transform(tree: Tree): Tree = tree match {
          //////////////////////////////////////////////////////////////////////
          // Recompute type of the definitions whose body have changed
          //////////////////////////////////////////////////////////////////////

          case desc: Desc =>
            if (modified.pop()) {
              desc.symbol.kind = null
              fe.typeOf(desc.symbol, desc.symbol.loc, refresh = true) ensuring {
                _.isInstanceOf[Complete[_]]
              }
            }
            hierarcy.pop()
            desc

          case _ => tree
        }

        override protected def finalCheck(tree: Tree): Unit = {
          assert(modified.length == 1)
          assert(hierarcy.isEmpty)
        }
      } rewrite new StatelessTreeTransformer {
        override protected def enter(tree: Tree): Option[Tree] = tree match {
          case _: DescParametrized =>
            Some(tree) // retained for later error reporting but can be ignored
          case _: DescAlias => unreachable // removed in previous transform

          //////////////////////////////////////////////////////////////////////
          // Replace parameter calls with the specializations
          //////////////////////////////////////////////////////////////////////

          case expr @ ExprCall(tgt, _) if tgt.tpe.isParametrized =>
            Some {
              val special = expr.tpe match {
                case TypePackage(symbol, _)          => symbol
                case TypeType(TypeEntity(symbol, _)) => symbol
                case TypeType(TypeRecord(symbol, _)) => symbol
                case _                               => unreachable
              }
              walk(tgt) match {
                case ExprSym(_)       => TypeAssigner(ExprSym(special) withLocOf tree)
                case ExprSymSel(e, _) => TypeAssigner(ExprSymSel(e, special) withLocOf tree)
                case _                => unreachable
              }
            }

          //////////////////////////////////////////////////////////////////////
          // Recompute types of symbol refs as symbol types might have changed
          //////////////////////////////////////////////////////////////////////

          case expr: ExprSym =>
            Some(TypeAssigner(expr.copy() withLocOf tree))

          case expr @ ExprSymSel(tgt, _) =>
            Some(TypeAssigner(expr.copy(expr = walkSame(tgt)) withLocOf tree))

          case _ => None
        }
      }

      // Done
      (self, dependencies)
    }

    // ExprSymSel -> ExprSel
    object Transform extends StatelessTreeTransformer {
      override def enter(tree: Tree): Option[Tree] = tree match {
        case _: DescParametrized => Some(tree)
        case _                   => None
      }
      override def transform(tree: Tree): Tree = tree match {
        case ExprSymSel(expr, symbol) => TypeAssigner(ExprSel(expr, symbol.name) withLocOf tree)
        case _                        => tree
      }
    }

    process(desc) pipe {
      case (desc, dependencies) =>
        (desc rewrite Transform, dependencies.asPar.map(_ rewrite Transform))
    }
  }

}
