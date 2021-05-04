////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.Messages.Note
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.BigIntOps.BigIntClassOps
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.util.chaining._

private[frontend] object Specialize {

  def simplifyParamExpr(expr: Expr)(implicit fe: Frontend): FinalResult[Expr] = {
    object Transform extends StatelessTreeTransformer {
      override protected val typed: Boolean = false
      var bad: Option[FinalResult[Expr]] = None

      override def enter(tree: Tree): Option[Tree] = tree match {
        case _ if bad.isDefined => Some(tree) // Bail quickly, on issue

        case expr: Expr if expr.forall { case ExprUnary("'", _) => false } =>
          fe.tryEvaluate(expr) match {
            case Complete(Right(value)) =>
              Some {
                value.asExpr(expr.tpe.underlying) tap {
                  _ visitAll { case tree => tree withLocOf expr }
                }
              }
            case Complete(_)      => None
            case failure: Failure => bad = Some(failure); Some(tree)
            case unknown: Unknown => bad = Some(unknown); Some(tree)
          }

        case _ => None
      }
    }
    val transformed = expr rewrite Transform
    Transform.bad.getOrElse(Complete(transformed))
  }

  def paramsSuffix(
      body: List[Tree]
    )(
      implicit
      fe: Frontend
    ): String = {
    def gatherParamDescs(trees: List[Tree]): List[Desc] = trees map {
      case Splice(spliceable) => spliceable
      case other              => other
    } flatMap {
      case DescGenScope(_, _, body, _) => gatherParamDescs(body)
      case d: DescParam                => List(d)
      case d: DescParamType            => List(d)
      case _                           => Nil
    }

    gatherParamDescs(body)
      .map {
        case d: DescParam =>
          fe.evaluate(d.symbol, d.ref.loc, unreachable, markUsed = false)
            .flatMap(fe.evaluate(_, unreachable))
            .get
            .pipe(v => s"${d.name}=$v")
        case d: DescParamType =>
          fe.typeOf(d.symbol, d.symbol.loc)
            .map(_.asType.kind.toName)
            .get
            .pipe(name => s"${d.name}_$name")
        case _ => unreachable
      }
      .mkString("(", ",", ")")
  }

  def apply(
      symbol: Symbol,
      params: List[Arg],
      loc: Loc
    )(
      implicit
      cc: CompilerContext,
      fe: Frontend
    ): FinalResult[Symbol] = {
    require(fe.typeOf(symbol, symbol.loc) contains TypeParametrized(symbol))

    // simplify params as much as we can to avoid repeat work
    params
      .map {
        case a: ArgP => simplifyParamExpr(a.expr).map(e => a.copy(expr = e) withLocOf a)
        case a: ArgN => simplifyParamExpr(a.expr).map(e => a.copy(expr = e) withLocOf a)
        case _: ArgD => unreachable
      }
      .distil
      .flatMap { params =>
        params partitionMap { // Separate named and positional parameters
          case a: ArgP => Left(a)
          case a: ArgN => Right(a)
          case _: ArgD => unreachable
        } pipe { // Check there are only named or only positional parameters
          case (posParams, namParams) =>
            if (posParams.nonEmpty && namParams.nonEmpty) {
              Failure(loc, "Mixing positional and named parameter assignments is not allowed")
            } else {
              Complete((posParams, namParams.sortBy(_.name)))
            }
        }
      }
      .flatMap { // Check named parameters are used if there is a generated parameter
        case (posParams, namParams) =>
          val DescParametrized(_, _, desc, symtab) = symbol.desc.asInstanceOf[DescParametrized]
          assert(desc.isParametrized)
          if (desc.hasGeneratedParam && posParams.nonEmpty) {
            Failure(
              Error(
                loc,
                "Type with generated parameters requires named parameter assignment"
              ) withNote Note.definedHere(symbol.desc)
            )
          } else {
            Complete((desc, symtab, posParams concat namParams))
          }
      }
      .flatMap { // Attempt the the actual specialization
        case (desc, symtab, params) =>
          // Consult the specialization cache
          val specializations = symbol.attr.specializations.getOrElse(Map.empty)
          specializations.get(params) match {
            case Some(sSymbol) =>
              // Already seen these parameters, use cached value
              Complete(sSymbol)
            case None =>
              // Haven't tried these parameters yet, so go ahead and specialize

              // This is how to do it
              def specialize[T <: Tree: ListElaborable](
                  body: List[T],
                  mkDesc: (Sym, List[T]) => Desc
                ): FinalResult[Symbol] = {
                // Construct new symbol representing the specialized
                // definition. We use a temporary name to start with, as
                // the proper name can only be determined after type
                // checking the result
                val newSymbol = Symbol("`specialization-temp", symbol.loc)
                // Make an initial definition so we can examine enclosing symbols
                mkDesc(Sym(newSymbol), body)
                @tailrec
                def loop(body: List[T]): FinalResult[Symbol] =
                  fe.elaborate(body, Some(newSymbol), symtab, Some((loc, params))) match {
                    case Success(body) =>
                      // Construct the specialized definition. This will attach
                      // it to the symbol.
                      val newDesc = mkDesc(Sym(newSymbol) withLocOf desc.ref, body) withLocOf desc
                      // As the elaboration of the body is complete, the
                      // specialized definition must also type check at this point.
                      fe.typeCheck(newDesc) tapEach { _ =>
                        // Assign the name of the result symbol, now that type
                        // checking has passed and we know actual parameter values
                        val newName = symbol.name + paramsSuffix(body)
                        newSymbol.name = newName
                        newSymbol.origName = newName
                      } flatMap { _ =>
                        // Check that all named params have been used up
                        def gatherParamNames(tree: Tree): Iterator[String] = tree.flatCollect {
                          case d: DescParam         => Iterator.single(d.symbol.name)
                          case d: DescParamType     => Iterator.single(d.symbol.name)
                          case d: DescGenScope      => d.body flatMap gatherParamNames
                          case d: Desc if d ne tree => Iterator.empty // Otherwise stop recursion
                        }
                        val paramNames = Set.from(gatherParamNames(newDesc))
                        params.collect {
                          case a: ArgN =>
                            if (!(paramNames contains a.name)) {
                              Failure(a, s"'${symbol.name}' has no parameter '${a.name}'")
                            } else {
                              Complete(())
                            }
                        }.distil
                      } map { _ =>
                        // We are returning the new symbol
                        newSymbol
                      }
                    case Partial(body, _) =>
                      // Update initial definition so we can examine enclosing symbols
                      mkDesc(Sym(newSymbol), body)
                      loop(body)
                    case unknown: Unknown => unknown
                    case failure: Failure => failure
                    case _                => unreachable // Success covers the rest
                  }
                loop(body)
              }

              // Now actually do it
              desc pipe {
                case DescPackage(_, _, body) =>
                  specialize[Pkg](body, DescPackage(_, Nil, _))
                case DescEntity(_, _, variant, body) =>
                  specialize[Ent](body, DescEntity(_, Nil, variant, _))
                case DescRecord(_, _, body) =>
                  specialize[Rec](body, DescRecord(_, Nil, _))
                case _ => unreachable
              } tapEach { newSymbol =>
                // Attach attributes to the specialization
                newSymbol.attr.update(desc.attr)
              } map { newSymbol =>
                // Check we actually ended up with a new parametrization. Note
                // we cannot reliably determine this earlier because we cannot
                // evaluate unary ticks in the actual parameters without actually
                // seeing the elaborated (specialized) parameter definitions.
                val resultSymbol = specializations
                  .collectFirst {
                    // Find a specialization with the same name. Due to the name
                    // suffixes we add, specialized symbols with the same name must
                    // have been created from the same actual parameter values.
                    case (_, cachedSymbol) if cachedSymbol.name == newSymbol.name => cachedSymbol
                  }
                  .getOrElse(newSymbol)
                // update cache
                symbol.attr.specializations updateWith {
                  case None    => Some(Map(params -> resultSymbol))
                  case Some(m) => Some(m + (params -> resultSymbol))
                }
                // Yield the specialized symbol
                resultSymbol
              }
          }
      }
  }

}
