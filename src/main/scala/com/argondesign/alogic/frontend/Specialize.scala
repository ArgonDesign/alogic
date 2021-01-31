////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.Messages.Note
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.util.chaining._

private[frontend] object Specialize {

  def paramsSuffix(
      body: List[Tree]
    )(
      implicit
      cc: CompilerContext,
      fe: Frontend
    ): String = {
    def gatherParamDescs(trees: List[Tree]): List[Desc] = trees map {
      case Splice(spliceable) => spliceable
      case other              => other
    } flatMap {
      case DescGenScope(_, _, body) => gatherParamDescs(body)
      case d: DescParam             => List(d)
      case d: DescParamType         => List(d)
      case _                        => Nil
    }

    gatherParamDescs(body) map {
      case d: DescParam =>
        fe.evaluate(d.symbol, d.ref.loc, unreachable, markUsed = false)
          .flatMap(fe.evaluate(_, unreachable))
          .get
          .pipe {
            case v if v >= 0 => s"${d.name}_$v"
            case v           => s"${d.name}_n${-v}"
          }
      case d: DescParamType =>
        def typeToName(kind: TypeFund): String = kind match {
          case TypeSInt(size)          => s"i$size"
          case TypeUInt(size)          => s"u$size"
          case TypeNum(true)           => "int"
          case TypeNum(false)          => "uint"
          case TypeVector(eType, size) => s"${typeToName(eType)}[$size]"
          case TypeVoid                => "void"
          case TypeStr                 => unreachable
          case TypeRecord(symbol, _)   => symbol.name
          case TypeEntity(symbol, _)   => symbol.name
        }
        fe.typeOf(d.symbol, d.symbol.loc)
          .map(kind => typeToName(kind.asType.kind))
          .get
          .pipe(name => s"${d.name}_$name")
      case _ => unreachable
    } mkString cc.sep
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

    // TODO: simplify params as much as we can to avoid repeat work

    // Separate named and positional parameters
    params partition {
      case _: ArgP => true
      case _: ArgN => false
      case _: ArgD => unreachable
    } pipe { // Check there are only named or only positional parameters
      case (posParams, namParams) =>
        if (posParams.nonEmpty && namParams.nonEmpty) {
          Failure(loc, "Mixing positional and named parameter assignments is not allowed")
        } else {
          Complete((posParams, namParams))
        }
    } flatMap { // Check named parameters are used if there is a generated parameter
      case (posParams, namParams) =>
        val DescParametrized(_, _, desc, symtab) = symbol.desc.asInstanceOf[DescParametrized]
        assert(desc.isParametrized)
        if (desc.hasGeneratedParam && posParams.nonEmpty) {
          Failure(
            Seq(
              Error(loc, "Type with generated parameters requires named parameter assignment"),
              Note.definedHere(symbol.desc)
            )
          )
        } else {
          Complete((desc, symtab, posParams concat namParams))
        }
    } flatMap { // Attempt the the actual specialization
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
              @tailrec
              def loop(body: List[T]): FinalResult[Symbol] =
                fe.elaborate(body, symtab, Some((loc, params))) match {
                  case Success(body) =>
                    // Construct new symbol representing the specialized
                    // definition. We use a temporary name to start with, as
                    // the proper name can only be determined after type
                    // checking the result
                    val newSymbol = cc.newSymbol("@@@specialization-temp@@@", symbol.loc)
                    // Construct the new definition. This will attach it to the
                    // symbol.
                    val newDesc = mkDesc(Sym(newSymbol) withLocOf desc.ref, body) withLocOf desc
                    // As the elaboration of the body is complete, the
                    // specialized definition must also type check at this point.
                    fe.typeCheck(newDesc) tapEach { _ =>
                      // Assign the name of the result symbol, now that we
                      // type checking has passed
                      val newName = s"${symbol.name}${cc.sep}${paramsSuffix(body)}"
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
                  case Partial(body, _) => loop(body)
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
              specializations collectFirst {
                // Find a specialization with the same name. Due to the name
                // suffixes we add, specialized symbols with the same name must
                // have been created from the same actual parameter values.
                case (_, cachedSymbol) if cachedSymbol.name == newSymbol.name => cachedSymbol
              } getOrElse {
                // Genuinely new parameter values, update cache
                symbol.attr.specializations updateWith {
                  case None    => Some(Map(params -> newSymbol))
                  case Some(m) => Some(m + (params -> newSymbol))
                }
                // Yield the new specialized symbol
                newSymbol
              }
            }
        }
    }
  }

}
