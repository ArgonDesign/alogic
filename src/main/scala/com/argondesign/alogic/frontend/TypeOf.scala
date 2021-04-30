////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Compute type of symbol based on its definition.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeReady
import com.argondesign.alogic.core.FuncVariant
import com.argondesign.alogic.core.Locatable
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.Messages.Note
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable
import com.argondesign.alogic.util.BigIntOps._

import scala.annotation.tailrec
import scala.util.chaining._

private[frontend] object TypeOf {

  private def parametrizedWhat(symbol: Symbol): String =
    symbol.desc.asInstanceOf[DescParametrized].desc match {
      case _: DescPackage => "package"
      case _: DescEntity  => "entity"
      case _: DescRecord  => "struct"
      case _              => unreachable
    }

  private def simpleTypeFrom(spec: Expr)(implicit fe: Frontend): FinalResult[TypeFund] =
    fe.typeCheck(spec) flatMap {
      case TypeType(_: TypeEntity) => Failure(spec, "Type specifier refers to an entity")
      case TypeType(kind)          => Complete(kind)
      case TypeParametrized(symbol) =>
        Failure(spec, s"Parametrized ${parametrizedWhat(symbol)} requires parameter list")
      case _ => Failure(spec, s"Type specifier does not name a type")
    }

  private def entityTypeFrom(spec: Expr)(implicit fe: Frontend): FinalResult[TypeEntity] =
    fe.typeCheck(spec) flatMap {
      case TypeType(kind: TypeEntity) => Complete(kind)
      case TypeParametrized(symbol) =>
        Failure(spec, s"Parametrized ${parametrizedWhat(symbol)} requires parameter list")
      case _ => Failure(spec, "Expression does not name an entity")
    }

  private def ensureNotNum[T: Locatable](
      item: T,
      hint: String
    )(
      kind: TypeFund
    ): FinalResult[TypeFund] =
    if (kind.isNum) Failure(item, s"$hint cannot have an unsized integer type") else Complete(kind)

  private def ensureNotVoid[T: Locatable](
      item: T,
      hint: String
    )(
      kind: TypeFund
    ): FinalResult[TypeFund] =
    if (kind.isVoid) Failure(item, s"$hint cannot have a 'void' type") else Complete(kind)

  private def ensureNotNumNorVoid[T: Locatable](
      item: T,
      hint: String
    )(
      kind: TypeFund
    ): FinalResult[TypeFund] = ensureNotNum(item, hint)(kind) flatMap ensureNotVoid(item, hint)

  // Extract list of symbols, defined by trees
  private def definedSymbols(
      trees: List[Tree],
      filter: Desc => Boolean = _ => true
    )(
      implicit
      fe: Frontend
    ): FinalResult[List[Symbol]] =
    trees.iterator collect {
      case Splice(tree: Tree) => tree
      case tree               => tree
    } filter {
      case _: Desc  => true
      case _: Using => true
      case _        => false
    } map {
      case u: Using        => Unknown(ReasonUnelaborated(u))
      case d: DescGenIf    => Unknown(ReasonUnelaborated(d))
      case d: DescGenFor   => Unknown(ReasonUnelaborated(d))
      case d: DescGenRange => Unknown(ReasonUnelaborated(d))
      case Desc(i: Ident)  => Unknown(ReasonUnelaborated(i))
      case d: Desc         => Complete(d)
      case _               => unreachable
    } collect {
      case Complete(d: Desc) if filter(d) => Complete(d.symbol)
      case unknown: Unknown               => unknown
    } pipe {
      _.toList.distil
    } flatMap { symbols =>
      symbols.map { s =>
        fe.typeOf(s, s.loc) map { _ => s }
      }.distil
    }

  private def portSymbols(
      trees: List[Tree]
    )(
      implicit
      fe: Frontend
    ): FinalResult[List[Symbol]] = definedSymbols(trees) map {
    _ filter {
      _.kind match {
        case _: TypeIn | _: TypePipeIn | _: TypeOut | _: TypePipeOut | _: TypeSnoop => true
        case _                                                                      => false
      }
    }
  }

  private def pipelineVariableSymbolsWithin(
      symbols: List[Symbol] // The entities we are examining
    )(
      implicit
      fe: Frontend
    ): FinalResult[List[Symbol]] = {
    def gather(symbol: Symbol): FinalResult[List[Symbol]] = {
      val body = symbol.desc match {
        case DescEntity(_, _, _, body)    => body
        case DescSingleton(_, _, _, body) => body
        case _                            => unreachable
      }

      // There is a circularity problem as definedSymbols will try to type check
      // all definitions in the body, which includes the entity containing
      // the pipeline port definition that invoked this check. As we are only
      // interested in the pipeline variables in the body, we break the infinite
      // descent, by ignoring everything but the pipeline variables. Note that we
      // also need to consider ones created by 'gen', which we can tell from the
      // form of the alias.
      @tailrec
      def consider(desc: Desc): Boolean = desc match {
        case _: DescPipeVar                      => true
        case DescAlias(_, _, ExprSym(symbol), _) => consider(symbol.desc)
        case _                                   => false
      }

      definedSymbols(body, consider) map {
        _ filter {
          _.kind match {
            case _: TypePipeVar => true
            case _              => false
          }
        }
      }
    }

    symbols.map(gather).distil.map(_.flatten.distinctBy(_.name))
  }

  def apply(
      symbol: Symbol,
      refresh: Boolean
    )(
      implicit
      fe: Frontend
    ): FinalResult[Type] =
    if (!refresh && symbol.kindIsSet) {
      Complete(symbol.kind)
    } else {
      symbol.descOption.map {
        case DescVar(_, _, spec, _) =>
          simpleTypeFrom(spec) flatMap ensureNotNumNorVoid(spec, "Variable")
        case DescVal(_, _, spec, _) =>
          simpleTypeFrom(spec) flatMap ensureNotVoid(spec, "Constant")
        case DescStatic(_, _, spec, _) =>
          simpleTypeFrom(spec) flatMap ensureNotNumNorVoid(spec, "Static variable")
        case DescIn(_, _, spec, fc) =>
          simpleTypeFrom(spec) flatMap ensureNotNum(spec, "Input port") flatMap {
            if (fc == FlowControlTypeNone) {
              ensureNotVoid(spec, "Input port without flow control")
            } else {
              Complete(_)
            }
          } map { TypeIn(_, fc) }
        case DescOut(_, _, spec, fc, st, _) =>
          simpleTypeFrom(spec) flatMap ensureNotNum(spec, "Output port") flatMap {
            if (fc == FlowControlTypeNone) {
              ensureNotVoid(spec, "Output port without flow control")
            } else {
              Complete(_)
            }
          } map { TypeOut(_, fc, st) }
        case d @ DescSnoop(_, _, spec, fc) =>
          simpleTypeFrom(spec) flatMap ensureNotNum(spec, "Snoop port") flatMap { kind =>
            if (fc != FlowControlTypeReady) {
              Failure(d, s"'snoop' port must use 'sync ready' flow control")
            } else {
              Complete(kind)
            }
          } map { TypeSnoop(_) }
        case DescPipeVar(_, _, spec) =>
          simpleTypeFrom(spec) flatMap ensureNotNumNorVoid(
            spec,
            "Pipeline variable"
          ) map TypePipeVar.apply
        case d @ DescPipeIn(_, _, hosts, fc) =>
          if (hosts.isEmpty) {
            Unknown(ReasonUnelaborated(d))
          } else {
            val symbols = hosts.collect { case ExprSym(symbol) => symbol }
            pipelineVariableSymbolsWithin(symbols) map { TypePipeIn(_, fc) }
          }
        case d @ DescPipeOut(_, _, hosts, fc, st) =>
          if (hosts.isEmpty) {
            Unknown(ReasonUnelaborated(d))
          } else {
            val symbols = hosts.collect { case ExprSym(symbol) => symbol }
            pipelineVariableSymbolsWithin(symbols) map { TypePipeOut(_, fc, st) }
          }
        case d @ DescParam(_, _, _, None, _) =>
          // Missing parameter assignment caught in Elaborate
          Unknown(ReasonUnelaborated(d))
        case DescParam(_, _, spec, _, _) =>
          simpleTypeFrom(spec) flatMap ensureNotVoid(spec, "Parameter") map TypeConst.apply
        case d @ DescParamType(_, _, _, false) =>
          Unknown(ReasonUnelaborated(d))
        case d @ DescParamType(_, _, None, _) =>
          // Missing parameter assignment caught in Elaborate
          Unknown(ReasonUnelaborated(d))
        case DescParamType(_, _, Some(init), _) =>
          fe.typeCheck(init) flatMap {
            case tt: TypeType => Complete(tt)
            case _ =>
              if (symbol.desc.loc contains init.loc) {
                Failure(init, "Type parameter default initializer does not name a type")
              } else {
                Failure(
                  Error(
                    init,
                    s"Actual value of parameter '${symbol.name}' does not name a type"
                  ) withNote Note.definedHere(symbol.desc)
                )
              }
          }
        case DescConst(_, _, spec, _) =>
          simpleTypeFrom(spec) flatMap ensureNotVoid(spec, "Constant") map TypeConst.apply
        case DescArray(_, _, elem, size) =>
          simpleTypeFrom(elem) flatMap ensureNotNumNorVoid(elem, "Array element") flatMap { e =>
            fe.evaluate(size, "Size of array") map { s => TypeArray(e, s.asLong) }
          }
        case DescSram(_, _, elem, size, st) =>
          simpleTypeFrom(elem) flatMap ensureNotNumNorVoid(elem, "SRAM element") flatMap { e =>
            fe.evaluate(size, "Size of SRAM") map { s => TypeSram(e, s.asLong, st) }
          }
        case DescType(_, _, spec) => simpleTypeFrom(spec) map TypeType.apply
        case DescEntity(_, _, _, body) =>
          portSymbols(body) map { symbols => TypeType(TypeEntity(symbol, symbols)) }
        case DescRecord(_, _, body) =>
          definedSymbols(body) map { symbols => TypeType(TypeRecord(symbol, symbols)) }
        case DescInstance(_, _, spec, _) => entityTypeFrom(spec)
        case DescSingleton(_, _, _, body) =>
          portSymbols(body) map { symbols => TypeEntity(symbol, symbols) }
        case DescFunc(_, _, variant, ret, args, _) =>
          args
            .map(desc => fe.typeOf(desc.symbol, desc.symbol.loc) map { _.asFund })
            .distil
            .flatMap { aTypes =>
              simpleTypeFrom(ret) flatMap ensureNotNum(ret, "Function return value") map { rType =>
                variant match {
                  case FuncVariant.Ctrl   => TypeCtrlFunc(symbol, rType, aTypes)
                  case FuncVariant.Comb   => TypeCombFunc(symbol, rType, aTypes)
                  case FuncVariant.Xeno   => TypeXenoFunc(symbol, rType, aTypes)
                  case FuncVariant.Static => TypeStaticMethod(symbol, rType, aTypes)
                  case FuncVariant.Method => TypeNormalMethod(symbol, rType, aTypes)
                }
              }
            }
        case DescPackage(_, _, body) =>
          definedSymbols(body)
            .map(
              // Drop non-exported aliases
              _.filter(symbol =>
                symbol.desc match {
                  case DescAlias(_, _, _, exprt) => exprt
                  case _                         => true
                }
              )
            )
            .map(symbols => TypePackage(symbol, symbols))

        case DescGenVar(_, _, spec, _) =>
          simpleTypeFrom(spec) flatMap ensureNotVoid(spec, "Constant") map TypeGen.apply
        case d: DescGenIf    => Unknown(ReasonUnelaborated(d))
        case d: DescGenFor   => Unknown(ReasonUnelaborated(d))
        case d: DescGenRange => Unknown(ReasonUnelaborated(d))
        case DescGenScope(_, _, body, _) =>
          definedSymbols(body) map { symbols => TypeScope(symbol, symbols) }
        case DescAlias(_, _, expr, _)     => fe.typeCheck(expr)
        case DescParametrized(_, _, _, _) => Complete(TypeParametrized(symbol))
      } getOrElse {
        // Must be a builtin with no desc
        Complete(TypeBuiltin)
      } tapEach {
        symbol.kind = _
      }
    }

}
