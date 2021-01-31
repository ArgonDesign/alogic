////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.ParOrSeqIterable
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.util.unreachable

object DescToDeclDefnTransform extends StatelessTreeTransformer {

  def convert(desc: Desc): (Decl, Defn) = walk(desc) match {
    case Thicket(List(decl: Decl, defn: Defn)) => (decl, defn)
    case _                                     => unreachable
  }

  private def partitionDeclsFromRest[T <: Tree](body: List[T]): (List[Decl], List[T]) =
    body partitionMap {
      case Splice(decl: Decl) => Left(decl)
      case other              => Right(other)
    }

  override protected def enter(tree: Tree): Option[Tree] = tree match {
    case DescFunc(Sym(symbol), _, variant, ret, args, body) =>
      Some {
        // Need to do the args up front as Decl/Defn is not valid in the
        // position of args in DescFunc
        val (argDecls, argDefns) = args.map(convert).unzip
        // Need to walk the body as normal, just walking the Defn will do ...
        Thicket(
          TypeAssigner(DeclFunc(symbol, variant, ret, argDecls) withLocOf tree),
          walkSame(TypeAssigner(DefnFunc(symbol, argDefns, body) withLocOf tree))
        )
      }

    // Skip
    case _: Expr => Some(tree)

    case _ => None
  }

  override protected def transform(tree: Tree): Tree = tree match {
    case desc: Desc =>
      desc pipe {
        case DescVar(Sym(symbol), _, spec, initOpt) =>
          (DeclVar(symbol, spec), DefnVar(symbol, initOpt))
        case DescVal(Sym(symbol), _, spec, init) =>
          (DeclVal(symbol, spec), DefnVal(symbol, init))
        case DescStatic(Sym(symbol), _, spec, initOpt) =>
          (DeclStatic(symbol, spec), DefnStatic(symbol, initOpt))
        case DescIn(Sym(symbol), _, spec, fc) =>
          (DeclIn(symbol, spec, fc), DefnIn(symbol))
        case DescOut(Sym(symbol), _, spec, fc, st, initOpt) =>
          (DeclOut(symbol, spec, fc, st), DefnOut(symbol, initOpt))
        case DescPipeVar(Sym(symbol), _, spec) =>
          (DeclPipeVar(symbol, spec), DefnPipeVar(symbol))
        case DescPipeIn(Sym(symbol), _, fc) =>
          (DeclPipeIn(symbol, fc), DefnPipeIn(symbol))
        case DescPipeOut(Sym(symbol), _, fc, st) =>
          (DeclPipeOut(symbol, fc, st), DefnPipeOut(symbol))
        case DescParam(Sym(symbol), _, spec, initOpt, _) =>
          // Note: we are changing to 'const'
          (DeclConst(symbol, spec), DefnConst(symbol, initOpt.get))
        case DescParamType(Sym(symbol), _, initOpt, _) =>
          // Note: we are changing to 'typedef'
          (DeclType(symbol, initOpt.get), DefnType(symbol))
        case DescConst(Sym(symbol), _, spec, init) =>
          (DeclConst(symbol, spec), DefnConst(symbol, init))
        case DescArray(Sym(symbol), _, elem, size) =>
          (DeclArray(symbol, elem, size.valueOption.get.toLong), DefnArray(symbol))
        case DescSram(Sym(symbol), _, elem, size, st) =>
          (DeclSram(symbol, elem, size.valueOption.get.toLong, st), DefnSram(symbol))
        case DescType(Sym(symbol), _, spec) =>
          (DeclType(symbol, spec), DefnType(symbol))
        case DescEntity(Sym(symbol), _, variant, body) =>
          val (decls, rest) = partitionDeclsFromRest(body)
          (DeclEntity(symbol, decls), DefnEntity(symbol, variant, rest))
        case DescRecord(Sym(symbol), _, body) =>
          val (decls, rest) = partitionDeclsFromRest(body)
          (DeclRecord(symbol, decls), DefnRecord(symbol, rest))
        case DescInstance(Sym(symbol), _, spec) =>
          (DeclInstance(symbol, spec), DefnInstance(symbol))
        case DescSingleton(Sym(symbol), _, variant, body) =>
          val (decls, rest) = partitionDeclsFromRest(body)
          (DeclSingleton(symbol, decls), DefnSingleton(symbol, variant, rest))
        case DescGenVar(Sym(symbol), _, spec, init) =>
          // Note: we are changing to 'const'
          (DeclConst(symbol, spec), DefnConst(symbol, init))
        case _: Desc =>
          // DescFunc removed in 'enter', DescPackage removed by DropPackages,
          // and everything else is removed by the frontend
          unreachable
      } pipe {
        case (decl, defn) =>
          Thicket(
            TypeAssigner(decl withLocOf desc),
            TypeAssigner(defn withLocOf desc)
          )
      }

    case _ => tree
  }

  override protected def finalCheck(tree: Tree): Unit = tree visit {
    case desc: Desc => throw Ice(desc, "Desc remains after DescToDeclDefn")
  }

}

object DescToDeclDefn extends SimplePass[ParOrSeqIterable[Desc], Pairs] {
  val name = "desc-to-decl-defn"

  override protected def dump(result: Pairs, tag: String)(implicit cc: CompilerContext): Unit =
    result.asPar foreach { case (decl, defn) => cc.dump(decl, defn, "." + tag) }

  override protected def process(
      input: ParOrSeqIterable[Desc]
    )(
      implicit
      cc: CompilerContext
    ): Pairs =
    input.asPar.map(DescToDeclDefnTransform.convert)

}
