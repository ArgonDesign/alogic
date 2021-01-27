////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Normalize trees prior to elaboration
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.util.unreachable
import scala.util.chaining._

final class SyntaxNormalize(implicit cc: CompilerContext) extends StatefulTreeTransformer {
  override val typed: Boolean = false

  private val tmpNames: Iterator[String] = LazyList.from(0).iterator.map(n => s"`tmp_$n")

  private def adjUnnamed(desc: Desc, isLoop: Boolean): Thicket = {
    require(desc.ref == Ident("", Nil))
    val name = tmpNames.next()
    val usng = if (isLoop) UsingGenLoopBody(_, Set.empty) else UsingAll(_, exprt = true)
    Thicket(
      desc.copyRef(ref = Ident(name, Nil) withLocOf desc) withLocOf desc,
      usng(ExprIdent(name, Nil) withLoc Loc.synthetic) withLoc Loc.synthetic
    )
  }

  private def adjDictNamed(
      ident: Ident,
      attr: List[Attr],
      body: List[Tree],
      makeDesc: (Ident, List[Tree]) => Desc
    ): Thicket = {
    val newName = tmpNames.next()
    Thicket(
      makeDesc(
        Ident(newName, Nil) withLocOf ident,
        (DescGenScope(ident, attr, body) withLocOf ident) :: Nil
      ),
      UsingGenLoopBody(
        ExprIdent(newName, Nil) withLoc Loc.synthetic,
        Set.empty
      ) withLoc Loc.synthetic
    )
  }

  private def wrapParametrized(desc: Desc): Desc = {
    val newName = tmpNames.next()
    val newDesc = desc.copyRef(ref = Ident(newName, Nil) withLocOf desc.ref) withLocOf desc
    DescParametrized(desc.ref, Nil, newDesc, SymbolTable.empty) withLocOf desc
  }

  private def localName(expr: Expr): Option[Ident] = expr match {
    case e @ ExprIdent(base, idxs) => Some(Ident(base, idxs) withLocOf e)
    case e @ ExprDot(_, selector, idxs) =>
      Some {
        val locStart = e.loc.point + 1
        Ident(selector, idxs) withLoc e.loc.copy(start = locStart, point = locStart)
      }
    case _: ExprCall => None
    case _           => unreachable
  }

  override def transform(tree: Tree): Tree = tree match {
    ////////////////////////////////////////////////////////////////////////////
    // Normalize 'gen' block names
    ////////////////////////////////////////////////////////////////////////////

    case desc: Desc =>
      desc match {
        // Unnamed 'gen' blocks
        case d @ DescGenIf(Ident("", Nil), _, _, _)          => adjUnnamed(d, isLoop = false)
        case d @ DescGenFor(Ident("", Nil), _, _, _, _, _)   => adjUnnamed(d, isLoop = true)
        case d @ DescGenRange(Ident("", Nil), _, _, _, _, _) => adjUnnamed(d, isLoop = true)
        // 'gen' loops with a non-dict identifier
        case DescGenFor(Ident(_, Nil), _, _, _, _, _)   => tree
        case DescGenRange(Ident(_, Nil), _, _, _, _, _) => tree
        // 'gen' loops with a dict identifier
        case d @ DescGenFor(ident: Ident, attr, _, _, _, body) =>
          adjDictNamed(ident, attr, body, { case (r, b) => d.copy(ref = r, body = b) withLocOf d })
        case d @ DescGenRange(ident: Ident, attr, _, _, _, body) =>
          adjDictNamed(ident, attr, body, { case (r, b) => d.copy(ref = r, body = b) withLocOf d })
        // Possibly parametrized
        case d if d.isParametrized => wrapParametrized(d)
        // Other
        case _ => tree
      }

    ////////////////////////////////////////////////////////////////////////////
    // Ensure UsingOne instances have explicit name
    ////////////////////////////////////////////////////////////////////////////

    case usng @ UsingOne(expr, None) =>
      localName(expr) match {
        case some: Some[_] => usng.copy(identOpt = some) withLocOf tree
        case None          => cc.error(usng, "'using' requires 'as' clause"); tree
      }

    ////////////////////////////////////////////////////////////////////////////
    // Turn From instances to Import + Using
    ////////////////////////////////////////////////////////////////////////////

    case from @ FromOne(path, name, identOpt) =>
      identOpt orElse localName(name) match {
        case some: Some[_] =>
          val tmpName = tmpNames.next()
          val selected = {
            def reselect(stem: Expr, name: Expr): Expr = name match {
              case ExprIdent(base, idxs) =>
                ExprDot(stem, base, idxs) withLocOf name
              case ExprDot(expr, selector, idxs) =>
                ExprDot(reselect(stem, expr), selector, idxs) withLocOf name
              case ExprCall(expr, args) =>
                ExprCall(reselect(stem, expr), args) withLocOf name
              case _ => unreachable // TODO: sytnaxcheck
            }
            reselect(ExprIdent(tmpName, Nil) withLocOf name, name)
          }
          Thicket(
            ImportOne(path, Ident(tmpName, Nil) withLocOf tree) withLocOf tree,
            UsingOne(selected, some) withLocOf tree
          )
        case None => cc.error(from, "from import requires 'as' clause"); tree
      }

    case FromAll(path) =>
      val tmpName = tmpNames.next()
      Thicket(
        ImportOne(path, Ident(tmpName, Nil) withLocOf tree) withLocOf tree,
        UsingAll(ExprIdent(tmpName, Nil) withLocOf tree, exprt = false) withLocOf tree
      )

    ////////////////////////////////////////////////////////////////////////////
    // Ensure PkgCompile instances have explicit name when required
    ////////////////////////////////////////////////////////////////////////////

    case PkgCompile(expr, None) =>
      expr match {
        case ExprIdent(_, Nil)             => tree
        case ExprDot(_: ExprIdent, _, Nil) => tree
        case _                             => cc.error(tree, "'compile' requires 'as' clause"); tree
      }

    //
    case _ => tree
  }

}

object SyntaxNormalize {

  def apply(tree: Desc)(implicit cc: CompilerContext): Option[Desc] = {
    tree rewrite new SyntaxNormalize pipe { Option.unless(cc.hasError)(_) }
  }

}
