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

  private var tempCnt: Int = 0

  private def nextTempIdent(): Ident =
    Ident(s"`tmp_$tempCnt", Nil) tap { _ => tempCnt += 1 }

  private def adjUnnamed(desc: Desc, isLoop: Boolean): Thicket = {
    require(desc.ref == Ident("", Nil))
    val ident = nextTempIdent() withLocOf desc
    val using = if (isLoop) UsingGenLoopBody(_, Set.empty) else UsingAll(_, exprt = true)
    Thicket(
      desc.copyRef(ref = ident) withLocOf desc,
      using(ExprIdent(ident) withLoc Loc.synthetic) withLoc Loc.synthetic
    )
  }

  private def adjDictNamed(
      ident: Ident,
      attr: List[Attr],
      body: List[Tree],
      makeDesc: (Ident, List[Tree]) => Desc
    ): Thicket = {
    val newIdent = nextTempIdent() withLocOf ident
    val newBody = List(
      DescGenScope(ident, attr, body) withLocOf ident
    )
    val newDesc = makeDesc(newIdent, newBody)
    Thicket(
      newDesc,
      UsingGenLoopBody(ExprIdent(newIdent) withLoc Loc.synthetic, Set.empty) withLoc Loc.synthetic
    )
  }

  private def wrapParametrized(desc: Desc): Desc = {
    val newIdent = nextTempIdent() withLocOf desc.ref
    val newDesc = desc.copyRef(ref = newIdent) withLocOf desc
    DescParametrized(desc.ref, Nil, newDesc, SymbolTable.empty) withLocOf desc
  }

  private def localName(expr: Expr): Option[Ident] = expr match {
    case ExprIdent(ident: Ident) => Some(ident)
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
    // Ensure ImportOne instances have explicit name
    ////////////////////////////////////////////////////////////////////////////

    case imprt @ ImportOne(_, expr, None) =>
      localName(expr) match {
        case some: Some[_] => imprt.copy(identOpt = some) withLocOf tree
        case None          => cc.error(imprt, "import requires 'as' clause"); tree
      }

    ////////////////////////////////////////////////////////////////////////////
    // Ensure UsingOne instances have explicit name
    ////////////////////////////////////////////////////////////////////////////

    case using @ UsingOne(expr, None) =>
      localName(expr) match {
        case some: Some[_] => using.copy(identOpt = some) withLocOf tree
        case None          => cc.error(using, "'using' requires 'as' clause"); tree
      }

    ////////////////////////////////////////////////////////////////////////////
    // Turn From instances to Import + Using
    ////////////////////////////////////////////////////////////////////////////

    case from @ FromOne(relative, expr, name, identOpt) =>
      identOpt orElse localName(name) match {
        case some: Some[_] =>
          val imprtTmpIdent = nextTempIdent() withLocOf expr
          val selected = {
            def reselect(stem: Expr, name: Expr): Expr = name match {
              case ExprIdent(Ident(base, idxs)) =>
                ExprDot(stem, base, idxs) withLocOf name
              case ExprDot(expr, selector, idxs) =>
                ExprDot(reselect(stem, expr), selector, idxs) withLocOf name
              case ExprCall(expr, args) =>
                ExprCall(reselect(stem, expr), args) withLocOf name
              case _ => unreachable // TODO: sytnaxcheck
            }
            reselect(ExprIdent(imprtTmpIdent) withLocOf name, name)
          }
          Thicket(
            ImportOne(relative, expr, Some(imprtTmpIdent)) withLocOf tree,
            UsingOne(selected, some) withLocOf tree
          )
        case None => cc.error(from, "from import requires 'as' clause"); tree
      }

    case FromAll(relative, expr) =>
      val imprtTmpIdent = nextTempIdent() withLocOf expr
      Thicket(
        ImportOne(relative, expr, Some(imprtTmpIdent)) withLocOf tree,
        UsingAll(ExprIdent(imprtTmpIdent) withLocOf expr, exprt = false) withLocOf tree
      )

    ////////////////////////////////////////////////////////////////////////////
    // Ensure PkgCompile instances have explicit name when required
    ////////////////////////////////////////////////////////////////////////////

    case PkgCompile(expr, None) =>
      expr match {
        case ExprIdent(Ident(_, Nil))      => tree
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
