////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Rewrite basic syntactic sugar into their equivalent normal form.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.Expr.InstancePortSel
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.TypeAssigner
import com.argondesign.alogic.core.Types.TypeEntity
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable

final class DesugarTransform extends StatelessTreeTransformer {

  private val enclosing = mutable.Stack[Defn]()

  override protected def enter(tree: Tree): Option[Tree] = {
    tree match {
      case d: DefnEntity    => enclosing.push(d)
      case d: DefnSingleton => enclosing.push(d)
      case _                =>
    }
    None
  }

  override def transform(tree: Tree): Tree = tree match {
    // "a++" rewritten as  "a = a + <width of a>'(s?)d1"
    case StmtPost(lhs, op) =>
      val rhs = op match {
        case "++" => lhs.inc
        case "--" => lhs.dec
        case _    => unreachable
      }
      TypeAssigner(StmtAssign(lhs, rhs) withLoc tree.loc)

    // "a += b" rewritten as "a = a + b"
    case StmtUpdate(lhs, op, expr) =>
      StmtAssign(lhs, ExprBinary(lhs, op, expr)) regularize tree.loc

    // "let(<init>) <stmts>" rewritten as "<init> <stmts>"
    case StmtLet(inits, stmts) => Thicket(inits ::: stmts)

    case _: DefnEntity =>
      enclosing.pop()
      tree

    // "new entity <name> {<body>}" rewritten as "entity <name> {<body>} <name> = new <name>();"
    case DeclSingleton(symbol, decls) =>
      val eSymbol = symbol.dup
      val declEntity = TypeAssigner(DeclEntity(eSymbol, decls) withLoc tree.loc)
      val specInstance = TypeAssigner(ExprSym(eSymbol) withLoc symbol.loc)
      val declInstance = TypeAssigner(
        DeclInstance(symbol, specInstance, bind = false) withLoc tree.loc
      )
      Thicket(List(declEntity, declInstance))

    case DefnSingleton(symbol, variant, body) =>
      enclosing.pop()
      val eSymbol = symbol.decl match {
        case DeclInstance(`symbol`, ExprSym(s), _) => s
        case _                                     => unreachable
      }
      val defnEntity = TypeAssigner(DefnEntity(eSymbol, variant, body) withLoc tree.loc)
      val defnInstance = TypeAssigner(DefnInstance(symbol) withLoc tree.loc)
      Thicket(List(defnEntity, defnInstance))

    // Convert EntConnects to EntAssigns (note this swap the order of LHS/RHS).
    // Make cardinal port references explicit.
    case EntConnect(lhs, rhss) =>
      val newRhs = lhs.tpe match {
        case _: TypeEntity => TypeAssigner(ExprSel(lhs, "out") withLoc lhs.loc)
        case _             => lhs
      }
      Thicket(
        rhss map { rhs =>
          val newLhs = rhs.tpe match {
            case _: TypeEntity => TypeAssigner(ExprSel(rhs, "in") withLoc rhs.loc)
            case _             => rhs
          }
          val loc = tree.loc.copy(end = rhs.loc.end, point = rhs.loc.start)
          TypeAssigner(EntAssign(newLhs, newRhs) withLoc loc)
        }
      )

    // Convert EntConnectInputs
    case EntConnectInputs(expr) =>
      val iSymbol = expr match {
        case ExprSym(symbol) => symbol
        case _               => unreachable
      }
      val pSymbols =
        expr.tpe.asEntity.portMembers.filter(s => s.kind.isIn || s.kind.isPipeIn || s.kind.isSnoop)
      val connectedInEnclosing: Set[Symbol] = Set from {
        def connected(ent: Ent): Iterator[Symbol] =
          ent
            .pipe {
              case EntConnect(_, rhss) => rhss.iterator
              case _: EntAssign        => unreachable
              case _                   => Iterator.empty
            }
            .flatMap(_.collect { case InstancePortSel(`iSymbol`, pSymbol) => pSymbol })
        enclosing.head match {
          case DefnEntity(_, _, body)    => body.iterator.flatMap(connected)
          case DefnSingleton(_, _, body) => body.iterator.flatMap(connected)
          case _                         => unreachable
        }
      }

      val portsDefinedInEnclosingEntities = Map from {
        enclosing
          .flatMap {
            _.symbol.decl
              .pipe {
                case d: DeclEntity => d.ports
                case d: DeclInstance =>
                  d.symbol.kind.asEntity.symbol.decl.asInstanceOf[DeclEntity].ports
                case _ => unreachable
              }
          }
          .distinctBy(_.name)
          .map(s => s.name -> s)
      }

      Thicket {
        pSymbols.filterNot(connectedInEnclosing).map { symbol =>
          EntAssign(
            ExprSym(iSymbol) sel symbol.name,
            ExprSym(portsDefinedInEnclosingEntities(symbol.name))
          ) regularize tree.loc
        }
      }

    //
    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(enclosing.isEmpty)
    tree visit {
      case node: StmtLet       => throw Ice(node, s"StmtLet remains")
      case node: StmtUpdate    => throw Ice(node, s"StmtUpdate remains")
      case node: StmtPost      => throw Ice(node, s"StmtPost remains")
      case node: DeclSingleton => throw Ice(node, s"DeclSingleton remains")
      case node: DefnSingleton => throw Ice(node, s"DefnSingleton remains")
      case node: EntConnect    => throw Ice(node, "EntConnect with remains")
    }
  }

}

object Desugar extends PairTransformerPass {
  val name = "desugar"

  def transform(decl: Decl, defn: Defn)(implicit cc: CompilerContext): (Tree, Tree) = {
    val transform = new DesugarTransform
    (transform(decl), transform(defn))
  }

}
