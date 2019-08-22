////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// The checker runs immediately after the builder and checks that input that
// is although valid according to the input grammar, is also valid in the
// context it is used in. The job of the checker is to ensure that assumptions
// made by later stages hold for the current input. The only rewrites the
// checker is allowed to do are:
// - Remove entire offending subtrees where such removal yields a valid tree
//   enabling forward progress
// - Replace illegal subtrees with alternatives where such a fix yields a valid
//   tree enabling forward progress
// - Replace offending subtrees with Error nodes
// The checker should always signal an error when a rewrite is done, and not
// rewrite any nodes which are not erroneous. Then trying to fix the tree, the
// checker should apply the 'most correct' fix that would enable the compiler
// to proceed as far as possible in order to allow reporting of further issues.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.TreeCopier
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeAccept
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeReady
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeValid
import com.argondesign.alogic.core.StorageTypes.StorageTypeReg
import com.argondesign.alogic.core.StorageTypes.StorageTypeSlices
import com.argondesign.alogic.core.StorageTypes.StorageTypeWire
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.mutable

final class Checker(implicit cc: CompilerContext) extends TreeTransformer {

  override val typed: Boolean = false

  private[this] val variantStack = mutable.Stack[String]()

  private[this] val paramConstAllowed = mutable.Stack[Boolean](true)

  private[this] var loopLevel: Int = 0

  override def enter(tree: Tree): Unit = tree match {
    case entity @ Entity(ident: Ident, _) =>
      variantStack push {
        ident.attr("//variant").asInstanceOf[ExprStr].value match {
          case "fsm"      => "fsm"
          case "network"  => "network"
          case "verbatim" => "verbatim"
          case other      => cc.ice(entity, s"Unknown entity variant '$other'")
        }
      }
      paramConstAllowed push true
    case _: Gen =>
      paramConstAllowed push false
    case _: StmtLoop | _: StmtWhile | _: StmtFor | _: StmtDo =>
      loopLevel += 1
    case _ =>
  }

  def err(node: Tree, content: String) = {
    cc.error(node, s"'${variantStack.top}' entity cannot contain ${content}")
    Thicket(Nil) withLoc node.loc
  }

  def notVariant(str: String) = variantStack.nonEmpty && variantStack.top != str

  override def transform(tree: Tree): Tree = tree match {

    case node: EntInstance if notVariant("network") => err(node, "instantiations")

    case node: EntConnect if notVariant("network") => err(node, "connections")

    case node: EntFunction if notVariant("fsm") => err(node, "function definitions")

    case node: EntCombProcess if notVariant("fsm") => err(node, "fence blocks")

    case node: EntEntity if notVariant("network") => err(node, "nested entities")

    case node @ EntDecl(decl @ DeclIdent(ident, kind, _)) =>
      variantStack.top match {
        case "fsm" =>
          kind match {
            case _: TypePipeline => err(node, "pipeline variable declarations")
            case _               => node
          }
        case "network" =>
          kind match {
            case _: TypeIn       => node
            case _: TypeOut      => node
            case _: TypeParam    => node
            case _: TypeConst    => node
            case _: TypePipeline => node
            case _: TypeArray    => err(decl, "distributed memory declarations")
            case _: TypeSram     => err(decl, "SRAM declarations")
            case _               => err(decl, "variable declarations")
          }
        case "verbatim" =>
          kind match {
            case _: TypeIn                       => node
            case _: TypeOut                      => node
            case _: TypeParam                    => node
            case _: TypeConst                    => node
            case _: TypeArray                    => err(decl, "distributed memory declarations")
            case _: TypePipeline                 => err(decl, "pipeline variable declarations")
            case TypeSram(_, _, StorageTypeWire) => node
            case TypeSram(_, _, _)               => err(decl, "registered SRAM declarations")
            case _                               => err(decl, "variable declarations")
          }
        case _ => unreachable
      }

    case entity @ Entity(ident: Ident, body) => {
      val newBody = if (entity.combProcesses.lengthIs > 1) {
        entity.combProcesses foreach {
          cc.error(_, s"Multiple fence blocks specified in entity '${ident.name}'")
        }
        body filter {
          case _: EntCombProcess => false
          case _                 => true
        }
      } else {
        body
      }

      case class Seen(
          entity: Boolean = false,
          instance: Boolean = false,
          connect: Boolean = false,
          fence: Boolean = false,
          function: Boolean = false,
          verbatim: Boolean = false
      ) {
        def ||(that: Seen) = Seen(
          entity || that.entity,
          instance || that.instance,
          connect || that.connect,
          fence || that.fence,
          function || that.function,
          verbatim || that.verbatim
        )
      }

      def checkDeclOrder(trees: List[Tree], seen: Seen): Seen = {
        def checkDecl(decl: DeclIdent) = {
          val kind = decl.kind
          if (kind.isIn || kind.isOut || kind.isParam) {
            val prefix = if (kind.isParam) {
              "Parameter declarations must appear before"
            } else {
              "Port declarations must appear before"
            }
            if (seen.entity) cc.error(decl, s"${prefix} nested entities")
            if (seen.instance) cc.error(decl, s"${prefix} instances")
            if (seen.connect) cc.error(decl, s"${prefix} connections")
            if (seen.fence) cc.error(decl, s"${prefix} 'fence' block")
            if (seen.function) cc.error(decl, s"${prefix} function definitions")
            if (seen.verbatim) cc.error(decl, s"${prefix} 'verbatim' blocks")
          }
        }

        if (trees.isEmpty) {
          seen
        } else {
          val newSeen = trees.head match {
            case EntDecl(decl: DeclIdent)     => checkDecl(decl); seen
            case GenDecl(decl: DeclIdent)     => checkDecl(decl); seen
            case _: EntDefn                   => seen
            case _: EntEntity                 => seen.copy(entity = true)
            case _: EntInstance               => seen.copy(instance = true)
            case _: EntConnect                => seen.copy(connect = true)
            case _: EntCombProcess            => seen.copy(fence = true)
            case _: EntFunction               => seen.copy(function = true)
            case _: EntVerbatim               => seen.copy(verbatim = true)
            case EntGen(GenIf(_, t, e))       => checkDeclOrder(t, seen) || checkDeclOrder(e, seen)
            case EntGen(GenFor(_, _, _, b))   => checkDeclOrder(b, seen)
            case EntGen(GenRange(_, _, _, b)) => checkDeclOrder(b, seen)
            case GenIf(_, t, e)               => checkDeclOrder(t, seen) || checkDeclOrder(e, seen)
            case GenFor(_, _, _, b)           => checkDeclOrder(b, seen)
            case GenRange(_, _, _, b)         => checkDeclOrder(b, seen)
            case _: GenDefn                   => seen
            case _                            => unreachable
          }
          checkDeclOrder(trees.tail, newSeen)
        }
      }

      checkDeclOrder(entity.body, Seen())

      if (variantStack.top != "verbatim" &&
          entity.verbatims.nonEmpty &&
          entity.instances.isEmpty &&
          entity.connects.isEmpty &&
          entity.functions.isEmpty &&
          entity.combProcesses.isEmpty &&
          entity.entities.isEmpty) {
        cc.warning(
          ident,
          s"Entity '${ident.name}' contains only verbatim blocks, use a 'verbatim entity' instead")
      }

      TreeCopier(entity)(ident, newBody)
    } tap { _ =>
      variantStack.pop()
      paramConstAllowed.pop()
    }

    case decl @ DeclIdent(_, kind, _) if !paramConstAllowed.top && (kind.isParam || kind.isConst) =>
      val hint = if (kind.isParam) "param" else "const"
      cc.error(decl, s"'$hint' declaration cannot appear inside 'gen' construct")
      decl

    case decl @ DeclIdent(_, kind, _)
        if unitType(kind).isNum && !kind.isConst && !kind.isParam && !kind.isGen => {
      val s = if (unitType(kind).isSigned) "int" else "uint"
      cc.error(s"Only compile time constant scalars can be declared with type '${s}'")
      decl
    }

    case decl @ DeclIdent(Ident(name), TypeOut(kind, fc, st), None) => {

      val newSt = (fc, st) match {
        case (FlowControlTypeNone, _: StorageTypeSlices) => {
          cc.error(tree,
                   s"Output port '${name}' without flow control specifier cannot use output slices")
          StorageTypeReg
        }
        case (FlowControlTypeValid, _: StorageTypeSlices) => {
          cc.error(
            tree,
            s"Output port '${name}' with 'sync' flow control specifier cannot use output slices")
          StorageTypeReg
        }
        case _ => st
      }

      if (newSt eq st) {
        tree
      } else {
        decl.copy(kind = TypeOut(kind, fc, newSt)) withLoc tree.loc
      }
    }

    case decl @ DeclIdent(_, kind, Some(init)) => {
      val hintOpt = kind match {
        case _: TypeIn => Some("Input port declarations")
        case TypeOut(_, FlowControlTypeNone, StorageTypeWire) => {
          Some("Output port with 'wire' storage specifier")
        }
        case TypeOut(_, FlowControlTypeValid, _) => {
          Some("Output port declarations with 'sync' flow control")
        }
        case TypeOut(_, FlowControlTypeReady, _) => {
          Some("Output port declarations with 'sync ready' flow control")
        }
        case TypeOut(_, FlowControlTypeAccept, _) => {
          Some("Output port declarations with 'sync accept' flow control")
        }
        case _: TypePipeline => Some("Pipeline variable declarations")
        case _: TypeArray    => Some("Array declarations")
        case _: TypeSram     => Some("SRAM declarations")
        case _               => None
      }

      hintOpt map { hint =>
        cc.error(init, s"${hint} cannot have an initializer")
        decl.copy(init = None) withLoc decl.loc
      } getOrElse decl
    }

    case decl @ DeclIdent(_, kind, None) => {
      val hintOpt = kind match {
        case _: TypeConst => Some("Constant")
        case _            => None
      }

      hintOpt map { hint =>
        cc.error(decl, s"${hint} declarations must have an initializer")
        val num = ExprNum(true, 0) withLoc decl.loc
        decl.copy(init = Some(num)) withLoc decl.loc
      } getOrElse decl
    }

    case gen: Gen => {
      gen match {
        case GenFor(inits, cond, step, _) =>
          if (cond.isEmpty) cc.error(tree, "'gen for' must have a termination condition")
          if (step.isEmpty) cc.error(tree, "'gen for' must have at least one step statement")
          if (inits.isEmpty) {
            cc.error(tree, "'gen for' must have at least one declaration initializer")
          } else if (inits exists { !_.isInstanceOf[StmtDecl] }) {
            cc.error(tree, "'gen for' must use only declaration initializers")
          }
          tree
        case _ => tree
      }
    } tap { _ =>
      paramConstAllowed.pop()
    }

    case StmtRead() => {
      if (variantStack.lengthIs <= 1) {
        cc.error(tree, "Read statements are only allowed inside nested entities")
        StmtError() withLoc tree.loc
      } else {
        tree
      }
    }

    case StmtWrite() => {
      if (variantStack.lengthIs <= 1) {
        cc.error(tree, "Write statements are only allowed inside nested entities")
        StmtError() withLoc tree.loc
      } else {
        tree
      }
    }

    case StmtDecl(decl: DeclIdent) => {
      def msg(kind: Type) = kind match {
        case _: TypeIn       => Some("Input ports must be declared in the enclosing entity")
        case _: TypeOut      => Some("Output ports must be declared in the enclosing entity")
        case _: TypeParam    => Some("Parameters must be declared in the enclosing entity")
        case _: TypeConst    => Some("Constants must be declared in the enclosing entity")
        case _: TypeArray    => Some("Arrays must be declared in the enclosing entity")
        case _: TypePipeline => Some("Pipeline variables must be declared in the outer entity")
        case _: TypeSram     => Some("SRAMs must be declared in the enclosing entity")
        case _               => None
      }

      msg(decl.kind) map { hint =>
        cc.error(tree, "Only variables can be declared in declaration statements", hint)
        StmtError() withLoc tree.loc
      } getOrElse tree
    }

    case StmtCase(_, cases) => {
      val defaults = cases collect { case c: CaseDefault => c }
      if (defaults.lengthIs <= 1) {
        tree
      } else {
        defaults foreach {
          cc.error(_, "Multiple 'default' clauses specified in case statement")
        }
        StmtError() withLoc tree.loc
      }
    }

    case StmtAssign(lhs, _) => {
      if (lhs.isLValueExpr) {
        tree
      } else {
        cc.error(lhs, "Invalid expression on left hand side of '='")
        StmtError() withLoc tree.loc
      }
    }

    case StmtUpdate(lhs, op, _) => {
      if (lhs.isLValueExpr) {
        tree
      } else {
        cc.error(lhs, s"Invalid expression on left hand side of '${op}='")
        StmtError() withLoc tree.loc
      }
    }

    case StmtPost(lhs, op) => {
      if (lhs.isLValueExpr) {
        tree
      } else {
        cc.error(lhs, s"Invalid expression on left hand side of '${op}'")
        StmtError() withLoc tree.loc
      }
    }

    case connect @ EntConnect(lhs, rhss) => {
      val hint = "Only identifiers, optionally followed by a single field selector are allowed"

      val newLhs = if (lhs.isPortRefExpr) {
        lhs
      } else {
        cc.error(lhs, s"Invalid port reference on left hand side of '->'", hint)
        ExprError() withLoc lhs.loc
      }

      val (goodRhss, badRhss) = rhss partition { _.isPortRefExpr }

      badRhss foreach {
        cc.error(_, s"Invalid port reference on right hand side of '->'", hint)
      }

      val newRhss = if (badRhss.isEmpty) rhss else goodRhss

      TreeCopier(connect)(newLhs, newRhss)
    }

    case ExprCat(List(_)) => {
      cc.warning(tree, "Single expression concatenation")
      tree
    }

    case StmtBreak() if loopLevel == 0 => {
      cc.error(tree, "Break statements are only allowed inside looping statements")
      StmtError() withLoc tree.loc
    }

    case StmtContinue() if loopLevel == 0 => {
      cc.error(tree, "Continue statements are only allowed inside looping statements")
      StmtError() withLoc tree.loc
    }

    case _: StmtLoop | _: StmtWhile | _: StmtFor | _: StmtDo => {
      tree
    } tap { _ =>
      loopLevel -= 1
    }

    case _ => tree
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(variantStack.isEmpty)
    assert(paramConstAllowed.sizeIs == 1)
    assert(loopLevel == 0)
  }

  @tailrec
  private def unitType(kind: Type): Type = kind.underlying match {
    case TypeArray(ekind, _)   => unitType(ekind)
    case TypeVector(ekind, _)  => unitType(ekind)
    case TypeSram(ekind, _, _) => unitType(ekind)
    case TypeStack(ekind, _)   => unitType(ekind)
    case other                 => other
  }

}

object Checker extends TreeTransformerPass {
  val name = "checker"
  def create(implicit cc: CompilerContext) = new Checker
}
