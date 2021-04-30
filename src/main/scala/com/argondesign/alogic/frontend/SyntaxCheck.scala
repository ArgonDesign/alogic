////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  The parser grammar is deliberately permissive and accepts a lot of
//  syntactic constructs that are not part of the language (context sensitive
//  constructs in particular). SyntaxCheck implements additional checks and
//  rejects input that could be parsed but is nevertheless invalid based on
//  syntactic structure only. Allowing the parser grammar to accept a larger
//  set of input than necessary has two benefits. Firstly, it keeps the parser
//  grammar smaller and easier to comprehend. Secondly, it allows us to emit
//  more intelligible error messages in SyntaxCheck as the analysis is
//  decoupled from the auto generated parser implementation. SyntaxCheck is
//  invoked twice, before and after elaboration, the second invocation ensures
//  that 'gen' constructs all expanded into valid trees as well.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeReady
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeValid
import com.argondesign.alogic.core.FuncVariant
import com.argondesign.alogic.core.StorageTypes.StorageTypeDefault
import com.argondesign.alogic.core.StorageTypes.StorageTypeSlices
import com.argondesign.alogic.core.StorageTypes.StorageTypeWire
import com.argondesign.alogic.core.enums.EntityVariant
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.Messages.Message
import com.argondesign.alogic.util.unreachable

import scala.annotation.tailrec
import scala.collection.mutable

final class SyntaxCheck extends StatelessTreeTransformer {

  override val typed: Boolean = false

  private val mb = new MessageBuffer

  def messages: Seq[Message] = mb.messages

  private val variantStack = mutable.Stack[EntityVariant.Type]()

  private val singletonStack = mutable.Stack[Boolean]()

  private var loopLevel: Int = 0

  override def enter(tree: Tree): Option[Tree] = {
    tree match {
      case DescEntity(ref: Ref, _, variant, body) =>
        variantStack push variant
        singletonStack push false
        checkEntityBody(ref, variant, body)
      case DescSingleton(ref: Ref, _, variant, body) =>
        variantStack push variant
        singletonStack push true
        checkEntityBody(ref, variant, body)
      case _: StmtLoop | _: StmtWhile | _: StmtFor | _: StmtDo =>
        loopLevel += 1
      case _ =>
    }
    None
  }

  private def entErr(node: Tree, content: String): Unit = {
    val variant = variantStack.top match {
      case EntityVariant.Fsm => "fsm"
      case EntityVariant.Net => "network"
      case EntityVariant.Ver => "verbatim entity"
      case _                 => unreachable
    }
    mb.error(node, s"'$variant' cannot contain $content")
  }

  private def recErr(node: Tree, content: String): Unit =
    mb.error(node, s"'struct' cannot contain $content")

  private def notVariant(variant: EntityVariant.Type): Boolean =
    variantStack.nonEmpty && variantStack.top != variant

  private def checkEntityBody(ref: Ref, variant: EntityVariant.Type, body: List[Ent]): Unit = {
    val combProcesses = body collect { case node: EntCombProcess => node }

    if (combProcesses.lengthIs > 1) {
      combProcesses foreach {
        mb.error(_, s"Multiple fence blocks specified in entity")
      }
    }

    val verbatims = body collect { case node: EntVerbatim => node }

    val nonports = body filter {
      case EntSplice(_: DescIn)        => false
      case EntSplice(_: DescOut)       => false
      case EntSplice(_: DescParam)     => false
      case EntSplice(_: DescParamType) => false
      case EntSplice(_: DescConst)     => false
      case _                           => true
    }

    if (variant != EntityVariant.Ver && verbatims.nonEmpty && verbatims.length == nonports.length) {
      mb.warning(
        ref,
        s"Entity contains only verbatim blocks, use a 'verbatim entity' instead"
      )
    }
  }

  @tailrec
  private def checkValidUsingSubject(tree: Tree, hint: String, expr: Expr): Unit = expr match {
    case _: ExprIdent | _: ExprSym => // Ok
    case ExprDot(expr, _, _)       => checkValidUsingSubject(tree, hint, expr)
    case ExprSel(expr, _)          => checkValidUsingSubject(tree, hint, expr)
    case ExprCall(expr, _)         => checkValidUsingSubject(tree, hint, expr)
    case _                         => mb.error(tree, s"Subject of '$hint' directive is invalid.")
  }

  override def transform(tree: Tree): Tree = tree tap {
    case UsingOne(expr, identOpt) =>
      checkValidUsingSubject(expr, "using", expr)
      if (identOpt.isEmpty) {
        expr match {
          case _: ExprIdent => mb.error(tree, "Redundant 'using' directive")
          case _: ExprCall  => mb.error(tree, "'using' requires 'as' clause")
          case _            =>
        }
      }

    case UsingAll(expr, _) =>
      checkValidUsingSubject(expr, "using", expr)

    case FromOne(_, expr, identOpt) =>
      checkValidUsingSubject(expr, "from", expr)
      if (identOpt.isEmpty) {
        expr match {
          case _: ExprCall => mb.error(tree, "'from' requires 'as' clause")
          case _           =>
        }
      }

    case PkgCompile(expr, identOpt) =>
      checkValidUsingSubject(expr, "compile", expr)
      if (identOpt.isEmpty) {
        expr match {
          case _: ExprCall => mb.error(tree, "'compile' requires 'as' clause")
          case _           =>
        }
      }

    case PkgSplice(desc: Desc) =>
      def err(hint: String): Unit = mb.error(desc, s"$hint definition cannot appear in file scope")
      desc match {
        case _: DescVar | _: DescStatic | _: DescPipeVar => err("Variable")
        case _: DescIn | _: DescOut                      => err("Port")
        case _: DescArray                                => err("Distributed memory")
        case _: DescSram                                 => err("SRAM")
        case _: DescInstance                             => err("Instance")
        case _: DescSingleton                            => err("Singleton entity")
        case _: DescVal                                  => unreachable
        case _                                           =>
      }

    case PkgSplice(a: Assertion) =>
      a match {
        case _: AssertionStatic => // OK
        case _                  => mb.error(a, "Only static assertions are allowed in file scope")
      }

    case node: EntConnect =>
      if (notVariant(EntityVariant.Net)) {
        entErr(node, "connections")
      }
      node.rhs.foreach { rhs =>
        if (!rhs.isLValueExpr) {
          mb.error(rhs, "Right hand side of '->' is not a valid assignment target")
        }
      }

    case node: EntCombProcess if notVariant(EntityVariant.Fsm) => entErr(node, "fence blocks")

    case EntSplice(_: DescParam) if singletonStack.top =>
      mb.error(tree, "Singleton entity cannot have parameters. Use a 'const' definition instead.")

    case EntSplice(_: DescParamType) if singletonStack.top =>
      mb.error(tree, "Singleton entity cannot have type parameters. Use a 'typedef' instead.")

    case EntSplice(desc: Desc) =>
      variantStack.top match {
        case EntityVariant.Fsm =>
          desc match {
            case _: DescPipeVar             => entErr(desc, "pipeline variable definitions")
            case _: DescEntity              => entErr(desc, "nested entities")
            case d: DescInstance if !d.bind => entErr(desc, "instantiations")
            case _: DescSingleton           => entErr(desc, "singleton entities")
            case _                          =>
          }
        case EntityVariant.Net =>
          desc match {
            case _: DescVar | _: DescVal | _: DescStatic => entErr(desc, "variable definitions")
            case _: DescArray                            => entErr(desc, "distributed memory definitions")
            case _: DescSram                             => entErr(desc, "SRAM definitions")
            case DescFunc(_, _, funcVariant, _, _, _) if funcVariant != FuncVariant.Xeno =>
              entErr(desc, "function definitions")
            case _ =>
          }
        case EntityVariant.Ver =>
          desc match {
            case _: DescVar | _: DescVal | _: DescStatic => entErr(desc, "variable definitions")
            case _: DescPipeVar                          => entErr(desc, "pipeline variable definitions")
            case _: DescArray                            => entErr(desc, "distributed memory definitions")
            case DescSram(_, _, _, _, st) if st != StorageTypeWire =>
              entErr(desc, "registered SRAM definitions")
            case _: DescEntity    => entErr(desc, "nested entities")
            case _: DescInstance  => entErr(desc, "instantiations")
            case _: DescSingleton => entErr(desc, "singleton entities")
            case _: DescFunc      => entErr(desc, "function definitions")
            case _                =>
          }
        case _ => unreachable
      }

    case EntSplice(assertion: Assertion) =>
      assertion match {
        case _: AssertionStatic => // OK
        case _                  => entErr(assertion, "non-static assertions")
      }

    case _: DescEntity =>
      variantStack.pop()
      singletonStack.pop()

    case _: DescSingleton =>
      variantStack.pop()
      singletonStack.pop()

    case RecSplice(desc: Desc) =>
      desc match {
        case _: DescIn        => recErr(desc, "port definitions")
        case _: DescOut       => recErr(desc, "port definitions")
        case _: DescPipeVar   => recErr(desc, "pipeline variable definitions")
        case _: DescArray     => recErr(desc, "distributed memory definitions")
        case _: DescSram      => recErr(desc, "SRAM definitions")
        case _: DescEntity    => recErr(desc, "entity definitions")
        case _: DescSingleton => recErr(desc, "entity definitions")
        case _: DescInstance  => recErr(desc, "instance definitions")
        case _                =>
      }

    case DescOut(_, _, _, fc, st, None) =>
      (fc, st) match {
        case (FlowControlTypeNone, _: StorageTypeSlices) =>
          mb.error(tree, s"Output port without flow control cannot use output slices")
        case (FlowControlTypeValid, _: StorageTypeSlices) =>
          mb.error(tree, s"Output port with 'sync' flow control cannot use output slices")
        case _ =>
      }
      if (variantStack.headOption.contains(EntityVariant.Ver) && st != StorageTypeDefault) {
        mb.error(tree, "'verbatim entity' output ports cannot use a storage specifier")
      }

    case DescOut(_, _, _, fc, st, Some(init)) =>
      (fc, st) pipe {
        case (FlowControlTypeNone, StorageTypeWire) => Some("'wire' storage specifier")
        case (FlowControlTypeValid, _)              => Some("'sync' flow control")
        case (FlowControlTypeReady, _)              => Some("'sync ready' flow control")
        case _                                      => None
      } foreach { hint =>
        mb.error(init, s"Output port with $hint cannot have an initializer")
      }

    case _: DescPipeIn | _: DescPipeOut if variantStack.lengthIs <= 1 =>
      mb.error(tree, "Pipeline ports are only allowed inside nested entities")

    case StmtSplice(desc: Desc) =>
      desc match {
        case _: DescVar      =>
        case _: DescVal      =>
        case _: DescStatic   =>
        case _: DescGenIf    =>
        case _: DescGenFor   =>
        case _: DescGenRange =>
        case _: DescGenVar   => // After Elaboration
        case _: DescFunc     =>
        case _ =>
          mb.error(tree, "Only variables can be defined in statement position")
      }

    case StmtCase(_, cases) =>
      cases.collect { case c: CaseDefault => c } match {
        case Nil => // OK
        case default :: Nil =>
          if (default ne cases.last) {
            mb.error(default, "'default' clause must come last in a case statement")
          }
        case defaults =>
          defaults foreach {
            mb.error(_, "Multiple 'default' clauses specified in case statement")
          }
      }

    case StmtAssign(lhs, _) =>
      if (!lhs.isLValueExpr) {
        mb.error(lhs, "Left hand side of '=' is not a valid assignment target")
      }

    case StmtUpdate(lhs, op, _) if !lhs.isLValueExpr =>
      mb.error(lhs, s"Left hand side of '$op=' is not a valid assignment target")

    case StmtPost(lhs, op) if !lhs.isLValueExpr =>
      mb.error(lhs, s"Operand of '$op' is not a valid assignment target")

    case StmtGoto(expr) =>
      expr match {
        case _: ExprCall =>
        case _ =>
          mb.error(expr, s"Target of 'goto' statement must be a function call expression")
      }

    case ExprCat(List(_)) =>
      mb.warning(tree, "Single expression concatenation")

    case _: StmtBreak if loopLevel == 0 =>
      mb.error(tree, "Break statements are only allowed inside looping statements")

    case _: StmtContinue if loopLevel == 0 =>
      mb.error(tree, "Continue statements are only allowed inside looping statements")

    case _: StmtLoop | _: StmtWhile | _: StmtFor | _: StmtDo =>
      loopLevel -= 1

    case _ =>
  }

  override def finalCheck(tree: Tree): Unit = {
    assert(variantStack.isEmpty)
    assert(singletonStack.isEmpty)
    assert(loopLevel == 0)
  }

}

object SyntaxCheck {

  def apply(tree: Tree): Seq[Message] = {
    val checker = new SyntaxCheck
    checker(tree)
    checker.messages
  }

}
