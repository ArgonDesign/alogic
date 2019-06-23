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
// The Typer:
// - Type checks all tree nodes
// - Assigns types to all nodes
// The typer is special and will not rewrite any of the trees, only check and
// assign types. The rest of the compiler (and the typer itself) relies on
// this property.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.typer

import com.argondesign.alogic.analysis.WrittenRefs
import com.argondesign.alogic.ast.TreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.TreeInTypeTransformer
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Math.clog2
import com.argondesign.alogic.lib.Stack
import com.argondesign.alogic.lib.TreeLike
import com.argondesign.alogic.passes._
import com.argondesign.alogic.util.FollowedBy
import com.argondesign.alogic.util.unreachable

import scala.language.implicitConversions

class BoolHelpers(val value: Boolean) extends AnyVal {
  // Non short-circuiting &&
  def &&&(other: Boolean): Boolean = value && other
  // Non short-circuiting |||
  def |||(other: Boolean): Boolean = value || other
  // Run f if value is false, yield the value
  def ifFalse(f: => Unit): Boolean = {
    if (!value) f
    value
  }
}

final class Typer(externalRefs: Boolean = false)(implicit cc: CompilerContext)
    extends TreeTransformer
    with FollowedBy {

  override val typed: Boolean = false

  private final val mixedWidthBinaryOps = Set("<<", ">>", "<<<", ">>>", "&&", "||")
  private final val comparisonBinaryOps = Set(">", ">=", "<", "<=", "==", "!=")

  private def hasError(node: TreeLike): Boolean = node.children exists {
    case child: Tree => child.hasTpe && child.tpe.isError
    case child: Type => child.children exists hasError
    case _           => unreachable
  }

  implicit def boolean2BoolHelper(value: Boolean) = new BoolHelpers(value)

  private var hadError = false

  private def error(tree: Tree, mark: Tree, msg: String*): Unit = {
    if (msg.nonEmpty) {
      cc.error(tree, msg: _*)
    }
    if (tree.hasTpe) {
      assert(tree.tpe.isError)
    } else {
      tree withTpe TypeError
    }
    hadError = true
  }

  private def error(tree: Tree, msg: String*): Unit = error(tree, tree, msg: _*)

  private def checkPacked(expr: Expr, msg: String): Boolean = {
    expr.tpe.isPacked ifFalse {
      cc.error(expr, s"${msg} is of non-packed type")
    }
  }

  private def checkNumeric(expr: Expr, msg: String): Boolean = {
    expr.tpe.underlying.isNumeric ifFalse {
      cc.error(expr, s"${msg} is of non-numeric type")
    }
  }

  private def checkNumericOrPacked(expr: Expr, msg: String): Boolean = {
    (expr.tpe.isNumeric || expr.tpe.isPacked) ifFalse {
      cc.error(expr, s"${msg} is of neither numeric nor packed type")
    }
  }

  private def checkWidth(width: Int, expr: Expr, msg: String): Boolean = {
    val exprWidth = expr.tpe.width
    (exprWidth == width) ifFalse {
      cc.error(expr, s"${msg} yields ${exprWidth} bits, ${width} bits are expected")
    }
  }

  private def checkSign(sign: Boolean, expr: Expr, msg: String): Boolean = {
    (expr.tpe.isSigned == sign) ifFalse {
      cc.error(expr, s"${msg} must be ${if (sign) "signed" else "unsigned"}")
    }
  }

  private def checkBlock(stmts: List[Stmt]): Boolean = {
    val hasCtrl = stmts exists { _.tpe == TypeCtrlStmt }
    val lstCtrl = stmts.nonEmpty && stmts.last.tpe == TypeCtrlStmt
    (!hasCtrl || lstCtrl) ifFalse {
      cc.error(stmts.last,
               "Block must contain only combinatorial statements, or end with a control statement")
    }
  }

  private def checkModifiable(expr: Expr): Boolean = {
    WrittenRefs(expr) map {
      case ref @ ExprRef(symbol) =>
        symbol.kind match {
          case _: TypeParam => cc.error(ref, "Parameter cannot be modified"); false
          case _: TypeConst => cc.error(ref, "Constant cannot be modified"); false
          case _: TypeIn    => cc.error(ref, "Input port cannot be modified"); false
          case _: TypeArray => cc.error(ref, "Memory can only be modified using .write()"); false
          case TypeOut(_, fct, _) if fct != FlowControlTypeNone =>
            cc.error(ref, "Output port with flow control can only be modified using .write()")
            false
          case _ => true
        }
    } forall identity
  }

  private def checkNameHidingByExtensionType(decl: Decl): Unit = {
    decl.symbol.kind match {
      case eKind: ExtensionType => {
        eKind.kind match {
          case cKind: CompoundType => {
            for {
              (field, nKind) <- eKind.extensions
              pKind <- cKind(field)
            } {
              cc.error(
                decl.loc,
                s"Field '$field' of type '${pKind.toSource}' of symbol '${decl.symbol.name}'",
                s"defined in type '${cKind.toSource}'",
                s"is hidden by extension field of the same name of type '${nKind.toSource}'",
                s"defined by extension type '${eKind.toSource}'"
              )
            }
          }
          case _ => ()
        }
      }
      case _ => ()
    }
  }

  private def checkIndex(expectedWidth: Int,
                         idx: Expr,
                         checkConst: Boolean,
                         msg: String): Boolean = {
    idx.tpe.isNum || {
      checkPacked(idx, msg) && {
        checkWidth(expectedWidth, idx, msg) &&&
          checkSign(false, idx, msg) &&&
          (!checkConst || checkKnownConst(idx, msg))
      }
    }
  }

  private def checkKnownConst(expr: Expr, msg: String): Boolean = {
    expr.isKnownConst ifFalse {
      cc.error(expr, s"$msg must be a constant expression")
    }
  }

  private val contextKind = Stack[Type]()

  private val contextNode = Stack[Int]()

  private def pushContextWidth(node: Tree, kind: Type) = {
    contextNode push node.id
    contextKind push kind.underlying
  }

  private[this] object TypeTyper extends TreeInTypeTransformer(this) {
    override def transform(kind: Type): Type = {
      val result = super.transform(kind)
      if (hasError(result)) TypeError else result
    }
  }

  override def skip(tree: Tree): Boolean = tree match {
    case _: Entity  => false
    case _: Connect => !externalRefs
    case _          => tree.hasTpe
  }

  override def enter(tree: Tree): Unit = tree match {
    case EntityNamed(symbol, _, _, _, _, _, _, _, _) => {
      // Type the source attributes
      // TODO: do them all in a systematic way..
      symbol.attr.stackLimit.get foreach { symbol.attr.stackLimit set walk(_).asInstanceOf[Expr] }
    }

    case Function(Sym(symbol), _) => {
      // Type the source attributes
      // TODO: do them all in a systematic way..
      symbol.attr.recLimit.get foreach { symbol.attr.recLimit set walk(_).asInstanceOf[Expr] }
    }

    case decl @ Decl(symbol, initOpt) => {
      // Type check trees in the type of the symbol
      val origKind = symbol.kind
      val kind = origKind rewrite TypeTyper
      if (kind ne origKind) {
        symbol.kind = kind
      }

      checkNameHidingByExtensionType(decl)

      if (kind.isPacked && kind.underlying != TypeVoid && kind.width < 1) {
        error(decl, s"'${symbol.name}' is declared with width ${kind.width}")
      }

      // Track unary tick result types
      if (initOpt.isDefined) {
        pushContextWidth(tree, symbol.kind)
      }
    }

    // Type check the lhs up front
    case StmtAssign(lhs, _) if !walk(lhs).tpe.isError => {
      if (!(checkPacked(lhs, "Left hand side of assignment") && checkModifiable(lhs))) {
        error(tree)
      }
      pushContextWidth(tree, lhs.tpe)
    }

    // Type check the lhs up front
    case StmtUpdate(lhs, _, _) if !walk(lhs).tpe.isError => {
      if (!(checkPacked(lhs, "Left hand side of assignment") && checkModifiable(lhs))) {
        error(tree)
      }
      pushContextWidth(tree, lhs.tpe)
    }

    // Type check target up front
    case ExprIndex(tgt, _) if !walk(tgt).tpe.isError => {
      if (!tgt.tpe.underlying.isNum) {
        val shapeIter = tgt.tpe.shapeIter
        if (!shapeIter.hasNext && !tgt.tpe.underlying.isNum) {
          error(tree, tgt, "Target is not indexable")
        } else {
          pushContextWidth(tree, TypeUInt(Expr(clog2(shapeIter.next) max 1) regularize tgt.loc))
        }
      }
    }

    // Type check target up front
    case ExprSlice(tgt, _, _, _) if !walk(tgt).tpe.isError => {
      if (!tgt.tpe.underlying.isNum) {
        val shapeIter = tgt.tpe.shapeIter
        if (!shapeIter.hasNext || tgt.tpe.isArray) {
          error(tree, tgt, "Target is not sliceable")
        } else {
          pushContextWidth(tree, TypeUInt(Expr(clog2(shapeIter.next) max 1) regularize tgt.loc))
        }
      }
    }

    // Type check target up front
    case ExprCall(tgt, args) if !walk(tgt).tpe.isError => {
      def process(kinds: List[Type]): Unit = {
        if (kinds.length != args.length) {
          error(tree, s"Function call expects ${kinds.length} arguments, ${args.length} given")
        } else {
          for ((kind, arg) <- (kinds zip args).reverse) {
            pushContextWidth(arg, kind)
          }
        }
      }
      tgt.tpe match {
        case TypeCombFunc(argTypes, _) => process(argTypes)
        case TypeCtrlFunc(argTypes, _) => process(argTypes)
        case _: TypePolyFunc           => ()
        case _                         => error(tree, tgt, s"'${tgt.toSource}' is not callable")
      }
    }

    case _ => ()
  }

  override def transform(tree: Tree): Tree = {
    // TODO: reduction of 1 bit value is error
    // TODO: Warn for non power of 2 dimensions

    val alreadyHadError = hadError

    tree match {
      ////////////////////////////////////////////////////////////////////////////
      // Don't type check already typed nodes
      ////////////////////////////////////////////////////////////////////////////

      case _ if tree.hasTpe => ()

      ////////////////////////////////////////////////////////////////////////////
      // Propagate type errors
      ////////////////////////////////////////////////////////////////////////////

      case node if hasError(node) => error(tree)

      ////////////////////////////////////////////////////////////////////////////
      // Type check other nodes
      ////////////////////////////////////////////////////////////////////////////

      case entity: EntityNamed => {
        entity.fenceStmts filter { _.tpe == TypeCtrlStmt } foreach {
          error(tree, _, "'fence' block must contain only combinatorial statements")
        }
      }

      case Function(ref, body) => {
        if (body.isEmpty) {
          error(tree, ref, "Body of function must end in a control statement")
        } else if (body.last.tpe != TypeCtrlStmt) {
          error(tree, body.last, "Body of function must end in a control statement")
        }
      }

      case conn: Connect => ConnectChecks(conn)

      case Decl(symbol, Some(init)) => {
        if (symbol.kind.underlying.isNum && init.tpe.isPacked) {
          if (symbol.kind.isParam) {
            error(tree, init, "Unsized integer parameter assigned a packed value")
          } else {
            error(tree, "Unsized integer declaration has packed initializer")
          }
        } else if (!symbol.kind.underlying.isNum && !init.tpe.isNum) {
          val msg = if (symbol.kind.isParam) "Parameter value" else "Initializer expression"
          if (!(checkPacked(init, msg) && checkWidth(symbol.kind.width, init, msg))) {
            error(tree)
          }
        } else if ((symbol.kind.isConst || symbol.kind.isParam) && !init.isKnownConst) {
          val what = if (symbol.kind.isConst) "cons" else "param"
          error(tree, init, s"Initializer of '${what}' declaration must be compile time constant")
        }

        // If there were no errors, attach initializer expression attribute
        if (!tree.hasTpe) {
          symbol.attr.init set {
            if (symbol.kind.isPacked && init.tpe.isNum) {
              val kind = TypeInt(init.tpe.isSigned, Expr(symbol.kind.width) regularize init.loc)
              (init cast kind).simplify
            } else {
              init.simplify
            }
          }
        }
      }

      ////////////////////////////////////////////////////////////////////////////
      // Type check statements
      ////////////////////////////////////////////////////////////////////////////

      case StmtBlock(body) if !checkBlock(body) => error(tree)

      case StmtIf(_, thenStmt, Some(elseStmt)) if thenStmt.tpe != elseStmt.tpe =>
        error(tree, "Either both or neither branches of if-else must be control statements")

      case StmtCase(_, cases) => {
        val allCtrl = cases forall { _.stmt.tpe == TypeCtrlStmt }
        val allComb = cases forall { _.stmt.tpe == TypeCombStmt }
        if (!allComb && !allCtrl) {
          error(tree, "Either all or no cases of a case statement must be control statements")
        }
      }

      case StmtLoop(body) =>
        if (body.isEmpty) {
          error(tree, "Body of 'loop' must be a control statement")
        } else if (body.last.tpe != TypeCtrlStmt) {
          error(tree, "Body of 'loop' must end in a control statement")
        }

      case StmtAssign(lhs, rhs) if !rhs.tpe.isNum => {
        // lhs have already been checked in enter
        val ok = checkPacked(rhs, "Right hand side of assignment") &&
          checkWidth(lhs.tpe.width, rhs, "Right hand side of assignment")
        if (!ok) error(tree)
      }

      case StmtUpdate(lhs, _, rhs) if !rhs.tpe.isNum => {
        // lhs have already been checked in enter
        val ok = checkPacked(rhs, "Right hand side of assignment") &&
          checkWidth(lhs.tpe.width, rhs, "Right hand side of assignment")
        if (!ok) error(tree)
      }

      case StmtPost(expr, op) => {
        if (!(checkPacked(expr, s"Target of postfix '${op}'") && checkModifiable(expr))) {
          error(tree)
        }
      }

      case StmtExpr(expr) if !expr.tpe.isVoid =>
        expr match {
          case ExprCall(_: ExprSelect, _) => ()
          case _                          => error(tree, "A pure expression in statement position does nothing")
        }

      ////////////////////////////////////////////////////////////////////////////
      // Type check expressions
      ////////////////////////////////////////////////////////////////////////////

      case ExprSelect(expr, sel) => {
        val field = expr.tpe match {
          case TypeType(kind: CompoundType) => kind(sel)
          case kind: CompoundType           => kind(sel)
          case _                            => None
        }

        if (field.isEmpty) {
          val what = if (expr.tpe.isInstance) "port" else "field"
          error(tree, s"No $what named '$sel' in '${expr.toSource}' of type '${expr.tpe.toSource}'")
        }
      }

      case ExprCall(expr, args) => {
        def check(kinds: List[Type]) =
          for (((kind, arg), index) <- (kinds zip args).zipWithIndex) {
            val i = index + 1
            if (kind.isType) {
              if (!arg.tpe.isType) error(tree, arg, s"Argument $i expects a type")
            } else if (kind.isNum) {
              if (!arg.tpe.isNum) error(tree, arg, s"Argument $i expects an unsized value")
            } else if (!arg.tpe.isNum) {
              val ok = checkPacked(arg, s"Argument $i of function call") &&
                checkWidth(kind.width, arg, s"Argument $i of function call")
              if (!ok) error(tree)
            }
          }

        expr.tpe match {
          case TypeCombFunc(argTypes, _) => check(argTypes)
          case TypeCtrlFunc(argTypes, _) => check(argTypes)
          case tpe: TypePolyFunc =>
            tpe.resolve(args) match {
              case Some(symbol) => tree withTpe symbol.kind.asInstanceOf[TypeCombFunc].retType
              case None =>
                val msg = s"Builtin function '${expr.toSource}' cannot be applied to arguments" :: {
                  args map { arg =>
                    s"'${arg.toSource}' of type ${arg.tpe.toSource}"
                  }
                }
                error(tree, msg: _*)
            }
          case _ => unreachable // Handled in enter
        }
      }

      case ExprCat(parts) =>
        for ((part, i) <- parts.zipWithIndex) {
          if (!checkPacked(part, s"Part ${i + 1} of bit concatenation")) {
            error(tree)
          }
        }

      case ExprRep(count, expr) => {
        val ok = checkNumeric(count, "Count of bit repetition") && {
          checkKnownConst(count, "Count of bit repetition") &&&
            checkPacked(expr, "Value of bit repetition")
        }
        if (!ok) error(tree)
      }

      case ExprIndex(tgt, idx) =>
        if (tgt.tpe.underlying.isNum) {
          if (!checkKnownConst(idx, "Index of unsized integer value")) {
            error(tree)
          }
        } else {
          val ok = (tgt.tpe.isArray || checkPacked(tgt, "Target of index")) &&
            checkIndex(contextKind.top.width, idx, false, "Index")
          if (!ok) error(tree)
        }

      case ExprSlice(tgt, lidx, op, ridx) =>
        if (tgt.tpe.underlying.isNum) {
          val ok = checkKnownConst(lidx, "Left index of unsized integer value") &&&
            checkKnownConst(ridx, "Right index of unsized integer value")
          if (!ok) error(tree)
        } else {
          val ok = checkPacked(tgt, "Target of slice") && {
            checkIndex(contextKind.top.width, lidx, op == ":", "Left index") &&&
              checkIndex(contextKind.top.width, ridx, true, "Right index")
          }
          if (!ok) error(tree)
        }

      // Unary ops

      // Unary ' is special ant the type of the node must be assigned here
      // based on context
      case ExprUnary("'", op) =>
        if (hadError) {
          // If we had a type error, the context stack might be out of sync
          // so don't type check any further unary tick nodes
          error(tree)
        } else if (contextKind.isEmpty) {
          error(tree, "Unary ' operator used in invalid context")
        } else if (checkPacked(op, "Operand of unary ' operator")) {
          if (contextKind.top.isNum) {
            tree withTpe TypeNum(op.tpe.isSigned)
          } else {
            val resWidth = contextKind.top.width
            val opWidth = op.tpe.width
            if (resWidth < opWidth) {
              error(tree, s"Unary ' causes narrowing of width from $opWidth to $resWidth")
            } else {
              tree withTpe TypeInt(op.tpe.isSigned, Expr(resWidth) regularize tree.loc)
            }
          }
        } else {
          error(tree)
        }

      case ExprUnary(op, expr) =>
        if (expr.tpe.isNum) {
          if ("&|^" contains op) {
            error(tree, s"Unary operator '$op' cannot be applied to unsized integer value")
          }
        } else if (!checkPacked(expr, s"Operand of unary operator '$op'")) {
          error(tree)
        }

      // Binary ops
      case ExprBinary(lhs, op, rhs) => {
        if (!lhs.tpe.isNum || !rhs.tpe.isNum) {
          lazy val strictWidth = !(mixedWidthBinaryOps contains op)
          if (lhs.tpe.isNum && strictWidth) {
            if (!checkPacked(rhs, s"Right hand operand of '$op'")) {
              error(tree)
            }
          } else if (rhs.tpe.isNum && strictWidth) {
            if (!checkPacked(lhs, s"Left hand operand of '$op'")) {
              error(tree)
            }
          } else if (strictWidth) {
            if (!checkPacked(lhs, s"Left hand operand of '$op'") |||
                  !checkPacked(rhs, s"Right hand operand of '$op'")) {
              error(tree)
            } else if (lhs.tpe.width != rhs.tpe.width) {
              error(
                tree,
                s"Both operands of binary '$op' must have the same width, but",
                s"left  hand operand is ${lhs.tpe.width} bits wide, and",
                s"right hand operand is ${rhs.tpe.width} bits wide",
              )
            }
          } else {
            if (!checkNumericOrPacked(lhs, s"Left hand operand of '$op'") |||
                  !checkNumericOrPacked(rhs, s"Right hand operand of '$op'")) {
              error(tree)
            }
          }
        }

        if (!tree.hasTpe) {
          if ((op == "<<<" || op == ">>>") && !lhs.tpe.isSigned) {
            cc.warning(lhs, "Arithmetic shift used on unsigned left operand")
          } else if ((op == "<<" || op == ">>") && lhs.tpe.isSigned) {
            cc.warning(lhs, "Logical shift used on signed left operand")
          } else if (comparisonBinaryOps contains op) {
            if (lhs.tpe.isSigned && !rhs.tpe.isSigned) {
              cc.warning(tree, "Comparison between signed and unsigned operands")
            } else if (!lhs.tpe.isSigned && rhs.tpe.isSigned) {
              cc.warning(tree, "Comparison between unsigned and signed operands")
            }
          }
        }
      }

      case ExprTernary(cond, thenExpr, elseExpr) =>
        if (!checkNumericOrPacked(cond, "Condition of '?:'")) {
          error(tree)
        } else if (!thenExpr.tpe.isNum || !elseExpr.tpe.isNum) {
          lazy val okThen = checkPacked(thenExpr, "'then' operand of '?:'")
          lazy val okElse = checkPacked(elseExpr, "'else' operand of '?:'")
          if (thenExpr.tpe.isNum) {
            if (!okElse) error(tree)
          } else if (elseExpr.tpe.isNum) {
            if (!okThen) error(tree)
          } else if (okElse &&& okThen) {
            if (thenExpr.tpe.width != elseExpr.tpe.width) {
              error(
                tree,
                s"'then' and 'else' operands of ternary '?:' must have the same width, but",
                s"'then' operand is ${thenExpr.tpe.width} bits wide, and",
                s"'else' operand is ${elseExpr.tpe.width} bits wide"
              )
            }
          } else {
            error(tree)
          }
        }

      case ExprType(kind) if (kind rewrite TypeTyper).isError => error(tree)

      case ExprCast(kind, _) if (kind rewrite TypeTyper).isError => error(tree)

      //////////////////////////////////////////////////////////////////////////
      // Done
      //////////////////////////////////////////////////////////////////////////

      case _ => ()
    }

    assert(alreadyHadError || !hadError || tree.tpe.isError)

    // Pop context stack if required. This is a loop as some nodes might have
    // been pushed multiple times, e.g.: port.write(array[index]), both the
    // ExprCall and ExprIndex would have pushed the ExprIndex node
    while (contextNode.nonEmpty && contextNode.top == tree.id) {
      contextNode.pop()
      contextKind.pop()
    }

    // There is a race between the Typer running on multiple trees in parallel.
    // The trees have already undergone parameter specialization and therefore
    // might share instances of isomorphic sub-trees. As the type can be
    // assigned only once, we apply synchronization on the tesult node for this
    // step. This is not a problem as all threads would assign the same type.

    tree synchronized {
      // Assign type if have not been assigned by now
      if (tree.hasTpe) tree else TypeAssigner(tree)
    } followedBy {
      if (tree.tpe.isNum && !tree.asInstanceOf[Expr].isKnownConst) {
        cc.error(tree, "Expression of unsized integer type must be compile time constant")
      }

      hadError |= tree.tpe.isError
    }
  }

  override def finalCheck(tree: Tree): Unit = {
    if (!tree.tpe.isError) {
      assert(contextKind.isEmpty, s"${contextKind.toList} ${contextNode.toList}")

      def check(tree: TreeLike) {
        tree visitAll {
          case node: Tree if !node.hasTpe => {
            if (externalRefs) {
              cc.ice(node, "Typer: untyped node remains", node.toString)
            }
          }
          case Decl(symbol, _) => check(symbol.kind)
          case Sym(symbol)     => check(symbol.kind)
//        case ExprRef(symbol) => check(symbol.kind) // TODO: fix this
          case ExprType(kind) => check(kind)
        }
      }

      check(tree)
    }
  }
}

object Typer {
  class TyperPass(externalRefs: Boolean) extends TreeTransformerPass {
    val name = "typer"
    def create(implicit cc: CompilerContext) = new Typer(externalRefs)(cc)
  }

  def apply(externalRefs: Boolean): Pass = {
    new TyperPass(externalRefs)
  }
}
