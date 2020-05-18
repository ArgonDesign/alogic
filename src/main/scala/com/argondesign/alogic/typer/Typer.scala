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

import com.argondesign.alogic.analysis.WrittenSyms
import com.argondesign.alogic.ast.StatefulTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.CompoundType
import com.argondesign.alogic.core.ExtensionType
import com.argondesign.alogic.core.FlowControlTypes.FlowControlTypeNone
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Math.clog2
import com.argondesign.alogic.util.unreachable

import scala.collection.mutable
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

  // Implication
  def implies(other: => Boolean): Boolean = !value || other
}

final class Typer(
    errorForParametrized: Boolean = true
  )(
    implicit
    cc: CompilerContext)
    extends StatefulTreeTransformer {

  override val typed: Boolean = false

  final private val mixedWidthBinaryOps = Set("<<", ">>", "<<<", ">>>", "&&", "||")
  final private val comparisonBinaryOps = Set(">", ">=", "<", "<=", "==", "!=")

  private def hasError(node: Tree): Boolean = node.children exists { child =>
    child.hasTpe && (child.tpe.isError || child.tpe.isParametrized)
  }

  private def hasUnknown(node: Tree): Boolean = node.children exists { child =>
    child.hasTpe && child.tpe.isUnknown
  }

  implicit def boolean2BoolHelper(value: Boolean): BoolHelpers = new BoolHelpers(value)

  private var hadError = false

  // Utility that will issue the compiler error 'msg' at the 'loc' provided,
  // assign TypeError to the 'tree' node and set the 'hadError' flag
  private def error(tree: Tree, loc: Loc, msg: String*): Unit = {
    if (msg.nonEmpty) {
      cc.error(loc, msg: _*)
    }
    if (tree.hasTpe) {
      assert(tree.tpe.isError)
    } else {
      tree withTpe TypeError
    }
    hadError = true
  }

  // Same as above but location provided from the 'mark' node.
  private def error(tree: Tree, mark: Tree, msg: String*): Unit = error(tree, mark.loc, msg: _*)

  // Wrapper for above when 'tree' and 'mark' are the same
  private def error(tree: Tree, msg: String*): Unit = error(tree, tree.loc, msg: _*)

  // The following check* methods all have the following uniform behaviour:
  // - Perform a method specific check
  // - If the check failed:
  //   - Issue an error message
  //   - Mark the implicitly provided tree node as having TypeError
  // - Return check result

  private def checkPacked(expr: Expr, msg: String)(implicit tree: Tree): Boolean = {
    expr.tpe.isPacked ifFalse {
      error(tree, expr, s"$msg is of non-packed type")
    }
  }

  private def checkPackedType(expr: Expr, msg: String)(implicit tree: Tree): Boolean = {
    expr.tpe.asType.kind.isPacked ifFalse {
      error(tree, expr, s"$msg is of non-packed type")
    }
  }

  private def checkNumeric(expr: Expr, msg: String)(implicit tree: Tree): Boolean = {
    expr.tpe.underlying.isNumeric ifFalse {
      error(tree, expr, s"$msg is of non-numeric type")
    }
  }

  private def checkNumericOrPacked(expr: Expr, msg: String)(implicit tree: Tree): Boolean = {
    (expr.tpe.underlying.isNumeric || expr.tpe.isPacked) ifFalse {
      error(tree, expr, s"$msg is of neither numeric nor packed type")
    }
  }

  private def pluralize(value: BigInt, singular: String, plural: String): String = {
    if (value == 1) s"$value $singular" else s"$value $plural"
  }

  private def checkWidth(width: BigInt, expr: Expr, msg: String)(implicit tree: Tree): Boolean = {
    val expected = s"${pluralize(width, "bit is", "bits are")} expected"
    if (expr.tpe.isNum) {
      error(tree, s"$msg yields an unsized value, $expected")
      false
    } else if (expr.tpe.width != width) {
      error(tree, expr, s"$msg yields ${pluralize(expr.tpe.width, "bit", "bits")}, $expected")
      false
    } else {
      true
    }
  }

  private def checkSign(sign: Boolean, expr: Expr, msg: String)(implicit tree: Tree): Boolean = {
    (expr.tpe.isSigned == sign) ifFalse {
      error(tree, expr, s"$msg must be ${if (sign) "signed" else "unsigned"}")
    }
  }

  private def checkKnownConst(expr: Expr, msg: String)(implicit tree: Tree): Boolean = {
    expr.isKnownConst ifFalse {
      error(tree, expr, s"$msg must be a constant expression")
    }
  }

  private def checkPositive(expr: Expr, msg: String)(implicit tree: Tree): Boolean = {
    expr.value match {
      case Some(v) if v > 0 => true
      case Some(v) =>
        error(tree, expr, s"$msg must be positive (not $v)"); false
      case None =>
        error(tree, expr, s"$msg must be a compile time constant"); false
    }
  }

  private def checkBlock(stmts: List[Stmt])(implicit tree: Tree): Boolean = {
    val hasCtrl = stmts exists { _.tpe == TypeCtrlStmt }
    val lstCtrl = stmts.nonEmpty && stmts.last.tpe == TypeCtrlStmt
    (!hasCtrl || lstCtrl) ifFalse {
      error(
        tree,
        stmts.last,
        "Block must contain only combinational statements, or end with a control statement"
      )
    }
  }

  private def checkIndex(
      expectedWidth: BigInt,
      idx: Expr,
      checkConst: Boolean,
      msg: String
    )(
      implicit
      tree: Tree
    ): Boolean = {
    idx.tpe.underlying.isNum || {
      checkPacked(idx, msg) && {
        checkWidth(expectedWidth, idx, msg) &&&
          checkSign(false, idx, msg) &&&
          (!checkConst || checkKnownConst(idx, msg))
      }
    }
  }

  private def checkModifiable(expr: Expr)(implicit tree: Tree): Boolean = {
    WrittenSyms(expr) map {
      case ref @ ExprSym(symbol) =>
        symbol.kind match {
          case _: TypeParam => error(tree, ref, "Parameter cannot be modified"); false
          case _: TypeConst => error(tree, ref, "Constant cannot be modified"); false
          case _: TypeIn    => error(tree, ref, "Input port cannot be modified"); false
          case _: TypeArray => error(tree, ref, "Memory can only be modified using .write()"); false
          case TypeOut(_, FlowControlTypeNone, _) =>
            true
          case _: TypeOut =>
            error(tree, ref, "Output port with flow control can only be modified using .write()")
            false
          case _ if symbol.decl.isInstanceOf[DeclVal] =>
            error(tree, ref, "'const' qualified variable cannot be modified")
            false
          case _ => true
        }
    } forall identity
  }

  private def checkType(expr: Expr)(implicit tree: Tree): Boolean = {
    expr.tpe.isType && !expr.tpe.asType.kind.isEntity ifFalse {
      error(tree, expr, "Expression does not name a type")
    }
  }

  private def checkEntity(expr: Expr)(implicit tree: Tree): Boolean = {
    expr.tpe.isType && expr.tpe.asType.kind.isEntity ifFalse {
      error(tree, expr, "Expression does not name an entity")
    }
  }

  // Other check* methods

  private def checkNameHidingByExtensionType(decl: Decl): Unit = {
    decl.symbol.kind match {
      case eKind: ExtensionType =>
        eKind.kind match {
          case cKind: CompoundType =>
            for {
              cSymbol <- cKind.publicSymbols
              eSymbol <- eKind(cSymbol.name)
              if cSymbol ne eSymbol
            } {
              cc.error(
                decl.loc,
                s"Field '${cSymbol.name}' of type '${cSymbol.kind.toSource}' of symbol '${decl.symbol.name}'",
                s"defined in type '${cKind.toSource}'",
                s"is hidden by extension field of the same name of type '${eSymbol.kind.toSource}'",
                s"defined by extension type '${eKind.toSource}'"
              )
            }
          case _ => ()
        }
      case _ => ()
    }
  }

  private val contextKind = mutable.Stack[Type]()

  private val contextNode = mutable.Stack[Int]()

  private def pushContextWidth(node: Tree, kind: Type) = {
    contextNode push node.id
    contextKind push kind.underlying
  }

  // TODO: Type attributes in a systematic way..

  override def enter(tree: Tree): Option[Tree] = {
    implicit val itree: Tree = tree
    tree match {
      // Allow unary ticks in initializers
      case defn: Defn =>
        defn.initializer foreach { init =>
          pushContextWidth(init, defn.symbol.kind)
        }

      // Type check the lhs up front
      case StmtAssign(lhs, _) if !walk(lhs).tpe.isError =>
        if (!(lhs.tpe.isGen && lhs.tpe.underlying.isNum)) {
          checkPacked(lhs, "Left hand side of assignment") && checkModifiable(lhs)
        }
        pushContextWidth(tree, lhs.tpe)

      // Type check the lhs up front
      case StmtUpdate(lhs, _, _) if !walk(lhs).tpe.isError =>
        if (!(lhs.tpe.isGen && lhs.tpe.underlying.isNum)) {
          checkPacked(lhs, "Left hand side of assignment") && checkModifiable(lhs)
        }
        // TODO: this is not quite right for shift and compare etc
        pushContextWidth(tree, lhs.tpe)

      case _: StmtReturn =>
        enclosingSymbols.headOption foreach {
          _.kind match {
            case TypeCallable(_, retType, _) if retType != TypeVoid =>
              pushContextWidth(tree, retType)
            case _ =>
          }
        }

      // Type check target up front
      case ExprIndex(tgt, _) if !walk(tgt).tpe.isError =>
        assert(!tgt.tpe.isUnknown)
        if (tgt.tpe.isType) {
          pushContextWidth(tree, TypeUnknown)
        } else if (!tgt.tpe.underlying.isNum) {
          val shapeIter = tgt.tpe.shapeIter
          if (!shapeIter.hasNext && !tgt.tpe.underlying.isNum) {
            error(tree, tgt, "Target is not indexable")
          } else {
            pushContextWidth(tree, TypeUInt(clog2(shapeIter.next) max 1))
          }
        }

      // Type check target up front
      case ExprSlice(tgt, lIdx, op, rIdx) if !walk(tgt).tpe.isError =>
        if (!tgt.tpe.underlying.isNum) {
          val shapeIter = tgt.tpe.shapeIter
          if (!shapeIter.hasNext || tgt.tpe.isArray) {
            error(tree, tgt, "Target is not sliceable")
          } else {
            val size = shapeIter.next
            val lWidth = clog2(size) max 1
            val rWidth = if (op == ":") lWidth else clog2(size + 1)
            pushContextWidth(rIdx, TypeUInt(rWidth))
            pushContextWidth(lIdx, TypeUInt(lWidth))
          }
        }

      // Special case uint(n)/int(n)/instance for now
      case ExprCall(tgt, _) if walk(tgt).tpe.isType => // TODO: is this still needed?

      // Type check target up front
      case ExprCall(tgt, args) if !walk(tgt).tpe.isError =>
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
          case TypeCombFunc(_, _, argTypes)     => process(argTypes)
          case TypeCtrlFunc(_, _, argTypes)     => process(argTypes)
          case TypeXenoFunc(_, _, argTypes)     => process(argTypes)
          case TypeStaticMethod(_, _, argTypes) => process(argTypes)
          case TypeNormalMethod(_, _, argTypes) => process(argTypes)
          case _: TypePolyFunc                  =>
          case _: TypeParametrized              =>
          case _: TypeType                      =>
          case TypeNone(_: TypeNormalMethod) =>
            error(tree, tgt, "Attempting to call non-static method via type")
          case _ => error(tree, tgt, s"Expression is not callable")
        }

      case _ => ()
    }
    None
  }

  override def transform(tree: Tree): Tree = {
    // TODO: reduction of 1 bit value is error
    // TODO: Warn for non power of 2 dimensions
    implicit val itree: Tree = tree

    val alreadyHadError = hadError

    tree match {
      //////////////////////////////////////////////////////////////////////////
      // Don't type check already typed nodes
      //////////////////////////////////////////////////////////////////////////

      case _ if tree.hasTpe => ()

      //////////////////////////////////////////////////////////////////////////
      // Propagate type errors
      //////////////////////////////////////////////////////////////////////////

      case node if hasError(node) => error(tree)

      //////////////////////////////////////////////////////////////////////////
      // Propagate type uncertainty
      //////////////////////////////////////////////////////////////////////////

      case node if hasUnknown(node) => tree withTpe TypeUnknown

      //////////////////////////////////////////////////////////////////////////
      // Decl
      //////////////////////////////////////////////////////////////////////////

      case decl: Decl =>
        // TODO: These need loads more checks (eg, void variable, etc etc...)
        val ok = decl match {
          case DeclVar(_, spec)       => checkType(spec)
          case DeclVal(_, spec)       => checkType(spec)
          case DeclIn(_, spec, _)     => checkType(spec)
          case DeclOut(_, spec, _, _) => checkType(spec)
          case DeclPipeline(_, spec)  => checkType(spec)
          case DeclConst(_, spec)     => checkType(spec)
          case DeclGen(_, spec)       => checkType(spec)
          case DeclArray(_, elem, size) =>
            (checkType(elem) &&& checkPositive(size, "Size of distributed memory")) &&
              checkPackedType(elem, "Memory element")
          case DeclSram(_, elem, size, _) =>
            (checkType(elem) &&& checkPositive(size, "Size of SRAM")) &&
              checkPackedType(elem, "SRAM element")
          case _: DeclStack                    => unreachable
          case DeclType(_, spec)               => checkType(spec)
          case _: DeclEntity                   => true
          case _: DeclRecord                   => true
          case DeclInstance(_, spec)           => checkEntity(spec)
          case _: DeclSingleton                => true
          case DeclFunc(_, variant, ret, args) => checkType(ret)
          case _: DeclState                    => unreachable
        }

        if (ok) {
          checkNameHidingByExtensionType(decl)

          // TODO: Add these back
//          @tailrec
//          def elem(kind: Type): Type = kind match {
//            case TypeVector(e, _) => elem(e)
//            case TypeArray(e, _)  => elem(e)
//            case _                => kind
//          }
//
//          decl match {
//            case kind: TypeVector if elem(kind).underlying.isRecord =>
//              cc.error(tree, "Vector element cannot have a struct type")
//            case kind: TypeArray if elem(kind).underlying.isRecord =>
//              cc.error(tree, "Memory element cannot have a struct type")
//            case _ => ()
//          }
        }

      case DefnFunc(symbol, _, body) =>
        if (symbol.kind.isCtrlFunc) {
          if (body.isEmpty) {
            error(tree, symbol.loc, "Body of control function must end in a control statement")
          } else if (body.last.tpe != TypeCtrlStmt) {
            error(tree, body.last, "Body of control function must end in a control statement")
          }
        } else if (symbol.kind.isMethod) {
          body.iterator filter { _.tpe.isCtrlStmt } foreach { stmt =>
            error(
              tree,
              stmt,
              "Body of combinational function must contain only combinational statements"
            )
          }
        } else if (!symbol.kind.isXenoFunc) {
          cc.ice(tree, "Unknown function definition")
        }

      case defn: Defn =>
        defn.initializer foreach { init =>
          val symbol = defn.symbol
          if (symbol.kind.underlying.isNum && init.tpe.isPacked) {
            if (symbol.kind.isParam) {
              error(tree, init, "Unsized integer parameter assigned a packed value")
            } else {
              error(tree, "Unsized integer declaration has packed initializer")
            }
          } else if (!symbol.kind.underlying.isNum && !init.tpe.underlying.isNum) {
            val msg = if (symbol.kind.isParam) "Parameter value" else "Initializer expression"
            if (!(checkPacked(init, msg) && checkWidth(symbol.kind.width, init, msg))) {
              error(tree)
            }
          } else if ((symbol.kind.isConst || symbol.kind.isParam) && !init.isKnownConst) {
            val what = if (symbol.kind.isConst) "cons" else "param"
            error(tree, init, s"Initializer of '$what' declaration must be compile time constant")
          }
        }

      //////////////////////////////////////////////////////////////////////////
      // Assertion
      //////////////////////////////////////////////////////////////////////////

      case AssertionAssert(cond, _) => checkNumericOrPacked(cond, "Condition of 'assert'")

      case AssertionStatic(cond, _) => checkNumericOrPacked(cond, "Condition of 'static assert'")

      //////////////////////////////////////////////////////////////////////////
      // Ent
      //////////////////////////////////////////////////////////////////////////

      case EntCombProcess(stmts) =>
        stmts filter { _.tpe == TypeCtrlStmt } foreach {
          error(tree, _, "'fence' block must contain only combinational statements")
        }

      case conn: EntConnect => ConnectChecks(conn)

      //////////////////////////////////////////////////////////////////////////
      // Stmt
      //////////////////////////////////////////////////////////////////////////

      case StmtBlock(body) if !checkBlock(body) => error(tree)

      case StmtIf(cond, ts, es) =>
        if (!checkNumericOrPacked(cond, "Condition of 'if' statement")) {
          error(tree)
        } else if (!checkWidth(1, cond, "Condition of 'if' statement")) {
          error(tree)
        }
        if (!(checkBlock(ts) &&& checkBlock(es))) {
          error(tree)
        } else if (ts.nonEmpty && es.nonEmpty && ts.last.tpe != es.last.tpe) {
          error(tree, "Either both or neither branches of if-else must be control statements")
        }

      case StmtCase(expr, cases) =>
        if (!checkNumericOrPacked(expr, "'case' expression")) {
          error(tree)
        } else if (expr.tpe.isPacked && expr.tpe.width == 0) {
          error(tree, expr, "'case' expression has width zero")
        }
        val oks = cases map { _.stmts } map checkBlock
        if (oks exists { !_ }) {
          error(tree)
        } else {
          val allComb = cases forall { _.stmts forall { _.tpe.isCombStmt } }
          val allCtrl = cases forall { _.stmts exists { _.tpe.isCtrlStmt } }
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

      case StmtLet(_, body) if !checkBlock(body) => error(tree)

      case StmtAssign(lhs, rhs) if !rhs.tpe.underlying.isNum =>
        if (lhs.tpe.underlying.isNum && rhs.tpe.isPacked) {
          error(tree, "Unsized integer variable assigned a packed value")
        } else {
          // lhs have already been checked in enter
          val ok = checkPacked(rhs, "Right hand side of assignment") &&
            checkWidth(lhs.tpe.width, rhs, "Right hand side of assignment")
          if (!ok) error(tree)
        }

      // TODO: this is not right for shifts which can have any right hand side
      case StmtUpdate(lhs, _, rhs) if !rhs.tpe.underlying.isNum =>
        if (lhs.tpe.underlying.isNum && rhs.tpe.isPacked) {
          error(tree, "Unsized integer variable assigned a packed value")
        } else {
          // lhs have already been checked in enter
          val ok = checkPacked(rhs, "Right hand side of assignment") &&
            checkWidth(lhs.tpe.width, rhs, "Right hand side of assignment")
          if (!ok) error(tree)
        }

      case StmtPost(expr, op) =>
        val ok = expr.tpe.isGen && expr.tpe.underlying.isNum ||
          checkPacked(expr, s"Target of postfix '$op'") && checkModifiable(expr)
        if (!ok) error(tree)

      case stmt: StmtReturn =>
        enclosingSymbols.head.kind match {
          case TypeCallable(symbol, TypeVoid, _) =>
            stmt.exprOpt match {
              case Some(expr) =>
                error(tree, expr, s"void function '${symbol.name}' cannot return a value")
              case _ =>
            }
          case TypeCallable(symbol, retType, _) =>
            stmt.exprOpt match {
              case None => error(tree, s"non-void function '${symbol.name}' must return a value")
              case Some(expr) =>
                if (!expr.tpe.underlying.isNum) {
                  val ok = checkPacked(expr, "Return value") &&
                    checkWidth(retType.width, expr, "Return value")
                  if (!ok) error(tree)
                }
            }
          case _ => error(tree, "'return' statement not inside function")
        }

      case StmtExpr(expr) if expr.isPure =>
        error(tree, "A pure expression in statement position does nothing")

      //////////////////////////////////////////////////////////////////////////
      // Expr
      //////////////////////////////////////////////////////////////////////////

      case ExprSelect(expr, sel, idxs) =>
        assert(idxs.isEmpty)

        val field = expr.tpe match {
          case TypeType(kind: CompoundType) => kind(sel)
          case TypeNone(kind: CompoundType) => kind(sel)
          case kind: CompoundType           => kind(sel)
          case _                            => None
        }

        if (field.isEmpty) {
          expr.tpe match {
            case TypeEntity(symbol, _) =>
              error(tree, s"No port named '$sel' on instance of entity '${symbol.name}'")
            case kind =>
              error(tree, s"No member named '$sel' in value of type '${kind.toSource}'")
          }
        }

      case ExprCall(expr, args) =>
        def check(kinds: List[Type]): Unit = {
          val as = args map {
            case ArgP(a) => a
            case _       => unreachable
          }
          for (((kind, arg), index) <- (kinds zip as).zipWithIndex) {
            val i = index + 1
            if (kind.isType) {
              if (!arg.tpe.isType) error(tree, arg, s"Argument $i expects a type")
            } else if (kind.isNum) {
              if (!arg.tpe.underlying.isNum)
                error(tree, arg, s"Argument $i expects an unsized value")
            } else if (!arg.tpe.underlying.isNum) {
              val ok = checkPacked(arg, s"Argument $i of function call") &&
                checkWidth(kind.width, arg, s"Argument $i of function call")
              if (!ok) error(tree)
            }
          }
        }

        expr.tpe match {
          case TypeCombFunc(_, _, argTypes)     => check(argTypes)
          case TypeCtrlFunc(_, _, argTypes)     => check(argTypes)
          case TypeXenoFunc(_, _, argTypes)     => check(argTypes)
          case TypeStaticMethod(_, _, argTypes) => check(argTypes)
          case TypeNormalMethod(_, _, argTypes) => check(argTypes)
          case tpe: TypePolyFunc =>
            tpe.resolve(args) match {
              case Some(symbol) => tree withTpe symbol.kind.asCombFunc.retType
              case None =>
                val msg = s"Builtin function '${expr.toSource}' cannot be applied to arguments" :: {
                  args map { arg =>
                    s"'${arg.toSource}' of type ${arg.tpe.toSource}"
                  }
                }
                error(tree, msg: _*)
            }
          case _: TypeType         =>
          case _: TypeParametrized =>
          case _                   => unreachable // Handled in enter
        }

      case ExprCat(parts) =>
        for ((part, i) <- parts.zipWithIndex) {
          if (!checkPacked(part, s"Part ${i + 1} of bit concatenation")) {
            error(tree)
          }
        }

      case ExprRep(count, expr) =>
        val ok = checkNumeric(count, "Count of bit repetition") && {
          checkKnownConst(count, "Count of bit repetition") &&&
            checkPacked(expr, "Value of bit repetition")
        }
        if (!ok) error(tree)

      case ExprIndex(tgt, idx) =>
        if (tgt.tpe.isType) {
          checkPositive(idx, "Size of vector")
          tgt.tpe.asType.kind match {
            case _: TypeRecord => error(tree, tgt, "Vector element must not have 'struct' type")
            case TypeVoid      => error(tree, tgt, "Vector element must not have 'void' type")
            case kind =>
              if (!kind.isPacked) {
                error(tree, tgt, "Vector element must have a packed type")
              }
          }
        } else if (tgt.tpe.underlying.isNum) {
          checkKnownConst(idx, "Index of unsized integer value")
        } else {
          (tgt.tpe.isArray || checkPacked(tgt, "Target of index")) &&
          checkIndex(contextKind.top.width, idx, false, "Index")
        }

      case ExprSlice(tgt, lIdx, op, rIdx) =>
        if (tgt.tpe.underlying.isNum) {
          val ok = checkKnownConst(lIdx, "Left index of unsized integer value") &&&
            checkKnownConst(rIdx, "Right index of unsized integer value")
          if (!ok) error(tree)
        } else {
          val ok = checkPacked(tgt, "Target of slice") && {
            val size = tgt.tpe.shapeIter.next
            val lWidth = clog2(size) max 1
            val rWidth = if (op == ":") lWidth else clog2(size + 1)
            checkIndex(lWidth, lIdx, op == ":", "Left index") &&&
              checkIndex(rWidth, rIdx, true, "Right index")
          }
          if (!ok) error(tree)
        }

      // Unary ops

      // Unary ' is special and the type of the node must be assigned here
      // based on context as it cannot be determined from the operand only.
      case ExprUnary("'", op) =>
        if (hadError) {
          // If we had a type error, the context stack might be out of sync
          // so don't type check any further unary tick nodes
          error(tree)
        } else if (contextKind.isEmpty || contextKind.top.isUnknown) {
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
              tree withTpe TypeInt(op.tpe.isSigned, resWidth)
            }
          }
        } else {
          error(tree)
        }

      case ExprUnary(op, expr) =>
        if (expr.tpe.underlying.isNum) {
          if ("&|^" contains op) {
            error(tree, s"Unary operator '$op' cannot be applied to unsized integer value")
          }
        } else if (!checkPacked(expr, s"Operand of unary operator '$op'")) {
          error(tree)
        }

      // Binary ops
      case ExprBinary(lhs, op, rhs) =>
        if (!lhs.tpe.underlying.isNum || !rhs.tpe.underlying.isNum) {
          lazy val strictWidth = !(mixedWidthBinaryOps contains op)
          if (lhs.tpe.underlying.isNum && strictWidth) {
            if (!checkPacked(rhs, s"Right hand operand of '$op'")) {
              error(tree)
            }
          } else if (rhs.tpe.underlying.isNum && strictWidth) {
            if (!checkPacked(lhs, s"Left hand operand of '$op'")) {
              error(tree)
            }
          } else if (strictWidth) {
            if (
              !checkPacked(lhs, s"Left hand operand of '$op'") |||
                !checkPacked(rhs, s"Right hand operand of '$op'")
            ) {
              error(tree)
            } else if (lhs.tpe.width != rhs.tpe.width) {
              error(
                tree,
                s"Both operands of binary '$op' must have the same width, but",
                s"left  hand operand is ${lhs.tpe.width} bits wide, and",
                s"right hand operand is ${rhs.tpe.width} bits wide"
              )
            }
          } else {
            if (
              !checkNumericOrPacked(lhs, s"Left hand operand of '$op'") |||
                !checkNumericOrPacked(rhs, s"Right hand operand of '$op'")
            ) {
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

      case ExprTernary(cond, thenExpr, elseExpr) =>
        if (!checkNumericOrPacked(cond, "Condition of '?:'")) {
          error(tree)
        } else if (!thenExpr.tpe.underlying.isNum || !elseExpr.tpe.underlying.isNum) {
          lazy val okThen = checkPacked(thenExpr, "'then' operand of '?:'")
          lazy val okElse = checkPacked(elseExpr, "'else' operand of '?:'")
          if (thenExpr.tpe.underlying.isNum) {
            if (!okElse) error(tree)
          } else if (elseExpr.tpe.underlying.isNum) {
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

      //////////////////////////////////////////////////////////////////////////
      // Done
      //////////////////////////////////////////////////////////////////////////

      case _ => ()
    }

    // Check that if we have just found an error, then we have also
    // marked the tree with TypeError
    assert((!alreadyHadError && hadError) implies tree.tpe.isError)

    // Pop context stack if required. This is a loop as some nodes might have
    // been pushed multiple times, e.g.: port.write(array[index]), both the
    // ExprCall and ExprIndex would have pushed the ExprIndex node
    while (contextNode.headOption contains tree.id) {
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
    } tap { _ =>
      if (errorForParametrized && tree.tpe.isParametrized) {
        cc.error(tree, s"Parametrized type requires parameter list")
        hadError = true
      }

      tree match {
        case expr: Expr if expr.tpe.underlying.isNum && !expr.isKnownConst =>
          cc.error(expr, "Expression of unsized integer type must be compile time constant")
        case _ =>
      }

      hadError |= tree.tpe.isError
    }
  }

  override def finalCheck(tree: Tree): Unit = {
    if (tree.hasTpe && !tree.tpe.isError) {
      assert(contextKind.isEmpty, s"${tree.loc.prefix}\n$contextKind\n$contextNode")

      tree visitAll {
        case n: Tree if !n.hasTpe => cc.ice(n, "Typer: untyped node remains", n.toString)
      }
    }
  }

}
