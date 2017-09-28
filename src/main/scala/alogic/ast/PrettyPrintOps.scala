////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017 Argon Design Ltd. All rights reserved.
//
// Module : Scala Alogic Compiler
// Author : Peter de Rivaz/Geza Lore
//
// DESCRIPTION:
//
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic.ast

trait DeclarationPrettyPrintOps { this: Decl =>
  def toSource: String = this match {
    case DeclVar(kind, id, Some(init))    => s"${kind.toSource} ${id} = ${init.toSource}"
    case DeclVar(kind, id, None)          => s"${kind.toSource} ${id}"
    case DeclArr(kind, id, dims)          => s"${kind.toSource} ${id}${dims map { _.toSource } mkString ("[", "][", "]")}"
    case DeclParam(kind, id, init)        => s"param ${kind.toSource} ${id} = ${init.toSource}"
    case DeclConst(kind, id, init)        => s"const ${kind.toSource} ${id} = ${init.toSource}"
    case DeclVerilogVar(kind, id)         => s"verilog ${kind.toSource} ${id}"
    case DeclVerilogArr(kind, id, dims)   => s"verilog ${kind.toSource} ${id}${id}${dims map { _.toSource } mkString ("[", "][", "]")}"
    case DeclOut(kind, id, fctype, stype) => s"out ${fctype.toSource} ${stype.toSource} ${kind.toSource} ${id}"
    case DeclIn(kind, id, fctype)         => s"in ${fctype.toSource} ${kind.toSource} ${id}"
    case DeclPippeVar(kind, id)           => s"pipeline ${kind.toSource} ${id}"
  }
}

trait FlowControlTypePrettyPrintOps { this: FlowControlType =>
  def toSource: String = this match {
    case FlowControlTypeNone   => ""
    case FlowControlTypeValid  => "sync"
    case FlowControlTypeReady  => "sync ready"
    case FlowControlTypeAccept => "sync accept"
  }
}

trait StorageTypePrettyPrintOps { this: StorageType =>
  def toSource: String = this match {
    case StorageTypeWire   => "wire"
    case StorageTypeBubble => "bubble"
    case StorageTypeReg    => ""
  }
}

trait TypePrettyPrintOps { this: Type =>
  def toSource: String = this match {
    case IntType(true, size)   => s"i${size}"
    case IntType(false, size)  => s"u${size}"
    case IntVType(true, args)  => s"int(${args map (_.toSource) mkString ", "})"
    case IntVType(false, args) => s"uint(${args map (_.toSource) mkString ", "})"
    case Struct(name, _)       => s"struct $name"
    case VoidType              => "void"
  }
}

trait NodePrettyPrintOps { this: Node =>
  def toSource: String = {
    def visitExpr(expr: Expr): String = {
      def v(expr: Expr): String = expr match {
        case DottedName(_, names)              => names mkString "."
        case ExprArrIndex(_, name, index)      => s"${v(name)}${index map v mkString ("[", "][", "]")}"
        case ExprVecIndex(_, ref, index)       => s"${v(ref)}${index map v mkString ("[", "][", "]")}"
        case Slice(_, ref, l, op, r)           => s"${v(ref)}[${v(l)}$op${v(r)}]"
        case CallExpr(_, name, args)           => s"${v(name)}(${args map v mkString ", "})"
        case Zxt(_, numbits, expr)             => s"@zx(${v(numbits)}, ${v(expr)})"
        case Sxt(_, numbits, expr)             => s"@sx(${v(numbits)}, ${v(expr)})"
        case DollarCall(_, name, args)         => "$" + s"$name(${args map v mkString ", "})"
        case ReadCall(_, name)                 => s"${v(name)}.read()"
        case _: PipelineRead                   => "read()"
        case _: PipelineWrite                  => "write()"
        case WaitCall(_, name)                 => s"${v(name)}.wait()"
        case ValidCall(_, name)                => s"${v(name)}.valid()"
        case WriteCall(_, name, args)          => s"${v(name)}.write(${args map v mkString ", "})"
        case BinaryOp(_, lhs, op, rhs)         => s"(${lhs.toSource})$op(${rhs.toSource})"
        case UnaryOp(_, op, lhs)               => s"${op}(${v(lhs)})"
        case Bracket(_, e)                     => s"(${v(e)})"
        case TernaryOp(_, cond, lhs, rhs)      => s"(${v(cond)}) ? (${v(lhs)}) : (${v(rhs)}) "
        case BitRep(_, count, value)           => s"{${v(count)}{${v(value)}}}"
        case BitCat(_, parts)                  => s"{${parts map v mkString ", "}}"
        case Literal(_, value)                 => value
        case Num(_, false, None, value)        => s"'d${value}"
        case Num(_, true, None, value)         => s"'sd${value}"
        case Num(_, false, Some(width), value) => s"${width}'d${value}"
        case Num(_, true, Some(width), value)  => s"${width}'sd${value}"
        case _: ErrorExpr                      => "/*Error expression*/"
      }
      v(expr)
    }

    def visitLVal(lval: LVal): String = {
      def v(lval: LVal): String = lval match {
        case LValName(_, names)              => names mkString "."
        case LValArrayLookup(_, name, index) => s"${v(name)}${index map { _.toSource } mkString ("[", "][", "]")}"
        case LValSlice(_, ref, l, op, r)     => s"${v(ref)}[${l.toSource}$op${r.toSource}]"
        case LValCat(_, parts)               => s"{${parts map v mkString ", "}}"
      }
      v(lval)
    }

    def v(indent: Int)(node: Node): String = {
      val i = "  " * indent
      node match {
        case expr: Expr => visitExpr(expr)
        case lval: LVal => visitLVal(lval)

        case Instantiate(_, id, module, args) => {
          val pas = for ((lhs, rhs) <- args.toList) yield { s"${lhs} = ${v(indent)(rhs)}" }
          s"${i}new $id  = ${module}(${pas mkString ", "});\n"
        }
        case Connect(_, lhs, rhs)     => s"$lhs -> ${rhs map v(indent) mkString ", "}\n"
        case Function(_, name, body)  => s"void $name() ${v(indent)(body)}"
        case FenceFunction(_, body)   => s"void fence() ${v(indent)(body)}"
        case VerilogFunction(_, body) => s"void verilog() {$body}"
        case FsmTask(_, name, decls, fns, fencefn, vfns) =>
          s"""|fsm $name {
              |${i}  /////////////////////////////////
              |${i}  // Declarations
              |${i}  /////////////////////////////////
              |
              |${i}  ${decls map (_.toSource + ";") mkString s"\n${i}  "}
              |
              |${i}  /////////////////////////////////
              |${i}  // Fence function
              |${i}  /////////////////////////////////
              |
              |${i}  ${if (fencefn.isDefined) v(indent + 1)(fencefn.get) else "// None"}
              |
              |${i}  /////////////////////////////////
              |${i}  // Functions
              |${i}  /////////////////////////////////
              |
              |${i}  ${fns map v(indent + 1) mkString s"\n\n${i}  "}
              |
              |${i}  /////////////////////////////////
              |${i}  // Verilog functions
              |${i}  /////////////////////////////////
              |
              |${i}  ${vfns map v(indent + 1) mkString s"\n\n${i}  "}
              |
              |${i}}""".stripMargin

        case StateTask(_, name, decls, sbs, fencefn, vfns) =>
          s"""|@StateTask $name {
              |${i}  /////////////////////////////////
              |${i}  // Declarations
              |${i}  /////////////////////////////////
              |
              |${i}  ${decls map (_.toSource + ";") mkString s"\n${i}  "}
              |
              |${i}  /////////////////////////////////
              |${i}  // Fence function
              |${i}  /////////////////////////////////
              |
              |${i}  ${if (fencefn.isDefined) v(indent + 1)(fencefn.get) else "// None"}
              |
              |${i}  /////////////////////////////////
              |${i}  // States
              |${i}  /////////////////////////////////
              |
              |${i}  ${sbs map v(indent + 1) mkString s"\n\n${i}  "}
              |
              |${i}  /////////////////////////////////
              |${i}  // Verilog functions
              |${i}  /////////////////////////////////
              |
              |${i}  ${vfns map v(indent + 1) mkString s"\n${i}  "}
              |
              |${i}}""".stripMargin

        case NetworkTask(_, name, decls, inst, conn, vfns, fsms) => s"TODO: NetworkTask(name, decls, fns)"
        case VerilogTask(_, name, decls, fns)                    => s"TODO: VerilogTask(name, decls, fns)"
        case Assign(_, lhs, rhs)                                 => s"${v(indent)(lhs)} = ${v(indent)(rhs)};"
        case Update(_, lhs, op, rhs)                             => s"${v(indent)(lhs)} ${op}= ${v(indent)(rhs)};"
        case Plusplus(_, lhs)                                    => s"${v(indent)(lhs)}++;"
        case Minusminus(_, lhs)                                  => s"${v(indent)(lhs)}--;"
        case DeclarationStmt(_, decl)                            => s"${decl.toSource};"

        case CombinatorialBlock(_, cmds) =>
          s"""|{
              |${i}  ${cmds map v(indent + 1) mkString s"\n${i}  "}
              |${i}}""".stripMargin
        case ControlBlock(_, cmds) =>
          s"""|{
              |${i}  ${cmds map v(indent + 1) mkString s"\n${i}  "}
              |${i}}""".stripMargin

        case StateBlock(_, state, cmds) =>
          s"""|@state ${state} {
              |${i}  ${cmds map v(indent + 1) mkString s"\n${i}  "}
              |${i}}""".stripMargin

        case CombinatorialIf(_, cond, body, Some(e)) => s"if (${v(indent)(cond)}) ${v(indent)(body)} else ${v(indent)(e)}"
        case CombinatorialIf(_, cond, body, None)    => s"if (${v(indent)(cond)}) ${v(indent)(body)}"
        case ControlIf(_, cond, body, Some(e))       => s"if (${v(indent)(cond)}) ${v(indent)(body)} else ${v(indent)(e)}"
        case ControlIf(_, cond, body, None)          => s"if (${v(indent)(cond)}) ${v(indent)(body)}"

        case CombinatorialCaseStmt(_, value, cases, None) =>
          s"""|case (${v(indent)(value)}) {
              |${i}  ${cases map v(indent + 1) mkString s"\n${i}  "}
              |${i}}""".stripMargin
        case CombinatorialCaseStmt(_, value, cases, Some(default)) =>
          s"""|case (${v(indent)(value)}) {
              |${i}  ${cases map v(indent + 1) mkString s"\n${i}  "}
              |${i}  default: ${v(indent + 1)(default)}
              |${i}}""".stripMargin
        case ControlCaseStmt(_, value, cases, None) =>
          s"""|case (${v(indent)(value)}) {
              |${i}  ${cases map v(indent + 1) mkString s"\n${i}  "}
              |${i}}""".stripMargin
        case ControlCaseStmt(_, value, cases, Some(default)) =>
          s"""|case (${v(indent)(value)}) {
              |${i}  ${cases map v(indent + 1) mkString s"\n${i}  "}
              |${i}  default: ${v(indent + 1)(default)}
              |${i}}""".stripMargin

        case ControlCaseLabel(_, Nil, body)        => s"default : ${v(indent)(body)}"
        case CombinatorialCaseLabel(_, Nil, body)  => s"default : ${v(indent)(body)}"
        case ControlCaseLabel(_, cond, body)       => s"${cond map v(indent) mkString ", "} : ${v(indent)(body)}"
        case CombinatorialCaseLabel(_, cond, body) => s"${cond map v(indent) mkString ", "} : ${v(indent)(body)}"

        case ControlLoop(_, ControlBlock(_, body)) =>
          s"""|loop {
              |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
              |${i}}""".stripMargin
        case ControlWhile(_, cond, body) =>
          s"""|while (${v(indent)(cond)}) {
              |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
              |${i}}""".stripMargin
        case ControlFor(_, init, cond, incr, body) =>
          s"""|for (${v(indent)(init)} ; ${v(indent)(cond)} ; ${v(indent)(incr)}) {
              |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
              |${i}}""".stripMargin
        case ControlDo(_, cond, body) =>
          s"""|do {
              |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
              |${i}} while (${v(indent)(cond)});""".stripMargin

        case ExprStmt(_, expr)      => s"${expr.toSource};"
        case CallStmt(_, name)      => s"$name();"

        case _: FenceStmt           => "fence;"
        case _: BreakStmt           => "break;"
        case _: ReturnStmt          => "return;"
        case GotoStmt(_, target)    => s"goto $target;"
        case GotoState(_, tgt)      => s"goto state ${tgt};"
        case CallState(_, tgt, ret) => s"@push state ${tgt} ${ret}"
        case _: ReturnState         => "@pop state"
        case AlogicComment(_, str)  => s"TODO: AlogicComment(str)"

        case _: ErrorStmt           => "/*Error statement*/"

      }
    }

    v(0)(this)
  }
}
