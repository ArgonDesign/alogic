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

trait DeclarationPrettyPrintOps { this: Declaration =>
  def toSource: String = this match {
    case VarDeclaration(decltype, id, Some(init))    => s"${decltype.toSource} ${id} = ${init.toSource}"
    case VarDeclaration(decltype, id, None)          => s"${decltype.toSource} ${id}"
    case ArrayDeclaration(decltype, id, dims)        => s"${decltype.toSource} ${id}${dims map { _.toSource } mkString ("[", "][", "]")}"
    case ParamDeclaration(decltype, id, init)        => s"param ${decltype.toSource} ${id} = ${init.toSource}"
    case ConstDeclaration(decltype, id, init)        => s"const ${decltype.toSource} ${id} = ${init.toSource}"
    case VerilogVarDeclaration(decltype, id)         => s"verilog ${decltype.toSource} ${id}"
    case VerilogArrayDeclaration(decltype, id, dims) => s"verilog ${decltype.toSource} ${id}${id}${dims map { _.toSource } mkString ("[", "][", "]")}"
    case OutDeclaration(fctype, decltype, id, stype) => s"out ${fctype.toSource} ${stype.toSource} ${decltype.toSource} ${id}"
    case InDeclaration(fctype, decltype, id)         => s"in ${fctype.toSource} ${decltype.toSource} ${id}"
    case PipelineVarDeclaration(decltype, id)        => s"pipeline ${decltype.toSource} ${id}"
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
        case DottedName(names)              => names mkString "."
        case ArrayLookup(name, index)       => s"${v(name)}${index map v mkString ("[", "][", "]")}"
        case Slice(ref, l, op, r)           => s"${v(ref)}[${v(l)}$op${v(r)}]"
        case CallExpr(name, args)           => s"${v(name)}(${args map v mkString ", "})"
        case Zxt(numbits, expr)             => s"@zx(${v(numbits)}, ${v(expr)})"
        case Sxt(numbits, expr)             => s"@sx(${v(numbits)}, ${v(expr)})"
        case DollarCall(name, args)         => "$" + s"$name(${args map v mkString ", "})"
        case ReadCall(name)                 => s"${v(name)}.read()"
        case PipelineRead                   => "read()"
        case PipelineWrite                  => "write()"
        case LockCall(name)                 => s"${v(name)}.lock()"
        case ValidCall(name)                => s"${v(name)}.valid()"
        case WriteCall(name, args)          => s"${v(name)}.write(${args map v mkString ", "})"
        case BinaryOp(lhs, op, rhs)         => s"(${lhs.toSource})$op(${rhs.toSource})"
        case UnaryOp(op, lhs)               => s"${op}(${v(lhs)})"
        case Bracket(e)                     => s"(${v(e)})"
        case TernaryOp(cond, lhs, rhs)      => s"(${v(cond)}) ? (${v(lhs)}) : (${v(rhs)}) "
        case BitRep(count, value)           => s"{${v(count)}{${v(value)}}}"
        case BitCat(parts)                  => s"{${parts map v mkString ", "}}"
        case Literal(value)                 => value
        case Num(false, None, value)        => s"'d${value}"
        case Num(true, None, value)         => s"'sd${value}"
        case Num(false, Some(width), value) => s"${width}'d${value}"
        case Num(true, Some(width), value)  => s"${width}'sd${value}"
        case ErrorExpr                      => "/*Error expression*/"
      }
      v(expr)
    }

    def v(indent: Int)(node: Node): String = {
      val i = "  " * indent
      node match {
        case expr: Expr => visitExpr(expr)

        case Instantiate(id, module, args) => {
          val pas = for ((lhs, rhs) <- args.toList) yield { s"${lhs} = ${v(indent)(rhs)}" }
          s"${i}new $id  = ${module}(${pas mkString ", "});\n"
        }
        case Connect(lhs, rhs)     => s"$lhs -> ${rhs map v(indent) mkString ", "}\n"
        case Function(name, body)  => s"void $name() ${v(indent)(body)}"
        case FenceFunction(body)   => s"void fence() ${v(indent)(body)}"
        case VerilogFunction(body) => s"void verilog() {$body}"
        case FsmTask(name, decls, fns, fencefn, vfns, hasnew) =>
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

        case StateTask(name, decls, sbs, fencefn, vfns) =>
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

        case NetworkTask(name, decls, inst, conn, vfns, fsms) => s"TODO: NetworkTask(name, decls, fns)"
        case VerilogTask(name, decls, fns)                    => s"TODO: VerilogTask(name, decls, fns)"
        case Assign(lhs, rhs)                                 => s"${v(indent)(lhs)} = ${v(indent)(rhs)};"
        case Update(lhs, op, rhs)                             => s"${v(indent)(lhs)} ${op}= ${v(indent)(rhs)};"
        case Plusplus(lhs)                                    => s"${v(indent)(lhs)}++;"
        case Minusminus(lhs)                                  => s"${v(indent)(lhs)}--;"
        case DeclarationStmt(decl)                            => s"${decl.toSource};"

        case CombinatorialBlock(cmds) =>
          s"""|{
              |${i}  ${cmds map v(indent + 1) mkString s"\n${i}  "}
              |${i}}""".stripMargin
        case ControlBlock(cmds) =>
          s"""|{
              |${i}  ${cmds map v(indent + 1) mkString s"\n${i}  "}
              |${i}}""".stripMargin

        case StateBlock(state, cmds) =>
          s"""|@state ${state} {
              |${i}  ${cmds map v(indent + 1) mkString s"\n${i}  "}
              |${i}}""".stripMargin

        case CombinatorialIf(cond, body, Some(e)) => s"if (${v(indent)(cond)}) ${v(indent)(body)} else ${v(indent)(e)}"
        case CombinatorialIf(cond, body, None)    => s"if (${v(indent)(cond)}) ${v(indent)(body)}"
        case ControlIf(cond, body, Some(e))       => s"if (${v(indent)(cond)}) ${v(indent)(body)} else ${v(indent)(e)}"
        case ControlIf(cond, body, None)          => s"if (${v(indent)(cond)}) ${v(indent)(body)}"

        case CombinatorialCaseStmt(value, cases, None) =>
          s"""|case (${v(indent)(value)}) {
              |${i}  ${cases map v(indent + 1) mkString s"\n${i}  "}
              |${i}}""".stripMargin
        case CombinatorialCaseStmt(value, cases, Some(default)) =>
          s"""|case (${v(indent)(value)}) {
              |${i}  ${cases map v(indent + 1) mkString s"\n${i}  "}
              |${i}  default: ${v(indent + 1)(default)}
              |${i}}""".stripMargin
        case ControlCaseStmt(value, cases, None) =>
          s"""|case (${v(indent)(value)}) {
              |${i}  ${cases map v(indent + 1) mkString s"\n${i}  "}
              |${i}}""".stripMargin
        case ControlCaseStmt(value, cases, Some(default)) =>
          s"""|case (${v(indent)(value)}) {
              |${i}  ${cases map v(indent + 1) mkString s"\n${i}  "}
              |${i}  default: ${v(indent + 1)(default)}
              |${i}}""".stripMargin

        case ControlCaseLabel(Nil, body)        => s"default : ${v(indent)(body)}"
        case CombinatorialCaseLabel(Nil, body)  => s"default : ${v(indent)(body)}"
        case ControlCaseLabel(cond, body)       => s"${cond map v(indent) mkString ", "} : ${v(indent)(body)}"
        case CombinatorialCaseLabel(cond, body) => s"${cond map v(indent) mkString ", "} : ${v(indent)(body)}"

        case ControlLoop(ControlBlock(body)) =>
          s"""|loop {
              |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
              |${i}}""".stripMargin
        case ControlWhile(cond, body) =>
          s"""|while (${v(indent)(cond)}) {
              |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
              |${i}}""".stripMargin
        case ControlFor(init, cond, incr, body) =>
          s"""|for (${v(indent)(init)} ; ${v(indent)(cond)} ; ${v(indent)(incr)}) {
              |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
              |${i}}""".stripMargin
        case ControlDo(cond, body) =>
          s"""|do {
              |${i}  ${body map v(indent + 1) mkString s"\n${i}  "}
              |${i}} while (${v(indent)(cond)});""".stripMargin

        case ExprStmt(expr)      => s"${expr.toSource};"
        case CallStmt(name)      => s"$name();"

        case FenceStmt           => "fence;"
        case BreakStmt           => "break;"
        case ReturnStmt          => "return;"
        case GotoStmt(target)    => s"goto $target;"
        case GotoState(tgt)      => s"goto state ${tgt};"
        case CallState(tgt, ret) => s"@push state ${tgt} ${ret}"
        case ReturnState         => "@pop state"
        case AlogicComment(str)  => s"TODO: AlogicComment(str)"

        case ErrorStmt           => "/*Error statement*/"

      }
    }

    v(0)(this)
  }
}
