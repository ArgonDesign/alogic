// Pretty printers for various internal structures

package alogic.ast

object PrettyPrinters {

  implicit class SyncTypePritner(val kind: SyncType) {
    def toSource: String = kind match {
      case SyncReadyBubble => "sync ready bubble"
      case SyncReady       => "sync ready"
      case SyncAccept      => "sync accept"
      case Sync            => "sync"
      case WireSyncAccept  => "wire sync accept"
      case WireSync        => "wire sync"
      case Wire            => "wire"
    }
  }

  implicit class AlogicTypePrinter(val kind: Type) {
    def toSource: String = kind match {
      case IntType(true, size)   => s"i${size}"
      case IntType(false, size)  => s"u${size}"
      case IntVType(true, args)  => s"int(${args map (_.toSource) mkString ", "})"
      case IntVType(false, args) => s"uint(${args map (_.toSource) mkString ", "})"
      case Struct(fields)        => s"struct { ${(for ((n, t) <- fields) yield s"${t.toSource} $n;") mkString " "} }"
      case State                 => "@state"
    }
  }

  implicit class DeclarationPrinter(val decl: Declaration) extends AnyVal {
    def toSource: String = decl match {
      case VarDeclaration(decltype, id, Some(init)) => s"${decltype.toSource} ${id.toSource} = ${init.toSource}"
      case VarDeclaration(decltype, id, None)       => s"${decltype.toSource} ${id.toSource}"
      case ParamDeclaration(decltype, id, init)     => s"param ${decltype.toSource} ${id} = ${init.toSource}"
      case ConstDeclaration(decltype, id, init)     => s"const ${decltype.toSource} ${id} = ${init.toSource}"
      case VerilogDeclaration(decltype, id)         => s"verilog ${decltype.toSource} ${id.toSource}"
      case OutDeclaration(synctype, decltype, name) => s"out ${synctype.toSource} ${decltype.toSource} ${name}"
      case InDeclaration(synctype, decltype, name)  => s"in ${synctype.toSource} ${decltype.toSource} ${name}"
    }
  }

  implicit class AlogicExprPrinter(val tree: Expr) extends AnyVal {
    def toSource: String = {
      def v(node: Expr): String = node match {
        case DottedName(names)                    => names mkString "."
        case ArrayLookup(name, index)             => s"${v(name)}${index map v mkString ("[", "][", "]")}"
        case Slice(ref, l, op, r)                 => s"${v(ref)}[${v(l)}$op${v(r)}]"
        case CallExpr(name, args)                 => s"${v(name)}(${args map v mkString ", "})"
        case Zxt(numbits, expr)                   => s"zxt(${v(numbits)}, ${v(expr)})"
        case Sxt(numbits, expr)                   => s"sxt(${v(numbits)}, ${v(expr)})"
        case DollarCall(name, args)               => "$" + s"$name(${args map v mkString ", "})"
        case ReadCall(name)                       => s"${v(name)}.read()"
        case LockCall(name)                       => s"${v(name)}.lock()"
        case UnlockCall(name)                     => s"${v(name)}.unlock()"
        case ValidCall(name)                      => s"${v(name)}.valid()"
        case WriteCall(name, args)                => s"${v(name)}.write(${args map v mkString ", "})"
        case BinaryOp(lhs, op, rhs)               => s"(${lhs.toSource})$op(${rhs.toSource})"
        case UnaryOp(op, lhs)                     => s"${op}(${v(lhs)})"
        case Bracket(e)                           => s"(${v(e)})"
        case TernaryOp(cond, lhs, rhs)            => s"(${v(cond)}) ? (${v(lhs)}) : (${v(rhs)}) "
        case BitRep(count, value)                 => s"{${v(count)}{${v(value)}}}"
        case BitCat(parts)                        => s"{${parts map v mkString ", "}}"
        case Literal(value)                       => value
        case Num(None, None, value)               => s"${value}"
        case Num(None, Some(width), value)        => s"/*${width}'*/${value}"
        case Num(Some(false), None, value)        => s"'d${value}"
        case Num(Some(true), None, value)         => s"'sd${value}"
        case Num(Some(false), Some(width), value) => s"${width}'d${value}"
        case Num(Some(true), Some(width), value)  => s"${width}'sd${value}"
      }

      v(tree)
    }
  }

  implicit class AlogicASTPrinter[T <: Node](val tree: T) extends AnyVal {
    def toSource: String = {
      def v(indent: Int)(node: Node): String = {
        val i = "  " * indent
        node match {
          case expr: Expr => expr.toSource

          case Instantiate(id, module, args) => {
            val pas = for ((lhs, rhs) <- args.toList) yield { s"${lhs} = ${v(indent)(rhs)}" }
            s"${i}new $id  = ${module}(${pas mkString ", "});\n"
          }
          case Connect(lhs, rhs)     => s"$lhs -> ${rhs map v(indent) mkString ", "}\n"
          case Function(name, body)  => s"void $name() ${v(indent)(body)}"
          case FenceFunction(body)   => s"void fence() ${v(indent)(body)}"
          case VerilogFunction(body) => s"void verilog() {$body}"
          case FsmTask(name, decls, fns, fencefn, vfns) =>
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

          case NetworkTask(name, decls, inst, conn, vfns) => s"TODO: NetworkTask(name, decls, fns)"
          case VerilogTask(name, decls, fns)              => s"TODO: VerilogTask(name, decls, fns)"
          case Assign(lhs, rhs)                           => s"${v(indent)(lhs)} = ${v(indent)(rhs)};"
          case Update(lhs, op, rhs)                       => s"${v(indent)(lhs)} ${op}= ${v(indent)(rhs)};"
          case Plusplus(lhs)                              => s"${v(indent)(lhs)}++;"
          case Minusminus(lhs)                            => s"${v(indent)(lhs)}--;"
          case DeclarationStmt(decl)                      => s"${decl.toSource};"

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
        }
      }

      v(0)(tree)
    }
  }
}
