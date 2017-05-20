// This class constructs a Verilog file from an AST

package alogic

import scala.collection._
import java.io._

import AstOps._
import scala.annotation.tailrec

import scala.language.implicitConversions

final class MakeVerilog {

  val id2decl = mutable.Map[String, Declaration]()

  val nxMap = mutable.Map[String, String]() // Returns string to use when this identifier is accesses

  val Arrays = mutable.Set[String]()

  val go = "go" // Name of the signal used to decide whether to clock registers

  var numstates = 0

  var log2numstates = 0

  def error(msg: String) {
    println(msg)
    System.exit(-1)
  }

  // Name of signal used to check that a port contains valid data
  def valid(s: String): String =
    s + "_valid"

  // Name of signal used to signal that a port contains ready data
  def ready(s: String): String =
    s + "_ready"

  // Name of signal used to signal that a port contains ready data
  def accept(s: String): String =
    s + "_accept"

  // Compute number of address bits required for a given depth
  def ceillog2(x: Int): Int = {
    var y = 0
    while ((1 << y) < x)
      y += 1
    y
  }

  var outtype = "Unknown"

  def apply(tree: StateProgram, fname: String): Unit = {
    numstates = tree.numStates
    log2numstates = ceillog2(numstates)

    // Collect all the declarations
    VisitAST(tree) {
      case Task(t, _, decls, _) => {
        outtype = t match {
          case Fsm() | Pipeline()    => "reg "
          case Network() | Verilog() => "wire "
        }
        decls foreach { x => id2decl(ExtractName(x)) = x }; true
      }
      // We remove the initializer from declaration statements
      // These will be reset inline where they are declared.
      case DeclarationStmt(VarDeclaration(decltype, id, _)) => { id2decl(ExtractName(id)) = VarDeclaration(decltype, id, None); false }
      case _ => true
    }

    // Emit header and combinatorial code
    var fns: List[StrTree] = Nil // Collection of function code
    var fencefns: List[StrTree] = Nil // Collection of fence function code
    var clears: List[StrTree] = Nil // Collection of outputs to clear if !go
    var defaults: List[StrTree] = Nil // Collection of things to set at start of each cycle
    var clocks: List[StrTree] = Nil // Collection of things to clock if go
    var resets: List[StrTree] = Nil // Collection of things to reset
    var verilogfns: List[StrTree] = Nil // Collection of raw verilog text

    val pw = new PrintWriter(new File(fname))

    def writeSigned(signed: Boolean) = if (signed) "signed " else ""

    def writeSize(size: Int) = if (size > 1) s"[$size-1:0] " else ""

    def writeOut(typ: AlogicType, name: StrTree): Unit = typ match {
      case IntType(b, size) => pw.println("  output " + outtype + writeSigned(b) + writeSize(size) + MakeString(name) + ";")
      case _                => // TODO support IntVType
    }
    def writeIn(typ: AlogicType, name: StrTree): Unit = typ match {
      case IntType(b, size) => pw.println(s"  input wire ${writeSigned(b)}${writeSize(size)}" + MakeString(name) + ";")
      case _                => // TODO support IntVType
    }
    def typeString(typ: AlogicType): String = {
      val typ2 = typ match {
        case State() => IntType(false, log2numstates)
        case x       => x
      }
      typ2 match {
        case IntType(b, size) => writeSigned(b) + writeSize(size)
        case IntVType(b, args) => {
          val sz = MakeString(StrProduct(args.map(MakeExpr)))
          writeSigned(b) + "[" + sz + "-1:0] "
        }
        case _ => error("Cannot make type for $typ"); ""
      }
    }
    def writeVarInternal(typ: AlogicType, name: StrTree, resetToZero: Boolean): Unit = {
      // Convert state to uint type
      val typ2 = typ match {
        case State() => IntType(false, log2numstates)
        case x       => x
      }
      val nm = MakeString(name)
      typ2 match {
        case IntType(_, _) | IntVType(_, _) => {
          pw.println(s"  reg " + typeString(typ2) + MakeString(nx(nm)) + ", " + MakeString(reg(nm)) + ";")
          if (resetToZero)
            resets = StrList(Str("      ") :: Str(reg(name)) :: Str(s" <= 'b0;\n") :: Nil) :: resets
          clocks = StrList(Str("        ") :: Str(reg(name)) :: Str(" <= ") :: Str(nx(name)) :: Str(";\n") :: Nil) :: clocks
          defaults = StrList(Str("    ") :: Str(nx(name)) :: Str(" = ") :: Str(reg(name)) :: Str(";\n") :: Nil) :: defaults
        }
        case _ =>
      }
    }
    def writeVarWithReset(typ: AlogicType, name: StrTree): Unit = writeVarInternal(typ, name, true)
    def writeVarWithNoReset(typ: AlogicType, name: StrTree): Unit = writeVarInternal(typ, name, false)

    // Prepare the mapping for the nx() map
    // Different types of variables need different suffices
    // For efficiency this returns the comma separated list of underlying fields
    // TODO better for Verilator simulation speed if we could detect assignments and expand this concatenation
    def SetNxType(typ: AlogicType, name: String, suffix: String): String = typ match {
      case Struct(fields) => {
        val c = for { f <- fields } yield f match {
          case Field(t, n) => SetNxType(t, name + '_' + n, suffix)
          case _           => ""
        }
        val commaFields = c.mkString(",")
        nxMap(name) = "{" + commaFields + "}"
        commaFields
      }
      case _ => {
        val t = name + suffix
        nxMap(name) = t
        t
      }
    }

    VisitAST(tree) {
      case Task(t, name, decls, fns) => {
        pw.println(s"module $name (")
        pw.println("  input wire clk,")
        pw.println("  input wire rst_n,")

        id2decl.values.foreach({
          case ParamDeclaration(decltype, id, Some(init)) => {
            SetNxType(decltype, id, "")
            pw.println("param " + id + " = " + MakeString(MakeExpr(init)) + ";")
          }
          case ParamDeclaration(decltype, id, None) => {
            SetNxType(decltype, id, "")
            pw.println("param " + id + ";")
          }
          case OutDeclaration(synctype, decltype, name) => {
            if (HasValid(synctype)) {
              pw.println("  output " + outtype + valid(name) + ";")
              clears = Str("      " + valid(name) + " = 1'b0;\n") :: clears
              defaults = Str("    " + valid(name) + " = 1'b0;\n") :: defaults
            }
            if (HasReady(synctype))
              pw.println("  input wire " + ready(name) + ";")
            if (HasAccept(synctype))
              pw.println("  output " + outtype + accept(name) + ";")
            SetNxType(decltype, name, "") // TODO decide on NxType based on synctype
            VisitType(decltype, name)(writeOut) // TODO declare nxt values for outputs

          }
          case InDeclaration(synctype, decltype, name) => {
            if (HasValid(synctype))
              pw.println("  input wire " + valid(name) + ";")
            if (HasReady(synctype)) {
              pw.println("  output " + outtype + ready(name) + ";")
              clears = Str("      " + ready(name) + " = 1'b0;\n") :: clears
              defaults = Str("    " + ready(name) + " = 1'b0;\n") :: defaults
            }
            if (HasAccept(synctype))
              pw.println("  in wire " + accept(name) + ";")
            SetNxType(decltype, name, "")
            VisitType(decltype, name)(writeOut)

          }
          case VerilogDeclaration(decltype, id) => {
            SetNxType(decltype, ExtractName(id), "")
          }
          case _ =>
        })
        pw.println(") begin")
        // declare remaining variables
        id2decl.values.foreach({
          case VarDeclaration(decltype, ArrayLookup(DottedName(names), index), None) => {
            // Arrays only work with non-struct types
            // TODO maybe figure out the number of bits in the type and declare as this many?
            // TODO maybe detect more than one write to the same array in the same cycle?
            val n = names.mkString("_")
            Arrays.add(n)
            val depth = MakeString(MakeExpr(index)).toInt // TODO detect if this fails and fail gracefully
            val log2depth = ceillog2(depth)
            val t = typeString(decltype)
            pw.println(s"  reg ${n}_wr;")
            pw.println(s"  reg ${t}${n}_wrdata;")
            pw.println(s"  reg [${log2depth - 1}:0]${n}_wraddr;")
            SetNxType(decltype, names.head, "")
            defaults = StrList(
              Str(s"    ${n}_wr = 1'b0;\n") ::
                Str(s"    ${n}_wraddr = 'b0;\n") ::
                Str(s"    ${n}_wrdata = 'b0;\n") :: Nil) :: defaults
            clears = Str(s"      ${n}_wr = 1'b0;\n") :: clears
            clocks = Str(s"""
        if (${n}_wr)  ${n}[ ${n}_wraddr ] <= ${n}_wrdata;
""") :: clocks
          }
          case VarDeclaration(decltype, name, None) => {
            val n = ExtractName(name)
            SetNxType(decltype, n, "_nxt")
            VisitType(decltype, n)(writeVarWithReset)

          }
          case VarDeclaration(decltype, name, Some(init)) => {
            val n = ExtractName(name)
            SetNxType(decltype, n, "_nxt")
            VisitType(decltype, n)(writeVarWithNoReset)
            resets = StrList(Str("      ") :: Str(reg(n)) :: Str(s" <= ") :: MakeExpr(init) :: Str(";\n") :: Nil) :: resets
          }
          case _ =>
        })
        true
      }
      case DeclarationStmt(VarDeclaration(decltype, name, init)) => { VisitType(decltype, ExtractName(name))(writeVarWithReset); false } // These resets are done when variable is declared in the code
      case Function(name, body) => { fns = CombStmt(6, body) :: fns; true }
      case FenceFunction(body) => { fencefns = CombStmt(4, body) :: fencefns; true }
      case VerilogFunction(body) => { verilogfns = body :: verilogfns; false }
      case _ => true
    }
    if (verilogfns.length > 0) {
      pw.write(MakeString(verilogfns))
    }
    if (fns.length > 0 || fencefns.length > 0) {
      // Start main combinatorial loop
      pw.println()
      pw.println("  always @* begin")
      pw.println("    go = 1'b1;")
      // Prepare defaults
      if (defaults.length > 0)
        pw.println(MakeString(StrList(defaults)))
      if (fencefns.length > 0)
        pw.println(MakeString(StrList(fencefns)))
      pw.println("    case(state_q) begin")
      pw.println("      default: begin")
      pw.println(MakeString(StrList(fns)))
      pw.println("      end")
      pw.println("    end")
      if (clears.length > 0) {
        pw.println(s"    if (!$go) begin")
        pw.write(MakeString(clears))
        pw.println("    end")
      }
      pw.println("  end")
      // Now emit clocked blocks
      pw.println("")
      pw.println("  always @(posedge clk or negedge rst_n) begin")
      pw.println("    if (!rst_n) begin")
      pw.print(MakeString(StrList(resets)))
      pw.println("    end else begin")
      pw.println(s"      if ($go) begin")
      pw.print(MakeString(StrList(clocks)))
      pw.println("      end")
      pw.println("    end")
      pw.println("  end")
    }
    pw.println("endmodule")
    pw.close()
  }

  // We would prefer to use _d and _q, except that it is more useful
  // for mixing with verilog and for output ports to use the name without a suffix for direct access

  // Construct the string to be used when this identifier is used on the RHS of an assignment
  def nx(names: List[String]): String = nxMap(names.mkString("_")) // TODO inspect type and expand
  def nx(name: StrTree): String = nxMap(MakeString(name))

  // Construct the string to be used when this identifier is used on the LHS of an assignment
  def reg(names: List[String]): String = names.mkString("_") // TODO inspect type and expand
  def reg(name: StrTree): String = MakeString(name)

  implicit def string2StrTree(s: String): StrTree = Str(s)
  implicit def stringList2StrTree(s: List[StrTree]): StrTree = StrList(s)

  // Compute an string tree for the number of bits in this type
  def MakeNumBits(typ: AlogicType): StrTree = typ match {
    case IntType(signed, size)  => Str(s"$size")
    case IntVType(signed, args) => StrProduct(args.map(MakeExpr))
    case State()                => Str(s"$log2numstates")
    case Struct(fields)         => StrSum(fields.map(MakeNumBits))
  }

  def MakeNumBits(typ: FieldType): StrTree = typ match { case Field(typ2, name) => MakeNumBits(typ2) }

  implicit def Decl2Typ(decl: Declaration): AlogicType = decl match {
    case ParamDeclaration(decltype, _, _) => decltype
    case OutDeclaration(_, decltype, _)   => decltype
    case InDeclaration(_, decltype, _)    => decltype
    case VarDeclaration(decltype, _, _)   => decltype
    case VerilogDeclaration(decltype, _)  => decltype
  }

  // Return the type for an AST
  def GetType(tree: AlogicAST): AlogicType = tree match {
    case DottedName(names) => {
      val n = names.mkString("_")
      id2decl(n)
    }
    case ReadCall(name) => GetType(name)
    case _              => { error(s"Cannot compute type for $tree"); State() }
  }

  // Construct a string for an expression
  def MakeExpr(tree: AlogicAST): StrTree = {
    tree match {
      case ArrayLookup(name, index)              => StrList(List(MakeExpr(name), "[", MakeExpr(index), "]"))
      case BinaryArrayLookup(name, lhs, op, rhs) => StrList(List(MakeExpr(name), "[", MakeExpr(lhs), op, MakeExpr(rhs), "]"))
      case ValidCall(DottedName(names)) => id2decl(names.head) match {
        case OutDeclaration(synctype, decl, n) => if (HasValid(synctype)) valid(n) else { error(s"Port $names does not use valid"); "" }
        case InDeclaration(synctype, decl, n)  => if (HasValid(synctype)) valid(n) else { error(s"Port $names does not use valid"); "" }
        case _                                 => error(s"Cannot access valid on $names"); ""
      }
      case FunCall(name, args) => StrList(List(MakeExpr(name), "(", StrCommaList(args.map(MakeExpr)), ")"))
      case Zxt(numbits, expr) => {
        val totalSz = MakeExpr(numbits)
        val exprSz = MakeNumBits(GetType(expr))
        StrList(List("{{", totalSz, " - ", exprSz, "{1'b0}},", MakeExpr(expr), "}"))
      }
      // TODO     case Sxt(numbits, expr) => ...
      case DollarCall(name, args)    => StrList(List(name, "(", StrCommaList(args.map(MakeExpr)), ")"))
      case ReadCall(name)            => MakeExpr(name)
      case BinaryOp(lhs, op, rhs)    => StrList(List(MakeExpr(lhs), Str(" "), op, Str(" "), MakeExpr(rhs)))
      case UnaryOp(op, lhs)          => StrList(List(op, MakeExpr(lhs)))
      case Bracket(content)          => StrList(List("(", MakeExpr(content), ")"))
      case TernaryOp(cond, lhs, rhs) => StrList(List(MakeExpr(cond), " ? ", MakeExpr(lhs), " : ", MakeExpr(rhs)))
      case BitRep(count, value)      => StrList(List("{", MakeExpr(count), "{", MakeExpr(value), "}}"))
      case BitCat(parts)             => StrList(List("{", StrCommaList(parts.map(MakeExpr)), "}"))
      case DottedName(names)         => nx(names)
      case Literal(s)                => StrList(List(""""""", s, """""""))
      case Num(n)                    => n
      case e                         => error(s"Unexpected expression $e"); ""
    }
  }

  // Take a combinatorial statement and return stall conditions that should be emitted now based on ids that are read/written
  // This code will be emitted before the actual statement
  // It is useful to keep as a list of strings here so we can decide when to insert an extra begin/end block
  // Strings have \n at the end, but indent will be added later
  def StallExpr(tree: AlogicAST): List[String] = {
    // Use a local function to avoid having to copy emitted list down the stack
    var blockingStatements: List[String] = Nil

    def add(s: String): Unit = blockingStatements = s :: blockingStatements

    def AddRead(name: DottedName, isLock: Boolean): Boolean = {
      val n: String = ExtractName(name)
      val d: Declaration = id2decl(n)
      d match {
        case InDeclaration(synctype, _, _) => {
          if (HasValid(synctype))
            add(s"$go = $go && ${valid(n)};\n")
          if (!isLock && HasReady(synctype))
            add(ready(n) + " = 1'b1;\n")
        }
        case _ => error(s"$name cannot be read"); false // TODO check this earlier?
      }
      false // No need to recurse
    }

    def v(tree: AlogicAST): Boolean = tree match {
      case CombinatorialCaseStmt(value, _) =>
        VisitAST(value)(v); false
      case CombinatorialBlock(_) => false
      case CombinatorialIf(cond, _, _) =>
        VisitAST(cond)(v); false
      case ReadCall(name)   => AddRead(name, false)
      case LockCall(name)   => AddRead(name, true)
      case UnlockCall(name) => AddRead(name, false)
      case WriteCall(name, _) => {
        val n: String = ExtractName(name)
        val d: Declaration = id2decl(n)
        d match {
          case OutDeclaration(synctype, _, _) => {
            synctype match {
              case SyncReadyBubble() => add(s"$go = $go && !${valid(n)};\n")
              case SyncReady()       => add(s"$go = $go && (!${valid(n)} || !${ready(n)});\n")
              case SyncAccept()      => error("sync accept only supported as wire output type") // TODO check this earlier
              case WireSyncAccept()  => add(s"$go = $go && ${accept(n)};\n")
              case _                 =>
            }
            if (HasValid(synctype))
              add(s"${valid(n)} = 1'b1;\n")
          }
          case _ => error(s"$name cannot be written"); false // TODO check this earlier?
        }
        true // Recurse in case arguments use reads
      }
      case _ => true
    }
    VisitAST(tree)(v)
    return blockingStatements
  }

  // Called with an integer representing a state, returns the appropriate string
  def MakeState(state: Int): String = s"$log2numstates'd$state"

  // This function defines how to write next values (D input on flip-flops)
  def nx(x: String): StrTree = StrList(List(x, Str("_nxt")))

  def AddStall(indent: Int, stalls: List[String], expr: StrTree): StrTree = {
    if (stalls.isEmpty)
      expr
    else
      StrList(Str(" " * indent) :: Str("begin\n") ::
        stalls.map(x => Str(" " * (indent) + x)) :::
        expr ::
        Str(" " * indent) :: Str("end\n") :: Nil)
  }

  // Produce code to go into the case statements (we assume we have already had a "case(state) default: begin"
  // The top call should be with Function or VerilogFunction
  def CombStmt(indent: Int, tree: AlogicAST): StrTree = tree match {
    case Assign(ArrayLookup(DottedName(names), index), "=", rhs) if (Arrays contains names.head) => {
      val i = " " * indent
      val n = names.head
      val r = MakeString(MakeExpr(rhs))
      val d = MakeString(MakeExpr(index))
      val a = Str(s"""${i}begin
$i  ${n}_wr = 1'b1;
$i  ${n}_addr = $r;
$i  ${n}_wrdata = $d;
${i}end
""")
      AddStall(indent, StallExpr(index) ::: StallExpr(rhs), a)
    }
    case Assign(lhs, "=", rhs) => AddStall(indent, StallExpr(lhs) ::: StallExpr(rhs), StrList(List(Str(" " * indent), MakeExpr(lhs), " = ", MakeExpr(rhs), ";\n")))
    case CombinatorialCaseStmt(value, cases) => AddStall(indent, StallExpr(value),
      StrList(Str(" " * indent + "case(") :: MakeExpr(value) :: Str(") begin\n") ::
        StrList(for (c <- cases) yield CombStmt(indent + 4, c)) ::
        Str(" " * indent) :: Str("endcase\n") :: Nil))
    case CombinatorialIf(cond, body, Some(elsebody)) => AddStall(indent, StallExpr(cond),
      StrList(Str(" " * indent) :: Str("if (") :: MakeExpr(cond) :: Str(")\n") ::
        CombStmt(indent + 4, body) ::
        Str(" " * indent) :: Str("else\n") ::
        CombStmt(indent + 4, elsebody) :: Nil))
    case CombinatorialIf(cond, body, None) => AddStall(indent, StallExpr(cond),
      StrList(Str(" " * indent) :: Str("if\n") ::
        CombStmt(indent + 4, body) :: Nil))
    case LockCall(name)   => AddStall(indent, StallExpr(name), Str(""))
    case UnlockCall(name) => AddStall(indent, StallExpr(name), Str(""))
    case WriteCall(name, args) if (args.length == 1) => AddStall(indent, StallExpr(name) ::: StallExpr(args(0)),
      StrList(List(Str(" " * indent), MakeExpr(name), " = ", MakeExpr(args(0)), Str(";\n"))))
    case CombinatorialBlock(cmds) => StrList(Str(" " * indent) :: Str("begin\n") ::
      StrList(for { cmd <- cmds } yield CombStmt(indent + 4, cmd)) ::
      Str(" " * indent) :: Str("end\n") :: Nil)
    case DeclarationStmt(VarDeclaration(decltype, id, Some(rhs))) => CombStmt(indent, Assign(id, "=", rhs))
    case DeclarationStmt(VarDeclaration(decltype, id, None))      => CombStmt(indent, Assign(id, "=", Num("'b0")))
    case AlogicComment(s)                                         => s"// $s\n"
    case StateStmt(state)                                         => StrList(List(" " * (indent - 4), "end\n", " " * (indent - 4), MakeState(state), ": begin\n"))
    case GotoState(target)                                        => StrList(List(" " * indent, nx("state"), " = ", MakeState(target), ";\n"))
    case GotoStmt(target)                                         => StrList(List(" " * indent, nx("state"), " = ", target, ";\n"))
    case CombinatorialCaseLabel(Nil, body) => StrList(
      Str(" " * indent) ::
        Str("default:\n") ::
        CombStmt(indent + 4, body) :: Nil)
    case CombinatorialCaseLabel(conds, body) => StrList(
      Str(" " * indent) ::
        StrCommaList(conds.map(MakeExpr)) ::
        Str(":\n") :: CombStmt(indent + 4, body) :: Nil)
    case DollarCall(name, args) => StrList(List(" " * indent, name, StrCommaList(args.map(MakeExpr))))
    case x                      => error(s"Don't know how to emit code for $x"); Str("")
  }

  // Note that valid can depend on accept
  //      and ready can depend on valid
  //
  // Accept is useful when we want low-latency wire communications from A->B (e.g. A is decoding bits in a tight feedback loop with B)
  // We cannot use sync ready because A's valid depends on B's ready (as this is a wire port), and B's ready depends on A's valid.
  // Instead we use sync accept.
  // B outputs accept if it contains a p.read() in the current state
  // The accept should only be based on B's local registers, and constructed in a separate always_comb block.
  //
  // If we assert accept we are contractually obliged to go if and only if the valid ends up high.
  //
  // Therefore we need to check:
  //   There are no other stalling port reads/writes in this state
  //   None of the variables that affect whether p.read() is called are written in this state, we call forbid to ban assigns to these registers

}
