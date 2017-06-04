// This class constructs a Verilog file from an AST

package alogic

import scala.collection._
import java.io._

import AstOps._
import scala.annotation.tailrec

import scala.language.implicitConversions

final class MakeVerilog {

  val id2decl = mutable.Map[String, Declaration]()

  val nxMap = mutable.Map[String, String]() // Returns string to use when this identifier is accessed
  val regMap = mutable.Map[String, String]() // Map from name in alogic to name in Verilog
  val modMap = mutable.Map[String, ModuleInstance]()
  var modules: List[ModuleInstance] = Nil // Keep a list of all modules instantiated (not including this)
  val unames = mutable.Set[String]() // Set of names used to instantiate modules

  val Arrays = mutable.Set[String]()

  val go = "go" // Name of the signal used to decide whether to clock registers

  var numstates = 0

  var log2numstates = 0

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

  def getModule(ast: AlogicAST): (DottedName, ModuleInstance) = {
    val f = ast.asInstanceOf[DottedName]
    val n = f.names(0)
    if (modMap contains n) {
      (f, modMap(n))
    } else {
      Message.fatal(s"Unknown module name $n")
      (DottedName(Nil), new ModuleInstance("Unknown", "Unknown", Nil))
    }
  }

  var outtype = "Unknown"

  var modname = "Unknown" // Save the name of this module

  var makingAccept = false // We use this to decide how to emit expressions

  def apply(tree: StateProgram, fname: String): Unit = {
    numstates = tree.numStates
    log2numstates = ceillog2(numstates)

    // Collect all the declarations
    VisitAST(tree) {
      case Task(t, n, decls, _) => {
        modname = n
        outtype = t match {
          case Fsm | Pipeline    => "reg "
          case Network | Verilog => "wire "
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
    var acceptfns: List[StrTree] = Nil // Collection of code to generate accept outputs

    val pw = new PrintWriter(new File(fname))

    def writeSigned(signed: Boolean) = if (signed) "signed " else ""

    def writeSize(size: Int) = if (size > 1) s"[$size-1:0] " else ""

    def writeOut(typ: AlogicType, name: StrTree): Unit = typ match {
      case IntType(b, size) => pw.println("  output " + outtype + writeSigned(b) + writeSize(size) + name + ";")
      case _                => // TODO support IntVType
    }
    def writeIn(typ: AlogicType, name: StrTree): Unit = typ match {
      case IntType(b, size) => pw.println(s"  input wire ${writeSigned(b)}${writeSize(size)}" + name + ";")
      case _                => // TODO support IntVType
    }
    def writeWire(typ: AlogicType, name: StrTree): Unit = typ match {
      case IntType(b, size) => pw.println(s"  wire ${writeSigned(b)}${writeSize(size)}" + name + ";")
      case _                => // TODO support IntVType
    }
    def typeString(typ: AlogicType): String = {
      val typ2 = typ match {
        case State => IntType(false, log2numstates)
        case x     => x
      }
      typ2 match {
        case IntType(b, size) => writeSigned(b) + writeSize(size)
        case IntVType(b, args) => {
          val sz = StrList(args.map(MakeExpr), "*").toString
          writeSigned(b) + "[" + sz + "-1:0] "
        }
        case _ => Message.fatal(s"Cannot make type for $typ"); ""
      }
    }
    def writeVarInternal(typ: AlogicType, name: StrTree, resetToZero: Boolean): Unit = {
      // Convert state to uint type
      val typ2 = typ match {
        case State => IntType(false, log2numstates)
        case x     => x
      }
      val nm = name.toString
      typ2 match {
        case IntType(_, _) | IntVType(_, _) => {
          pw.println(s"  reg " + typeString(typ2) + nx(nm) + ", " + reg(nm) + ";")
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
    def SetNxType(m: mutable.Map[String, String], typ: AlogicType, name: String, suffix: String): String = typ match {
      case Struct(fields) => {
        val c = for { f <- fields } yield f match {
          case Field(t, n) => SetNxType(m, t, name + '_' + n, suffix)
          case _           => ""
        }
        val commaFields = c.mkString(",")
        m(name) = "{" + commaFields + "}"
        commaFields
      }
      case _ => {
        val t = name + suffix
        m(name) = t
        t
      }
    }

    var generateAccept = false // As an optimization, don't bother generating accept for modules without any ports that require it

    VisitAST(tree) {
      case Task(t, name, decls, fns) => {
        pw.print(s"module $name ")

        val paramDecls = id2decl.values collect {
          case x: ParamDeclaration => x
        }

        if (paramDecls.isEmpty) {
          pw.println(s"(")
        } else {
          pw.println(s"#(")
          paramDecls foreach {
            case ParamDeclaration(decltype, id, init) => {
              SetNxType(nxMap, decltype, id, "")
              SetNxType(regMap, decltype, id, "")
              pw.println("  parameter " + id + " = " + MakeExpr(init) + ";")
            }
          }
          pw.println(s") (")
        }

        pw.println("  input wire clk,")
        pw.println("  input wire rst_n,")

        id2decl.values.foreach({
          case OutDeclaration(synctype, decltype, name) => {
            if (HasValid(synctype)) {
              pw.println("  output " + outtype + valid(name) + ";")
              clears = Str("      " + valid(name) + " = 1'b0;\n") :: clears
              defaults = Str("    " + valid(name) + " = 1'b0;\n") :: defaults
            }
            if (HasReady(synctype))
              pw.println("  input wire " + ready(name) + ";")
            if (HasAccept(synctype)) {
              pw.println("  output " + outtype + accept(name) + ";")
              generateAccept = true;
            }
            SetNxType(nxMap, decltype, name, "") // TODO decide on NxType based on synctype
            SetNxType(regMap, decltype, name, "")
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
            SetNxType(nxMap, decltype, name, "")
            SetNxType(regMap, decltype, name, "")
            VisitType(decltype, name)(writeOut)

          }
          case VerilogDeclaration(decltype, id) => {
            SetNxType(nxMap, decltype, ExtractName(id), "")
            SetNxType(regMap, decltype, ExtractName(id), "")
          }
          case _ =>
        })
        pw.println(")")

        // declare remaining variables
        id2decl.values.foreach({
          case VarDeclaration(decltype, ArrayLookup(DottedName(names), index), None) => {
            // Arrays only work with non-struct types
            // TODO maybe figure out the number of bits in the type and declare as this many?
            // TODO maybe detect more than one write to the same array in the same cycle?
            val n = names.mkString("_")
            Arrays.add(n)
            val depth = MakeExpr(index).toString.toInt // TODO detect if this fails and fail gracefully
            val log2depth = ceillog2(depth)
            val t = typeString(decltype)
            pw.println(s"  reg ${n}_wr;")
            pw.println(s"  reg ${t}${n}_wrdata;")
            pw.println(s"  reg [${log2depth - 1}:0]${n}_wraddr;")
            SetNxType(nxMap, decltype, names.head, "")
            SetNxType(regMap, decltype, names.head, "")
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
            SetNxType(nxMap, decltype, n, "_nxt")
            SetNxType(regMap, decltype, n, "")
            VisitType(decltype, n)(writeVarWithReset)

          }
          case VarDeclaration(decltype, name, Some(init)) => {
            val n = ExtractName(name)
            SetNxType(nxMap, decltype, n, "_nxt")
            SetNxType(regMap, decltype, n, "")
            VisitType(decltype, n)(writeVarWithNoReset)
            resets = StrList(Str("      ") :: Str(reg(n)) :: Str(s" <= ") :: MakeExpr(init) :: Str(";\n") :: Nil) :: resets
          }
          case _ =>
        })
        true
      }
      case DeclarationStmt(VarDeclaration(decltype, name, init)) => false
      case Function(name, body) => {
        fns = CombStmt(6, body) :: fns
        if (generateAccept) {
          makingAccept = true
          AcceptStmt(6, body) match {
            case Some(a) => acceptfns = a :: acceptfns
            case None    =>
          }
          makingAccept = false
        }
        true
      }
      case FenceFunction(body)   => { fencefns = CombStmt(4, body) :: fencefns; false }
      case VerilogFunction(body) => { verilogfns = body :: verilogfns; false }
      case Instantiate(id, module, args) => {
        if (modMap.isEmpty)
          modMap("this") = new ModuleInstance("this", modname, Nil);
        var c = 0;
        var uname = id + "_" + c
        while (unames contains uname) {
          c += 1
          uname = id + "_" + c
        }
        unames.add(uname)
        val m = new ModuleInstance(uname, module, args);
        modMap(id) = m
        modules = m :: modules
        false
      }
      case Connect(from, to) => {
        val (n, m) = getModule(from)
        m.connect(n, to map getModule); false
      }
      case _ => true
    }
    if (verilogfns.length > 0) {
      pw.print(StrList(verilogfns))
    }
    if (fns.length > 0 || fencefns.length > 0) {
      // Start main combinatorial loop
      pw.println()
      pw.println("  always @* begin")
      pw.println("    go = 1'b1;")
      // Prepare defaults
      if (defaults.length > 0)
        pw.println(StrList(defaults))
      if (fencefns.length > 0)
        pw.println(StrList(fencefns))
      pw.println("    case(state_q) begin")
      pw.println("      default: begin")
      pw.println(StrList(fns))
      pw.println("      end")
      pw.println("    end")
      if (clears.length > 0) {
        pw.println(s"    if (!$go) begin")
        pw.print(StrList(clears))
        pw.println("    end")
      }
      pw.println("  end")
      // Now emit clocked blocks
      pw.println("")
      pw.println("  always @(posedge clk or negedge rst_n) begin")
      pw.println("    if (!rst_n) begin")
      pw.print(StrList(resets))
      pw.println("    end else begin")
      pw.println(s"      if ($go) begin")
      pw.print(StrList(clocks))
      pw.println("      end")
      pw.println("    end")
      pw.println("  end")
    }
    if (!modMap.isEmpty) {
      val t = modMap("this")
      def declareWires(m: ModuleInstance): Unit = {
        for ((p, decl) <- m.outs) {
          VisitType(decl.decltype, m.outwires(p) + '_' + decl.name)(writeWire)
          // TODO add valid and ready and accept wires?
          // Perhaps add a VisitSyncType?
        }
      }
      declareWires(t)
      for (m <- modules)
        declareWires(m)
      // Make modules
      for ((m, unitnum) <- modules.zipWithIndex) {
        // TODO m.instantiateModule(unitnum, pw)
      }
      // TODO Connect top-level ports to the appropriate wires
    }
    pw.println("endmodule")
    pw.close()
  }

  // We would prefer to use _d and _q, except that it is more useful
  // for mixing with verilog and for output ports to use the name without a suffix for direct access

  // Construct the string to be used when this identifier is used on the RHS of an assignment
  def nx(name: String): String = if (makingAccept) {
    IdsUsedToMakeAccept.add(name)
    name
  } else nxMap(name)

  def nx(names: List[String]): String = nx(names.mkString("_"))
  def nx(name: StrTree): String = nx(name.toString)

  // Construct the string to be used when this identifier is used on the LHS of an assignment
  def reg(names: List[String]): String = regMap(names.mkString("_"))
  def reg(name: StrTree): String = regMap(name.toString)

  implicit def string2StrTree(s: String): StrTree = Str(s)

  // Compute an string tree for the number of bits in this type
  def MakeNumBits(typ: AlogicType): StrTree = typ match {
    case IntType(signed, size)  => Str(s"$size")
    case IntVType(signed, args) => StrList(args.map(MakeExpr), "*")
    case State                  => Str(s"$log2numstates")
    case Struct(fields)         => StrList(fields.map(MakeNumBits), "+")
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
    case _              => { Message.fatal(s"Cannot compute type for $tree"); State }
  }

  // Construct a string for an expression
  def MakeExpr(tree: AlogicAST): StrTree = {
    tree match {
      case ArrayLookup(name, index)              => StrList(List(MakeExpr(name), "[", MakeExpr(index), "]"))
      case BinaryArrayLookup(name, lhs, op, rhs) => StrList(List(MakeExpr(name), "[", MakeExpr(lhs), op, MakeExpr(rhs), "]"))
      case ValidCall(DottedName(names)) => id2decl(names.head) match {
        case OutDeclaration(synctype, decl, n) => if (HasValid(synctype)) valid(n) else { Message.fatal(s"Port $names does not use valid"); "" }
        case InDeclaration(synctype, decl, n)  => if (HasValid(synctype)) valid(n) else { Message.fatal(s"Port $names does not use valid"); "" }
        case _                                 => Message.fatal(s"Cannot access valid on $names"); ""
      }
      case FunCall(name, args) => StrList(List(MakeExpr(name), "(", StrList(args.map(MakeExpr), ","), ")"))
      case Zxt(numbits, expr) => {
        val totalSz = MakeExpr(numbits)
        val exprSz = MakeNumBits(GetType(expr))
        StrList(List("{{", totalSz, " - ", exprSz, "{1'b0}},", MakeExpr(expr), "}"))
      }
      case Sxt(numbits, expr) => {
        val totalSz = MakeExpr(numbits)
        val exprSz = MakeNumBits(GetType(expr))
        val e = MakeExpr(expr)
        StrList(List("{{", totalSz, " - ", exprSz, "{", e, "[(", exprSz, ") - 1]}},", e, "}"))
      }
      case DollarCall(name, args)    => StrList(List(name, "(", StrList(args.map(MakeExpr), ","), ")"))
      case ReadCall(name)            => MakeExpr(name)
      case BinaryOp(lhs, op, rhs)    => StrList(List(MakeExpr(lhs), Str(" "), op, Str(" "), MakeExpr(rhs)))
      case UnaryOp(op, lhs)          => StrList(List(op, MakeExpr(lhs)))
      case Bracket(content)          => StrList(List("(", MakeExpr(content), ")"))
      case TernaryOp(cond, lhs, rhs) => StrList(List(MakeExpr(cond), " ? ", MakeExpr(lhs), " : ", MakeExpr(rhs)))
      case BitRep(count, value)      => StrList(List("{", MakeExpr(count), "{", MakeExpr(value), "}}"))
      case BitCat(parts)             => StrList(List("{", StrList(parts.map(MakeExpr), ","), "}"))
      case DottedName(names)         => nx(names)
      case Literal(s)                => StrList(List(""""""", s, """""""))
      case Num(n)                    => n
      case e                         => Message.fatal(s"Unexpected expression $e"); ""
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
        case _ => Message.fatal(s"$name cannot be read"); false // TODO check this earlier?
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
              case SyncReadyBubble => add(s"$go = $go && !${valid(n)};\n")
              case SyncReady       => add(s"$go = $go && (!${valid(n)} || !${ready(n)});\n")
              case SyncAccept      => Message.fatal("sync accept only supported as wire output type") // TODO check this earlier
              case WireSyncAccept  => add(s"$go = $go && ${accept(n)};\n")
              case _               =>
            }
            if (HasValid(synctype))
              add(s"${valid(n)} = 1'b1;\n")
          }
          case _ => Message.fatal(s"$name cannot be written"); false // TODO check this earlier?
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
  //def nx(x: String): StrTree = StrList(List(x, Str("_nxt")))

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
    case Assign(ArrayLookup(DottedName(names), index), rhs) if (Arrays contains names.head) => {
      val i = " " * indent
      val n = names.head
      val r = MakeExpr(rhs).toString
      val d = MakeExpr(index).toString
      val a = Str(s"""${i}begin
$i  ${n}_wr = 1'b1;
$i  ${n}_addr = $r;
$i  ${n}_wrdata = $d;
${i}end
""")
      AddStall(indent, StallExpr(index) ::: StallExpr(rhs), a)
    }
    case Assign(lhs, rhs) => AddStall(indent, StallExpr(lhs) ::: StallExpr(rhs), StrList(List(Str(" " * indent), MakeExpr(lhs), " = ", MakeExpr(rhs), ";\n")))
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
      StrList(Str(" " * indent) :: Str("if (") :: MakeExpr(cond) :: Str(")\n") ::
        CombStmt(indent + 4, body) :: Nil))
    case LockCall(name)   => AddStall(indent, StallExpr(name), Str(""))
    case UnlockCall(name) => AddStall(indent, StallExpr(name), Str(""))
    case WriteCall(name, args) if (args.length == 1) => AddStall(indent, StallExpr(name) ::: StallExpr(args(0)),
      StrList(List(Str(" " * indent), MakeExpr(name), " = ", MakeExpr(args(0)), Str(";\n"))))
    case CombinatorialBlock(cmds) => StrList(Str(" " * indent) :: Str("begin\n") ::
      StrList(for { cmd <- cmds } yield CombStmt(indent + 4, cmd)) ::
      Str(" " * indent) :: Str("end\n") :: Nil)
    case DeclarationStmt(VarDeclaration(decltype, id, Some(rhs))) => CombStmt(indent, Assign(id, rhs))
    case DeclarationStmt(VarDeclaration(decltype, id, None))      => CombStmt(indent, Assign(id, Num("'b0")))
    case AlogicComment(s)                                         => s"// $s\n"
    case StateBlock(state, cmds) => StrList(List(" " * (indent - 4), "end\n", " " * (indent - 4), MakeState(state), ": begin\n",
      StrList(for { cmd <- cmds } yield CombStmt(indent, cmd))))
    //case StateStmt(state)  => Str(s"Bad state stmt for $state")
    case GotoState(target) => StrList(List(" " * indent, nx("state"), " = ", MakeState(target), ";\n"))
    case GotoStmt(target)  => StrList(List(" " * indent, nx("state"), " = ", target, ";\n"))
    case CombinatorialCaseLabel(Nil, body) => StrList(
      Str(" " * indent) ::
        Str("default:\n") ::
        CombStmt(indent + 4, body) :: Nil)
    case CombinatorialCaseLabel(conds, body) => StrList(
      Str(" " * indent) ::
        StrList(conds.map(MakeExpr), ",") ::
        Str(":\n") :: CombStmt(indent + 4, body) :: Nil)
    case DollarCall(name, args) => StrList(List(" " * indent, name, StrList(args.map(MakeExpr), ",")))
    case x                      => Message.fatal(s"Don't know how to emit code for $x"); Str("")
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
  //
  // As an optimization we can skip all of this if there are no sync accept ports
  //
  // While we are generating accept, the nx function is adjusted to point to the reg version instead.
  // It also captures the ids that are used so we can check they are stable

  val IdsUsedToMakeAccept = mutable.Set[String]() // This allows us to flag errors where the accept signal would be generated incorrectly
  val IdsWritten = mutable.Set[String]()

  var syncPortsFound: Int = 0 // This allows us to flag errors if we use two accept ports in the same state

  var usesPort: Option[String] = None // This allows us to flag errors if another port is read

  // Combine a list of stalls (often empty) with an optional body (often empty)
  // Return None if result is empty
  def AddAccept(indent: Int, stalls: List[String], expr: Option[StrTree]): Option[StrTree] = {
    if (stalls.isEmpty)
      expr
    else {
      val e: List[StrTree] = expr match {
        case Some(x) => x :: Nil
        case None    => Nil
      }
      Some(StrList(Str(" " * indent) :: Str("begin\n") ::
        stalls.map(x => Str(" " * (indent) + x)) :::
        e :::
        Str(" " * indent) :: Str("end\n") :: Nil))
    }
  }

  def AcceptStmt(indent: Int, tree: AlogicAST): Option[StrTree] = tree match {
    case Assign(lhs, rhs) => {
      IdsWritten.add(ExtractName(lhs))
      AddAccept(indent, AcceptExpr(lhs) ::: AcceptExpr(rhs), None)
    }
    case CombinatorialCaseStmt(value, cases) => {
      // Take care to only use MakeExpr when we are sure the code needs to be emitted
      // This is because MakeExpr will track the used ids
      val s: List[Option[StrTree]] = for (c <- cases) yield AcceptStmt(indent + 4, c)
      val s2: List[StrTree] = s.flatten
      val e = if (s2.length == 0)
        None
      else
        Some(StrList(Str(" " * indent + "case(") :: MakeExpr(value) :: Str(") begin\n") ::
          StrList(s2) :: Str(" " * indent) :: Str("endcase\n") :: Nil))
      AddAccept(indent, AcceptExpr(value), e)
    }
    case CombinatorialIf(cond, body, Some(elsebody)) =>
      {
        val b = AcceptStmt(indent + 4, body)
        val eb = AcceptStmt(indent + 4, elsebody)
        val gen = b.isDefined || eb.isDefined
        val bs = b match {
          case Some(a) => a
          case None    => Str(" " * indent + "begin\n" + " " * indent + "end\n")
        }
        val ebs = eb match {
          case Some(a) => a
          case None    => Str(" " * indent + "begin\n" + " " * indent + "end\n")
        }
        val e = if (gen)
          Some(StrList(Str(" " * indent) :: Str("if (") :: MakeExpr(cond) :: Str(")\n") :: bs :: Str(" " * indent) :: Str("else\n") :: ebs :: Nil))
        else
          None
        AddAccept(indent, AcceptExpr(cond), e)
      }
    case CombinatorialIf(cond, body, None) => {
      val b = AcceptStmt(indent + 4, body)
      // We take care to only call MakeExpr when the body would have something to generate
      // This avoids forbidding ids that are not actually important for generating accept
      val e = b match {
        case None    => None
        case Some(a) => Some(StrList(Str(" " * indent) :: Str("if (") :: MakeExpr(cond) :: Str(")\n") :: a :: Nil))
      }
      AddAccept(indent, AcceptExpr(cond), e)
    }

    case LockCall(name)                              => AddAccept(indent, AcceptExpr(name), None)
    case UnlockCall(name)                            => AddAccept(indent, AcceptExpr(name), None)
    case WriteCall(name, args) if (args.length == 1) => AddAccept(indent, AcceptExpr(name) ::: AcceptExpr(args(0)), None)
    case CombinatorialBlock(cmds) => {
      val s: List[Option[StrTree]] = for (c <- cmds) yield AcceptStmt(indent + 4, c)
      val s2: List[StrTree] = s.flatten
      if (s2.length == 0)
        None
      else
        Some(StrList(Str(" " * indent) :: Str("begin\n") :: StrList(s2) :: Str(" " * indent) :: Str("end\n") :: Nil))
    }

    case DeclarationStmt(VarDeclaration(decltype, id, Some(rhs))) => AcceptStmt(indent, Assign(id, rhs))
    case StateBlock(state, cmds) => {
      // Clear sets used for tracking
      syncPortsFound = 0
      IdsUsedToMakeAccept.clear()
      IdsWritten.clear()
      // See if there is anything to do for this state
      val s = for { cmd <- cmds } yield AcceptStmt(indent, cmd)
      val s2 = s.flatten
      if (s2.length > 0) {
        // Check for error conditions
        if (syncPortsFound > 1) Message.fatal(s"Found multiple accept port reads in same cycle: $cmds")
        if (usesPort.isDefined) Message.fatal(s"Cannot access port $usesPort while generating accept: $cmds")
        if (!IdsUsedToMakeAccept.intersect(IdsWritten).isEmpty) Message.fatal(s"Cannot generate accept because an identifier is being written to: $cmds")
        Some(StrList(List(" " * (indent - 4), MakeState(state), ": begin\n", StrList(s2), " " * (indent - 4), "end\n")))
      } else
        None
    }
    case CombinatorialCaseLabel(Nil, body) => {
      val b = AcceptStmt(indent + 4, body)
      b match {
        case None    => None
        case Some(a) => Some(StrList(Str(" " * indent) :: Str("default:\n") :: a :: Nil))
      }
    }
    case CombinatorialCaseLabel(conds, body) => {
      val b = AcceptStmt(indent + 4, body)
      val e = b match {
        case None => None
        case Some(a) => Some(StrList(
          Str(" " * indent) ::
            StrList(conds.map(MakeExpr), ",") ::
            Str(":\n") :: a :: Nil))
      }
      val as = conds.map(AcceptExpr).flatten
      AddAccept(indent, as, e)
    }

    case x => None
  }

  // Take a combinatorial statement and return accept statements that should be emitted now based on ids that are read/written
  // This code will be emitted before the actual statement
  // It is useful to keep as a list of strings here so we can decide when to insert an extra begin/end block
  // Strings have \n at the end, but indent will be added later
  def AcceptExpr(tree: AlogicAST): List[String] = {
    // Use a local function to avoid having to copy emitted list down the stack
    var blockingStatements: List[String] = Nil

    def add(s: String): Unit = blockingStatements = s :: blockingStatements

    def AddRead(name: DottedName): Boolean = {
      val n: String = ExtractName(name)
      id2decl(n) match {
        case InDeclaration(synctype, _, _) => {
          if (HasAccept(synctype)) {
            syncPortsFound += 1
            add(accept(n) + " = 1'b1;\n")
          }
          if (HasReady(synctype))
            usesPort = Some(MakeExpr(name).toString)
        }
        case _ =>
      }
      false // No need to recurse
    }

    def v(tree: AlogicAST): Boolean = tree match {
      case CombinatorialCaseStmt(value, _) =>
        VisitAST(value)(v); false
      case CombinatorialBlock(_) => false
      case CombinatorialIf(cond, _, _) =>
        VisitAST(cond)(v); false
      case ReadCall(name)   => AddRead(name)
      case LockCall(name)   => AddRead(name)
      case UnlockCall(name) => AddRead(name)
      case WriteCall(name, _) => {
        val n: String = ExtractName(name)
        val d: Declaration = id2decl(n)
        d match {
          case OutDeclaration(synctype, _, _) => {
            if (HasValid(synctype))
              usesPort = Some(MakeExpr(name).toString)
          }
          case _ => false
        }
        true // Recurse in case arguments use reads
      }
      case _ => true
    }
    VisitAST(tree)(v)
    return blockingStatements
  }

}
