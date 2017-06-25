// This class constructs a Verilog file from an AST

package alogic

import java.io._

import scala.collection._
import scala.collection.mutable.Stack
import scala.language.implicitConversions

import alogic.ast._
import alogic.ast.AstOps._
import alogic.ast.ExprOps._

final class MakeVerilog(moduleCatalogue: Map[String, Task]) {

  val i0 = "  "; // Single indentation depth (2 spaces)

  val id2decl = mutable.Map[String, Declaration]()

  val nxMap = mutable.Map[String, String]() // Returns string to use when this identifier is accessed
  val regMap = mutable.Map[String, String]() // Map from name in alogic to name in Verilog
  val modMap = mutable.Map[String, ModuleInstance]() withDefault {
    name => Message.fatal(s"Unknown module name '$name'")
  }

  // Map of names used to instantiate modules to multiplicity of that name
  val namecnt = mutable.Map[String, Int]() withDefaultValue (0)

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

  var makingAccept = false // We use this to decide how to emit expressions

  def apply(task: Task, fname: String): Unit = {
    val Task(modname, decls) = task // Save the name of this module

    val outtype = task match {
      case _: StateTask   => "reg "
      case _: FsmTask     => "reg "
      case _: NetworkTask => "wire "
      case _: VerilogTask => "wire "
    }

    numstates = task match {
      case StateTask(_, _, s, _, _) => s.length
      case _                        => 0
    }
    log2numstates = ceillog2(numstates)

    // Collect all the declarations
    decls foreach { x => id2decl(ExtractName(x)) = x }
    task visit {
      // We remove the initializer from declaration statements
      // These will be reset inline where they are declared.
      case DeclarationStmt(VarDeclaration(decltype, id, _)) => { id2decl(ExtractName(id)) = VarDeclaration(decltype, id, None); false }
      case _ => true
    }

    // Emit header and combinatorial code
    val states = mutable.Map[Int, StrTree]() // Collection of state code
    val fencefns = Stack[StrTree]() // Collection of fence function code
    val clears = Stack[StrTree]() // Collection of outputs to clear if !go
    val defaults = Stack[StrTree]() // Collection of things to set at start of each cycle
    val clocks = Stack[StrTree]() // Collection of things to clock if go
    val clocks_no_reset = Stack[StrTree]() // Collection of things to clock if go but do not need reset
    val resets = Stack[StrTree]() // Collection of things to reset
    val verilogfns = Stack[StrTree]() // Collection of raw verilog text
    val acceptfns = Stack[StrTree]() // Collection of code to generate accept outputs
    val modules = Stack[ModuleInstance]() // Keep a list of all modules instantiated (not including this)

    val pw = new PrintWriter(new File(fname))

    def writeSigned(signed: Boolean) = if (signed) "signed " else ""

    def writeSize(size: Int) = if (size > 1) s"[$size-1:0] " else ""

    def writeOut(typ: Type, name: StrTree): Unit = typ match {
      case IntType(b, size) => pw.println("  output " + outtype + writeSigned(b) + writeSize(size) + name + ",")
      case _                => // TODO support IntVType
    }
    def writeIn(typ: Type, name: StrTree): Unit = typ match {
      case IntType(b, size) => pw.println(s"  input wire ${writeSigned(b)}${writeSize(size)}" + name + ",")
      case _                => // TODO support IntVType
    }
    def typeString(typ: Type): String = {
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
    def writeVarInternal(typ: Type, name: StrTree, resetToZero: Boolean): Unit = {
      // Convert state to uint type
      val typ2 = typ match {
        case State => IntType(false, log2numstates)
        case x     => x
      }
      val nm = name.toString
      typ2 match {
        case IntType(_, _) | IntVType(_, _) => {
          pw.println(s"  reg " + typeString(typ2) + nx(nm) + ", " + reg(nm) + ";")
          if (resetToZero) {
            resets push StrList(Str("      ") :: Str(reg(name)) :: Str(s" <= 'b0;\n") :: Nil)
          }
          clocks push StrList(Str("        ") :: Str(reg(name)) :: Str(" <= ") :: Str(nx(name)) :: Str(";\n") :: Nil)
          defaults push StrList(Str("    ") :: Str(nx(name)) :: Str(" = ") :: Str(reg(name)) :: Str(";\n") :: Nil)
        }
        case _ =>
      }
    }
    def writeVarWithReset(typ: Type, name: StrTree): Unit = writeVarInternal(typ, name, true)
    def writeVarWithNoReset(typ: Type, name: StrTree): Unit = writeVarInternal(typ, name, false)

    // Prepare the mapping for the nx() map
    // Different types of variables need different suffices
    // For efficiency this returns the comma separated list of underlying fields
    // TODO better for Verilator simulation speed if we could detect assignments and expand this concatenation
    def SetNxType(m: mutable.Map[String, String], typ: Type, name: String, suffix: String): String = typ match {
      case Struct(fields) => {
        val c = for ((n, t) <- fields) yield {
          SetNxType(m, t, name + '_' + n, suffix)
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

    id2decl.values foreach {
      case InDeclaration(synctype, decltype, name) => {
        SetNxType(nxMap, decltype, name, "")
        SetNxType(regMap, decltype, name, "")
      }
      case OutDeclaration(synctype, decltype, name) => {
        SetNxType(nxMap, decltype, name, "") // TODO decide on NxType based on synctype
        SetNxType(regMap, decltype, name, "")
      }
      case ParamDeclaration(decltype, id, init) => {
        SetNxType(nxMap, decltype, id, "")
        SetNxType(regMap, decltype, id, "")
      }
      case ConstDeclaration(decltype, id, init) => {
        SetNxType(nxMap, decltype, id, "")
        SetNxType(regMap, decltype, id, "")
      }
      case VerilogDeclaration(decltype, id) => {
        SetNxType(nxMap, decltype, ExtractName(id), "")
        SetNxType(regMap, decltype, ExtractName(id), "")
      }
      case VarDeclaration(decltype, ArrayLookup(DottedName(name :: Nil), _), None) => {
        SetNxType(nxMap, decltype, name, "")
        SetNxType(regMap, decltype, name, "")
      }
      case VarDeclaration(decltype, DottedName(name :: Nil), _) => {
        SetNxType(nxMap, decltype, name, "_nxt")
        SetNxType(regMap, decltype, name, "")
      }
      case x => Message.fatal(s"Don't know how to handle ${x}") // TODO: turn this into ice
    }

    var generateAccept = false // As an optimization, don't bother generating accept for modules without any ports that require it

    pw.println(s"`default_nettype none")
    pw.println()

    pw.print(s"module $modname ")

    val paramDecls = id2decl.values collect {
      case x: ParamDeclaration => x
    }

    if (paramDecls.isEmpty) {
      pw.println(s"(")
    } else {
      pw.println(s"#(")
      // Emit parameter declarations
      val s = for (ParamDeclaration(decltype, id, init) <- paramDecls) yield {
        decltype match {
          case IntType(b, size) => s"${i0}parameter " + writeSigned(b) + writeSize(size) + id + "=" + MakeExpr(init)
          case x                => ??? // TODO support IntVType
        }
      }
      pw.println(s mkString ",\n")
      pw.println(s") (")
    }

    // Emit port declarations
    id2decl.values foreach {
      case OutDeclaration(synctype, decltype, name) => {
        if (HasValid(synctype)) {
          pw.println("  output " + outtype + valid(name) + ",")
          clears push Str("      " + valid(name) + " = 1'b0;\n")
          defaults push Str("    " + valid(name) + " = 1'b0;\n")
        }
        if (HasReady(synctype))
          pw.println("  input wire " + ready(name) + ",")
        if (HasAccept(synctype)) {
          pw.println("  output " + outtype + accept(name) + ",")
          generateAccept = true;
        }
        VisitType(decltype, name)(writeOut) // TODO declare nxt values for outputs

      }
      case InDeclaration(synctype, decltype, name) => {
        if (HasValid(synctype))
          pw.println("  input wire " + valid(name) + ",")
        if (HasReady(synctype)) {
          pw.println("  output " + outtype + ready(name) + ",")
          clears push Str("      " + ready(name) + " = 1'b0;\n")
          defaults push Str("    " + ready(name) + " = 1'b0;\n")
        }
        if (HasAccept(synctype))
          pw.println("  input wire " + accept(name) + ",")
        VisitType(decltype, name)(writeOut)

      }
      case _ =>
    }

    //Goes on bottom so we don't need to special case the lack of comma after the last port
    pw.println("  input wire clk,")
    pw.println("  input wire rst_n")
    pw.println(");\n")

    // Emit localparam (const) declaratoins
    id2decl.values foreach {
      case ConstDeclaration(decltype, id, init) => {
        decltype match {
          case IntType(b, size) => {
            pw.println(s"  localparam " + writeSigned(b) + writeSize(size) + id + "=" + MakeExpr(init) + ";")
          }
          case x => ??? //() // TODO support IntVType
        }
      }
      case _ =>
    }

    // Emit remaining variable declarations
    id2decl.values foreach {
      case VarDeclaration(decltype, ArrayLookup(DottedName(names), index :: Nil), None) => {
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
        pw.println(s"  reg [${log2depth - 1}:0] ${n}_wraddr;")
        pw.println(s"  reg ${n} [${depth - 1}:0];")
        defaults push StrList(
          Str(s"    ${n}_wr = 1'b0;\n") ::
            Str(s"    ${n}_wraddr = 'b0;\n") ::
            Str(s"    ${n}_wrdata = 'b0;\n") :: Nil)
        clears push Str(s"      ${n}_wr = 1'b0;\n")
        clocks_no_reset push Str(s"""|${i0 * 3}if (${n}_wr) begin
                                         |${i0 * 4}${n}[${n}_wraddr] <= ${n}_wrdata;
                                         |${i0 * 3}end
                                         |""".stripMargin)
      }
      case VarDeclaration(decltype, name, None) => {
        val n = ExtractName(name)
        VisitType(decltype, n)(writeVarWithReset)
      }
      case VarDeclaration(decltype, name, Some(init)) => {
        val n = ExtractName(name)
        VisitType(decltype, n)(writeVarWithNoReset)
        resets push StrList(Str("      ") :: Str(reg(n)) :: Str(s" <= ") :: MakeExpr(init) :: Str(";\n") :: Nil)
      }
      case _ =>
    }

    val portConnections = Stack[(ModuleInstance, Port, ModuleInstance, Port)]()

    // Walk the tree and construct always block contents
    task visit {
      case blk @ StateBlock(n, _) => {
        val indent = if (numstates == 1) 2 else 3
        states(n) = MakeStmt(indent)(blk)
        if (generateAccept) {
          makingAccept = true
          AcceptStmt(indent, blk) match {
            case Some(a) => acceptfns push a
            case None    =>
          }
          makingAccept = false
        }
        false
      }
      case FenceFunction(body)   => { fencefns push MakeStmt(2)(body); false }
      case VerilogFunction(body) => { verilogfns push body; false }

      case Instantiate(id, module, args) => {
        if (!(moduleCatalogue contains module)) {
          Message.error(s"Cannot instantiate undefined module '${module}' in module '${modname}'")
        }

        if (modMap.isEmpty) {
          modMap("this") = new ModuleInstance("this", task, Nil);
        }

        val name = id + "_" + namecnt(id)
        namecnt(id) += 1

        val m = new ModuleInstance(name, moduleCatalogue(module), args);
        modMap(id) = m
        modules push m
        false
      }

      case Connect(DottedName(fromName :: fromPortName :: Nil), to) => {
        val fromInstance = modMap(fromName)
        val fromPort = if (fromName == "this") fromInstance.iwires(fromPortName) else fromInstance.owires(fromPortName)
        to foreach {
          case DottedName(toName :: toPortName :: Nil) => {
            val toInstance = modMap(toName)
            val toPort = if (toName == "this") toInstance.owires(toPortName) else toInstance.iwires(toPortName)
            // TODO: check ports have compatible control flow
            portConnections push ((fromInstance, fromPort, toInstance, toPort))
          }
        }
        false
      }

      case _: Task => true
      case _       => Message.ice("unreachable")
    }

    if (verilogfns.length > 0) {
      pw.print(StrList(verilogfns))
    }
    if (states.size > 0 || fencefns.length > 0) {
      // Start main combinatorial loop
      pw.println()
      pw.println("  always @* begin")
      pw.println("    go = 1'b1;")
      // Prepare defaults
      if (defaults.length > 0)
        pw.println(StrList(defaults))
      if (fencefns.length > 0)
        pw.println(StrList(fencefns))

      if (numstates > 1) {
        pw.println("    case (state)")
        pw.println("      default: begin")
        pw.println("      end")
        for ((n, c) <- states.toList.sortBy(_._1)) {
          pw.println(s"      ${MakeState(n)}: ${c}")
        }
        pw.println("    endcase")
      } else {
        pw.println(s"    ${states(0)}")
      }

      pw.println()
      if (clears.length > 0) {
        pw.println(s"    if (!$go) begin")
        pw.print(StrList(clears))
        pw.println("    end")
      }
      pw.println("  end")
      // Now emit clocked blocks
      pw.println()
      pw.println("  always @(posedge clk or negedge rst_n) begin")
      pw.println("    if (!rst_n) begin")
      pw.print(StrList(resets))
      pw.println("    end else begin")
      pw.println(s"      if ($go) begin")
      pw.print(StrList(clocks))
      pw.println("      end")
      pw.println("    end")
      pw.println("  end")

      if (!clocks_no_reset.isEmpty) {
        pw.println()
        pw.println(s"  always @(posedge clk) begin")
        pw.println(s"    if ($go) begin")
        pw.print(StrList(clocks_no_reset.toList))
        pw.println(s"    end")
        pw.println(s"  end")
      }
    }
    if (modMap.nonEmpty) {
      for (instance <- modules) {
        // declare all port wires
        pw.println(s"${i0}// Port wires for ${instance.name}")
        for (port <- instance.wires; signal <- port.signals) {
          pw.println(i0 + signal.declString)
        }
        pw.println()

        // instantiate
        if (instance.paramAssigns.nonEmpty) {
          val paramAssigns = instance.paramAssigns map {
            case ParamAssign(lhs, rhs) => s".${lhs}(${rhs.toVerilog})"
          }
          pw.println(s"""|${i0}${instance.task.name} #(
                         |${i0 * 2}${paramAssigns mkString s",\n${i0 * 2}"}
                         |${i0})""".stripMargin)
        } else {
          pw.println(s"${i0}${instance.task.name}")
        }

        val portAssigns = for {
          (port, wire) <- instance.ports zip instance.wires
          (Signal(portName, _, _), Signal(wireName, _, _)) <- port.signals zip wire.signals
        } yield {
          s".${portName}(${wireName})"
        }
        val miscAssigns = ".clk(clk)" :: ".rst_n(rst_n)" :: Nil
        pw.println(s"""|${i0}${instance.name} (
                       |${i0 * 2}${miscAssigns ::: portAssigns mkString s",\n${i0 * 2}"}
                       |${i0});""".stripMargin)
        pw.println()
      }

      // Assign all ports
      for ((srcInstance, srcPort, dstInstance, dstPort) <- portConnections) {
        val dstSignals = dstPort.payload map { _.name }
        val srcSignals = srcPort.payload map { _.name }
        pw.println(s"${i0}// ${srcInstance.name}.${srcPort.name} -> ${dstInstance.name}.${dstPort.name}")
        pw.print(s"${i0}assign ")
        dstSignals match {
          case sig :: Nil => pw.print(s"${sig} = ")
          case sigs => pw.print(s"""|{
                                    |${i0 * 2}${sigs mkString s",\n${i0 * 2}"}
                                    |${i0}} = """.stripMargin)
        }
        srcSignals match {
          case sig :: Nil => pw.println(s"${sig};")
          case sigs => pw.println(s"""|{
                                      |${i0 * 2}${sigs mkString s",\n${i0 * 2}"}
                                      |${i0}};""".stripMargin)
        }

        // TODO: connect flow control signals

        pw.println()
      }
      pw.println()
    }

    pw.println("endmodule")
    pw.println()
    pw.println(s"`default_nettype wire")

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
  def MakeNumBits(typ: Type): StrTree = typ match {
    case IntType(signed, size)  => Str(s"$size")
    case IntVType(signed, args) => StrList(args.map(MakeExpr), "*")
    case State                  => Str(s"$log2numstates")
    case Struct(fields)         => StrList(fields.values.toList map MakeNumBits, "+")
  }

  implicit def Decl2Typ(decl: Declaration): Type = decl match {
    case ParamDeclaration(decltype, _, _) => decltype
    case ConstDeclaration(decltype, _, _) => decltype
    case OutDeclaration(_, decltype, _)   => decltype
    case InDeclaration(_, decltype, _)    => decltype
    case VarDeclaration(decltype, _, _)   => decltype
    case VerilogDeclaration(decltype, _)  => decltype
  }

  // Return the type for an AST
  def GetType(tree: Expr): Type = {

    def LookUpField(names: List[String], kind: Type): Type = {
      val n :: ns = names
      kind match {
        case Struct(fields) => {
          if (fields contains n) {
            ns match {
              case Nil => fields(n)
              case _   => LookUpField(ns, fields(n))
            }
          } else {
            Message.fatal(s"No field named '$n' in struct '$kind'") // TODO: better error message, check earlier
          }
        }
        case _ => Message.fatal(s"Cannot find field '$n' in non-struct type '$kind'")
      }
    }

    tree match {
      case ReadCall(n)          => GetType(n)
      case DottedName(n :: Nil) => id2decl(n)
      case DottedName(n :: ns)  => LookUpField(ns, id2decl(n))
      case _ => {
        Message.fatal(s"Cannot compute type for $tree")
      }
    }
  }

  // Construct a string for an expression
  def MakeExpr(tree: Node): StrTree = {
    tree match {
      case ArrayLookup(name, index) => {
        val indices = index map MakeExpr mkString ("[", "][", "]")
        StrList(List(MakeExpr(name), Str(indices)))
      }
      case Slice(ref, l, op, r) => StrList(List(MakeExpr(ref), "[", MakeExpr(l), op, MakeExpr(r), "]"))
      case ValidCall(DottedName(names)) => id2decl(names.head) match {
        case OutDeclaration(synctype, decl, n) => if (HasValid(synctype)) valid(n) else { Message.fatal(s"Port $names does not use valid"); "" }
        case InDeclaration(synctype, decl, n)  => if (HasValid(synctype)) valid(n) else { Message.fatal(s"Port $names does not use valid"); "" }
        case _                                 => Message.fatal(s"Cannot access valid on $names"); ""
      }
      case CallExpr(name, args) => StrList(List(MakeExpr(name), "(", StrList(args.map(MakeExpr), ","), ")"))
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
      case n: Num                    => n.toVerilog
      case e                         => Message.fatal(s"Unexpected expression $e"); ""
    }
  }

  // Called with an integer representing a state, returns the appropriate string
  def MakeState(state: Int): String = s"$log2numstates'd$state"

  // This function defines how to write next values (D input on flip-flops)
  //def nx(x: String): StrTree = StrList(List(x, Str("_nxt")))

  // Produce code to go into the case statements (we assume we have already had a "case(state) default: begin end"
  // The top call should be with Function or VerilogFunction
  def MakeStmt(indent: Int)(tree: Node): StrTree = {
    val i = i0 * indent
    val si = Str(i)

    // Take a combinatorial statement and return stall conditions that should be emitted now based on ids that are read/written
    // This code will be emitted before the actual statement
    // It is useful to keep as a list of strings here so we can decide when to insert an extra begin/end block
    def StallExpr(tree: Node): List[String] = {
      // Use a local function to avoid having to copy emitted list down the stack
      var blockingStatements: List[String] = Nil

      def add(s: String): Unit = blockingStatements = s :: blockingStatements

      def AddRead(name: DottedName, isLock: Boolean): Boolean = {
        val n: String = ExtractName(name)
        val d: Declaration = id2decl(n)
        d match {
          case InDeclaration(synctype, _, _) => {
            if (HasValid(synctype))
              add(s"$go = $go && ${valid(n)};")
            if (!isLock && HasReady(synctype))
              add(ready(n) + " = 1'b1;")
          }
          case _ => Message.fatal(s"$name cannot be read"); false // TODO check this earlier?
        }
        false // No need to recurse
      }

      def v(tree: Node): Boolean = tree match {
        case CombinatorialCaseStmt(value, _, _) =>
          value visit v; false
        case CombinatorialBlock(_) => false
        case CombinatorialIf(cond, _, _) =>
          cond visit v; false
        case ReadCall(name)   => AddRead(name, false)
        case LockCall(name)   => AddRead(name, true)
        case UnlockCall(name) => AddRead(name, false)
        case WriteCall(name, _) => {
          val n: String = ExtractName(name)
          val d: Declaration = id2decl(n)
          d match {
            case OutDeclaration(synctype, _, _) => {
              synctype match {
                case SyncReadyBubble => add(s"$go = $go && !${valid(n)};")
                case SyncReady       => add(s"$go = $go && (!${valid(n)} || !${ready(n)});")
                case SyncAccept      => Message.fatal("sync accept only supported as wire output type") // TODO check this earlier
                case WireSyncAccept  => add(s"$go = $go && ${accept(n)};")
                case _               =>
              }
              if (HasValid(synctype))
                add(s"${valid(n)} = 1'b1;")
            }
            case _ => Message.fatal(s"$name cannot be written"); false // TODO check this earlier?
          }
          true // Recurse in case arguments use reads
        }
        case _ => true
      }
      tree visit v
      return blockingStatements
    }

    def AddStall(terms: Node*)(expr: StrTree): StrTree = {
      val stalls = terms.toList flatMap StallExpr
      if (stalls.isEmpty) {
        expr
      } else {
        val s = stalls map { x => Str(x) }
        Str(s"""|begin
                |${i + i0}${s mkString s"\n${i + i0}"}
                |${i + i0}${expr}
                |${i}end""".stripMargin)
      }
    }

    tree match {
      case Assign(ArrayLookup(DottedName(n :: _), index :: Nil), rhs) if (Arrays contains n) => AddStall(index, rhs) {
        Str(s"""|begin
                |${i + i0}${n}_wr = 1'b1;
                |${i + i0}${n}_addr = ${MakeExpr(index)};
                |${i + i0}${n}_wrdata = ${MakeExpr(rhs)};
                |${i}end""".stripMargin)
      }
      case Assign(lhs, rhs) => AddStall(lhs, rhs) {
        Str(s"${MakeExpr(lhs)} = ${MakeExpr(rhs)};")
      }

      case CombinatorialBlock(cmds) => {
        Str(s"""|begin
                |${i + i0}${cmds map MakeStmt(indent + 1) mkString s"\n${i + i0}"}
                |${i}end""".stripMargin)
      }

      case StateBlock(_, cmds) => MakeStmt(indent)(CombinatorialBlock(cmds))

      case CombinatorialIf(cond, thenBody, optElseBody) => AddStall(cond) {
        val condPart = s"if (${MakeExpr(cond)}) "
        val thenPart = thenBody match {
          case block: CombinatorialBlock => MakeStmt(indent)(block)
          case other                     => MakeStmt(indent)(CombinatorialBlock(other :: Nil))
        }
        val elsePart = optElseBody match {
          case None                            => ""
          case Some(block: CombinatorialBlock) => s" else ${MakeStmt(indent)(block)}"
          case Some(other)                     => s" else ${MakeStmt(indent)(CombinatorialBlock(other :: Nil))}"
        }
        Str(condPart + thenPart + elsePart)
      }

      // TODO: do the conds not need a stall expr to be precise?
      case CombinatorialCaseStmt(value, cases, None) => AddStall(value) {
        Str(s"""|case (${MakeExpr(value)})
                |${i + i0}${cases map MakeStmt(indent + 1) mkString s"\n${i + i0}"}
                |${i}endcase""".stripMargin)
      }
      case CombinatorialCaseStmt(value, cases, Some(default)) => AddStall(value) {
        Str(s"""|case (${MakeExpr(value)})
                |${i + i0}${cases map MakeStmt(indent + 1) mkString s"\n${i + i0}"}
                |${i + i0}default: ${MakeStmt(indent + 1)(default)}
                |${i}endcase""".stripMargin)
      }
      case CombinatorialCaseLabel(conds, body) => Str(s"${conds map MakeExpr mkString ", "}: ${MakeStmt(indent)(body)}")

      case GotoState(target) => {
        if (numstates == 1) {
          Str("")
        } else {
          StrList(List(nx("state"), " = ", MakeState(target), ";"))
        }
      }

      case CallState(tgt, ret) => {
        Str(s"""|begin
                |${i + i0}call_stack_wr = 1'b1;
                |${i + i0}call_stack_addr = call_depth_nxt;
                |${i + i0}call_stack_wrdata = ${MakeState(ret)};
                |${i + i0}call_depth_nxt = call_depth_nxt + 1'b1;
                |${i + i0}${nx("state")} = ${MakeState(tgt)};
                |${i}end""".stripMargin)
      }

      case ReturnState => {
        Str(s"""|begin
                |${i + i0}call_depth_nxt = call_depth_nxt - 1'b1;
                |${i + i0}${nx("state")} = call_stack[call_depth_nxt];
                |${i}end""".stripMargin)
      }

      case ExprStmt(LockCall(name))   => AddStall(name) { Str("") }
      case ExprStmt(UnlockCall(name)) => AddStall(name) { Str("") }
      case ExprStmt(WriteCall(name, arg :: Nil)) => AddStall(name, arg) {
        Str(s"${MakeExpr(name)} = ${MakeExpr(arg)};")
      }

      case DeclarationStmt(VarDeclaration(decltype, id, Some(rhs))) => MakeStmt(indent)(Assign(id, rhs))
      case DeclarationStmt(VarDeclaration(decltype, id, None)) => MakeStmt(indent)(Assign(id, Num(Some(false), None, 0))) // TODO: Why is this needed ?

      case ExprStmt(DollarCall(name, args)) => StrList(List(i, name, StrList(args.map(MakeExpr), ",")))
      case AlogicComment(s) => s"// $s\n"

      case x => Message.fatal(s"Don't know how to emit code for $x"); Str("")
    }
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
      val i = i0 * indent
      val e: List[StrTree] = expr match {
        case Some(x) => x :: Nil
        case None    => Nil
      }
      Some(StrList(Str(i) :: Str("begin\n") ::
        stalls.map(x => Str(i + x)) :::
        e :::
        Str(i) :: Str("end\n") :: Nil))
    }
  }

  def AcceptStmt(indent: Int, tree: Node): Option[StrTree] = tree match {
    case Assign(lhs, rhs) => {
      IdsWritten.add(ExtractName(lhs))
      AddAccept(indent, AcceptExpr(lhs) ::: AcceptExpr(rhs), None)
    }
    case CombinatorialCaseStmt(value, cases, Some(default)) => {
      // Take care to only use MakeExpr when we are sure the code needs to be emitted
      // This is because MakeExpr will track the used ids
      val s: List[Option[StrTree]] = for (c <- cases) yield AcceptStmt(indent + 1, c)
      val s2: List[StrTree] = (AcceptStmt(indent + 1, default) :: s).flatten
      val e = if (s2.length == 0)
        None
      else
        Some(StrList(Str(i0 * indent + "case(") :: MakeExpr(value) :: Str(") begin\n") ::
          StrList(s2) :: Str(i0 * indent) :: Str("endcase\n") :: Nil))
      AddAccept(indent, AcceptExpr(value), e)
    }
    case CombinatorialIf(cond, body, Some(elsebody)) =>
      {
        val b = AcceptStmt(indent + 1, body)
        val eb = AcceptStmt(indent + 1, elsebody)
        val gen = b.isDefined || eb.isDefined
        val bs = b match {
          case Some(a) => a
          case None    => Str(i0 * indent + "begin\n" + i0 * indent + "end\n")
        }
        val ebs = eb match {
          case Some(a) => a
          case None    => Str(i0 * indent + "begin\n" + i0 * indent + "end\n")
        }
        val e = if (gen)
          Some(StrList(Str(i0 * indent) :: Str("if (") :: MakeExpr(cond) :: Str(")\n") :: bs :: Str(i0 * indent) :: Str("else\n") :: ebs :: Nil))
        else
          None
        AddAccept(indent, AcceptExpr(cond), e)
      }
    case CombinatorialIf(cond, body, None) => {
      val b = AcceptStmt(indent + 1, body)
      // We take care to only call MakeExpr when the body would have something to generate
      // This avoids forbidding ids that are not actually important for generating accept
      val e = b match {
        case None    => None
        case Some(a) => Some(StrList(Str(i0 * indent) :: Str("if (") :: MakeExpr(cond) :: Str(")\n") :: a :: Nil))
      }
      AddAccept(indent, AcceptExpr(cond), e)
    }

    case LockCall(name)                              => AddAccept(indent, AcceptExpr(name), None)
    case UnlockCall(name)                            => AddAccept(indent, AcceptExpr(name), None)
    case WriteCall(name, args) if (args.length == 1) => AddAccept(indent, AcceptExpr(name) ::: AcceptExpr(args(0)), None)
    case CombinatorialBlock(cmds) => {
      val s: List[Option[StrTree]] = for (c <- cmds) yield AcceptStmt(indent + 1, c)
      val s2: List[StrTree] = s.flatten
      if (s2.length == 0)
        None
      else
        Some(StrList(Str(i0 * indent) :: Str("begin\n") :: StrList(s2) :: Str(i0 * indent) :: Str("end\n") :: Nil))
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
        Some(StrList(List(i0 * (indent - 1), MakeState(state), ": begin\n", StrList(s2), i0 * (indent - 1), "end\n")))
      } else
        None
    }
    case CombinatorialCaseLabel(Nil, body) => {
      val b = AcceptStmt(indent + 1, body)
      b match {
        case None    => None
        case Some(a) => Some(StrList(Str(i0 * indent) :: Str("default:\n") :: a :: Nil))
      }
    }
    case CombinatorialCaseLabel(conds, body) => {
      val b = AcceptStmt(indent + 1, body)
      val e = b match {
        case None => None
        case Some(a) => Some(StrList(
          Str(i0 * indent) ::
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
  def AcceptExpr(tree: Node): List[String] = {
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

    def v(tree: Node): Boolean = tree match {
      case CombinatorialCaseStmt(value, _, _) =>
        value visit v; false
      case CombinatorialBlock(_) => false
      case CombinatorialIf(cond, _, _) =>
        cond visit v; false
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
    tree visit v
    return blockingStatements
  }

}
